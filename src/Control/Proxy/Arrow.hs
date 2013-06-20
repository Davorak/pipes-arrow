{-# language NoMonomorphismRestriction #-}
{-# language TypeFamilies #-}
{-# language FlexibleInstances #-}
{-# language RankNTypes #-}
{-# language FlexibleContexts #-}
{-# language Arrows #-}
{-# language DoAndIfThenElse #-} -- needed to get cabalbuild to work
                                 -- Works fine compiling with ghc without.

-- {-# language ScopedTypeVariables #-} -- for ArrowApply
-- {-# language UndecidableInstances #-} -- yuck? needed for arrow choice(?)
                                         -- at least for now

module Control.Proxy.Arrow where


import Control.Proxy
import Control.Proxy.Trans.State
import Control.Proxy.Trans.Writer

import Prelude hiding (id, (.))

import Control.Arrow
import Control.Category

-- for ArrowPlus?
-- import Data.Monoid

{- | Proxy equivilent of arrow's first
 - 'view' gets the fst element of the tuple and stores the snd element in the
 - state monad.
 -
 - set retrives element in the statemonad and  the effected first element, forms
 - a 2-tuple and sends it downstream.
 -}
pFirstD'
  :: (Monad m, Proxy p) =>
     (a1 -> p a' b1 a1 b m r) -> a1 -> p a' (b1, a) a1 (b, a) m r
pFirstD' p = evalStateK Nothing $ viewFirst >-> liftP . p >-> setFirst
  where
    viewFirst x = do
        (e1, e2) <- request x
        put $ Just e2
        x2 <- respond e1
        viewFirst x2

    setFirst x = do
        a <- request x
        Just s <- get
        x2 <- respond (a, s)
        setFirst x2

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

{- | Proxy equivilent of arrow's second.
 -
 - Works the same way as pFirstD' but with the tuple elements swaped.
 - pSecondD' a === arr swap >>> pFirstD' a >>> arr swap
 -}
pSecondD'
  :: (Monad m, Proxy p) =>
     (a1 -> p a' b1 a1 b m r) -> a1 -> p a' (a, b1) a1 (a, b) m r
pSecondD' p = evalStateK Nothing $ viewSecond >-> liftP . p >-> setSecond
  where
    viewSecond x = do
        (e1, e2) <- request x
        put $ Just e1
        x2 <- respond e2
        viewSecond x2

    setSecond x = do
        a <- request x
        Just s <- get
        x2 <- respond (s, a)
        setSecond x2

-- Alternate definition, will not be needed in Pipes-4.0
-- newtype  PArrowD p a' m r a b = PArrowD { runPArrowD :: (Proxy p, Monad m) => a' -> IdentityP p a' a a' b m r }

-- | Using IdentityP means that runIdentityK is required but it makes the type signatures considerably
-- better.
-- runPArrowD' = runIdentityK . runPArrowD

newtype  PArrowD p a' m r a b = PArrowD { runPArrowD :: (Proxy p, Monad m) => a' -> p a' a a' b m r }

instance (Proxy p, Monad m) => Category (PArrowD p a' m r) where
    id = PArrowD pull
    (PArrowD p1) . (PArrowD p2) = PArrowD $ p1 <-< p2

{- |
 - >>> runProxy $ fromListS [1..5] >-> runPArrowD ( arr (*2) &&& arr (+1) ) >-> printD
 - (2,2)
 - (4,3)
 - (6,4)
 - (8,5)
 - (10,6)
 -}
instance  (Proxy p, Monad m) => Arrow (PArrowD p a' m r) where
    arr f = PArrowD $ mapD f
    first (PArrowD p) = PArrowD $ pFirstD' p

{- | Just copy Arrow choice implmentation from Proxy.Prelude
 -}
instance (Proxy p, Monad m) => ArrowChoice (PArrowD p a' m r) where
    left (PArrowD p) = PArrowD $ leftD p


-- instance (Proxy p, Monad m, Arrow x, x ~ (PArrowD p y m z), (x y z) ~ (PArrowD p y m z y z) ) => ArrowApply (x (x y z, y) z) where
--    app = PArrowD proxyApplyD'

{- | This mostly works as you would expect app to for PArrowD
 -}
appD = arrowApplyD'

proxyApplyD = go where
    go x = do
        (f, x) <- request x
        fx <-  unitD >-> fromListS [x] >-> f >-> returnDownStream $ x
--        fx <-  myFromListS [x] >-> f >-> returnDownStream $ x
        x2 <- respond $ fx
        go x2

arrowApplyD' =  PArrowD proxyApplyD' -- :: (Arrow x, x ~ (PArrowD p y m z), (x y z) ~ (PArrowD p y m z y z) ) => x (x y z, y) z

proxyApplyD' = go where
    go x = do
        (f, x) <- request x
        fx <-  myFromListS [x] >-> runPArrowD f >-> returnDownStream $ x
--        fx <-  myFromListS [x] >-> runPArrowD f >-> request $ x
        x2 <- respond $ fx
        go x2

returnDownStream = runIdentityK go where
    go x = do
        a <- request x
        return a


{- |
 - >>> runProxy $ fromListS [1..5] >-> runPArrowD ( addA (arr (*2)) (arr (+1))) >-> printD
 - 3
 - 6
 - 9
 - 12
 - 15
 -}
addA f g = proc x -> do
                y <- f -< x
                q <- g -< x
                returnA -< y + x

{- does not work
 -  Not in scope: `f'
applyA = proc (f, x) -> do
                fx <- f -< x
                returnA -< fx
-}

addTestA f g = proc x -> do
                y <- f -< x
                if even y
                then returnA -< y
                else do
                    q <- g -< x
                    returnA -< y + x

{- |
 - >>> runProxy $ fromListS [Left 1, Right 1] >-> runPArrowD (testCase (arr (*2)) (arr (*3))) >-> printD
 - 2
 - 3
 -}
testCase lp rp = proc x -> do
    case x of
        Left l -> do
            y <- lp -< l
            returnA -< y
        Right r -> do
            y <- rp -< r
            returnA -< y


-- | Not correct ArrowLoop instance, Laws not checked, still might be usefull though
instance  (Proxy p, Monad m) => ArrowLoop (PArrowD p a' m r) where
    loop (PArrowD p) = PArrowD $ (pLoop undefined p)

{- |
 - > runProxy $ (fromListS [1,1..]) >->  pLoop 1 (mapD (uncurry (+)) >-> mapD (id&&&id)) >-> printD >-> takeB 10
 - 2
 - 3
 - 4
 - 5
 - 6
 - 7
 - 8
 - 9
 - 10
 - 11
 -}
pLoop'' p = pLoop mempty p
pLoop init p = evalStateK init $ pairWith >-> liftP . p >-> getResult
  where
    pairWith = go where
        go x = do
            a <- request x
            s <- get
            x2 <- respond (a, s)
            go x2
    getResult = go where
        go x = do
            (a, s) <- request x
            put s
            x2 <- respond a
            go x2
pLoop' init p = evalStateK init $ pairWith >-> liftP . (printD >-> p) >-> getResult
  where
    pairWith = go where
        go x = do
            a <- request x
            s <- get
            x2 <- respond (a, s)
            go x2
    getResult = go where
        go x = do
            (a, s) <- request x
            put s
            x2 <- respond a
            go x2

{- | I currently do not have a funciton like delay so the state hase to be
 - initilized externally which is acttually  more like a real circuit.
 - >>>  runProxy $ (fromListS $ take 15 . cycle $ replicate 5 False ++ [True]) >-> runPArrowD' arrowCounter >-> printD
 - 0
 - 1
 - 2
 - 3
 - 4
 - 5
 - 0
 - 1
 - 2
 - 3
 - 4
 - 5
 - 0
 - 1
 - 2
 -}

aCInit n = PArrowD init where
    init x = do
        a  <- request x
        x2 <- respond n
        pull x2


aDelay n = aCInit n

arrowCounter = proc reset -> do
    rec
        output <- aDelay 0 -< next
        next   <- returnA -< if reset then 0 else output + 1
    returnA -< output


myFromListS xs = \_ -> foldr (\e a -> respond e ?>= \_ -> a) (return_P undefined) xs

-------- Broken code for ArrowPlus and ArrowZero

-- | I had to think a while for how the arrow zero and arrow plus should be organized.
-- I decided to mimic net wires implmentation.
-- "yes" . for 3 <|> "no" -- (<|>) === mplus === <+>
-- This net wries stays yes for 3 seconds then goes to no forever.
-- This is the same way a proxy acts with (>=>) so that is what I will use to implment it.

{-
 -
{-# language UndecidableInstances #-} -- for arrow zero
 - >>> runProxy $ fromListS [1..5] >-> runPArrowD zeroArrow >-> printD
instance (Proxy p, Monad m, a' ~ r) => ArrowZero (PArrowD p a' m r) where
    zeroArrow = PArrowD $ return_P
 -}


-- instance (Proxy p, ArrowZero (PArrowD p a' m r)) => ArrowPlus (PArrowD p a' m r) where

-- instance (Proxy p, Monad m, ArrowZero (PArrowD p a' m r), a' ~ r) => ArrowPlus (PArrowD p a' m r) where
-- I no longer think <+> matches >=> since p1 can consume imput and it is stolean
-- from p2.  <+> actually seems to match <|>
--    (PArrowD p1) <+> (PArrowD p2) = PArrowD $ p1 >=> p2
