{-# LANGUAGE TypeOperators, TemplateHaskell, ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Stream
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- A cofree comonad/f-branching stream  for use in returning towers of gradients. 
--
-----------------------------------------------------------------------------

module Numeric.AD.Stream 
    ( (:>)(..)
    , Comonad(..)
    , unfold
    , tails
    ) where

import Control.Applicative
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Numeric.AD.Classes
import Numeric.AD.Internal
import Language.Haskell.TH

infixl 3 :<, :>

class Functor f => Comonad f where
    extract :: (f :> a) -> a
    duplicate :: (f :> a) -> (f :> (f :> a))
    extend :: ((f :> a) -> b) -> (f :> a) -> (f :> b)

data (f :> a) = a :< f (f :> a)

instance Functor f => Functor ((:>)f) where
    fmap f (a :< as) = f a :< fmap f <$> as

instance Functor f => Comonad ((:>) f) where
    extract (a :< _) = a
    duplicate aas@(_ :< as) = aas :< duplicate <$> as
    extend f aas@(_ :< as) = f aas :< extend f <$> as

instance Foldable f => Foldable ((:>) f) where
    foldMap f (a :< as) = f a `mappend` foldMap (foldMap f) as

instance Traversable f => Traversable ((:>) f) where
    traverse f (a :< as) = (:<) <$> f a <*> traverse (traverse f) as

-- tails of the f-branching stream comonad/cofree comonad
tails :: (f :> a) -> f (f :> a)
tails (_ :< as) = as

unfold :: Functor f => (a -> (b, f a)) -> a -> (f :> b)
unfold f a = h :< unfold f <$> t 
    where
        (h, t) = f a

instance Primal ((:>) f) where
    primal (a :< _) = a

instance Mode f => Mode ((:>) f) where
    lift a = as
        where as = a :< lift as
    (a :< as) <+> (b :< bs) = (a + b) :< (as <+> bs)
    a *^ (b :< bs) = (a * b) :< (lift a *^ bs)
    (a :< as) ^* b = (a * b) :< (as ^* lift b)
    (a :< as) ^/ b = (a / b) :< (as ^/ lift b)

instance Mode f => Lifted ((:>) f) where
    showsPrec1 n (a :< _) = showsPrec n a
    (==!) = (==) `on` primal
    compare1 = compare `on` primal
    fromInteger1 a = fromInteger a :< fromInteger1 a
    (a :< as) +! (b :< bs) = (a + b) :< (as +! bs)
    (a :< as) -! (b :< bs) = (a - b) :< (as -! bs)
    (a :< as) *! (b :< bs) = (a * b) :< (as *! bs)
    negate1 (a :< as) = negate a :< negate1 as
    abs1 (a :< as) = abs a :< abs1 as
    signum1 (a :< as) = signum a :< signum1 as
    (a :< as) /! (b :< bs) = (a / b) :< (as /! bs)
    recip1 (a :< as) = recip a :< recip1 as
    fromRational1 n = fromRational n :< fromRational1 n
    toRational1 = toRational . primal
    pi1 = pi :< pi1
    exp1 (a :< as) = exp a :< exp1 as
    log1 (a :< as) = log a :< log1 as
    sqrt1 (a :< as) = sqrt a :< sqrt1 as
    (a :< as) **! (b :< bs) = (a ** b) :< (as **! bs)
    logBase1 (a :< as) (b :< bs) = logBase a b :< logBase1 as bs
    sin1 (a :< as) = sin a :< sin1 as
    cos1 (a :< as) = cos a :< cos1 as
    tan1 (a :< as) = tan a :< tan1 as
    asin1 (a :< as) = asin a :< asin1 as
    acos1 (a :< as) = acos a :< acos1 as
    atan1 (a :< as) = atan a :< atan1 as
    sinh1 (a :< as) = sinh a :< sinh1 as
    cosh1 (a :< as) = cosh a :< cosh1 as
    tanh1 (a :< as) = tanh a :< tanh1 as
    asinh1 (a :< as) = asinh a :< asinh1 as
    acosh1 (a :< as) = acosh a :< acosh1 as
    atanh1 (a :< as) = atanh a :< atanh1 as
    properFraction1 (a :< as) = (b, c :< cs) 
        where
            (b, c) = properFraction a
            (_ :: Int, cs) = properFraction1 as
    truncate1 = truncate . primal
    round1 = round . primal
    ceiling1 = ceiling . primal 
    floor1  = floor . primal 
    floatRadix1 = floatRadix . primal
    floatDigits1 = floatDigits . primal
    floatRange1 = floatRange . primal
    decodeFloat1 = decodeFloat . primal
    encodeFloat1 m e = encodeFloat m e :< encodeFloat1 m e
    exponent1 = exponent . primal 
    significand1 (a :< as) = significand a :< significand1 as
    scaleFloat1 n (a :< as) = scaleFloat n a :< scaleFloat1 n as
    isNaN1 = isNaN . primal 
    isInfinite1 = isInfinite . primal
    isDenormalized1 = isDenormalized . primal 
    isNegativeZero1 = isNegativeZero . primal 
    isIEEE1 = isIEEE . primal 
    atan21 (a :< as) (b :< bs) = atan2 a b :< atan21 as bs
    succ1 (a :< as) = succ a :< succ1 as
    pred1 (a :< as) = pred a :< pred1 as
    toEnum1 n = toEnum n :< toEnum1 n
    fromEnum1 = fromEnum . primal
    enumFrom1 = error "TODO"
    enumFromThen1 = error "TODO"
    enumFromTo1 = error "TODO"
    enumFromThenTo1 = error "TODO"
    minBound1 = minBound :< minBound1
    maxBound1 = maxBound :< maxBound1
    -- TODO:


-- instance (Mode f, Foo a) => Foo ((:>) f) ...
deriveNumeric 
    (classP (mkName "Mode") [varT $ mkName "f"]:) 
    (conT (mkName ":>") `appT` varT (mkName "f")) 
