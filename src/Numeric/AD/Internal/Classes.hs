{-# LANGUAGE Rank2Types, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, PatternGuards, CPP #-}
{-# LANGUAGE FlexibleContexts, FunctionalDependencies, UndecidableInstances, GeneralizedNewtypeDeriving, TemplateHaskell #-}
-- {-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.AD.Internal.Classes
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-----------------------------------------------------------------------------

module Numeric.AD.Internal.Classes
    (
    -- * AD modes
      Mode(..)
    , one
    -- * Automatically Deriving AD
    , Jacobian(..)
    , Primal(..)
    , deriveLifted
    , deriveNumeric
    , Lifted(..)
    , Iso(..)
    ) where

import Control.Applicative hiding ((<**>))
import Data.Char
import Language.Haskell.TH
import Numeric.AD.Internal.Combinators (on)

infixr 8 **!, <**>
infixl 7 *!, /!, ^*, *^, ^/
infixl 6 +!, -!, <+>
infix 4 ==!

class Iso a b where
    iso :: f a -> f b
    osi :: f b -> f a

instance Iso a a where
    iso = id
    osi = id

class Lifted t where
    showsPrec1          :: (Num a, Show a) => Int -> t a -> ShowS
    (==!)               :: (Num a, Eq a) => t a -> t a -> Bool
    compare1            :: (Num a, Ord a) => t a -> t a -> Ordering
    fromInteger1        :: Num a => Integer -> t a
    (+!),(-!),(*!)      :: Num a => t a -> t a -> t a
    negate1, abs1, signum1 :: Num a => t a -> t a
    (/!)                :: Fractional a => t a -> t a -> t a
    recip1              :: Fractional a => t a -> t a
    fromRational1       :: Fractional a => Rational -> t a
    toRational1         :: Real a => t a -> Rational -- unsafe
    pi1                 :: Floating a => t a
    exp1, log1, sqrt1   :: Floating a => t a -> t a
    (**!), logBase1     :: Floating a => t a -> t a -> t a
    sin1, cos1, tan1, asin1, acos1, atan1 :: Floating a => t a -> t a
    sinh1, cosh1, tanh1, asinh1, acosh1, atanh1 :: Floating a => t a -> t a
    properFraction1 :: (RealFrac a, Integral b) => t a -> (b, t a)
    truncate1, round1, ceiling1, floor1 :: (RealFrac a, Integral b) => t a -> b
    floatRadix1     :: RealFloat a => t a -> Integer
    floatDigits1    :: RealFloat a => t a -> Int
    floatRange1     :: RealFloat a => t a -> (Int, Int)
    decodeFloat1    :: RealFloat a => t a -> (Integer, Int)
    encodeFloat1    :: RealFloat a => Integer -> Int -> t a
    exponent1       :: RealFloat a => t a -> Int
    significand1    :: RealFloat a => t a -> t a
    scaleFloat1     :: RealFloat a => Int -> t a -> t a
    isNaN1, isInfinite1, isDenormalized1, isNegativeZero1, isIEEE1 :: RealFloat a => t a -> Bool
    atan21          :: RealFloat a => t a -> t a -> t a
    succ1, pred1    :: (Num a, Enum a) => t a -> t a
    toEnum1         :: (Num a, Enum a) => Int -> t a
    fromEnum1       :: (Num a, Enum a) => t a -> Int
    enumFrom1       :: (Num a, Enum a) => t a -> [t a]
    enumFromThen1   :: (Num a, Enum a) => t a -> t a -> [t a]
    enumFromTo1     :: (Num a, Enum a) => t a -> t a -> [t a]
    enumFromThenTo1 :: (Num a, Enum a) => t a -> t a -> t a -> [t a]
    minBound1       :: (Num a, Bounded a) => t a
    maxBound1       :: (Num a, Bounded a) => t a

class Lifted t => Mode t where
    -- | allowed to return False for items with a zero derivative, but we'll give more NaNs than strictly necessary
    isKnownConstant :: t a -> Bool
    isKnownConstant _ = False

    -- | allowed to return False for zero, but we give more NaN's than strictly necessary then
    isKnownZero :: Num a => t a -> Bool
    isKnownZero _ = False

    -- | Embed a constant
    lift  :: Num a => a -> t a

    -- | Vector sum
    (<+>) :: Num a => t a -> t a -> t a

    -- | Scalar-vector multiplication
    (*^) :: Num a => a -> t a -> t a

    -- | Vector-scalar multiplication
    (^*) :: Num a => t a -> a -> t a

    -- | Scalar division
    (^/) :: Fractional a => t a -> a -> t a

    -- | Exponentiation, this should be overloaded if you can figure out anything about what is constant!
    (<**>) :: Floating a => t a -> t a -> t a
--  x <**> y = lift2_ (**) (\z xi yi -> (yi *! z /! xi, z *! log1 xi)) x y

    -- | > 'zero' = 'lift' 0
    zero :: Num a => t a

    a *^ b = lift a *! b
    a ^* b = a *! lift b

    a ^/ b = a ^* recip b

    zero = lift 0

one :: (Mode t, Num a) => t a
one = lift 1
{-# INLINE one #-}

negOne :: (Mode t, Num a) => t a
negOne = lift (-1)
{-# INLINE negOne #-}

-- | 'Primal' is used by 'deriveMode' but is not exposed
-- via the 'Mode' class to prevent its abuse by end users
-- via the AD data type.
--
-- It provides direct access to the result, stripped of its derivative information,
-- but this is unsafe in general as (lift . primal) would discard derivative
-- information. The end user is protected from accidentally using this function
-- by the universal quantification on the various combinators we expose.

class Primal t where
    primal :: Num a => t a -> a

-- | 'Jacobian' is used by 'deriveMode' but is not exposed
-- via 'Mode' to prevent its abuse by end users
-- via the 'AD' data type.
class (Mode t, Mode (D t)) => Jacobian t where
    type D t :: * -> *

    unary  :: Num a => (a -> a) -> D t a -> t a -> t a
    lift1  :: Num a => (a -> a) -> (D t a -> D t a) -> t a -> t a
    lift1_ :: Num a => (a -> a) -> (D t a -> D t a -> D t a) -> t a -> t a

    binary :: Num a => (a -> a -> a) -> D t a -> D t a -> t a -> t a -> t a
    lift2  :: Num a => (a -> a -> a) -> (D t a -> D t a -> (D t a, D t a)) -> t a -> t a -> t a
    lift2_ :: Num a => (a -> a -> a) -> (D t a -> D t a -> D t a -> (D t a, D t a)) -> t a -> t a -> t a

withPrimal :: (Jacobian t, Num a) => t a -> a -> t a
withPrimal t a = unary (const a) one t
{-# INLINE withPrimal #-}

fromBy :: (Jacobian t, Num a) => t a -> t a -> Int -> a -> t a
fromBy a delta n x = binary (\_ _ -> x) one (fromIntegral1 n) a delta

fromIntegral1 :: (Integral n, Lifted t, Num a) => n -> t a
fromIntegral1 = fromInteger1 . fromIntegral
{-# INLINE fromIntegral1 #-}

square1 :: (Lifted t, Num a) => t a -> t a
square1 x = x *! x
{-# INLINE square1 #-}

discrete1 :: (Primal t, Num a) => (a -> c) -> t a -> c
discrete1 f x = f (primal x)
{-# INLINE discrete1 #-}

discrete2 :: (Primal t, Num a) => (a -> a -> c) -> t a -> t a -> c
discrete2 f x y = f (primal x) (primal y)
{-# INLINE discrete2 #-}

discrete3 :: (Primal t, Num a) => (a -> a -> a -> d) -> t a -> t a -> t a -> d
discrete3 f x y z = f (primal x) (primal y) (primal z)
{-# INLINE discrete3 #-}

-- | @'deriveLifted' t@ provides
--
-- > instance Lifted $t
--
-- given supplied instances for
--
-- > instance Lifted $t => Primal $t where ...
-- > instance Lifted $t => Jacobian $t where ...
--
-- The seemingly redundant @'Lifted' $t@ constraints are caused by Template Haskell staging restrictions.
deriveLifted :: ([Q Pred] -> [Q Pred]) -> Q Type -> Q [Dec]
deriveLifted f _t = do
        [InstanceD cxt0 type0 dec0] <- lifted
        return <$> instanceD (cxt (f (return <$> cxt0))) (return type0) (return <$> dec0)
    where
      lifted = [d|
       instance Lifted $_t where
        (==!)         = (==) `on` primal
        compare1      = compare `on` primal
        maxBound1     = lift maxBound
        minBound1     = lift minBound
        showsPrec1 d  = showsPrec d . primal
        fromInteger1 0 = zero
        fromInteger1 n = lift (fromInteger n)
        (+!)          = (<+>) -- binary (+) one one
        (-!)          = binary (-) one negOne -- TODO: <-> ? as it is, this might be pretty bad for Tower
        (*!)          = lift2 (*) (\x y -> (y, x))
        negate1       = lift1 negate (const negOne)
        abs1          = lift1 abs signum1
        signum1       = lift1 signum (const zero)
        fromRational1 0 = zero
        fromRational1 r = lift (fromRational r)
        x /! y        = x *! recip1 y
        recip1        = lift1_ recip (const . negate1 . square1)
        pi1       = lift pi
        exp1      = lift1_ exp const
        log1      = lift1 log recip1
        logBase1 x y = log1 y /! log1 x
        sqrt1     = lift1_ sqrt (\z _ -> recip1 (lift 2 *! z))
        (**!)     = (<**>)
        --x **! y
        --   | isKnownZero y     = 1
        --   | isKnownConstant y, y' <- primal y = lift1 (** y') ((y'*) . (**(y'-1))) x
        --   | otherwise         = lift2_ (**) (\z xi yi -> (yi *! z /! xi, z *! log1 xi)) x y
        sin1      = lift1 sin cos1
        cos1      = lift1 cos $ negate1 . sin1
        tan1 x    = sin1 x /! cos1 x
        asin1     = lift1 asin $ \x -> recip1 (sqrt1 (one -! square1 x))
        acos1     = lift1 acos $ \x -> negate1 (recip1 (sqrt1 (one -! square1 x)))
        atan1     = lift1 atan $ \x -> recip1 (one +! square1 x)
        sinh1     = lift1 sinh cosh1
        cosh1     = lift1 cosh sinh1
        tanh1 x   = sinh1 x /! cosh1 x
        asinh1    = lift1 asinh $ \x -> recip1 (sqrt1 (one +! square1 x))
        acosh1    = lift1 acosh $ \x -> recip1 (sqrt1 (square1 x -! one))
        atanh1    = lift1 atanh $ \x -> recip1 (one -! square1 x)

        succ1                 = lift1 succ (const one)
        pred1                 = lift1 pred (const one)
        toEnum1               = lift . toEnum
        fromEnum1             = discrete1 fromEnum
        enumFrom1 a           = withPrimal a <$> discrete1 enumFrom a
        enumFromTo1 a b       = withPrimal a <$> discrete2 enumFromTo a b
        enumFromThen1 a b     = zipWith (fromBy a delta) [0..] $ discrete2 enumFromThen a b where delta = b -! a
        enumFromThenTo1 a b c = zipWith (fromBy a delta) [0..] $ discrete3 enumFromThenTo a b c where delta = b -! a

        toRational1      = discrete1 toRational
        floatRadix1      = discrete1 floatRadix
        floatDigits1     = discrete1 floatDigits
        floatRange1      = discrete1 floatRange
        decodeFloat1     = discrete1 decodeFloat
        encodeFloat1 m e = lift (encodeFloat m e)
        isNaN1           = discrete1 isNaN
        isInfinite1      = discrete1 isInfinite
        isDenormalized1  = discrete1 isDenormalized
        isNegativeZero1  = discrete1 isNegativeZero
        isIEEE1          = discrete1 isIEEE
        exponent1 = exponent . primal
        scaleFloat1 n = unary (scaleFloat n) (scaleFloat1 n one)
        significand1 x =  unary significand (scaleFloat1 (- floatDigits1 x) one) x
        atan21 = lift2 atan2 $ \vx vy -> let r = recip1 (square1 vx +! square1 vy) in (vy *! r, negate1 vx *! r)
        properFraction1 a = (w, a `withPrimal` pb) where
             pa = primal a
             (w, pb) = properFraction pa
        truncate1 = discrete1 truncate
        round1    = discrete1 round
        ceiling1  = discrete1 ceiling
        floor1    = discrete1 floor |]

varA :: Q Type
varA = varT (mkName "a")

-- | Find all the members defined in the 'Lifted' data type
liftedMembers :: Q [String]
liftedMembers = do
#ifdef OldClassI
    ClassI (ClassD _ _ _ _ ds) <- reify ''Lifted
#else
    ClassI (ClassD _ _ _ _ ds) _ <- reify ''Lifted
#endif
    return [ nameBase n | SigD n _ <- ds]

-- | @'deriveNumeric' f g@ provides the following instances:
--
-- > instance ('Lifted' $f, 'Num' a, 'Enum' a) => 'Enum' ($g a)
-- > instance ('Lifted' $f, 'Num' a, 'Eq' a) => 'Eq' ($g a)
-- > instance ('Lifted' $f, 'Num' a, 'Ord' a) => 'Ord' ($g a)
-- > instance ('Lifted' $f, 'Num' a, 'Bounded' a) => 'Bounded' ($g a)
--
-- > instance ('Lifted' $f, 'Show' a) => 'Show' ($g a)
-- > instance ('Lifted' $f, 'Num' a) => 'Num' ($g a)
-- > instance ('Lifted' $f, 'Fractional' a) => 'Fractional' ($g a)
-- > instance ('Lifted' $f, 'Floating' a) => 'Floating' ($g a)
-- > instance ('Lifted' $f, 'RealFloat' a) => 'RealFloat' ($g a)
-- > instance ('Lifted' $f, 'RealFrac' a) => 'RealFrac' ($g a)
-- > instance ('Lifted' $f, 'Real' a) => 'Real' ($g a)
deriveNumeric :: ([Q Pred] -> [Q Pred]) -> Q Type -> Q [Dec]
deriveNumeric f t = do
    members <- liftedMembers
    let keep n = nameBase n `elem` members
    xs <- lowerInstance keep ((classP ''Num [varA]:) . f) t `mapM` [''Enum, ''Eq, ''Ord, ''Bounded, ''Show]
    ys <- lowerInstance keep f                            t `mapM` [''Num, ''Fractional, ''Floating, ''RealFloat,''RealFrac, ''Real]
    return (xs ++ ys)

lowerInstance :: (Name -> Bool) -> ([Q Pred] -> [Q Pred]) -> Q Type -> Name -> Q Dec
lowerInstance p f t n = do
#ifdef OldClassI
    ClassI (ClassD _ _ _ _ ds) <- reify n
#else
    ClassI (ClassD _ _ _ _ ds) _ <- reify n
#endif
    instanceD (cxt (f [classP n [varA]]))
              (conT n `appT` (t `appT` varA))
              (concatMap lower1 ds)
    where
        lower1 :: Dec -> [Q Dec]
        lower1 (SigD n' _) | p n'' = [valD (varP n') (normalB (varE n'')) []] where n'' = primed n'
        lower1 _          = []

        primed n' = mkName $ base ++ [prime]
            where
                base = nameBase n'
                h = head base
                prime | isSymbol h || h `elem` "/*-<>" = '!'
                      | otherwise = '1'
