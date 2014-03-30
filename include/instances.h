#ifndef BODY1
#define BODY1(x) x
#endif

#ifndef BODY2
#define BODY2(x,y) (x,y)
#endif

instance BODY2(Eq a, Num a) => Eq (HEAD) where
  (==)          = discrete2 (==)

instance BODY2(Ord a, Num a) => Ord (HEAD) where
  compare       = discrete2 compare

instance BODY2(Bounded a, Num a) => Bounded (HEAD) where
  maxBound      = auto maxBound
  minBound      = auto minBound

instance BODY1(Num a) => Num (HEAD) where
  fromInteger 0  = zero
  fromInteger n = auto (fromInteger n)
  (+)          = (<+>) -- binary (+) one one
  (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
  (*)          = lift2 (*) (\x y -> (y, x))
  negate       = lift1 negate (const (auto (-1)))
  abs          = lift1 abs signum
  signum a     = lift1 signum (const zero) a

instance BODY1(Fractional a) => Fractional (HEAD) where
  fromRational 0 = zero
  fromRational r = auto (fromRational r)
  x / y        = x * recip y
  recip        = lift1_ recip (const . negate . join (*))

instance BODY1(Floating a) => Floating (HEAD) where
  pi       = auto pi
  exp      = lift1_ exp const
  log      = lift1 log recip
  logBase x y = log y / log x
  sqrt     = lift1_ sqrt (\z _ -> recip (auto 2 * z))
  (**)     = (<**>)
  --x ** y
  --   | isKnownZero y     = 1
  --   | isKnownConstant y, y' <- primal y = lift1 (** y') ((y'*) . (**(y'-1))) x
  --   | otherwise         = lift2_ (**) (\z xi yi -> (yi * z / xi, z * log1 xi)) x y
  sin      = lift1 sin cos
  cos      = lift1 cos $ negate . sin
  tan      = lift1 tan $ recip . join (*) . cos
  asin     = lift1 asin $ \x -> recip (sqrt (auto 1 - join (*) x))
  acos     = lift1 acos $ \x -> negate (recip (sqrt (one - join (*) x)))
  atan     = lift1 atan $ \x -> recip (one + join (*) x)
  sinh     = lift1 sinh cosh
  cosh     = lift1 cosh sinh
  tanh     = lift1 tanh $ recip . join (*) . cosh
  asinh    = lift1 asinh $ \x -> recip (sqrt (one + join (*) x))
  acosh    = lift1 acosh $ \x -> recip (sqrt (join (*) x - one))
  atanh    = lift1 atanh $ \x -> recip (one - join (*) x)

instance BODY2(Num a, Enum a) => Enum (HEAD) where
  succ                 = lift1 succ (const one)
  pred                 = lift1 pred (const one)
  toEnum               = auto . toEnum
  fromEnum             = discrete1 fromEnum
  enumFrom a           = withPrimal a <$> enumFrom (primal a)
  enumFromTo a b       = withPrimal a <$> discrete2 enumFromTo a b
  enumFromThen a b     = zipWith (fromBy a delta) [0..] $ discrete2 enumFromThen a b where delta = b - a
  enumFromThenTo a b c = zipWith (fromBy a delta) [0..] $ discrete3 enumFromThenTo a b c where delta = b - a

instance BODY1(Real a) => Real (HEAD) where
  toRational      = discrete1 toRational

instance BODY1(RealFloat a) => RealFloat (HEAD) where
  floatRadix      = discrete1 floatRadix
  floatDigits     = discrete1 floatDigits
  floatRange      = discrete1 floatRange
  decodeFloat     = discrete1 decodeFloat
  encodeFloat m e = auto (encodeFloat m e)
  isNaN           = discrete1 isNaN
  isInfinite      = discrete1 isInfinite
  isDenormalized  = discrete1 isDenormalized
  isNegativeZero  = discrete1 isNegativeZero
  isIEEE          = discrete1 isIEEE
  exponent = exponent
  scaleFloat n = unary (scaleFloat n) (scaleFloat n one)
  significand x =  unary significand (scaleFloat (- floatDigits x) one) x
  atan2 = lift2 atan2 $ \vx vy -> let r = recip (join (*) vx + join (*) vy) in (vy * r, negate vx * r)

instance BODY1(RealFrac a) => RealFrac (HEAD) where
  properFraction a = (w, a `withPrimal` pb) where
      pa = primal a
      (w, pb) = properFraction pa
  truncate = discrete1 truncate
  round    = discrete1 round
  ceiling  = discrete1 ceiling
  floor    = discrete1 floor

instance BODY1(Erf a) => Erf (HEAD) where
  erf = lift1 erf $ \x -> (fromInteger 2 / sqrt pi) * exp (negate x * x)
  erfc = lift1 erfc $ \x -> (fromInteger (-2) / sqrt pi) * exp (negate x * x)
  normcdf = lift1 normcdf $ \x -> (fromInteger (-1) / sqrt pi) * exp (x * x * fromRational (- recip 2) / sqrt (fromInteger 2))

instance BODY1(InvErf a) => InvErf (HEAD) where
  inverf = lift1 inverfc $ \x -> recip $ (fromInteger 2 / sqrt pi) * exp (negate x * x)
  inverfc = lift1 inverfc $ \x -> recip $ negate (fromInteger 2 / sqrt pi) * exp (negate x * x)
  invnormcdf = lift1 invnormcdf $ \x -> recip $ (fromInteger (-1) / sqrt pi) * exp (x * x * fromRational (- recip 2) / sqrt (fromInteger 2))
