#ifndef BODY1
#define BODY1(x) x =>
#endif

#ifndef BODY2
#define BODY2(x,y) (x,y) =>
#endif

instance BODY2(Num a, Eq a) Eq (HEAD) where
  a == b = primal a == primal b

instance BODY2(Num a, Ord a) Ord (HEAD) where
  compare a b = compare (primal a) (primal b)

#ifndef NO_Bounded
instance BODY2(Num a, Bounded a) Bounded (HEAD) where
  maxBound = auto maxBound
  minBound = auto minBound
#endif

instance BODY1(Num a) Num (HEAD) where
  fromInteger 0  = zero
  fromInteger n = auto (fromInteger n)
  (+)          = (<+>) -- binary (+) 1 1
  (-)          = binary (-) (auto 1) (auto (-1)) -- TODO: <-> ? as it is, this might be pretty bad for Tower
  (*)          = mul -- lift2 (*) (\x y -> (y, x))
  negate       = lift1 negate (const (auto (-1)))
  abs          = lift1 abs signum
  signum a     = lift1 signum (const zero) a

instance BODY1(Fractional a) Fractional (HEAD) where
  fromRational 0 = zero
  fromRational r = auto (fromRational r)
  x / y        = x * recip y
  recip        = lift1_ recip (const . negate . join (*))

instance BODY1(Floating a) Floating (HEAD) where
  pi       = auto pi
  exp      = lift1_ exp const
  log      = lift1 log recip
  logBase x y = log y / log x
  sqrt     = lift1_ sqrt (\z _ -> recip (auto 2 * z))

  KnownZero ** y = auto (0 ** primal y)
  _ ** KnownZero = 1
  x ** Auto y    = lift1 (**y) (\z -> y *^ z ** auto (y-1)) x
  x ** y         = lift2_ (**) (\z xi yi -> (yi * xi ** (yi - 1), z * log xi)) x y

  sin      = lift1 sin cos
  cos      = lift1 cos $ negate . sin
  tan      = lift1 tan $ recip . join (*) . cos
  asin     = lift1 asin $ \x -> recip (sqrt (auto 1 - join (*) x))
  acos     = lift1 acos $ \x -> negate (recip (sqrt (1 - join (*) x)))
  atan     = lift1 atan $ \x -> recip (1 + join (*) x)
  sinh     = lift1 sinh cosh
  cosh     = lift1 cosh sinh
  tanh     = lift1 tanh $ recip . join (*) . cosh
  asinh    = lift1 asinh $ \x -> recip (sqrt (1 + join (*) x))
  acosh    = lift1 acosh $ \x -> recip (sqrt (join (*) x - 1))
  atanh    = lift1 atanh $ \x -> recip (1 - join (*) x)

instance BODY2(Num a, Enum a) Enum (HEAD) where
  succ             = lift1 succ (const 1)
  pred             = lift1 pred (const 1)
  toEnum           = auto . toEnum
  fromEnum a       = fromEnum (primal a)
  enumFrom a       = withPrimal a <$> enumFrom (primal a)
  enumFromTo a b   = withPrimal a <$> enumFromTo (primal a) (primal b)
  enumFromThen a b = zipWith (fromBy a delta) [0..] $ enumFromThen (primal a) (primal b) where delta = b - a
  enumFromThenTo a b c = zipWith (fromBy a delta) [0..] $ enumFromThenTo (primal a) (primal b) (primal c) where delta = b - a

instance BODY1(Real a) Real (HEAD) where
  toRational = toRational . primal

instance BODY1(RealFloat a) RealFloat (HEAD) where
  floatRadix     = floatRadix . primal
  floatDigits    = floatDigits . primal
  floatRange    = floatRange . primal
  decodeFloat    = decodeFloat . primal
  encodeFloat m e = auto (encodeFloat m e)
  isNaN          = isNaN . primal
  isInfinite     = isInfinite . primal
  isDenormalized = isDenormalized . primal
  isNegativeZero = isNegativeZero . primal
  isIEEE         = isIEEE . primal
  exponent = exponent . primal
  scaleFloat n = unary (scaleFloat n) (scaleFloat n 1)
  significand x =  unary significand (scaleFloat (- floatDigits x) 1) x
  atan2 = lift2 atan2 $ \vx vy -> let r = recip (join (*) vx + join (*) vy) in (vy * r, negate vx * r)

instance BODY1(RealFrac a) RealFrac (HEAD) where
  properFraction a = (w, a `withPrimal` pb) where
      pa = primal a
      (w, pb) = properFraction pa
  truncate = truncate . primal
  round    = round . primal
  ceiling  = ceiling . primal
  floor    = floor . primal

instance BODY1(Erf a) Erf (HEAD) where
  erf = lift1 erf $ \x -> (2 / sqrt pi) * exp (negate x * x)
  erfc = lift1 erfc $ \x -> ((-2) / sqrt pi) * exp (negate x * x)
  normcdf = lift1 normcdf $ \x -> recip (sqrt (2 * pi)) * exp (- x * x / 2)

instance BODY1(InvErf a) InvErf (HEAD) where
  inverf = lift1_ inverf $ \x _ -> sqrt pi / 2 * exp (x * x)
  inverfc = lift1_ inverfc $ \x _ -> negate (sqrt pi / 2) * exp (x * x)
  invnormcdf = lift1_ invnormcdf $ \x _ -> sqrt (2 * pi) * exp (x * x / 2)
