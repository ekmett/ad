import "hint" HLint.Default

infixr 8 **!, <**>
infixl 7 *!, /!, ^*, *^, ^/
infixl 6 +!, -!, <+>
infix  4 ==!
infixl 3 :-

-- this doesn't work well with Rank2Types
ignore "Eta reduce"

-- HLint can't see the uses caused by TemplateHaskell
