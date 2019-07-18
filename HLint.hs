import "hint" HLint.Default

infixr 8 **!, <**>
infixl 7 *!, /!, ^*, *^, ^/
infixl 6 +!, -!, <+>
infix  4 ==!
infixr 3 :-

-- this doesn't work well with Rank2Types
ignore "Eta reduce"
