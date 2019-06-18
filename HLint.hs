import "hint" HLint.Default

infixr 8 **!, <**>
infixl 7 *!, /!, ^*, *^, ^/
infixl 6 +!, -!, <+>
infix  4 ==!
infixl 3 :-

-- this doesn't work well with Rank2Types
ignore "Eta reduce"
