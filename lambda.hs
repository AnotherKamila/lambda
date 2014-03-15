import Data.List
import Data.Maybe

-- λ-term L ::= x | (L L) | λx.L
--             var   appl   abstr

type Name = String

data LExp = APPL LExp LExp   -- application
          | IDENT Name       -- variable
          | LAMBDA Name LExp -- abstraction
          -- I will add the following once I find out how my functions should handle those
          -- | SCONST Name      -- string constant (built-in function, ...)
          -- | CONST Integer    -- numeric constant
    deriving (Show, Read, Eq)

----------------------------------------------------------------------------------------------------

render :: LExp -> String
render (IDENT x)    = x
render (LAMBDA x m) = "λ"++x++"."++(render m)
render (APPL m n)   = "("++(render m)++" "++(render n)++")"

prender exp = putStrLn $ render exp

-- it is a monad, but I have no idea what I am talking about... TODO
-- also TODO: if I move the ' ' check to IDENT, brackets around application will not be needed
parse :: String -> (LExp, String)
parse ('(':xs)
    | head rest == ')' = (m, tail rest)
    | head rest == ' ' = let (n, ')':rest') = parse (tail rest) in (APPL m n, rest')
    where (m, rest) = parse xs
parse ('λ':xs) = (LAMBDA name parsed, rest')
                   where (name, ('.':exps)) = break (=='.') xs
                         (parsed, rest')     = parse exps
parse xs       = (IDENT x, rest)
                   where (x, rest) = break (`notElem` ['a'..'z']++['0'..'9']) xs

cparse s
    | snd parsed == "" = fst parsed
    | otherwise        = error "Parse error: not a single, complete expression"
    where parsed = parse s

----------------------------------------------------------------------------------------------------

used_names :: LExp -> [Name]
used_names (IDENT x) = [x]
used_names (LAMBDA x m) = [x] `union` (used_names m)
used_names (APPL m n) = (used_names m) `union` (used_names n)

newname :: [LExp] -> Name
newname xs = head $ (map (('x':).show) [1..]) \\ (nub . concat) (map used_names xs)

----------------------------------------------------------------------------------------------------

free :: LExp -> [Name]
free (IDENT x)    = [x]
free (LAMBDA x m) = delete x (free m)
free (APPL f a)   = (free f) `union` (free a)

subterms :: LExp -> [LExp]
subterms (IDENT x)    = [IDENT x]
subterms (LAMBDA x m) = (subterms m) `union` [LAMBDA x m]
subterms (APPL m n)   = (subterms m) `union` (subterms n) `union` [APPL m n]

subst :: LExp -> (Name, LExp) -> LExp
subst (IDENT x)    (name, exp) = if name == x then exp else IDENT x
subst (APPL m n)   (name, exp) = APPL (subst m (name, exp)) (subst n (name, exp))
subst (LAMBDA x m) (name, exp)
    | name == x = LAMBDA x m                      -- then x should be the lambda's x, not this x
    | name `elem` (free m) && x `elem` free exp = -- avoid trouble when y is in m
            let z = newname [exp,m] in LAMBDA z (subst (subst m (x,(IDENT z))) (name,exp))           
    | otherwise = LAMBDA x (subst m (name,exp))

----------------------------------------------------------------------------------------------------

k = cparse "λx.λy.x"
i = cparse "λx.x"
omg = cparse "λx.(x x)"
-- omgomg = APPL omg omg
-- omgwtf = APPL (APPL omg omg) omg -- TODO get from string
