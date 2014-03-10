import Data.List
import Data.Maybe

-- λ-term L ::= x | (L L) | λx.L
--             var   appl   abstr

type Name = String

data LExp = APPL LExp LExp   -- application
          | IDENT Name       -- variable
          | LAMBDA Name LExp -- abstraction
          -- I will add the following once I find out how my functions should
          -- handle those
          -- | SCONST Name      -- string constant (built-in function, ...)
          -- | CONST Integer    -- numeric constant
    deriving (Show, Read, Eq)

-------------------------------------------------------------------------------

render :: LExp -> String
render (IDENT x)    = x
render (LAMBDA x m) = "λ"++x++"."++(render m)
render (APPL m n)   = "("++(render m)++" "++(render n)++")"

prender exp = putStrLn $ render exp

-- it is a monad, but I have no idea what I am talking about... TODO
parse :: String -> (LExp, String)
parse ('λ':xs) = (LAMBDA name parsed, rest')
                   where (name, ('.':exps)) = break (=='.') xs
                         (parsed, rest')     = parse exps
parse ('(':xs) = (APPL m n, rest)
                   where (m, (' ':xs')) = parse xs
                         (n, (')':rest)) = parse xs'
parse xs       = (IDENT x, rest)
                   where (x, rest) = break (`notElem` ['a'..'z']) xs

cparse s
    | snd parsed == "" = fst parsed
    | otherwise        = error "Parse error: not a single, complete expression"
    where parsed = parse s

-------------------------------------------------------------------------------

free :: LExp -> [Name]
free (IDENT x)    = [x]
free (LAMBDA x m) = delete x (free m)
free (APPL f a)   = (free f) `union` (free a)

subt :: LExp -> [LExp]
subt (IDENT x)    = [IDENT x]
subt (LAMBDA x m) = (subt m) `union` [(LAMBDA x m)]
subt (APPL m n)   = (subt m) `union` (subt n) `union` [(APPL m n)]

-------------------------------------------------------------------------------

k = cparse "λx.λy.x"
i = cparse "λx.λy"
omg = cparse "λx.(x x)"
-- omgomg = APPL omg omg
--omgwtf = APPL (APPL omg omg) omg  -- TODO get from string
