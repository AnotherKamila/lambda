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
                   where (x, rest) = break (`notElem` ['a'..'z']++['A'..'Z']++['0'..'9']) xs

cparse s
    | snd parsed == "" = fst parsed
    | otherwise        = error "Parse error: not a single, complete expression"
    where parsed = parse s

----------------------------------------------------------------------------------------------------

fix' :: Eq a => (a -> a) -> a -> a
fix' f x = if x' == x then x else fix' f x' where x' = f x

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

beta_reduce :: LExp -> LExp
beta_reduce (APPL (LAMBDA x e) n) = subst e (x, n)
beta_reduce (APPL m n) = beta_reduce (APPL (beta_reduce m) (beta_reduce n))
beta_reduce (LAMBDA x e) = LAMBDA x (beta_reduce e)
beta_reduce m = m

normal_form :: LExp -> LExp
normal_form = fix' beta_reduce

----------------------------------------------------------------------------------------------------

-- yes, and this too should be a monad, if I only knew what monads were...
-- (i.e. TODO use the State monad or something to pass around context in normal beta reduce one day)

type Context = [(Name, LExp)] -- TODO change this to a map to get better time complexity

def :: Context -> Name -> LExp -> Context
def c n e = case lookup n c of Nothing -> (n,e):c
                               Just _  -> error $ "Symbol "++n++" already defined in context"

add_c :: Context -> LExp -> LExp
add_c bs = fix' (add_one bs)
        where add_one []     e = e
              add_one (b:bs) e = add_one bs (subst e b)

g :: Context
g = [ ("K",   cparse "λx.λy.x")
    , ("I",   cparse "λx.x")
    , ("OMG", cparse "λx.(x x)")
    , ("OMGOMG", cparse "(OMG OMG)")
    , ("OMGWTF", cparse "((OMG OMG) OMG)")
    ]
