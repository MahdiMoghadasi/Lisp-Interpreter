
-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE InstanceSigs #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Librairie d'analyse syntaxique.
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
import Distribution.Compat.Lens (_1)
import GHC.TypeLits (sameNat)
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> c /= '\n'));
                pChar '\n' <|> eof; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (== '-')
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + digitToInt c)
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (`elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 pSymchar;
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Ssym "quote") (Scons e Snil)) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces; pTail }
pTail :: Parser Sexp
pTail  = do { pChar ')'; return Snil }
     <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
              pChar ')' <|> error ("Missing ')' after: " ++ show e);
              return e }
     <|> do { e <- pSexp; pSpaces; Scons e <$> pTail; }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { Just <$> anyChar ; } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec :: Int -> ReadS Sexp
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = shows n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) =
    let showTail Snil = showChar ')'
        showTail (Scons e1' e2') =
            showChar ' ' . showSexp' e1' . showTail e2'
        showTail e = showString " . " . showSexp' e . showChar ')'
    in showChar '(' . showSexp' e1 . showTail e2

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""
---------------------------------------------------------------------------
-- Elements OF SEXP
---------------------------------------------------------------------------
elemsOfsexp :: Sexp -> [Sexp]
elemsOfsexp e = reverse (elemsOfsexp' e [])
                where
                    elemsOfsexp' ex es = case ex of
                                            (Scons e0 e1) -> elemsOfsexp' e1 (e0:es)
                                            Snil -> es
                                            _anyotherthing -> error ("expression could not be converted to list" ++ show ex)

sfoldl :: (t -> Sexp -> t) -> t -> Sexp -> t
sfoldl _ x Snil            = x                       --Scons (Snum 1) Snil OR Scons (Ssym "+") Snil 
sfoldl f x (Scons car cdr) = sfoldl f (f x car) cdr  --Scons (Ssym "+") (Scons (Snum 1) Snil)
sfoldl _ _ _                  = error "sexp could not be read corectly"

sfoldlist :: (t -> Sexp -> t) -> t -> [Sexp] -> t
sfoldlist _ x []            = x                    --Scons (Snum 1) Snil OR Scons (Ssym "+") Snil 
sfoldlist f x (s:ss) = sfoldlist f (f x s) ss      --Scons (Ssym "+") (Scons (Snum 1) Snil)
---------------------------------------------------------------------------
---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------
type Var = String

data Lexp = Lnum Int            -- Constante entière.
          | Lref Var            -- Référence à une variable.
          | Llambda Var Lexp    -- Fonction anonyme prenant un argument.
          | Lcall Lexp Lexp     -- Appel de fonction, avec un argument.
          | Lnil                -- Constructeur de liste vide.
          | Ladd Lexp Lexp      -- Constructeur de liste.
          | Lmatch Lexp Var Var Lexp Lexp -- Expression conditionelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
--s2l doit eliminer le sucre syntaxique

s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym "nil") = Lnil
s2l (Ssym s) = Lref s
-- ¡¡ COMPLETER !!
--Lcall
-- s2l (Scons f (Scons x Snil)) = Lcall (s2l f) (s2l x)
-- s2l (Scons f xs@(Scons _ _)) = sfoldl (\ g x -> Lcall g (s2l x)) (s2l f) xs
-- s2l xs@(Scons _ _) = error ("unknown function application: " ++ show xs)
s2l input@(Scons _ _) = let list = elemsOfsexp input in case list of
                [Ssym "fn", Scons (Ssym var) Snil, x2] -> Llambda var (s2l x2)
                [Ssym "fn", Scons (Ssym var) exps, x2] -> Llambda var (s2l (Scons (Ssym "fn") (Scons exps (Scons x2 Snil))))
                [Ssym "add", x1, x2 ] -> Ladd (s2l x1) (s2l x2)
                -- [Ssym "list", Ssym"nil"] -> [Ssym "add, Sssym"nil]
                [x0, x1] -> Lcall (s2l x0) (s2l x1)
                (x:xs)     -> sfoldlist (\ g y -> Lcall g (s2l y)) (s2l x) xs
                _anythingelse -> error "could not pattern match"






                        -- where sexps = elemsOfsexp s
s2l se = error ("Malformed Sexp: " ++ showSexp se)



--------------  -------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vfun (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _ Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _ _ = showString "<function>"

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = [("+", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x + y)))),
        ("*", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x * y)))),
        ("/", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x `div` y)))),
        ("-", Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x - y))))]

-- ii Fonctions Auxiliaire !!
-- envValues :: [Value]
-- envValues = []

-- envVars :: [Var]
-- envVars = []

envSearch :: Var -> Env -> Value
envSearch x [] = error ("Variable does not exist " ++ x)
envSearch x ((x1,v1):env) = if x == x1 then v1 else envSearch x env



envIndex :: Env -> Var -> Int
envIndex [] x  = error ("Variable does not exist " ++ x)
envIndex env x = envIndex' env 0
    where   envIndex' :: Env -> Int -> Int
            envIndex' [] _ = error ("Variable does not exist " ++ x)
            envIndex' (y:ys) i= if fst y == x then i else envIndex' ys (i+1)

varsIndex :: [Var] -> Var -> Int
varsIndex [] x  = error ("Variable does not exist " ++ x)
varsIndex vars x = envIndex' vars 0
    where   envIndex' :: [Var] -> Int -> Int
            envIndex' [] _ = -1
            -- envIndex' [] _ = error ("Variable does not exist" ++ x)
            envIndex' (y:ys) i= if y == x then i else envIndex' ys (i+1)


envElab :: Env -> Var -> Value -> Env
envElab env var val = (var, val):env
-- envElab2 :: Env -> Lexp -> Value -> Env
-- envElab2 env var exp = (var,eval exp env):env

-- ii Fin de Fonctions Auxiliaire !!

---------------------------------------------------------------------------
-- Représentation intermédiaire Dexp                                     --
---------------------------------------------------------------------------

-- Dexp est similaire à Lexp sauf que les variables sont représentées non
-- pas par des chaînes de caractères mais par des "Indexes de de Bruijn",
-- c'est à dire par leur distance dans l'environnment: la variable la plus
-- récemment déclarée a index 0, l'antérieure 1, etc...
--
-- I.e. Llambda "x" (Llambda "y" (Ladd (Lref "x") (Lref "y")))
-- se traduit par Dlambda (Dlambda (Dadd (Dref 1) (Dref 0)))

type Idx = Int
--l2d élimine les noms de variables, et les remplace par des indexes dans l’environnement
data Dexp = Dnum Int            -- Constante entière.
          | Dref Idx            -- Référence à une variable.
          | Dlambda Dexp        -- Fonction anonyme prenant un argument.
          | Dcall Dexp Dexp     -- Appel de fonction, avec un argument.
          | Dnil                -- Constructeur de liste vide.
          | Dadd Dexp Dexp      -- Constructeur de liste.
          | Dmatch Dexp Dexp Dexp -- Expression conditionelle.
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives.
          | Dfix [Dexp] Dexp
          deriving (Show, Eq)

-- Le premier argument contient la liste des variables du contexte.
l2d :: [Var] -> Lexp -> Dexp
l2d _ (Lnum n) = Dnum n
l2d _ Lnil = Dnil

l2d envVars (Lref var) = let n = varsIndex envVars var in Dref n


l2d envVars (Llambda var lexp) = Dlambda (l2d (var:envVars) lexp)



l2d envVars (Lcall lfun larg) = Dcall (l2d envVars lfun) (l2d envVars larg)


l2d _ _ = error "Incorrect Lexp."

-- l2d _ (Lref x) = Dlambda y
--                 where  a = [x | x <- map fst env0]
--                         b = [y | y <- map snd env0]
-- l2d _ (Lref f) = let
--                 f in map fst env0
-- l2d _ (Lref n) = let 


-- l2d _ env = Dlambda 
--         where env = [x | x<- map fst env0]
-- ¡¡ COMPLETER !!
-- l2d _  (Lref x) = -- should give something like a function so dlambda or dcall for example
-- l2d (map fst env0) Lref "+"
-- find :: Env -> Var -> Value
-- find [] nom = error ("Symbole '" ++ nom ++ "' inexistant")
-- find ((ident, val) : envRest) ident'
--     | ident == ident' = val
--     | otherwise = find envRest ident'
-- findIndex env val = env !! find en  v val




---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Le premier argument contient la liste des valeurs des variables du contexte,
-- dans le même ordre que ces variables ont été passées à `l2d`.
eval :: [Value] -> Dexp -> Value
eval _ (Dnum n) = Vnum n

eval envValues (Dref n) = envValues !! n



eval envValues (Dlambda dexp) =   Vfun (\v -> eval (v:envValues) dexp)


eval envValues (Dcall e1 e2) = case eval envValues e1 of
                                    Vfun f -> f (eval envValues e2)
                                    _anythingelse -> error "function not found"






eval _ _ = error "Incorrect Dexp"
-- eval
-- ¡¡ COMPLETER !!



---------------------;------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval (map snd env0) . l2d (map fst env0) . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (putStr . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

dexpOf :: String -> Dexp
dexpOf = l2d (map fst env0) . s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf
