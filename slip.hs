-- Emmanuelle Pilon ( Matricule : 20270418 ) et Árpád-Botond Rigó (20241538)
-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-} 
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
-- Sexp : expression synthaxique
data Sexp = Snil                        -- La liste vide
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          | Snode Sexp [Sexp]           -- Une liste non vide
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) ==> Snode (Ssym "+")
--                   [Snum 2, Snum 3]
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Snode (Ssym "/")
--       [Snode (Ssym "*")
--              [Snode (Ssym "-")
--                     [Snum 68, Snum 32],
--               Snum 5],
--        Snum 9]

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------
-- Pour lire un charactère
pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
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
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
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
              return (Snode (Ssym "quote") [e]) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces;
              ses <- pTail;
                    return (case ses of [] -> Snil
                                        se : ses' -> Snode se ses')
            }
pTail :: Parser [Sexp]
pTail  = do { pChar ')'; return [] }
     -- <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
     --          pChar ')' <|> error ("Missing ')' after: " ++ show e);
     --          return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (e : es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

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
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Snode h t) =
    let showTail [] = showChar ')'
        showTail (e : es) =
            showChar ' ' . showSexp' e . showTail es
    in showChar '(' . showSexp' h . showTail t

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire Lexp                                     --
---------------------------------------------------------------------------

type Var = String

data Lexp = Lnum Int             -- Constante entière.
          | Lbool Bool           -- Constante Booléenne.
          | Lvar Var             -- Référence à une variable.
          | Ltest Lexp Lexp Lexp -- Expression conditionelle.
          | Lfob [Var] Lexp      -- Construction de fobjet. 
          | Lsend Lexp [Lexp]    -- Appel de fobjet.
          | Llet Var Lexp Lexp   -- Déclaration non-récursive. 
          -- Déclaration d'une liste de variables qui peuvent être
          -- mutuellement récursives. (comme un 'let' de haskell)
          | Lfix [(Var, Lexp)] Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse une Sexp et construit une Lexp 
-- équivalente.
s2l :: Sexp -> Lexp

-- Un entier
s2l (Snum n) = Lnum n

-- Un booléen "true"
s2l (Ssym "true") = Lbool True

-- Un booléen "false"
s2l (Ssym "false") = Lbool False

-- Une variable
s2l (Ssym s) = Lvar s

-- Une condition "if"
s2l (Snode (Ssym "if") [cond, e1, e2]) = 
    Ltest (s2l cond) (s2l e1) (s2l e2)

-- Une déclaration "let"
s2l (Snode (Ssym "let") [Ssym x, e1, e2]) = Llet x (s2l e1) (s2l e2)

-- Une fonction de cas récursif "fix"
s2l (Snode (Ssym "fix") [declsNode, body]) =
    let decls = sexpToList declsNode
    in Lfix (map parseDecl decls) (s2l body)
  where
    -- Chat GPT nous a proposé cette structure et de séparer en 3 
    -- sous-fontions, afin de faciliter l'implémentation

    -- Transformer la déclaration Sexp vers un tuple afin de le 
    -- passer vers le constructeur 'Lfix' 
    parseDecl :: Sexp -> (Var, Lexp)
    -- La déclaration: (x e)
    parseDecl (Snode (Ssym x) [e]) = (x, s2l e)
    -- La fonction déclarée: ((x x1 ... xn) e)
    parseDecl (Snode (Snode (Ssym x) params) [e]) =
        let params' = map fromSsym params
        in (x, Lfob params' (s2l e))
    parseDecl se = error ("Déclaration non-valide: " ++ showSexp se)

    -- Convertir la liste de paramètres en type Var (string) pour la
    -- représentation Lexp
    fromSsym :: Sexp -> Var
    fromSsym (Ssym s) = s
    fromSsym se = error ("Mauvais symbole, reçu: " ++ showSexp se)

    -- Convertir une liste de type Sexp à une liste de type Haskell 
    -- avec des éléments de types Sexp
    sexpToList :: Sexp -> [Sexp]
    sexpToList Snil = []
    sexpToList (Snode h t) = h : t
    sexpToList se = error ("Mauvaise liste, reçu: " ++ showSexp se)

-- Une fonction
s2l (Snode (Ssym "fob") [paramsNode, e]) =
    let
        params = case paramsNode of
                    Snil -> []
                    Snode h t -> map fromSsym (h : t)
                    Ssym s -> [s]
                    _ -> error ("Paramètre non-valide dans fob: "
                     ++ showSexp paramsNode)
    in Lfob params (s2l e)
  where
    fromSsym (Ssym s) = s
    fromSsym se = error ("Mauvais symbole, reçu: " ++ showSexp se)

-- Un appel de fonction 
s2l (Snode func args) = Lsend (s2l func) (map s2l args)

s2l se = error ("Expression Psil inconnue: " ++ showSexp se)

---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

-- Type des valeurs manipulées à l'exécution.
data Value = Vnum Int
           | Vbool Bool
           | Vbuiltin ([Value] -> Value)
           | Vfob VEnv [Var] Lexp

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vbool b) = showsPrec p b
    showsPrec _ (Vbuiltin _) = showString "<primitive>"
    showsPrec _ (Vfob _ _ _) = showString "<fobjet>"

type VEnv = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 = let
          binop f op =
              Vbuiltin (\vs -> case vs of
                         [Vnum n1, Vnum n2] -> f (n1 `op` n2)
                         [_, _] -> error "Pas un nombre"
                         _ -> error "Nombre d'arguments incorrect")
      in
        [ ("+", binop Vnum (+)),
          ("*", binop Vnum (*)),
          ("/", binop Vnum div),
          ("-", binop Vnum (-)),
          ("<", binop Vbool (<)),
          (">", binop Vbool (>)),
          ("<=", binop Vbool (<=)),  -- Remplacement de '≤' avec '<='
          (">=", binop Vbool (>=)),  -- Remplacment de  '≥' avec '>='
          ("=",  binop Vbool (==)),
          ("true", Vbool True),
          ("false", Vbool False)]


---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Évaluateur
eval :: VEnv -> Lexp -> Value

-- Évaluation des entiers
eval _ (Lnum n) = Vnum n

-- Évaluation des booléens
eval _ (Lbool b) = Vbool b

-- Évaluation d'une déclaration de variable
eval env (Lvar x) = case lookup x env of
  Just v  -> v
  Nothing -> error ("Variable mal définie: " ++ x)

-- Évaluation d'expression conditionnelle
eval env (Ltest cond e2 e3) =
  case eval env cond of
    Vbool True  -> eval env e2
    Vbool False -> eval env e3
    _           -> error "La condition n'est pas booléen"

-- Évaluation d'un fonction
eval env (Lfob params body) = Vfob env params body

-- Évaluation d'un appel de fonction
eval env (Lsend efunc args) =
  let vfunc = eval env efunc
      vargs = map (eval env) args
  in case vfunc of
       Vbuiltin f -> f vargs
       Vfob closure params body ->
        -- Chat GPT nous à aider en simplifiant c'est deux
        -- conditions ('if' et 'else') afin de simplifier 
        -- notre première version implémentée.
         if length params /= length vargs then
           error "Mauvais nombre d'arguments"
         else
          -- Chat GPT à suggéré l'utilisation de 'closure'.
           let env' = zip params vargs ++ closure
           in eval env' body
       _ -> error "Valeur n'est pas une fonction"

-- Évaluation d'une déclaration "let"
eval env (Llet x e1 e2) =
  let v1 = eval env e1
      env' = (x, v1) : env
  in eval env' e2

-- Évaluation d'une déclaration récursive "fix"
eval env (Lfix defs body) =
  let env' = recEnv ++ env
      recEnv = [(x, eval env' e) | (x, e) <- defs]
  in eval env' body

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do inputHandle <- openFile filename ReadMode 
       hSetEncoding inputHandle utf8
       s <- hGetContents inputHandle
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
       hClose inputHandle

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

valOf :: String -> Value
valOf = evalSexp . sexpOf

