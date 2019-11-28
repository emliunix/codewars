{-# LANGUAGE LambdaCase #-}
module SimplifyMultiLinearPolynominals where

import Data.Char (isDigit, isAlpha)
import Data.List (sort, sortBy, groupBy)
import Data.Ord (comparing)
import Control.Monad (ap)

simplify :: String -> String
simplify = unparse . transform . parse

-- >>> transform . parse $ unparse [("acdxz",-12),("bcy",12),("dy",7),("acyz",0),("ady",0),("bdz",14),("bdz",1),("az",-4),("acxz",-12),("dxz",10),("cxyz",-2),("ayz",-10),("bdxz",3),("acxyz",-6),("cx",1),("abd",-7),("ady",4),("abcd",-14),("ayz",11),("abxy",-6),("cdz",-15),("c",2),("c",14),("abd",-3),("byz",6),("abcy",-14),("acz",4),("acd",-9),("acyz",-1),("",1),("abdxz",-3),("axz",12),("cdxyz",-8),("dz",13),("dxyz",7),("a",7),("cdz",13),("cz",15),("bcdxy",5),("yz",9),("cz",1),("acdx",-14),("abcxyz",-4),("dz",0)]
-- [("a",7),("c",16),("az",-4),("cx",1),("cz",16),("dy",7),("dz",13),("yz",9),("abd",-10),("acd",-9),("acz",4),("ady",4),("axz",12),("ayz",1),("bcy",12),("bdz",15),("byz",6),("cdz",-2),("dxz",10),("abcd",-14),("abcy",-14),("abxy",-6),("acdx",-14),("acxz",-12),("acyz",-1),("bdxz",3),("cxyz",-2),("dxyz",7),("abdxz",-3),("acdxz",-12),("acxyz",-6),("bcdxy",5),("cdxyz",-8),("abcxyz",-4)]
--


parse :: String -> [([Char], Integer)]
parse s =
    _parse s []
    where
        _parse [] o = reverse o
        _parse s o = _parseSign s o
        _parseSign (c:s) out
            | c == '+' = _parseNum s True [] out
            | c == '-' = _parseNum s False [] out
        _parseSign s out = _parseNum s True [] out
        _parseNum (c:s) sign nums out
            | isDigit c = _parseNum s sign (c:nums) out
            | c == '+' || c == '-' = _parse (c:s) (([], parseNum sign nums):out)
        _parseNum s sign nums out = _parseVars s (parseNum sign nums) [] out
        _parseVars (c:s) n v o
            | isAlpha c = _parseVars s n (c:v) o
        _parseVars s n v o = _parse s ((sort v, n):o)
        -- parseNum "" = 1
        -- parseNum "+" = 1
        -- parseNum "-" = -1
        -- parseNum s = case reverse s of
        --     ('+':s) -> read s::Integer
        --     s -> read s::Integer
        parseNum sign [] = if sign then 1 else -1
        parseNum sign nums =
            let n = (read $ reverse nums :: Integer) in
                if sign then n else -n

-- >>> parse "a+3c-4b+de"
-- [("a",1),("c",3),("b",-4),("de",1)]
--

transform :: [([Char], Integer)] -> [([Char], Integer)]
transform ps =
    let ordered = sortBy (\a b -> case (a, b) of ((a, _), (b, _)) -> comparing (\vs -> (length vs, vs)) a b) ps
        grouped = groupBy (\a b -> case (a, b) of ((a, _), (b, _)) -> a == b) ordered
    in
        filter (\p -> case p of (vs, c) -> c /= 0 && vs /= []) $
        fmap (
            \ps -> case ps of
                (vs, _):_ -> (vs, sum $ fmap (\p -> case p of (_, c) -> c) ps)
        ) grouped

-- >>> transform $ parse "a+3cde-4b-2ced+de"
-- [("a",1),("b",-4),("de",1),("cde",1)]
--

unparse :: [([Char], Integer)] -> String
unparse [] = ""
unparse (p:ps) =
    foldl1 (++) (unparse1 p : fmap unparsen ps)
    where
        unparse1 :: ([Char], Integer) -> String
        unparse1 (vs, c) = (coeff1 c) ++ vs
        unparsen :: ([Char], Integer) -> String
        unparsen (vs, c) = (coeffn c) ++ vs
        coeff1 1 = ""
        coeff1 (-1) = "-"
        coeff1 n = show n
        coeffn 1 = "+"
        coeffn (-1) = "-"
        coeffn n = (if n > 0 then "+" else "") ++ (show n)

-- >>> unparse $ transform $ parse "4+a+3cde-4b-2ced+de"
-- "4+a-4b+de+cde"
--

-- Parser Combinator

newtype ParseC t = ParseC { unParseC:: String -> Maybe (t, String) }

instance Functor ParseC where
    fmap f pv = ParseC { unParseC = fmap (\case (v, s) -> (f v, s)) . (unParseC pv)}

instance Applicative ParseC where
    pure v = ParseC
        { unParseC = \s -> Just (v, s)
        }
    (<*>) = ap

-- >>> :i ap
-- ap :: Monad m => m (a -> b) -> m a -> m b 	-- Defined in ‘GHC.Base’
--

instance Monad ParseC where
    pa >>= pbf = ParseC
        { unParseC = \s -> case (unParseC pa) s of
            Just (a, s) -> unParseC (pbf a) s
            _ -> Nothing
        }
    fail reason = ParseC { unParseC = \s -> Nothing }

satisfy :: (Char -> Bool) -> ParseC Char
satisfy test = ParseC
    { unParseC = \case
        c:s | test c -> Just (c, s)
        _ -> Nothing
    }

(<|>) :: ParseC a -> ParseC a -> ParseC a
pa <|> pb = ParseC { unParseC = \s ->
        case (unParseC pa) s of
            Nothing -> (unParseC pb) s
            v -> v
    }

many :: ParseC t -> ParseC [t]
many p = _many p []
    where
        _many :: ParseC t -> [t] -> ParseC [t]
        _many p vs = do {
                v <- p;
                _many p (v:vs)
            } <|> return vs

    
runParser :: ParseC t -> String -> Maybe (t, String)
runParser p s = (unParseC p) s

-- >>> runParser (satisfy (== 'a')) "abc"
-- Just ('a',"bc")
--

testParser :: ParseC String
testParser = do
    a <- many (satisfy (=='a'));
    b <- many (satisfy (=='b'));
    return $ a ++ b

-- >>> runParser testParser "aaabbbb"
-- Just ("aaabbbb","")
--

data PolyN = PolyN {
    coeff :: Integer,
    -- coeff :: String,
    vars :: [Char]
} deriving Show

polynParser :: ParseC [PolyN]
polynParser = many _1polyn
    where
        _1polyn :: ParseC PolyN
        _1polyn = do
            sign <- satisfy (=='+') <|> satisfy (=='-') <|> return '+'
            num <- many (satisfy isDigit)
            vars <- many (satisfy isAlpha)
            case vars of
                [] -> fail "fail"
                _ -> return $ mkPolyN sign num vars
        mkPolyN :: Char -> [Char] -> [Char] -> PolyN
        mkPolyN sign num vars =
                PolyN {
                    coeff =
                        let num2 = case num of
                                [] -> "0"
                                s -> s
                            sign2 = case sign of
                                '+' -> ""
                                '-' -> "-"
                            signednum = sign2 ++ num2
                        in  
                            (read signednum)::Integer,
                    -- coeff = sign:num,
                    vars = vars
                }

-- >>> runParser polynParser "a+3c-4b+de"
-- Just ([PolyN {coeff = 0, vars = "ed"},PolyN {coeff = -4, vars = "b"},PolyN {coeff = 3, vars = "c"},PolyN {coeff = 0, vars = "a"}],"")
--

-- >>> :i runParser
-- runParser :: ParseC t -> String -> Maybe (t, String)
--   	-- Defined at /home/liu/play/DbSlow/db-slow-hs/src/CwSimplifyMultiLinearPolynominals.hs:65:1
--

-- >>> (read "+3")::Integer
-- 3
--
