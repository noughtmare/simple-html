module Lib where

import Prelude hiding (break)
import qualified Prelude
import Control.Monad (ap)
import Control.Applicative ( Alternative((<|>), empty, many), optional)
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe)

data Doc = Node String [(String, String)] [Doc]
         | Text String
         | Comment String
         deriving Show

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }
  deriving Functor
instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (<*>) = ap
instance Monad Parser where
  Parser p >>= k = Parser (\s -> do (x, s') <- p s; parse (k x) s')
instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  Parser p <|> Parser q = Parser (\s -> p s <|> q s)

-- needs to match at least one character
break :: (Char -> Bool) -> Parser String
break f = Parser (\s -> case Prelude.break f s of
  ("", _) -> Nothing
  x -> Just x)

string :: String -> Parser ()
string s = Parser (\s' -> ((),) <$> stripPrefix s s')

between :: String -> String -> Parser a -> Parser a
between x y p = do
  string x
  z <- p
  string y
  pure z

-- Data.ByteString.breakSubstring / Data.Text.breakOn
breakSubstring :: String -> String -> (String, String)
breakSubstring "" = ("",)
breakSubstring (x:xs) = go "" where
  go :: String -> String -> (String, String)
  go res (y:ys) | x == y && xs `isPrefixOf` ys = (reverse res, y:ys)
                | otherwise = go (y:res) ys
  go res "" = (reverse res, "")

-- must match, so if it succeeds then the input string must
-- be the start of the remainder.
breakString :: String -> Parser String
breakString xs = Parser $ \s ->
  case breakSubstring xs s of
    (_,"") -> Nothing
    x -> Just x

docs :: Parser [Doc]
docs = many doc

doc :: Parser Doc
doc = Comment <$> between "<!--" "-->" (breakString "-->")
  <|> do string "<"
         name <- break (\c -> c == '>' || isSpace c)
         attrs <- many $ do
           _ <- break (not . isSpace)
           attrName <- break (\c -> c == '=' || c == '>' || isSpace c)
           attrValue <- optional $ do
             string "="
             (between "\"" "\"" (break (== '"'))
               <|> between "'" "'" (break (== '\''))
               <|> break (\c -> c == '>' || isSpace c))
           pure (attrName, fromMaybe "" attrValue)
         string ">"
         d <- Node name attrs <$> docs
         _ <- optional $ string $ "</" ++ name ++ ">"
         pure d
  <|> Text <$> break (== '<')


prettyDocs :: [Doc] -> String
prettyDocs = concatMap prettyDoc

filterTags :: (String -> Bool) -> [Doc] -> [Doc]
filterTags p = concatMap filterTagsOne where
  filterTagsOne :: Doc -> [Doc]
  filterTagsOne (Node t as xs)
    | p t = [Node t as (filterTags p xs)]
    | otherwise = filterTags p xs
  filterTagsOne (Text t) = [Text t]
  filterTagsOne (Comment c) = [Comment c]

prettyDoc :: Doc -> String
prettyDoc (Text t) = t
prettyDoc (Comment t) = "<!--" ++ t ++ "-->"
prettyDoc (Node name [] children) =
  "<" ++ name ++ ">" ++ prettyDocs children ++ "</" ++ name ++ ">"
prettyDoc (Node name attrs children) =
  "<" ++ name ++ " " ++ concatMap (\(x,y) -> case y of "" -> x; _ -> x ++ "=\"" ++ y ++ "\"") attrs ++ ">"
    ++ prettyDocs children
    ++ "</" ++ name ++ ">"

whiteList :: String -> Bool
whiteList "script" = True
whiteList "div" = True
whiteList _ = False

sanitize :: String -> String
sanitize = concatMap prettyDoc
  . filterTags (whiteList . map toLower)
  . fromMaybe []
  . fmap fst
  . parse docs