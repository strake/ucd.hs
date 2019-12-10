{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad (join)
import           Data.Bool (bool)
import           Data.Char
import           Data.Foldable
import           Data.List (dropWhileEnd, intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Text.Regex.Applicative (RE)
import qualified Text.Regex.Applicative as RE
import qualified Text.Regex.Applicative.Common as RE
import           Util hiding (intercalate)

import Distribution.Simple
import Distribution.Simple.PreProcess

main = defaultMainWithHooks simpleUserHooks
  { hookedPreProcessors =
    ("ucd-derived-core-properties", mkPP derivCorePropsPP) :
    ("ucd-bidi-brackets", mkPP bidiBracketsPP) :
    hookedPreProcessors simpleUserHooks
  }
  where
    mkPP pp _ _ _ = PreProcessor True $ mkSimplePreProcessor $ \ inFile outFile _verbosity ->
        readFile inFile >>= writeFile outFile . pp

derivCorePropsPP, bidiBracketsPP :: [Char] -> [Char]
derivCorePropsPP = ("module Data.Char.Properties.DerivedCore where\n" ++) . maybe (error "bad format") generate . parse
  where
    generate :: Map [Char] [Range Char] -> [Char]
    generate = unlines . ((:) <$> generateTypes . Map.keys <*> generateCode)

    generateTypes :: [[Char]] -> [Char]
    generateTypes names = intercalate ", " (("is" ++) <$> names) ++ " :: Char -> Bool"

    generateCode :: Map [Char] [Range Char] -> [[Char]]
    generateCode = toList . Map.mapWithKey (\ name ranges -> asum ["is", name, " x = ", intercalate " || " [asum ["x >= ", show a, " && x <= ", show b] | Range a b <- ranges]])

    parse :: [Char] -> Maybe (Map [Char] [Range Char])
    parse = parseLines & traverse (RE.match lineRE) & fmap ((fmap . fmap) pure & Map.fromListWith (flip (++)))

    lineRE :: RE Char ([Char], Range Char)
    lineRE = flip ((,) . filter (/= '_')) <$> rangeRE <* many (RE.psym isSpace) <* "; " <*> many (RE.psym (liftA2 (||) isAlphaNum (== '_')))
bidiBracketsPP = ("module Data.Char.Properties.BidiBrackets where\ndata Type = O | C\n" ++) . maybe (error "bad format") generate . parse
  where
    generate :: [(Char, Char, Bool)] -> [Char]
    generate = unlines . flip (++) ["    _ -> Nothing"] . (:) "paired :: Char -> Maybe (Char, Type)\npaired = \\ case" .
        fmap (\ (x, y, isC) -> asum ["    ", show x, " -> Just (", show y, ", ", bool "O" "C" isC, ")"])

    parse :: [Char] -> Maybe [(Char, Char, Bool)]
    parse = parseLines & traverse (RE.match lineRE)

    lineRE :: RE Char (Char, Char, Bool)
    lineRE = (,,) <$> n <* "; " <*> n <* "; " <*> asum [False <$ "o", True <$ "c"] where n = toEnum <$> RE.hexadecimal

parseLines :: [Char] -> [[Char]]
parseLines = lines & fmap (takeWhile (/= '#') & dropWhileEnd isSpace) & filter (not . null)

data Range a = Range a a
  deriving (Foldable, Functor, Traversable)

rangeRE :: Enum n => RE Char (Range n)
rangeRE = join Range <$> n <|> Range <$> n <* ".." <*> n where n = toEnum <$> RE.hexadecimal
