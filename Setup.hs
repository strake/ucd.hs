{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad (join)
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
  { hookedPreProcessors = ("ucd-derived-core-properties", \ _ _ _ -> PreProcessor
      { platformIndependent = True
      , runPreProcessor = mkSimplePreProcessor $ \ inFile outFile _verbosity ->
        readFile inFile >>=
        writeFile outFile .
        ("module Data.Char.Properties.DerivedCore where\n" ++) .
        maybe (error "bad format") generate . parseProps
      }) : hookedPreProcessors simpleUserHooks
  }

data Range a = Range a a
  deriving (Foldable, Functor, Traversable)

rangeRE :: RE Char (Range Int)
rangeRE = join Range <$> n <|> Range <$> n <* ".." <*> n where n = RE.hexadecimal

lineRE :: RE Char ([Char], Range Char)
lineRE = flip ((,) . filter (/= '_')) . fmap toEnum <$> rangeRE <* many (RE.psym isSpace) <* "; " <*> many (RE.psym (liftA2 (||) isAlphaNum (== '_')))

generate :: Map [Char] [Range Char] -> [Char]
generate = unlines . ((:) <$> generateTypes . Map.keys <*> generateCode)

generateTypes :: [[Char]] -> [Char]
generateTypes names = intercalate ", " (("is" ++) <$> names) ++ " :: Char -> Bool"

generateCode :: Map [Char] [Range Char] -> [[Char]]
generateCode = toList . Map.mapWithKey (\ name ranges -> asum ["is", name, " x = ", intercalate " || " [asum ["x >= ", show a, " && x <= ", show b] | Range a b <- ranges]])

parseProps :: [Char] -> Maybe (Map [Char] [Range Char])
parseProps = lines & fmap (takeWhile (/= '#') & dropWhileEnd isSpace) & filter (not . null) & traverse (RE.match lineRE) & fmap ((fmap . fmap) pure & Map.fromListWith (flip (++)))
