{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase, MonadComprehensions #-}
{-# OPTIONS_GHC -Wall
                -Wcompat
                -Wredundant-constraints
                -Wno-name-shadowing
                -Wincomplete-uni-patterns
                -Wincomplete-record-updates
                -Werror=incomplete-patterns
                -Werror=incomplete-uni-patterns
                -Werror=incomplete-record-updates
                -Werror=missing-fields
                -Werror=missing-methods
  #-}
module Main where

import           Prelude hiding (lines)
import qualified Prelude
import           Control.Applicative
import           Control.Monad (join)
import           Data.Bits
import           Data.Bool (bool)
import           Data.Char
import           Data.Foldable
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Numeric (showHex)
import           System.Directory (createDirectoryIfMissing)
import           Text.Regex.Applicative
import           Text.Regex.Applicative.Common (hexadecimal)
import           Util hiding (intercalate)

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Types.BuildInfo

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { hookedPreProcessors =
    ("ucd-range-props", \ _ _ _ -> PreProcessor True $ \ (inBase, inFile) (outBase, outFile) _v ->
         let moduleNameOf = fmap (\ case '/' -> '.'; x -> x) . fst . break (== '.')
         in readFile (inBase ++ "/" ++ inFile) >>=
            writeFile (outBase ++ "/" ++ outFile) .
            maybe (error "bad format") (ppRangeProps (moduleNameOf inFile)) . parseRangeProps) :
    hookedPreProcessors simpleUserHooks
  , preBuild = \ args flags -> do
        let dir = "src/cbits"
        createDirectoryIfMissing True dir
        for_ [("DerivedCoreProperties", maybe (error "bad format") doRangeProps . parseRangeProps),
              ("BidiBrackets",          mkBidiBrackets)] $ \ (name, mk) ->
            readFile ("ucd/" ++ name ++ ".txt") >>= writeFile (dir ++ "/" ++ name ++ ".h") . mk
        [(Just emptyBuildInfo { includeDirs = ["src/cbits"] }, []) <> info | info <- preBuild simpleUserHooks args flags]
  }

ppRangeProps :: [Char] -> RangeProps Char -> [Char]
ppRangeProps moduleName props = unlines $
  [ asum ["module ", moduleName, " (", intercalate ", " (("is" ++) <$> names), ") where"]
  , "import Data.Word"
  , "import Foreign.Ptr"
  , intercalate ", " (("is" ++) <$> names) ++ " :: Char -> Bool"
  , "foreign import ccall unsafe \"range_search\" c_range_search :: Word32 -> Ptr Word64 -> Word -> Ptr Word64"
  ] ++ toList (Map.mapWithKey genCode1 props)
  where
    names = Map.keys props
    genCode1 name ranges = (unlines . fmap asum)
      [ ["is", name, " = \\ x -> nullPtr /= c_range_search ((fromIntegral . fromEnum) x) data", name, " ", show (length ranges)]
      , ["{-# INLINE is", name, " #-}"]
      , ["foreign import ccall unsafe \"&", name, "\" data", name, " :: Ptr Word64"]
      ]

doRangeProps :: RangeProps Char -> [Char]
doRangeProps = unlines . (["#include <stdint.h>", "typedef struct { uint32_t a, b; } RangeU32;"] ++) . toList . Map.mapWithKey genCode1
  where
    genCode1 name ranges = asum
      [ "extern const RangeU32 ", name, "[]; "
      , "const RangeU32 ", name, "[] = {"
      , intercalate ", " [asum ["{", show (fromEnum a), ", ", show (fromEnum b + 1), "}"] | Range a b <- ranges]
      , "};"]

mkBidiBrackets :: [Char] -> [Char]
mkBidiBrackets = ("#include <stdint.h>\nextern const uint32_t BidiBrackets[]; const uint32_t BidiBrackets[] = " ++) . (++ ";") . maybe (error "bad format") generate . parse lineRE
  where
    generate :: [(Char, Char, Bool)] -> [Char]
    generate = ("{" ++) . (++ "}") . intercalate ", " . fmap generate1

    generate1 :: (Char, Char, Bool) -> [Char]
    generate1 (x, y, isC)
      | abs d > 3 = error ("Difference too great: " ++ show (y, x))
      | otherwise = "0x" ++ showHex (fromEnum x .|. shiftL (d .&. 7) 29 .|. bool 0 (bit 28) isC) ""
      where d = fromEnum y - fromEnum x

    lineRE :: RE Char (Char, Char, Bool)
    lineRE = (,,) <$> spaced enum0x `ap'` spaced enum0x `ap'` (spaced . asum) [False <$ "o", True <$ "c"]

parseRangeProps :: [Char] -> Maybe (RangeProps Char)
parseRangeProps = parse lineRE & fmap ((fmap . fmap) pure & Map.fromListWith (flip (++)))
  where
    lineRE :: RE Char ([Char], Range Char)
    lineRE = flip ((,) . filter (/= '_')) <$> spaced rangeRE `ap'` spaced word

type RangeProps a = Map [Char] [Range a]

data Range a = Range a a
  deriving (Foldable, Functor, Traversable)

rangeRE :: Enum n => RE Char (Range n)
rangeRE = join Range <$> enum0x <|> Range <$> enum0x <* string ".." <*> enum0x

infixl 4 `ap'`
ap' :: RE Char (a -> b) -> RE Char a -> RE Char b
ap' f a = f <* sym ';' <*> a

spaced :: RE Char a -> RE Char a
spaced a = spaces *> a <* spaces

comment :: RE Char [Char]
comment = sym '#' *> many (psym (/= '\n'))

lines :: [Char] -> [[Char]]
lines = filter (not . all isSpace) . fmap (fst . break (== '#')) . Prelude.lines

parse :: RE Char a -> [Char] -> Maybe [a]
parse a = traverse (match a) . lines

spaces :: RE Char [Char]
spaces = many (psym isSpace)

enum0x :: Enum n => RE Char n
enum0x = toEnum <$> hexadecimal

word :: RE Char [Char]
word = many (psym (liftA2 (||) isAlphaNum (== '_')))
