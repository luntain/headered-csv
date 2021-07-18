module Tests where

import Test.Tasty
import Test.Tasty.HUnit
import Text.InterpolatedString.Perl6
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Text.Csv.Headered.Parser
import Data.Csv
import Data.Either

main = defaultMain tests

simpleInput :: V.Vector (V.Vector B.ByteString)
simpleInput = fromRight . decode $ [q|
col1,col2
r1c1,r1c2
r2c1,r2c2
|]

tests =
  testGroup "Parser tests"
    [ testCase "Parse simple" $ do
        parseRow <- toE $ parseHeader (stringField "col2") (simpleInput V.! 0)
        assertEqual "parse first row"  (pure "r1c2") (parseRow $ simpleInput V.! 1)
        assertEqual "parse second row" (pure "r2c2") (parseRow $ simpleInput V.! 2)
        assertEqual "parse invalid row" (fail "Invalid row, too few fields: 0") (parseRow $ V.empty)

    , testCase "Missing column" $ do
        parserE <- parseHeader (stringField "grail") (simpleInput V.! 0)
        assertEqual "column error" (fail [q|No such column 'grail' in headers: ["col1","col2"]|]) parserE
    ]
