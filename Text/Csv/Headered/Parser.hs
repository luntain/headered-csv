-- | The DSL will be made public, so I probably need to extract it into its own module
-- I can just explicitly export an explicit list in the main module, hopefully I can just suppress this modeul in the docs
module Text.Csv.Headered.Parser (
  parseHeader
  , RecordParser
  , RowParser
  , stringField
  , liftErrorOr
  ) where

import Data.ErrorOr
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Vector as V
import Data.List (elemIndex)
import Text.Printf (printf)
import Data.Char

newtype RecordParser a = RP ([String] -> ErrorOr (V.Vector B.ByteString -> ErrorOr a))

-- | Ready to parse a single row of a csv.
type RowParser a = V.Vector B.ByteString -> ErrorOr a

instance Functor RecordParser where
   fmap f (RP g) = RP (fmap (fmap . fmap . fmap $ f) g)

instance Applicative RecordParser where
  pure x = RP (const (return (const (return x))))
  (RP g) <*> (RP f) = RP $ \h -> case (g h, f h) of
                               (Error err, _) -> ErrorOr (Left err)
                               (_, Error err) -> ErrorOr (Left err)
                               (OK g', OK f') -> pure $ \r -> g' r <*> f' r


parseHeader :: RecordParser a -> V.Vector B.ByteString -> ErrorOr (RowParser a)
parseHeader (RP f) = f . V.toList . V.map (B8.unpack) -- use B8.strip? available in 0.10.12.0

-- parse :: RecordParser a -> V.Vector (V.Vector B.ByteString) -> ErrorOr [a]
-- parse (RP f) (hdr:rows) =
--   case f hdr of
--     Left err -> Left err
--     Right rParser -> catResults . map rParser $ rows

liftErrorOr :: ErrorOr a -> RecordParser a
liftErrorOr res = RP (const (pure $ const res))

-- |
stringField :: String -> RecordParser String
stringField colTitle = RP $ \hdr ->
  case elemIndex (map toLower colTitle) (map (map toLower) hdr) of
    Nothing -> fail (printf "No such column '%s' in %s" colTitle (show hdr))
    Just i -> pure $ \r ->
      let len = V.length r
      in if i >= V.length r
      then fail $ "Invalid row, too few fields: " ++ show len
      else pure . B8.unpack $ V.unsafeIndex r i

-- fromField :: String -> (String -> ErrorOr a) -> RecordParser a
-- fromField n f =
--     RP $ \h ->
--        case field h of
--          Left err ->  Left err
--          Right fn -> Right $ \r ->
--            case fn r of
--              Left err -> Left err
--              Right res -> tag ("parsing field " ++ n) $ f res
--   where RP field = stringField n

-- support tfield by index?
-- findByIndex :: Int -> [a] -> Maybe a
-- findByIndex i xs =
--   case drop i xs of
--     [] -> Nothing
--     x:_ -> return x
