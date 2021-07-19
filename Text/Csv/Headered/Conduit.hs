{-# language OverloadedStrings #-}
module Text.Csv.Headered.Conduit where

import Data.ErrorOr
import Data.Maybe
import Control.Monad.Catch
import qualified Data.ByteString as B
import Data.Conduit
import Data.Csv.Incremental
import qualified Data.Vector as V
import qualified Data.Text as T

import Text.Csv.Headered.Parser


-- TODO provide a version of that function that takes the separator, as Word8

-- | Convenient, throws exceptions on errors.
simpleCsv :: forall a m . MonadThrow m => RecordParser a -> ConduitT B.ByteString a m ()
simpleCsv rp = do
  let parseProgress = decode NoHeader -- 'NoHeader' means: don't skip the header
  goHeader 0 parseProgress
  where
    goHeader pos parseProgress =
      case parseProgress of
        Fail _unparsedPortion err ->
          throwM $ InvalidCsv pos err
        Done [] -> throwM (PrettyErrAcc $ ErrMessage "Empty input")
        Done (Left err:_) -> throwM (PrettyErrAcc $ ErrTag "Invalid header row" $ ErrMessage (T.pack err))
        Done (Right header:rest) ->
          case parseHeader rp header of
            Error err -> throwM (PrettyErrAcc $ ErrTag "Csv header error" err)
            OK parse -> go parse (succ pos) (Done rest)
        Many [] continueParsingCsv -> do
          mchunk <- await
          let newParseProgress = continueParsingCsv (fromMaybe B.empty mchunk)
          goHeader pos newParseProgress
        Many (Left err:_) _continueParsing ->
          throwM (PrettyErrAcc $ ErrMessage (T.pack err))
        Many (Right header:rest) continueParsingCsv ->
          case parseHeader rp header of
            Error err -> throwM (PrettyErrAcc $ ErrTag "Csv header error" err)
            OK parse -> go parse (succ pos) (Many rest continueParsingCsv)
    go parse pos parseProgress =
      case parseProgress of
        Fail _unparsedPortion err -> throwM $ InvalidCsv pos err
        Done rawRecords -> do
          _newPos <- emitRecords parse pos rawRecords
          return ()
        Many rawRecords continueParsingCsv -> do
          newPos <- emitRecords parse pos rawRecords
          mchunk <- await
          go parse newPos (continueParsingCsv (fromMaybe B.empty mchunk))

    emitRecords :: RowParser a -> Int -> [Either String (V.Vector B.ByteString)] -> ConduitT i a m Int
    emitRecords _ pos [] = pure pos
    emitRecords _ pos (Left err:_rs) =
      throwM (PrettyErrAcc $ ErrTag ("Invalid csv at record " <> (T.pack $ show pos))
               (ErrMessage (T.pack err)))
    emitRecords parse pos (Right r:rs) =
     case parse r of
       Error err -> throwM (PrettyErrAcc $ ErrTag ("Parse error at record " <> (T.pack $ show (succ pos))) err)
       OK a -> yield a >> emitRecords parse (succ pos) rs


data InvalidCsv = InvalidCsv Int String deriving (Show)
instance Exception InvalidCsv
