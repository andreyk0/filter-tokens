{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}


module Main where


import           Args
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson as J
import           Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.UTF8 as B8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.List as DL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Trie as TR
import           System.IO


main:: IO ()
main = runWithArgs $ \args@Args{..} -> do
  case argCmd
    of FilterJson fieldPath -> filterJSONObjects args fieldPath
       FilterText ->  filterTextLines args


filterTextLines :: Args
                -> IO ()
filterTextLines Args{..} = do
  let !caseConv = if argIgnoreCase then TE.encodeUtf8 . T.toCaseFold . TE.decodeUtf8 else id

  patterns <- LB.readFile argTokensFileName >>= return . (fmap caseConv) . B8.lines . LB.toStrict
  let !tr = TR.fromList $ fmap (\p -> (p,())) patterns

  let linesIn = CB.sourceHandle stdin =$= CB.lines

  let lineMatches l = isJust $ DL.find isJust $
          fmap (TR.match tr) $ BS.tails (caseConv l)

  runResourceT $ linesIn
    =$= CL.filter lineMatches
    =$= CL.map (<> "\n")
    $$ CB.sinkHandle stdout




filterJSONObjects :: Args
                  -> Text
                  -> IO ()
filterJSONObjects Args{..} fieldPath = do
  let !caseConv = if argIgnoreCase then T.toCaseFold else id

  patterns <- fmap (Set.fromList . T.lines . caseConv) $ TIO.readFile argTokensFileName

  let linesIn = CB.sourceHandle stdin =$= CB.lines

      parseJsonLine l = case decode (LB.fromStrict l)
                          of Nothing -> liftIO $ hPutStrLn stderr $ "Failed to decode input line " <> (show l)
                             Just jv -> yield jv

      jsonIn = linesIn =$= awaitForever parseJsonLine

      jsonObjMatches :: Value -> Bool
      jsonObjMatches o = case o ^? key fieldPath
                           of Nothing -> False
                              Just v -> case v
                                          of J.String t -> Set.member (caseConv t) patterns
                                             _          -> False

  runResourceT $ jsonIn
    =$= CL.filter jsonObjMatches
    =$= CL.map (LB.toStrict . encode)
    =$= CL.map (<> "\n")
    $$ CB.sinkHandle stdout
