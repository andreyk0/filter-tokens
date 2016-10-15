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
import qualified Data.ByteString.Lazy as B
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.IO


main:: IO ()
main = runWithArgs $ \args@Args{..} -> do
  patterns <- fmap (Set.fromList . T.lines . argCaseConv) $ TIO.readFile argTokensFileName

  case argCmd
    of FilterJson fieldPath -> filterJSONObjects args fieldPath patterns
       _ ->  putStrLn "todo"


filterJSONObjects :: Args
                  -> Text
                  -> Set Text
                  -> IO ()
filterJSONObjects Args{..} fieldPath patterns = do
  let linesIn = CB.sourceHandle stdin =$= CB.lines

      parseJsonLine l = case decode (B.fromStrict l)
                          of Nothing -> liftIO $ hPutStrLn stderr $ "Failed to decode input line " <> (show l)
                             Just jv -> yield jv

      jsonIn = linesIn =$= awaitForever parseJsonLine

      jsonObjMatches :: Value -> Bool
      jsonObjMatches o = case o ^? key fieldPath
                           of Nothing -> False
                              Just v -> case v
                                          of J.String t -> Set.member (argCaseConv t) patterns
                                             _          -> False

  runResourceT $ jsonIn
    =$= CL.filter jsonObjMatches
    =$= CL.map (B.toStrict . encode)
    =$= CL.map (<> "\n")
    $$ CB.sinkHandle stdout
