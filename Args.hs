{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}

module Args (
  Args(..)
, Cmd(..)
, runWithArgs
) where


import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Options.Applicative


data Cmd = FilterJson !Text -- ^ JSON field path, search JSON objects
         | FilterText -- ^ search text


data Args = Args { argTokensFileName :: !FilePath
                 , argCaseConv :: Text -> Text
                 , argCmd :: !Cmd
                 }


parseArgs :: Parser Args
parseArgs = Args
     <$> strOption
         ( long "tokens-file-name"
        <> short 't'
        <> help "Tokens to search for, one per line.")
     <*> ( fmap (\caseConv -> if caseConv then T.toCaseFold else id) $ switch
         ( long "ignore-case"
        <> short 'i'
        <> help "Case insensitive match." ))
     <*> subparser
       (   command "json"
            (info (FilterJson <$> fmap T.pack (argument str (metavar "json.field.path")))
                (progDesc "Filter JSON, one object per line."))
        <> command "text"
            (info (pure FilterText)
                (progDesc "Filter lines of text."))
       )


runWithArgs:: (Args -> IO ())
           -> IO ()
runWithArgs rwa = execParser opts >>= rwa
  where
    opts = info (helper <*> parseArgs)
      ( fullDesc
     <> progDesc ( "Reads input from STDIN. Filters lines of input (JSON objects or text) matching a given set of tokens.")
     <> header "Filters lines of input (JSON objects or text) matching a given set of tokens." )
