{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import PollSource.Prelude

import Data.String         ( String )
import Options.Applicative
import PollSource.Server   ( servePolls
                           , writeJs
                           , writeRuby
                           )
import System.Environment  ( lookupEnv )

data Cmd
  = Serve     (Maybe Int)
  | WriteJs   FilePath
  | WriteRuby FilePath

main :: IO ()
main = execParser opts >>= \case
  WriteJs   path -> writeJs   path
  WriteRuby path -> writeRuby path
  Serve     port -> serve     port
  where
    opts :: ParserInfo Cmd
    opts = info (helper <*> parseCmd) (fullDesc <> headerDoc (Just "PollSource"))

    parseCmd :: Parser Cmd
    parseCmd = subparser . foldMap command' $
      [ ("serve", "Launch the API server"     , Serve     <$> portMay)
      , ("js"   , "Write the JS client file"  , WriteJs   <$> jsPath)
      , ("ruby" , "Write the Ruby client file", WriteRuby <$> rubyPath)
      ]

    portMay = optional $ argument auto
      (  metavar "PORT"
      <> help ("Override default port (" <> show defaultPort <> ").")
      )

    jsPath = argument str
      ( metavar "PATH"
     <> help "Where to write the JS client file."
      )

    rubyPath = argument str
      ( metavar "PATH"
     <> help "Where to write the Ruby client file."
      )

    command' :: (String, String, Parser Cmd) -> Mod CommandFields Cmd
    command' (name, desc, parser) = command name (info' parser desc)

    info' :: Parser a -> String -> ParserInfo a
    info' p desc = info (helper <*> p) (fullDesc <> progDesc desc)

serve :: Maybe Int -> IO ()
serve portOverride = servePolls =<< case portOverride of
  Just p  -> return p
  Nothing -> maybe defaultPort parsePort <$> lookupEnv "PORT"
    where
      parsePort s = fromMaybe (panic $ "Invalid port: " <> show s) $ readMay s

defaultPort :: Int
defaultPort = 8080
