{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , defaultConfig
  , App
  , AppIO
  , ask
  , asks
  , liftIO
  , appRunWithConfig
  )
where

import Common

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Text (Text)

--------------------------------------------------------------------------------

data Config = Config
   { configOutputSep :: Text
   , configOutputIndent :: Text
   , configInputSep :: Text -- ^ empty string separates by whitespace
   }
   deriving (Eq, Show)


type App a = Reader Config a

type AppIO a = ReaderT Config IO a


defaultConfig = Config
   { configOutputSep = "\t"
   , configOutputIndent = "   "
   , configInputSep = ""
   }


appRunWithConfig :: Config -> AppIO a -> IO a
appRunWithConfig = flip runReaderT
