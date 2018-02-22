{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config(..)
  , defaultConfig
  , AppT
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


type AppT m a = ReaderT Config m a


defaultConfig = Config
   { configOutputSep = "\t"
   , configOutputIndent = "   "
   , configInputSep = ""
   }


appRunWithConfig :: Config -> AppT IO a -> IO a
appRunWithConfig = flip runReaderT
