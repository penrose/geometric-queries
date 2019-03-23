{-# LANGUAGE RankNTypes, AllowAmbiguousTypes, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleContexts #-}

module Main where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Monad.IO.Class as IO
import GHC.Generics
import Data.Aeson
import Web.Scotty as S
import Debug.Trace
import ShapeUtils

data Output = Output {
  oType :: String,
  value :: String
} deriving (Show, Generic)

instance ToJSON Output where
  toEncoding = genericToEncoding defaultOptions

data Request = Request {
  func :: String,
  args :: String
} deriving (Show, Generic, Read)

instance FromJSON Request

main :: IO ()
main = scotty 9200 $ do
  get "/" $ do
    IO.liftIO $ putStrLn "on home page..."
    setHeader "Content-Type" "text/html"
    file "index.html"

  post "/eval" $ do
    IO.liftIO $ putStrLn "eval request..."
    obj <- S.jsonData :: ActionM Request
    IO.liftIO $ putStrLn $ show obj
    setHeader "Content-Type" "application/json"
    S.json $ Output {
      oType = "success", 
      value = eval (func obj) (args obj)
    }

  get "/:file" $ do
    dir <- T.unpack <$> param "file"
    IO.liftIO $ putStrLn $ "get request: " ++ dir
    file dir

eval :: String -> String -> String
eval func args = case func of
  "greet" -> "hello from server!"
  "echo" -> let
    res = echo $ (read args :: (forall a. (Read a, Autofloat a) => [ShapeNode a]))
    in state2str res

