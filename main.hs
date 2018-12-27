{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Monad.IO.Class as IO
import GHC.Generics
import Data.Aeson
import Web.Scotty as S
import PointsAndLines
import Polygons
import Gradients
import Debug.Trace


data Output = Output {
  oType :: String,
  value :: String
} deriving (Show, Generic)


instance ToJSON Output where
  toEncoding = genericToEncoding defaultOptions

data Request = Request {
  func :: String,
  args :: [[[Double]]]
} deriving (Show, Generic, Read)

instance FromJSON Request

main :: IO ()
main = scotty 9200 $ do
  get "/" $ do
    IO.liftIO $ putStrLn "on home page..."
    setHeader "Content-Type" "text/html"
    file "index.html"

  post "/url" $ do
    IO.liftIO $ putStrLn "eval request..."
    obj <- S.jsonData :: ActionM Request
    IO.liftIO $ putStrLn $ show obj
    setHeader "Content-Type" "application/json"
    S.json $ Output {
      oType = "success", 
      value=eval (func obj) (args obj)
    }

  get "/:file" $ do
    dir <- T.unpack <$> param "file"
    IO.liftIO $ putStrLn $ "get request: " ++ dir
    file dir

eval :: String -> [[[Double]]] -> String
eval func args = case func of
  -- first 3: used for detect onclick
  "dist" -> show $ 
    dist (parsePt (args!!0)) (parsePt (args!!1))
  "outsidedness" -> show $ 
    outsidedness (parsePoly (args!!0)) (parsePt (args!!1))
  "shortestDistPS" -> show $ 
    shortestDistPS (parsePt (args!!0)) (parseSeg (args!!1))

  -- below: query functions to test
  "closestPointPS" -> strPt $
    closestPointPS (parsePt (args!!0)) (parseSeg (args!!1))
  "intersectionSS" -> strMaybePt $
    intersectionSS (parseSeg (args!!0)) (parseSeg (args!!1))
  "shortestSegmentSS" -> strSeg $
    shortestSegmentSS (parseSeg (args!!0)) (parseSeg (args!!1))
  "closestPointGP" -> strPt $
    closestPointGP (parsePoly (args!!0)) (parsePt (args!!1))
  "segIsInside" -> strBool $ 
    segIsInside (parsePoly (args!!0)) (parseSeg (args!!1))
  "shortestSegmentGS" -> strSeg $
    shortestSegmentGS (parsePoly (args!!0)) (parseSeg (args!!1))
  "shortestSegmentGG" -> strSeg $
    shortestSegmentGG (parsePoly (args!!0)) (parsePoly (args!!1))
  "maxUDistSegGS" -> strSeg $
    maxUDistSegGS (parsePoly (args!!0)) (parseSeg (args!!1))
  "maxUDistSegGG" -> strSeg $
    maxUDistSegGG (parsePoly (args!!0)) (parsePoly (args!!1))
  "minSignedDistSegGG" -> strSeg $
    minSignedDistSegGG (parsePoly (args!!0)) (parsePoly (args!!1))
  "maxSignedDistSegGG" -> strSeg $
    maxSignedDistSegGG (parsePoly (args!!0)) (parsePoly (args!!1))

  -- below: grad functions
  "graphDistPsiPSC" -> (\(a,b) -> "["++(show a)++","++(show b)++"]") $ 
    graphDistPsiPSC (parsePt (args!!0)) (parseSeg (args!!1)) (parsePt (args!!2))
  "graphDist2PsiPSC" -> (\(a,b) -> "["++(show a)++","++(show b)++"]") $ 
    graphDist2PsiPSC (parsePt (args!!0)) (parseSeg (args!!1)) (parseSeg (args!!2)) (parsePt (args!!3))
  "graphDistPsiPGC" -> (\(a,b) -> "["++(show a)++","++(show b)++"]") $
    graphDistPsiPGC (parsePt (args!!0)) (parsePoly (args!!1)) (parsePt (args!!2))
  "graphDistPsiSSC" -> (\(a,b) -> "["++(show a)++","++(show b)++"]") $
    graphDistPsiSSC (parseSeg (args!!0)) (parseSeg (args!!1)) (parsePt (args!!2))
  "graphDistPsiSGC" -> (\(a,b) -> "["++(show a)++","++(show b)++"]") $
    graphDistPsiSGC (parseSeg (args!!0)) (parsePoly (args!!1)) (parsePt (args!!2))
  "graphDistPsiGGC" -> (\(a,b) -> "["++(show a)++","++(show b)++"]") $
    graphDistPsiGGC (parsePoly (args!!0)) (parsePoly (args!!1)) (parsePt (args!!2))
  "scalePSgraph" -> (\(a,b) -> "["++(show a)++","++(show b)++"]") $
    scalePSgraph (parsePt (args!!0)) (parseSeg (args!!1))

  "movepPS" -> (\(a,b) -> "[" ++ (strPt a) ++ "," ++ (show b) ++ "]")$ 
    movepPS (parsePt (args!!0)) (parseSeg (args!!1))
  "movexyPS" -> (\(a,b) -> "[" ++ (strPt a) ++ "," ++ (show b) ++ "]")$ 
    movexyPS (parsePt (args!!0)) (parseSeg (args!!1))
  "rotxyPSTout" -> (\(a,b) -> "[" ++ (strSeg a) ++ "," ++ (show b) ++ "]") $
    rotxyPSTout (parsePt (args!!0)) (parseSeg (args!!1))
  "rotxyPSCout" -> (\(a,b,c) -> "[" ++ (strSeg a) ++ "," ++ (show b) ++ "," ++ (show c) ++ "]") $
    rotxyPSCout (parsePt (args!!0)) (parseSeg (args!!1)) (parsePt (args!!2)) (parseNum (args!!3))
  "scalexyPSCout" -> (\(a,b) -> "[" ++ (strSeg a) ++ "," ++ (show b) ++ "]") $
    scalexyPSCout (parsePt (args!!0)) (parseSeg (args!!1)) (parsePt (args!!2))
  -- reduced ones below
  "movepPSS" -> (\(a,b) -> "[" ++ (strPt a) ++ "," ++ (show b) ++ "]")$ 
    movepPSS (parsePt (args!!0)) (parseSeg (args!!1)) (parseSeg (args!!2))
  "movexyPSS" -> (\(a,b) -> "[" ++ (strPt a) ++ "," ++ (show b) ++ "]")$ 
    movexyPSS (parsePt (args!!0)) (parseSeg (args!!1)) (parseSeg (args!!2))
  "rotxyPSSCout" -> (\((a1,a2),b)->"[["++(strSeg a1)++","++(strSeg a2)++"],"++(show b)++"]")$
    rotxyPSSCout (parsePt (args!!0)) (parseSeg (args!!1)) (parseSeg (args!!2)) (parsePt (args!!3))
  "rotbPGCout" -> (\(a,b,c) -> "["++(strPoly a)++","++(show b)++ "," ++(show c)++"]") $
    rotbPGCout (parsePt (args!!0)) (parsePoly (args!!1)) (parsePt (args!!2)) (parseNum (args!!3))
  "rotbSSCout" -> (\(a,b,c) -> "[" ++ (strSeg a) ++ "," ++ (show b) ++ "," ++ (show c) ++ "]") $
    rotbSSCout (parseSeg (args!!0)) (parseSeg (args!!1)) (parsePt (args!!2)) (parseNum (args!!3))
  "rotbSGCout" -> (\(a,b,c) -> "["++(strPoly a)++","++(show b)++ "," ++(show c)++"]") $
    rotbSGCout (parseSeg (args!!0)) (parsePoly (args!!1)) (parsePt (args!!2)) (parseNum (args!!3))
  "rotbGGCout" -> (\(a,b,c) -> "["++(strPoly a)++","++(show b)++ "," ++(show c)++"]") $
    rotbGGCout (parsePoly (args!!0)) (parsePoly (args!!1)) (parsePt (args!!2)) (parseNum (args!!3))

  -- below: for test in js
  "maxUDistGG" -> show $
    maxUDistGG (parsePoly (args!!0)) (parsePoly (args!!1))
  "maxUDistGGtest" -> show $
    maxUDistGGtest (parsePoly (args!!0)) (parsePoly (args!!1))
  "maxUDistGGtestSeg" -> strSeg $
    maxUDistGGtestSeg (parsePoly (args!!0)) (parsePoly (args!!1))
  "maxUDistSegGSaprx" -> strSeg $ 
    maxUDistSegGSaprx (parsePoly (args!!0)) (parseSeg (args!!1))

parseNum :: [[Double]] -> Double
parseNum arg = arg!!0!!0

parsePt :: [[Double]] -> Point
parsePt arg = (arg!!0!!0, arg!!0!!1)

parseSeg :: [[Double]] -> LineSeg
parseSeg arg = let
  p1 = (arg!!0!!0, arg!!0!!1)
  p2 = (arg!!1!!0, arg!!1!!1)
  in (p1, p2)

parsePoly :: [[Double]] -> Polygon
parsePoly arg = map (\(a:b:_)->(a,b)) arg

strBool :: Bool -> String
strBool b = if b then "true" else "false"

strPt :: Point -> String
strPt (x, y) = "[" ++ (show x) ++ "," ++ (show y) ++ "]"

strSeg :: LineSeg -> String
strSeg (p1, p2) = "[" ++ (strPt p1) ++ "," ++ (strPt p2) ++ "]"

strPoly :: Polygon -> String
strPoly poly = let
  res = foldl (\a b->a++","++(strPt b)) (strPt $ poly!!0) (tail poly)
  in "[" ++ res ++ "]"

strMaybePt :: Maybe Point -> String
strMaybePt mp = case mp of 
  Nothing ->"{Just: false}"
  Just p -> 
    "{\"Just\": true, \"value\": " ++ (strPt p) ++ "}"
