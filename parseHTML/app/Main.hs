{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import qualified Data.Text.IO as T
import Network.HTTP.Conduit
import Text.HTML.DOM
import Text.XML.Cursor

main :: IO ()
main = do
  doc <- parseLBS <$> simpleHttp "https://qiita.com/kxkx5150/items/23922f2a88a4241c7779"
  let root = fromDocument doc
  let atags = getAtags root
  putStrLn $ show (length atags) ++ " elements."
  mapM_ T.putStrLn $ atags >>= attribute "href"

getAtags :: Cursor -> [Cursor]
getAtags root = do
  cs <- descendant root
  element "a" cs


