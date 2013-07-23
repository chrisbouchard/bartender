module BarTender.Options
    ( getConfigOpt'
    ) where

import Control.Monad

import Data.Char
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

import Text.ParserCombinators.Parsec

getConfigOpt' :: ArgOrder a -> [OptDescr a] -> FilePath -> IO ([a], [String], [String], [String])
getConfigOpt' order descList path = do
    errorOrPairs <- parseFromFile file path
    return $ case errorOrPairs of
        Left error  -> ([], [], [], [show error])
        Right pairs -> getOpt' order descList $ foldr fn [] pairs
    where
        fn :: (String, String) -> [String] -> [String]
        fn (key, value) list = (++ list) $ case getArgDescr key descList of
            Just (NoArg x)       -> ["--" ++ key]
            Just (ReqArg fn str) -> ["--" ++ key, value]
            Nothing              -> ["--" ++ key, value]

getArgDescr :: String -> [OptDescr a] -> Maybe (ArgDescr a)
getArgDescr key ls = listToMaybe . catMaybes $ do
    (Option shortLs longLs descr help) <- ls
    return $ if key `elem` longLs
        then Just $ mapDescr descr
        else Nothing
    where
        mapDescr :: ArgDescr a -> ArgDescr a
        mapDescr (NoArg x)       = NoArg x
        mapDescr (ReqArg fn str) = ReqArg fn str
        mapDescr (OptArg fn str) = ReqArg (fn . Just) str

eol :: Parser ()
eol = do oneOf "\n\r"
         return ()
      <?> "end of line"

comment :: Parser ()
comment = do char '#'
             manyTill anyChar (try eol)
             return ()
          <?> "comment"

item :: Parser (String, String)
item = do
    key <- manyTill anyChar $ char '='
    skipMany space
    value <- manyTill anyChar $ try eol <|> try comment <|> eof
    return (rstrip key, rstrip value)
    where
        rstrip :: String -> String
        rstrip = reverse . dropWhile isSpace . reverse

line :: Parser (Maybe (String, String))
line = do
    skipMany space
    try (comment >> return Nothing) <|> (item >>= return . Just)

file :: Parser [(String, String)]
file = many line >>= return . catMaybes

