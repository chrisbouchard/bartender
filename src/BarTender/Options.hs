module BarTender.Options
    ( getConfigOpt'
    , parseMaybeBool
    ) where

import Control.Monad

import Data.Char
import Data.Maybe

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

import Text.ParserCombinators.Parsec

import BarTender.Util

-- | Works like getOpt' from System.Console.GetOpt, except it parses a config
-- file instead of a list of tokens. Short option names are ignored.
getConfigOpt' :: ArgOrder a
              -> [OptDescr a]
              -> FilePath
              -> IO ([a], [String], [String], [String])
getConfigOpt' order descList path = do
    errorOrPairs <- parseFromFile file path
    return $ case errorOrPairs of
        Left error  -> ([], [], [], [show error])
        Right pairs -> getOpt' order descList $ foldr fn [] pairs
    where
        fn :: (String, String) -> [String] -> [String]
        fn (key, value) list = (++ list) $ case getArgDescr key descList of
            Just (NoArg x)       -> case parseMaybeBool value of
                                        Just True  -> ["--" ++ key]
                                        Just False -> ["--no-" ++ key]
                                        Nothing    -> []
            Just (OptArg fn str) -> if null value
                                        then ["--" ++ key]
                                        else ["--" ++ key, value]
            Just (ReqArg fn str) -> ["--" ++ key, value]
            Nothing              -> []

        getArgDescr :: String -> [OptDescr a] -> Maybe (ArgDescr a)
        getArgDescr key ls = listToMaybe . catMaybes $ do
            (Option shortLs longLs descr help) <- ls
            return $ ifJust (key `elem` longLs) descr

-- | Convert a string to a bool in an intelligent way: If the string is one of
-- "false", "no", or "off", then the result is Just False. If the string is one
-- of "true", "yes", or "on", then the result is Just True. Otherwise the
-- result is Nothing. The match is case-insensitive.
parseMaybeBool :: String -> Maybe Bool
parseMaybeBool str = listToMaybe . catMaybes . map getResult $
    [ (True,  [ "true", "yes", "on" ])
    , (False, [ "false", "no", "off" ])
    ]
    where
        getResult :: (Bool, [String]) -> Maybe Bool
        getResult (bool, ls) = ifJust ((elem . map toLower) str ls) bool

eol :: Parser ()
eol = do
    oneOf "\n\r"
    return ()
    <?> "end of line"

comment :: Parser ()
comment = do
    char '#'
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

