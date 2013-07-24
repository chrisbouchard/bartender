module BarTender.Options
    ( OptDescr (..)
    , ArgDescr (..)
    , FilePath
    , getConfigOpt
    , getConfigOpt'
    , getEnvironOpt
    , getEnvironOpt'
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

-- | Works like @getOpt@ from "System.Console.GetOpt", except it parses a
-- config file instead of a list of tokens. Short option names are ignored.
getConfigOpt :: [OptDescr a]
             -> FilePath
             -> IO ([a], [String], [String])
getConfigOpt descList path = do
    (x, y, z, _) <- getConfigOpt' descList path
    return (x, y, z)

-- | Works like @getOpt'@ from "System.Console.GetOpt", except it parses a
-- config file instead of a list of tokens. Short option names are ignored.
getConfigOpt' :: [OptDescr a]
              -> FilePath
              -> IO ([a], [String], [String], [String])
getConfigOpt' descList path = do
    errorOrPairs <- parseFromFile file path
    return $ case errorOrPairs of
        Left error  -> ([], [], [], [show error])
        Right pairs -> getOpt' RequireOrder descList $ foldr fn [] pairs
    where
        fn :: (String, String) -> [String] -> [String]
        fn (key, value) list = (++ list) $ case getArgDescr key descList of
            Just (NoArg _)    -> case smartReadBool value of
                                     Just True  -> ["--" ++ key]
                                     Just False -> ["--no-" ++ key]
                                     Nothing    -> ["--" ++ key]
            Just (OptArg _ _) -> if null value
                                     then ["--" ++ key]
                                     else ["--" ++ key, value]
            Just (ReqArg _ _) -> ["--" ++ key, value]
            Nothing           -> []

        getArgDescr :: String -> [OptDescr a] -> Maybe (ArgDescr a)
        getArgDescr key ls = listToMaybe . catMaybes $ do
            (Option shortLs longLs descr help) <- ls
            return $ ifJust (key `elem` longLs) descr

-- | Works like @getOpt@ from "System.Console.GetOpt", except it searches the
-- environment for options instead of parsing a list of tokens. Short option
-- names are ignored. A flag @--foo-bar@ will correspond to the environment
-- variable @FOO_BAR@.
getEnvironOpt :: [OptDescr a]
              -> IO ([a], [String], [String])
getEnvironOpt descList = do
    (x, y, z, _) <- getEnvironOpt' descList
    return (x, y, z)

-- | Works like @getOpt'@ from "System.Console.GetOpt", except it searches the
-- environment for options instead of parsing a list of tokens. Short option
-- names are ignored. A flag @--foo-bar@ will correspond to the environment
-- variable @FOO_BAR@.
getEnvironOpt' :: [OptDescr a]
               -> IO ([a], [String], [String], [String])
getEnvironOpt' descList = do
    argList <- fmap concat . sequence $ map optionToArgs descList
    return $ getOpt' RequireOrder descList $ argList
    where
        optionToArgs :: OptDescr a -> IO [String]
        optionToArgs (Option shortLs longLs descr help) = do
            mValue <- getEnvironValue longLs
            return $ case mValue of
                Nothing    -> []
                Just value -> let key = head longLs in
                    case descr of
                        NoArg _    -> ["--" ++ key]
                        OptArg _ _ -> ["--" ++ key, value]
                        ReqArg _ _ -> if null value
                                          then ["--" ++ key]
                                          else ["--" ++ key, value]

        getEnvironValue :: [String] -> IO (Maybe String)
        getEnvironValue keyLs = do
            resultLs <- sequence $ map (lookupEnv . toEnvironKey) keyLs
            return . listToMaybe $ catMaybes resultLs

        toEnvironKey :: String -> String
        toEnvironKey = map $ toUpper . (\c -> if c == '-' then '_' else c)

eol :: Parser ()
eol = do
    try (void $ oneOf "\n\r") <|> eof
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
    value <- manyTill anyChar $ try eol <|> comment
    return (rstrip key, rstrip value)
    where
        rstrip :: String -> String
        rstrip = reverse . dropWhile isSpace . reverse

line :: Parser (Maybe (String, String))
line = do
    mPair <- try (comment >> return Nothing) <|> (item >>= return . Just)
    skipMany space
    return mPair

file :: Parser [(String, String)]
file = do
    skipMany space
    fmap catMaybes $ many line

