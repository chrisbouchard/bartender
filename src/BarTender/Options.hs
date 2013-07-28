module BarTender.Options
    ( module System.Console.GetOpt
    , FilePath
    , Bound (..)
    , inBound
    , outOfBound
    , completeOption
    , getConfigOpt
    , getEnvironOpt
    , handleOpt
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

data Bound a = AtLeast a
             | AtMost a
             | Between a a
             | Exactly a
             | Unbounded
    deriving Eq

-- | Determine if a value is inside of a bound, inclusively.
inBound :: Ord a => a -> Bound a -> Bool
inBound x (AtLeast y)   = y <= x
inBound x (AtMost y)    = x <= y
inBound x (Between y z) = y <= x && x <= z
inBound x (Exactly y)   = x == y
inBound x Unbounded     = True

-- | Determine if a value is outside of a bound, where inclusion is inclusive.
outOfBound :: Ord a => a -> Bound a -> Bool
outOfBound = ((.) . (.)) not inBound


-- | Complement a flag with the corresponding "--no-" flag. The base flag
-- enables a feature, and its inverse disables the feature.
completeOption :: OptDescr (Bool -> a) -> [OptDescr a]
completeOption (Option shortLs longLs descr help) = case descr of
        NoArg fn    -> [ Option shortLs longLs (NoArg $ fn True) help
                       , Option [] (map ("--no-" ++) longLs) (NoArg $ fn False) ""
                       ]
        OptArg fn x -> [ Option shortLs longLs (OptArg (flip fn $ True) x) help ]
        ReqArg fn x -> [ Option shortLs longLs (ReqArg (flip fn $ True) x) help ]

-- | Works like @getOpt@ from "System.Console.GetOpt", except it parses a
-- config file instead of a list of tokens. Short option names are ignored.
getConfigOpt :: [OptDescr a]
             -> FilePath
             -> IO ([a], [String], [String])
getConfigOpt descList path = do
    errorOrPairs <- parseFromFile file path
    return $ case errorOrPairs of
        Left error  -> ([], [], [show error])
        Right pairs -> getOpt RequireOrder descList $ foldr fn [] pairs
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

-- | Works like @getOpt@ from "System.Console.GetOpt", except it searches the
-- environment for options instead of parsing a list of tokens. Short option
-- names are ignored. A flag @--foo-bar@ will correspond to the environment
-- variable @FOO_BAR@.
getEnvironOpt :: [OptDescr a]
              -> IO ([a], [String], [String])
getEnvironOpt descList = do
    ls <- fmap catMaybes . sequence $ map optionToValue descList
    return $ (ls, [], [])
    where
        optionToValue :: OptDescr a -> IO (Maybe a)
        optionToValue (Option _ longLs descr _) = do
            mValue <- getEnvironValue longLs
            return $ case mValue of
                Nothing    -> Nothing
                Just value -> Just $ case descr of
                    NoArg x     -> x
                    OptArg fn _ -> fn $ ifJust (null value) value
                    ReqArg fn _ -> fn value

        getEnvironValue :: [String] -> IO (Maybe String)
        getEnvironValue keyLs = do
            resultLs <- sequence $ map (lookupEnv . toEnvironKey) keyLs
            return . listToMaybe $ catMaybes resultLs

        toEnvironKey :: String -> String
        toEnvironKey = map $ toUpper . (\c -> if c == '-' then '_' else c)

-- | Use the results of a get*Opt function to generate either a result option
-- value or an error message.
handleOpt :: Bound Int
          -> a
          -> ([a -> Either String a], [String], [String])
          -> Either String (a, [String])
handleOpt bound initOpt (fnLs, nonOptLs, errorLs) = result >>= handleNonOpts
    where
        result = foldr (=<<) (Right initOpt) $ map (const . Left) errorLs ++ fnLs

        handleNonOpts opt = if inBound (length nonOptLs) bound
            then Right $ (opt, nonOptLs)
            else Left $ "incorrect number of positional arguments"

