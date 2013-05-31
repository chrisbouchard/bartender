module StatusBar.Options where

import Control.Applicative
import Control.Monad
import Control.Monad.Error

import Data.Data
import Data.ConfigFile
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

-- Create options that know their order in the specification
getOptionListWithIndex :: [OptDescr a] -> [OptDescr (Int, a)]
getOptionListWithIndex opts = getOptionWithIndex <$> zip [1..] opts
    where
        getOptionWithIndex :: (Int, OptDescr a) -> OptDescr (Int, a)
        getOptionWithIndex (i, Option flags names descr help) =
            Option flags names (getArgDescrWithIndex i descr) help

        getArgDescrWithIndex :: Int -> ArgDescr a -> ArgDescr (Int, a)
        getArgDescrWithIndex i (NoArg x) = NoArg (i, x)
        getArgDescrWithIndex i (ReqArg fn str) = ReqArg (\x -> (i, fn x)) str
        getArgDescrWithIndex i (OptArg fn str) = OptArg (\x -> (i, fn x)) str

readConfigFile :: [OptDescr (Int, a)] -> FilePath -> IO (Map Int a)
readConfigFile opts path = do
    eCp <- readfile emptyCP path
    return $ case eCp of
        Left _   -> M.empty
        Right cp -> M.unions $ readOption cp <$> opts
    where
        readOption :: ConfigParser -> OptDescr (Int, a) -> Map Int a
        readOption cp (Option _ names descr _) =
            createMap . listToMaybe . catMaybes $
                readSingleOption cp descr <$> names

        createMap :: Maybe (Int, a) -> Map Int a
        createMap = maybe M.empty $ uncurry M.singleton

        readSingleOption :: ConfigParser -> ArgDescr a -> String -> Maybe a

        readSingleOption cp (NoArg x) name =
            case get cp "DEFAULT" name of
                Right True -> Just x
                _          -> Nothing

        readSingleOption cp (ReqArg fn _) name =
            case get cp "DEFAULT" name of
                Right str -> Just $ fn str
                _         -> Nothing

        readSingleOption cp (OptArg fn _) name =
            case get cp "DEFAULT" name of
                Right ""  -> Just $ fn Nothing
                Right str -> Just $ fn $ Just str
                _         -> Nothing

readCommandArgs :: [OptDescr (Int, a)] -> ArgOrder (Int, a) -> IO (Map Int a)
readCommandArgs opts argorder = do
    argv <- getArgs
    case getOpt argorder opts argv of
         -- TODO: Return n somehow (non-options)
        (o, n, [])   -> return $ M.fromList o
        (_, _, errs) -> ioError $ userError "Oops"
    where

