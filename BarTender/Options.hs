module BarTender.Options where

import Control.Applicative
import Control.Monad
import Control.Monad.Error

import Data.Char
import Data.ConfigFile
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

readCommandLineOptions :: [OptDescr (Int, a)] -> ArgOrder (Int, a) -> IO (Map Int a)
readCommandLineOptions opts order = do
    argv <- getArgs
    case getOpt order opts argv of
         -- TODO: Return n somehow (non-options)
        (o, n, [])   -> return $ M.fromList o
        (_, _, errs) -> ioError $ userError "Oops"

readConfigFileOptions :: [OptDescr (Int, a)] -> FilePath -> IO (Map Int a)
readConfigFileOptions opts path = do
    eCp <- readfile emptyCP path
    return $ case eCp of
        Left _   -> M.empty
        Right cp -> M.unions $ readOption (ignoreCase cp) <$> opts
    where
        ignoreCase :: ConfigParser -> ConfigParser
        ignoreCase cp = cp { optionxform = map toUpper }

        readOption :: ConfigParser -> OptDescr (Int, a) -> Map Int a
        readOption cp (Option _ names descr _) =
            createMap . listToMaybe . catMaybes $
                readSingleOption cp descr <$> names

        createMap :: Maybe (Int, a) -> Map Int a
        createMap = maybe M.empty $ uncurry M.singleton

        readSingleOption :: ConfigParser -> ArgDescr (Int, a) -> String -> Maybe (Int, a)

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
                Right str -> Just . fn $ Just str
                _         -> Nothing

readEnvironmentOptions :: [OptDescr (Int, a)] -> String -> IO (Map Int a)
readEnvironmentOptions opts prefix = M.unions <$> (sequence $ lookupOption <$> opts)
    where
        lookupOption :: OptDescr (Int, a) -> IO (Map Int a)
        lookupOption (Option _ names descr _) =
            createMap <$> listToMaybe . catMaybes <$>
                sequence (lookupSingleOption descr . nameToKey <$> names)

        createMap :: Maybe (Int, a) -> Map Int a
        createMap = maybe M.empty $ uncurry M.singleton

        lookupSingleOption :: ArgDescr (Int, a) -> String -> IO (Maybe (Int, a))
        lookupSingleOption (NoArg x)     key = lookupEnv key >>= return . (const x <$>)
        lookupSingleOption (ReqArg fn _) key = lookupEnv key >>= return . (fn <$>)
        lookupSingleOption (OptArg fn _) key = do
            mStr <- lookupEnv key
            return $ case mStr of
                Just ""  -> Just $ fn Nothing
                Just str -> Just . fn $ Just str
                _        -> Nothing

        nameToKey :: String -> String
        nameToKey name = map toUpper $ prefix ++ "_" ++ name

readAllOptions :: [OptDescr a] -> FilePath -> ArgOrder a -> String -> IO [a]
readAllOptions opts path order prefix = do
    configOpts <- readConfigFileOptions indexedOpts path
    envOpts <- readEnvironmentOptions indexedOpts prefix
    cmdOpts <- readCommandLineOptions indexedOpts indexedArgOrder

    -- Union is left-biased, so we'll favor command-line options over
    -- environment options over config-file options.
    return . M.elems $ cmdOpts `M.union` envOpts `M.union` configOpts
    where
        -- I can't give these types because Haskell 98 doesn't allow you to
        -- include a type variable bound by your context.
        indexedOpts = getOptionListWithIndex opts
        indexedArgOrder = getArgOrderWithIndex order

        -- Create options that know their order in the specification
        getOptionListWithIndex :: [OptDescr a] -> [OptDescr (Int, a)]
        getOptionListWithIndex opts = getOptionWithIndex <$> zip [1..] opts

        getOptionWithIndex :: (Int, OptDescr a) -> OptDescr (Int, a)
        getOptionWithIndex (i, Option flags names descr help) =
            Option flags names (getArgDescrWithIndex i descr) help

        getArgDescrWithIndex :: Int -> ArgDescr a -> ArgDescr (Int, a)
        getArgDescrWithIndex i (NoArg x)       = NoArg (i, x)
        getArgDescrWithIndex i (ReqArg fn str) = ReqArg (\x -> (i, fn x)) str
        getArgDescrWithIndex i (OptArg fn str) = OptArg (\x -> (i, fn x)) str

        getArgOrderWithIndex :: ArgOrder a -> ArgOrder (Int, a)
        getArgOrderWithIndex (ReturnInOrder fn) = ReturnInOrder (\x -> (0, fn x))

