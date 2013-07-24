module BarTender.Util where

import Control.Applicative
import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

-- | Run a monad in a loop, each time testing its return value to decide if we
-- should continue
doWhile :: Monad m
        => (a -> Bool) -- ^ The condition function
        -> m a         -- ^ The loop body
        -> m a
doWhile cond body = do
    x <- body
    if cond x
        then doWhile cond body
        else return x

-- | Conditionally wraps a value in a Maybe. Satisfies the following axioms:
--
-- > ifJust True  ==  Just
-- > ifJust False  ==  const Nothing
ifJust :: Bool -> a -> Maybe a
ifJust b x = guard b >> Just x

-- | Read a value from a string, indicating failure with @Nothing@
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Convert a string to a bool in an intelligent way: If the string is one of
-- @\"false\"@, @\"no\"@, or @\"off\"@, then the result is @Just False@. If the
-- string is one of @\"true\"@, @\"yes\"@, or @\"on\"@, then the result is
-- @Just True@. Otherwise the result is @Nothing@. The match is
-- case-insensitive.
smartReadBool :: String -> Maybe Bool
smartReadBool str = listToMaybe . catMaybes . map getResult $
    [ (True,  [ "true", "yes", "on" ])
    , (False, [ "false", "no", "off" ])
    ]
    where
        getResult :: (Bool, [String]) -> Maybe Bool
        getResult (bool, ls) = ifJust ((elem . map toLower) str ls) bool

-- | Join a list of strings by spaces
spaced :: [String] -> String
spaced = intercalate " "

