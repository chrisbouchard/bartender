module BarTender.Util where

import Control.Applicative
import Control.Monad

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

-- | Read a value from a string, indicating failure with Nothing
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Join a list of strings by spaces
spaced :: [String] -> String
spaced = intercalate " "

-- | Conditionally wraps a value in a Maybe. Satisfies the following axioms:
--
-- > ifJust True  ==  Just
-- > ifJust False  ==  const Nothing
ifJust :: Bool -> a -> Maybe a
ifJust b x = guard b >> Just x

