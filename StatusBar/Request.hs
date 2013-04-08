module StatusBar.Request where

import Control.Applicative

import Data.List
import Data.Maybe

import Network.Socket

-- | A request from the client or server
data Request = Request SockAddr Message

instance Show Request where
    show (Request addr message) = "[" ++ show addr ++ "]: " ++ show message

-- | Handle a request, and possibly generate a response request
type RequestHandler = Request -> IO (Maybe Request)

-- | The message body of a request
data Message = RAck Int Int Int   -- ^ Message for a client to begin sending
                                  -- updates. Contains the client's assigned
                                  -- ID and the server's timeout and version.

             | RAlive Int         -- ^ Message to not drop the client. Contains
                                  -- the client's ID.

             | RError String      -- ^ Message to indicate that the previous
                                  -- message was not accepted. Contains the
                                  -- error message.

             | RInit String       -- ^ Message for the server to initialize a
                                  -- new client. Contains the client's name.

             | RMalformed String  -- ^ An improperly formed message. Contains
                                  -- the message contents.

             | RPoke              -- ^ Message for a client to send up update.
                                  -- Sent by the server before dropping the
                                  -- client.

             | RUpdate Int String -- ^ Message to update the status. Contains
                                  -- the sender's ID and the new status.

             | RUnknown String    -- ^ An unrecognized message.
    deriving Eq

instance Show Message where
    -- | Create a string representation of a message
    show (RAck cid timeout version) = spaced ["ack", show cid, show timeout, show version]
    show (RAlive cid)               = spaced ["alive", show cid]
    show (RError message)           = spaced ["error", message]
    show (RInit name)               = spaced ["init", name]
    show (RMalformed content)       = spaced ["[Malformed]", content]
    show RPoke                      = "poke"
    show (RUpdate cid content)      = spaced ["update", show cid, content]
    show (RUnknown message)         = spaced ["[Unknown]", message]

-- | Read a value from a string, indicating failure with Nothing
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Get the command from a message
messageCommand :: Message -> String
messageCommand (RAck _ _ _)   = "ack"
messageCommand (RAlive _)     = "alive"
messageCommand (RError _)     = "error"
messageCommand (RInit _)      = "init"
messageCommand (RMalformed _) = "[Malformed]"
messageCommand RPoke          = "poke"
messageCommand (RUpdate _ _)  = "update"
messageCommand (RUnknown cmd) = cmd

-- | Split a string into the next token and the remainder of the string.
nextToken :: String -> (String, String)
nextToken str = nextToken' (lstrip str) ""
    where
        nextToken' ""        tok = (tok, "")
        nextToken' (' ':str) tok = (tok, lstrip str)
        nextToken' (ch:str)  tok = nextToken' str (tok ++ [ch])

        lstrip ""        = ""
        lstrip (' ':str) = lstrip str
        lstrip str       = str

-- | Split off the next n tokens. The resulting list will contain the tokens
-- and the remainder of the string.
splitTokens :: Int -> String -> [String]
splitTokens 0 str = [str]
splitTokens n str = tok : splitTokens (n - 1) rest
    where (tok, rest) = nextToken str

-- | Generate an error message stating that a particular command is not
-- implemented.
notImplementedMessage :: Message -> Message
notImplementedMessage message = RError $ cmd ++ ": Not implemented"
    where cmd = messageCommand message

badClientMessage :: Int -> Message
badClientMessage cid = RError $ "Unkonwn client id: " ++ show cid

-- | Parse a string into a message
parseMessage :: String -> Message
parseMessage str = fromMaybe (RMalformed str) $ parseBody cmd body
    where
        (cmd, body) = nextToken str

        parseBody :: String -> String -> Maybe Message

        parseBody "ack" body = RAck <$> cid <*> timeout <*> version
            where (idStr : timeoutStr : versionStr : _) = splitTokens 3 body
                  cid = maybeRead idStr
                  timeout = maybeRead timeoutStr
                  version = maybeRead versionStr

        parseBody "alive" body = RAlive <$> cid
            where (idStr : _) = splitTokens 1 body
                  cid = maybeRead idStr

        parseBody "error" body = Just $ RError body

        parseBody "init" body = Just $ RInit name
            where (name : _) = splitTokens 1 body

        parseBody "poke" body = Just RPoke

        parseBody "update" body = RUpdate <$> cid <*> Just content
            where (idStr : content : _) = splitTokens 1 body
                  cid = maybeRead idStr

        parseBody "" body = Nothing

        parseBody cmd body = Just $ RUnknown cmd

-- | Join a list of strings by spaces
spaced :: [String] -> String
spaced = intercalate " "

