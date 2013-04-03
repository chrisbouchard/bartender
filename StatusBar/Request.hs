module StatusBar.Request where

import Network.Socket

-- | A request from the client or server
data Request = Request
    { requestCmd    :: String   -- ^ The command token
    , requestBody   :: String   -- ^ The remainder of the message
    , requestClient :: SockAddr -- ^ The address of the client who sent the
                                -- request
    }

-- | Split a string into the next token and the remainder of the string.
nextToken :: String -> (String, String)
nextToken str = nextToken' (lstrip str) ""
    where
        nextToken' ""        tok = (tok, "")
        nextToken' (' ':str) tok = (tok, lstrip str)
        nextToken' (ch:str)  tok = nextToken' str (tok ++ [ch])

        lstrip ""        = ""
        lstrip (' ':str) = lstrip str
        lstrip (_:str)   = str

-- | Create a request from a message string
parseRequestFrom :: SockAddr -> String -> Request
parseRequestFrom client message = let
    (cmd, body) = nextToken message in Request
        { requestCmd    = cmd
        , requestBody   = body
        , requestClient = client
        }

