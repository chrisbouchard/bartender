module StatusBar.Dzen where

-- | The color of a disabled panel in the status bar
disabledColor :: String
disabledColor = "#333333"

-- | Apply foreground and background color to string using Dzen escapes.
-- Note that @dzenColor fg bg@ and @dzenColorFG fg . dzenColorBG bg@ are
-- equivalent.
dzenColor :: String -- ^ Foreground color
          -> String -- ^ Background color
          -> String -- ^ The string to wrap with color
          -> String
dzenColor fg bg = dzenColorFG fg . dzenColorBG bg

-- | Apply foreground color to string using Dzen escapes.
dzenColorFG :: String -- ^ Foreground color
            -> String -- ^ The string to wrap with color
            -> String
dzenColorFG fg = wrap (wrap "^fg(" ")" fg) "^fg()"

-- | Apply background color to string using Dzen escapes.
dzenColorBG :: String -- ^ Background color
            -> String -- ^ The string to wrap with color
            -> String
dzenColorBG bg = wrap (wrap "^bg(" ")" bg) "^bg()"

-- | Strip all the Dzen color escapes from a string.
dzenRemoveColor :: String -- ^ String to strip of color
                -> String
dzenRemoveColor str = removeNormal str ""
    where
        removeNormal ""        result = result
        removeNormal ('^':str) result = removeCmd str "^" result
        removeNormal (ch:str)  result = removeNormal str (result ++ [ch])

        removeCmd ('f':'g':'(':str) hold result = removeToParen str result
        removeCmd ('b':'g':'(':str) hold result = removeToParen str result
        removeCmd str               hold result = removeNormal str (result ++ hold)

        removeToParen ""        result = result
        removeToParen (')':str) result = removeNormal str result
        removeToParen (_:str)   result = removeToParen str result

-- | Wrap a string with left- and right-hand strings.
wrap :: String -- ^ Left-hand string
     -> String -- ^ Right-hand string
     -> String -- ^ The string to wrap
     -> String
wrap left right str = left ++ str ++ right

