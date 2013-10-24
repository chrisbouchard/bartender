module BarTender.NewOptions
    ( OptionsDescr (..)
    , Flag (..)
    , Positional (..)
    , Subcommand (..)
    , Argument (..)
    , ArgOptionsDescr (..)
    , ConvertNoArg (..)
    , ArgParseSettings (..)
    , defaultArgParseSettings
    , argParse
    ) where

import Control.Applicative
import Control.Monad

import Data.Functor.Identity
import Data.List
import Data.Maybe

import BarTender.Util

-- | A description of options for `argParse`.
type ArgOptionsDescr a = OptionsDescr ConvertNoArg a

-- | A description of a command line.
data OptionsDescr f a
    -- | A description of a command line with positional arguments, with the
    -- flags it accepts, the positional arguments it accepts, and an optional
    -- final positional argument for extra trailing tokens.
    = PositionalDescr [Flag f a] [Positional a] (Maybe (Positional a))

    -- | A description of a command line with a subcommand, with the flags it
    -- accepts and the subcommands it accepts.
    | SubcommandDescr [Flag f a] [Subcommand f a]

    deriving Functor

-- | A description of a flag with a list of short flag names, long flag names,
-- a description of the argument, and a documentation string.
data Flag f a = Flag [Char] [String] (Argument f a) String
    deriving Functor

-- | A description of a positional argument, with a function translating the
-- token into a value and a string name for the argument.
data Positional a = Positional (String -> a) String
    deriving Functor

-- | A description of a subcommand, with a string for its name, a documentation
-- string, and a description of the subsequent command line.
data Subcommand f a = Subcommand [String] String (OptionsDescr f a)
    deriving Functor

-- | A description of the argument of a flag. The first argument is a functor
-- type used to generate additional flags for flags with no arguments via the
-- `ConvertNoArg` functor.
data Argument f a
    -- | This flag requires no argument.
    = NoArg (f a)

    -- | This flag requires a single argument.
    | ReqArg (String -> a) String

    -- | This flag requires zero or one arguments.
    | OptArg (Maybe String -> a) String

    deriving Functor

-- | Functor describing how to generate additional flags for flags with no
-- arguments.
data ConvertNoArg a
    -- | Leave this flag alone
    = AsIs a

    -- | Create a second flag to act as the inverse of this flag.
    | WithInverse (Bool -> a)

    -- | Convert this flag into one accepting an optional boolean argument.
    | AsOptArg (Bool -> a)
    deriving Functor

-- | A token from the input tagged with how "flaglike" it is.
data TaggedToken
    -- | This token could act as a short flag
    = ShortFlaglike String

    -- | This token could act as a long flag
    | LongFlaglike String

    -- | This token cannot act as a flag
    | NotFlaglike String

-- | Settings for the `argParse` function.
data ArgParseSettings = ArgParseSettings
    { argShortOpt    :: String -- ^ The prefix for a short flag
    , argLongOpt     :: String -- ^ The prefix for a long flag
    , argLongSep     :: String -- ^ The character that separates a long flag
                               --   from its argument
    , argEndFlags    :: String -- ^ The token that marks all subsequent tokens
                               --   as not flaglike
    , argInverse     :: String -- ^ The prefix for flags that are inverses of
                               --   other flags
    , argSplitShort  :: Bool   -- ^ Whether groupings of short pairs should be
                               --   split (i.e., -ofile to -o file and -abc to
                               --   -a -b -c)
    , argSplitLong   :: Bool   -- ^ Whether long flags with an argument should
                               --   be split (i.e., --flag=value to --flag
                               --   value)
    }
    deriving Show

-- | Some sane default settings for argParse. These settings reflect the
-- "expected behavior" of command line arguments for GNU-style programs.
defaultArgParseSettings = ArgParseSettings
    { argShortOpt    = "-"
    , argLongOpt     = "--"
    , argLongSep     = "="
    , argEndFlags    = "--"
    , argInverse     = "no-"
    , argSplitShort  = True
    , argSplitLong   = True
    }


-- | Parse a list of input tokens using a description of command line
-- arguments. Returns either an error string or a list of the values produced
-- during the parse.
argParse :: ArgParseSettings  -- ^ The settings for this parse
         -> ArgOptionsDescr a -- ^ A description of the command line arguments
         -> [String]          -- ^ The input tokens
         -> Either String [a]
argParse settings descr args =
    sequence $ parseDescr finalDescr Nothing finalToks
    where
        finalDescr = convertOptionsDescr $ fmap Right descr

        finalToks = tagFlags args -- >>= splitFlag


        -- Tag each token in the list with its "flagginess", i.e., whether it
        -- could act as a flag. This function will remove the flag end token
        -- from the list and mark all subsequent tokens as not flaglike. Each
        -- token is stipped of its flag prefix if present.
        tagFlags :: [String] -> [TaggedToken]
        tagFlags []            = []
        tagFlags (str:remArgs)
            | (str == argEndFlags settings) = map NotFlaglike remArgs
            | otherwise                     = tagType str : tagFlags remArgs
            where
                tagType
                    | (argLongOpt settings `isPrefixOf` str) &&
                      length str > length (argLongOpt settings) =
                        LongFlaglike . drop (length $ argLongOpt settings)
                    | (argShortOpt settings `isPrefixOf` str) &&
                      length str > length (argShortOpt settings) =
                        ShortFlaglike . drop (length $ argShortOpt settings)
                    | otherwise =
                        NotFlaglike

        -- Return the string value of the token
        value (ShortFlaglike str) = str
        value (LongFlaglike str) = str
        value (NotFlaglike str) = str

        -- Return the flag that generated the given tagged token
        untag (ShortFlaglike str) = argShortOpt settings ++ str
        untag (LongFlaglike str) = argLongOpt settings ++ str
        untag (NotFlaglike str) = str


        -- -- Break a long flag with an equal sign into two tokens: the flag and a
        -- -- non-flaglike token containing the argument
        -- splitFlag (LongFlaglike str) = [ LongFlaglike prefix ] ++
        --                                (maybeToList . liftM2 ifJust null NotFlaglike $ suffix)
        --     where (prefix, suffixWithSep) = break (== argLongSep settings) str
        --           suffix = drop 1 suffixWithSep

        -- splitFlag tok                = [tok]


        --- *** Multi-part definition of parseDescr ***

        parseDescr :: OptionsDescr Identity (Either String a)
                   -> Maybe (Argument Identity (Either String a))
                   -> [TaggedToken]
                   -> [Either String a]

        -- Positional description:
        -- The case where we previously processed a flag but no token is left
        -- in the list
        parseDescr descr@(PositionalDescr _ _ _) (Just prevArg) [] =
            case prevArg of
                (ReqArg fn name) -> [Left $ "Missing argument " ++ name]
                (OptArg fn _)    -> fn Nothing : parseDescr descr Nothing []
                (NoArg x)        -> runIdentity x : parseDescr descr Nothing []

        -- Subcommand description:
        -- The case where we previously processed a flag but no token is left
        -- in the list
        parseDescr descr@(SubcommandDescr _ _) (Just prevArg) [] =
            case prevArg of
                (ReqArg fn name) -> [Left $ "Missing argument " ++ name]
                (OptArg fn _)    -> fn Nothing : parseDescr descr Nothing []
                (NoArg x)        -> runIdentity x : parseDescr descr Nothing []

        -- Positional description:
        -- The case where we previously processed a flag and a token is left in
        -- the list
        parseDescr descr@(PositionalDescr _ _ _) (Just prevArg) (arg:remArgs) =
            case prevArg of
                (ReqArg fn _) -> (fn $ untag arg) : parseDescr descr Nothing remArgs
                (OptArg fn _) -> (fn . Just $ untag arg) : parseDescr descr Nothing remArgs
                (NoArg x)     -> runIdentity x : parseDescr descr Nothing (arg:remArgs)

        -- Subcommand description:
        -- The case where we previously processed a flag and a token is left in
        -- the list
        parseDescr descr@(SubcommandDescr _ _) (Just prevArg) (arg:remArgs) =
            case prevArg of
                (ReqArg fn _) -> (fn $ untag arg) : parseDescr descr Nothing remArgs
                (OptArg fn _) -> (fn . Just $ untag arg) : parseDescr descr Nothing remArgs
                (NoArg x)     -> runIdentity x : parseDescr descr Nothing (arg:remArgs)

        -- Positional description:
        -- The case where we're possibly expecting a positional argument but
        -- nothing is left in the list
        parseDescr (PositionalDescr _ positionals _) Nothing [] =
            case listToMaybe positionals of
                Nothing                    -> []
                (Just (Positional _ name)) -> [Left $ "Missing argument " ++ name]

        -- Subcommand description:
        -- The case where we're expecting a subcommand argument but nothing is
        -- left in the list
        parseDescr (SubcommandDescr _ _) Nothing [] = [Left "Expected a subcommand"]

        -- Positional description:
        -- The case where we're processing a non-flaglike token and it matches
        -- to a positional argument
        parseDescr (PositionalDescr flags ((Positional fn _):remPos) mPos) Nothing
                   ((NotFlaglike str):remArgs) =
            (fn str) : parseDescr (PositionalDescr flags remPos mPos) Nothing remArgs

        -- Subcommand description:
        -- The case where we're processing a non-flaglike token and it could be
        -- a subcommand
        parseDescr (SubcommandDescr _ subcommands) Nothing
                   ((NotFlaglike str):remArgs) =
            case findSubcommandDescr subcommands str of
                (Left error) -> [Left error]
                (Right descr) -> parseDescr descr Nothing remArgs

        -- Positional description:
        -- The case where we're processing a non-flaglike token and there are
        -- no more required positional arguments, but there is an optional
        -- positional argument
        parseDescr (PositionalDescr flags [] (Just pos)) Nothing
                   (arg@(NotFlaglike str):remArgs) =
            parseDescr (PositionalDescr flags [pos] (Just pos)) Nothing (arg:remArgs)

        -- Positional description:
        -- The case where we're processing a non-flaglike token and there are
        -- no more required positional arguments and no optional positional
        -- argument
        parseDescr (PositionalDescr flags [] Nothing) Nothing ((NotFlaglike str):_) =
            [Left "Too many arguments"]

        -- Positional description:
        -- The case where we're processing a flaglike token
        parseDescr descr@(PositionalDescr flags _ _) Nothing (tok:remToks) =
            case findFlagArgTok flags tok of
                Left error -> [Left error]
                Right (arg, newToks)  -> parseDescr descr (Just arg)
                                                    (newToks ++ remToks)

        -- Subcommand description:
        -- The case where we're processing a flaglike token
        parseDescr descr@(SubcommandDescr flags _) Nothing (tok:remToks) =
            case findFlagArgTok flags tok of
                Left error -> [Left error]
                Right (arg, newToks)  -> parseDescr descr (Just arg)
                                                    (newToks ++ remToks)

        --- *** End definition of parseDescr ***


        -- Find a flag and its argument by its tag and name. Return the
        -- argument and a list of new tokens to insert into the input. This
        -- allows us to handle short flags being crammed together, as well as a
        -- short flag being right against its input.
        findFlagArgTok flags (ShortFlaglike str) =
            case find (shortFlagMatch flagStr) flags of
                Nothing                 -> Left $ concat [ "Unknown flag "
                                                         , argShortOpt settings
                                                         , flagStr
                                                         ]
                (Just (Flag _ _ arg _)) -> Right $
                    (arg, maybeToList . liftM2 ifJust null ShortFlaglike $ restStr)
            where
                -- Decide whether to break off the first character based on
                -- settings
                (flagStr, restStr) = if argSplitShort settings
                    then splitAt 1 str
                    else (str, "")

                shortFlagMatch str (Flag shortLs _ _ _) = str `elem` map show shortLs

        findFlagArgTok flags (LongFlaglike str) =
            case find (longFlagMatch flagStr) flags of
                Nothing                 -> Left $ concat [ "Unknown flag "
                                                         , argLongOpt settings
                                                         , flagStr
                                                         ]
                (Just (Flag _ _ arg _)) -> Right $
                    (arg, maybeToList . liftM2 ifJust null NotFlaglike $ restStr)
            where
                -- Decide whether to break at the long separator based on
                -- settings
                (flagStr, restStr) = if argSplitLong settings
                    then breakOn (argLongSep settings) str
                    else (str, "")

                longFlagMatch str (Flag _ longLs _ _) = str `elem` longLs


        -- Find a subcommand and its sub-description by its name
        findSubcommandDescr subcommands str =
            case find (subcommandMatch str) subcommands of
                Nothing                       -> Left $ "Unknown subcommand " ++ str
                (Just (Subcommand _ _ descr)) -> Right $ descr
            where
                subcommandMatch str (Subcommand nameLs _ _) = str `elem` nameLs


        -- TODO: The rest of this looks pretty magical. I should go through it
        -- and comment it.

        convertOptionsDescr :: ArgOptionsDescr (Either String a)
                            -> OptionsDescr Identity (Either String a)
        convertOptionsDescr (PositionalDescr flagLs posLs rest) =
            PositionalDescr (flagLs >>= convertFlag) posLs rest
        convertOptionsDescr (SubcommandDescr flagLs subCmdLs) =
            SubcommandDescr (flagLs >>= convertFlag) (map convertSubcommand subCmdLs)

        convertSubcommand :: Subcommand ConvertNoArg (Either String a)
                          -> Subcommand Identity (Either String a)
        convertSubcommand (Subcommand name help opts) =
            Subcommand name help (convertOptionsDescr opts)


        convertFlag :: Flag ConvertNoArg (Either String a)
                    -> [Flag Identity (Either String a)]

        convertFlag (Flag shortLs longLs (NoArg conversion) help) = case conversion of
            AsIs x         -> [ Flag shortLs longLs
                                     (NoArg $ Identity x)
                                     help
                              ]
            WithInverse fn -> [ Flag shortLs longLs
                                     (NoArg . Identity $ fn True)
                                     help
                              , Flag [] (map ("no-" ++) longLs)
                                     (NoArg . Identity $ fn False)
                                     help
                              ]
            AsOptArg fn    -> [ Flag shortLs longLs
                                     (OptArg (convertWithDefault >=> fn) "BOOL")
                                     help
                              ]

        convertFlag (Flag shortLs longLs (ReqArg fn str) help) =
            [ Flag shortLs longLs (ReqArg fn str) help ]

        convertFlag (Flag shortLs longLs (OptArg fn str) help) =
            [ Flag shortLs longLs (OptArg fn str) help ]


        convertWithDefault :: Maybe String -> Either String Bool
        convertWithDefault = maybe (Right True) convertBool

        convertBool :: String -> Either String Bool
        convertBool str = smartReadBool str `maybeToEither`
                                        ("unable to parse argument `" ++ str ++ "'")

