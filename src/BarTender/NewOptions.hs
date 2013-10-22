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

data OptionsDescr f a = PositionalDescr [Flag f a] [Positional a] (Maybe (Positional a))
                      | SubcommandDescr [Flag f a] [Subcommand f a]
    deriving Functor

data Flag f a = Flag [Char] [String] (Argument f a) String
    deriving Functor

data Positional a = Positional (String -> a) String
    deriving Functor

data Subcommand f a = Subcommand String String (OptionsDescr f a)
    deriving Functor

data Argument f a = NoArg (f a)
                  | ReqArg (String -> a) String
                  | OptArg (Maybe String -> a) String
    deriving Functor

type ArgOptionsDescr a = OptionsDescr ConvertNoArg a

data ConvertNoArg a = AsIs a
                    | WithInverse (Bool -> a)
                    | AsOptArg (Bool -> a)
    deriving Functor

data TaggedToken = ShortFlaglike String
                    | LongFlaglike String
                    | NotFlaglike String
    deriving Show

data ArgParseSettings = ArgParseSettings
    { argShortOpt    :: String
    , argLongOpt     :: String
    , argLongSep     :: Char
    , argEndFlags    :: String
    , argSplitLong   :: Bool
    }
    deriving Show

defaultArgParseSettings = ArgParseSettings
    { argShortOpt    = "-"
    , argLongOpt     = "--"
    , argLongSep     = '='
    , argEndFlags    = "--"
    , argSplitLong   = True
    }


argParse :: ArgParseSettings
         -> ArgOptionsDescr a
         -> [String]
         -> Either String [a]
argParse settings descr args =
    sequence $ parseDescr finalDescr Nothing finalToks
    where
        finalDescr = convertOptionsDescr $ fmap Right descr

        finalToks = tagFlags args >>=
            if argSplitLong settings
                then splitAtEquals
                else \x -> [x]


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
                    | (argLongOpt settings `isPrefixOf` str) =
                        LongFlaglike . drop (length $ argLongOpt settings)
                    | (argShortOpt settings `isPrefixOf` str) =
                        ShortFlaglike . drop (length $ argShortOpt settings)
                    | otherwise =
                        NotFlaglike

        -- Return the name of the string value of the token
        untag (ShortFlaglike str) = str
        untag (LongFlaglike str) = str
        untag (NotFlaglike str) = str

        -- Return the flag that generated the given tagged token
        tagToFlag (ShortFlaglike str) = argShortOpt settings ++ str
        tagToFlag (LongFlaglike str) = argLongOpt settings ++ str


        -- Break a long flag with an equal sign into two tokens: the flag and a
        -- non-flaglike token containing the argument
        splitAtEquals (LongFlaglike str) = [ LongFlaglike prefix
                                           , NotFlaglike $ drop 1 suffix
                                           ]
            where (prefix, suffix) = break (== argLongSep settings) str

        splitAtEquals tok                = [tok]


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
            case findFlagArg flags tok of
                Left error -> [Left error]
                Right arg  -> parseDescr descr (Just arg) remToks

        -- Subcommand description:
        -- The case where we're processing a flaglike token
        parseDescr descr@(SubcommandDescr flags _) Nothing (tok:remToks) =
            case findFlagArg flags tok of
                Left error -> [Left error]
                Right arg  -> parseDescr descr (Just arg) remToks

        --- *** End definition of parseDescr ***


        -- Find a flag and its argument by its tag and name
        -- TODO: Right now we discard short flags with multi-character names.
        -- It might be better if we split them into multiple flags or a flag
        -- with an option. I.e.,
        --     -ofile == -o file
        -- or
        --     -abc == -a -b -c
        -- We could even do both in an intelligent way: Use the first if the
        -- flag expects an argument and the second otherwise.
        findFlagArg flags tok =
            case find (flagMatch tok) flags of
                Nothing                 -> Left $ "Unknown flag " ++ tagToFlag tok
                (Just (Flag _ _ arg _)) -> Right $ arg
            where
                flagMatch (ShortFlaglike (c:[])) (Flag shortChars _ _ _) = c `elem` shortChars
                flagMatch (ShortFlaglike str) (Flag shortChars _ _ _) = False

                flagMatch (LongFlaglike str) (Flag _ longNames _ _) = str `elem` longNames


        -- Find a subcommand and its sub-description by its name
        findSubcommandDescr subcommands str =
            case find (subcommandMatch str) subcommands of
                Nothing                       -> Left $ "Unknown subcommand " ++ str
                (Just (Subcommand _ _ descr)) -> Right $ descr
            where
                subcommandMatch str (Subcommand name _ _) = str == name


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

