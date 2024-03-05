module Args where

import Data.List
import qualified Query  as Q
import qualified Query.Parser  as Q
import Result
import Text.Read (readMaybe)

data SearchedFiles
  = Stdin
  | Files [String]
  deriving (Eq, Show)

data Args
  = Args
      { -- | Given as the first argument
        argQuery :: Q.Query,
        -- | Rest of arguments
        argFiles :: SearchedFiles,
        -- | Given as @--max-results value@
        argMaxResults :: Maybe Int
      }
  | Help String
  deriving (Eq, Show)

data ParseArgsError
  = NotEnoughArgs
  | InvalidArgs String
  deriving (Eq, Show)

usage progName = "Usage: " ++ progName ++ " query pattern [files...]"

-- | Parses the program arguments
--
-- First parameter is the name of the executable. The second parameter (of type @[String])@) contains the list of arguments.
--
-- The first positional argument represents the query and the rest of the arguments represent the files to check.
-- If no files are provided (i.e. only one argument is given, which represents the query), stdin should be used (i.e. the @Stdin@ constructor for @SearchedFiles@).
-- The @--max-results value@ flag is optional, and might be given in any position.
-- If the -h or --help arguments are provided, the function should return @Success@ and the usage in the @Help@ constructor
--
-- Useful functions:
-- - @separateFlags@
-- - @Query.Parser.parse@
--
-- >>> parseArgs "html-search.exe" ["-h"]
-- WAS Success (Help "Usage: html-search.exe query pattern [files...]")
-- NOW 
-- NOW GHC.ByteCode.Linker.lookupCE
-- NOW During interactive linking, GHCi couldn't find the following symbol:
-- NOW   interactive_Ghci1_evalPrint_closure
-- NOW This may be due to you not asking GHCi to load extra object files,
-- NOW archives or DLLs needed by your current session.  Restart GHCi, specifying
-- NOW the missing library using the -L/path/to/object/dir and -lmissinglibname
-- NOW flags, or simply by naming the relevant files on the GHCi command line.
-- NOW Alternatively, this link failure might indicate a bug in GHCi.
-- NOW If you suspect the latter, please report this as a GHC bug:
-- NOW   https://www.haskell.org/ghc/reportabug
--
-- >>> parseArgs "html-search.exe" ["div > h1.title", "file.html"]
-- WAS Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = ["title"], selectorAttributes = []})), argFiles = Files ["file.html"], argMaxResults = Nothing})
-- NOW parseArgs
--
-- >>> parseArgs "html-search.exe" ["div > h1"]
-- WAS Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Stdin, argMaxResults = Nothing})
-- NOW parseArgs
--
-- >>> parseArgs "html-search.exe" ["div h1", "file1.html", "file2.html"]
-- WAS Success (Args {argQuery = Descendant (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Files ["file1.html","file2.html"], argMaxResults = Nothing})
-- NOW parseArgs
--
-- >>> parseArgs "html-search.exe" ["div > h1", "file.html", "--max-results", "1"]
-- WAS Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Files ["file.html"], argMaxResults = Just 1})
-- NOW parseArgs
--
-- >>> parseArgs "html-search.exe" ["div > h1", "--max-results", "1", "file.html"]
-- WAS Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Files ["file.html"], argMaxResults = Just 1})
-- NOW parseArgs
--parseArgs :: String -> [String] -> Result ParseArgsError Args
--parseArgs _ _ = error "parseArgs"
parseArgs :: String -> [String] -> Result ParseArgsError Args
parseArgs progName args 
    | "-h" `elem` args || "--help" `elem` args = Success $ Help (usage progName)
    | otherwise = case separateFlags args of
        Success (flags, positionals) ->
            case positionals of
                (queryStr:fileArgs) -> 
                    case Q.parse queryStr of
                        Success query -> 
                            let files = if null fileArgs then Args.Stdin else Args.Files fileArgs
                                maxResults = lookup "--max-results" flags >>= readMaybe
                            in Success $ Args query files maxResults
                        Error err -> Error $ InvalidArgs (show err)
                [] -> Error NotEnoughArgs
        Error err -> Error err



-- | Separates positional arguments and flags
--
-- >>> separateFlags ["--flag1", "value1", "--flag2", "value2", "positional1"]
--
-- >>> separateFlags ["positional1", "--flag1", "value1", "--flag2", "value2"]
separateFlags ::
  [String] -> Result ParseArgsError ([(String, String)], [String])
separateFlags [] = Success ([], [])
separateFlags [last] =
  if "--" `isPrefixOf` last
    then Error $ InvalidArgs "Flag without value"
    else Success ([], [last])
separateFlags (arg : value : rest)
  | "--" `isPrefixOf` arg = do
      (flags, positionals) <- separateFlags rest
      return ((arg, value) : flags, positionals)
  | otherwise = do
      (flags, positionals) <- separateFlags (value : rest)
      return (flags, arg : positionals)
