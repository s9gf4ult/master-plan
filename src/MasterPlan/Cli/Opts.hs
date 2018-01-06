module MasterPlan.Cli.Opts where

import Control.Lens
import Data.Text as T
import Options.Applicative


-- |Type output from the command line parser
data Opts = Opts
  { oInputPath   :: Maybe FilePath
  , oOutputPath  :: Maybe FilePath
  , oRootKey     :: Maybe Text
  , oParseStrict :: Bool -- ^ every project has to be defined
  } deriving (Eq, Ord)

makeLenses ''Opts

opts :: ParserInfo Opts
opts =
  info (helper <*> optsParser)
  ( fullDesc
    <> progDesc "See documentation on how to write project plan files"
    <> header "master-plan - project management tool for hackers" )

-- |The command line parser
optsParser ∷ Parser Opts
optsParser = Opts
  <$> optional (strArgument ( help "plan file to read from (default from stdin)"
                              <> metavar "FILENAME" ))
  <*> optional (strOption ( long "output"
                            <> short 'o'
                            <> help "output file name (.png, .tif, .bmp, .jpg and .pdf supported)"
                            <> metavar "FILENAME" ))
  <*> optional ( T.pack <$> strOption
                 ( long "root"
                   <> short 'r'
                   <> help "name of the root project definition"
                   <> value "root"
                   <> showDefault
                   <> metavar "NAME"))
  <*> switch ( long "strict"
               <> help "strict parsing: every project has to be defined")
