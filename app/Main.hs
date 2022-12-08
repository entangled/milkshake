-- ~\~ language=Haskell filename=app/Main.hs
-- ~\~ begin <<lit/milkshake.md|app/Main.hs>>[init]
module Main where

import RIO
import qualified RIO.Text as T
import Options.Applicative
import Milkshake
    ( readConfig, loadIncludes, immediateActions, shake, shakeOptions, monitor, withWatchManager, want, enter
    , HasWatchManager, HasEventChannel(..), Config )
import qualified Milkshake as MS
import qualified Milkshake.Data as MS.Data

data Args = Args
    { inputFile :: FilePath
    , runOnce :: Bool }

argParser :: ParserInfo Args
argParser = info (args <**> helper)
              ( fullDesc
             <> progDesc "Build stuff on file system events."
             <> header "milkshake - file system event loops" )
    where args = Args <$> argument str (metavar "FILE" <> help "Input file")
                      <*> switch ( long "once" <> short '1' <> help "Run main target once" )

data Env = Env
    { _watchManager :: MS.WatchManager
    , _channel      :: Chan MS.Data.Target
    , _logger       :: LogFunc
    }

instance HasWatchManager Env where
    watchManager = lens _watchManager (\e m -> e { _watchManager = m })

instance HasLogFunc Env where
    logFuncL = lens _logger (\e l -> e { _logger = l })

instance HasEventChannel Env MS.Data.Target where
    eventChannel = lens _channel (\e c -> e { _channel = c })

runEnv :: MonadUnliftIO m => RIO Env a -> m a
runEnv x = do
    logOptions <- logOptionsHandle stderr True
    withLogFunc logOptions (\logFunc -> do
        withWatchManager (\wm -> do
            ch <- newChan
            let env = Env wm ch logFunc
            runRIO env x))

runAction :: Config -> [FilePath] -> RIO Env ()
runAction cfg tgts = do
    actions <- either throwM return $ immediateActions cfg
    liftIO $ shake shakeOptions (mapM_ enter actions >> want tgts)

mainLoop :: Text -> RIO Env ()
mainLoop path = do
    cfg <- loadIncludes =<< readConfig path
    chan <- view eventChannel
    stop <- monitor $ map (\MS.Data.Watch{..} -> (paths, \_ -> return target)) (MS.Data.watches cfg)
    target <- readChan chan
    stop
    case target of
        (MS.Data.File path) -> do
            logDebug $ "building " <> display path
            runAction cfg [T.unpack path]
        _           -> return ()
    mainLoop path

runMain :: Text -> RIO Env ()
runMain path = do
    cfg <- loadIncludes =<< readConfig path
    runAction cfg []

main :: IO ()
main = do
    args <- execParser argParser
    let path = T.pack (inputFile args)
    runEnv $ if (runOnce args) then runMain path else mainLoop path 
-- ~\~ end
