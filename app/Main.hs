-- ~\~ language=Haskell filename=app/Main.hs
-- ~\~ begin <<lit/index.md|app/Main.hs>>[0]
module Main where

import RIO
import Options.Applicative
import Milkshake

data Args = Args
    { inputFile :: FilePath }

argParser :: ParserInfo Args
argParser = info (args <**> helper)
              ( fullDesc
             <> progDesc "Build stuff on file system events."
             <> header "milkshake - file system event loops" )
    where args = Args <$> argument str (metavar "FILE" <> help "Input file")

data Env = Env
    { _watchManager :: WatchManager
    , _channel      :: Chan Event
    , _logger       :: LogFunc
    }

instance HasWatchManager Env where
    watchManager = lens _watchManager (\e m -> e { _watchManager = m })

instance HasLogFunc Env where
    logFuncL = lens _logger (\e l -> e { _logger = l })

instance HasEventChannel Env Event where
    eventChannel = lens _channel (\e c -> e { _channel = c })

runEnv :: MonadUnliftIO m => RIO Env a -> m a
runEnv x = do
    logOptions <- logOptionsHandle stderr True
    withLogFunc logOptions (\logFunc -> do
        withWatchManager (\wm -> do
            ch <- newChan
            let env = Env wm ch logFunc
            runRIO env x))

mainLoop :: FilePath -> RIO Env ()
mainLoop path = do
    cfg <- readConfig path
    return ()

main :: IO ()
main = do
    path <- inputFile <$> execParser argParser
    runEnv $ mainLoop path
-- ~\~ end
