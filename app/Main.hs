module Main where

import Options

import Lib
import ElmExport (writeElmFile)


data MainOpts = MainOpts
instance Options MainOpts where
    defineOptions = pure MainOpts


newtype ServerOpts = ServerOpts { port :: Int }
instance Options ServerOpts where
    defineOptions = pure ServerOpts
        <*> simpleOption "port" 8080 "which port to start the server on"


newtype ExportOpts = ExportOpts { exportToPath :: FilePath }
instance Options ExportOpts where
    defineOptions = pure ExportOpts
        <*> simpleOption "path" "src/client" "where to export the elm code"


start :: MainOpts -> ServerOpts -> [String] -> IO ()
start _ opts _ = startApp $ port opts


export :: MainOpts -> ExportOpts -> [String] -> IO ()
export _ opts _ = writeElmFile $ exportToPath opts


main :: IO ()
main = runSubcommand
    [ subcommand "start" start
    , subcommand "export" export
    ]
