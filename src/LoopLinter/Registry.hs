module LoopLinter.Registry where

import qualified Data.Map as Map
import System.Directory (createDirectoryIfMissing, listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (forM)
import GHC.Types.Id (Id)
import GHC.Plugins (getOccName, occNameString, nameModule_maybe, moduleNameString, moduleName, Name, getName)

-- Registry: Map from fully qualified name (String) to hasReg (Bool)
type Registry = Map.Map String Bool

registryDir :: FilePath
registryDir = ".loop-linter-store"

-- Convert an Id to a fully qualified string key
idToKey :: Id -> String
idToKey i = nameToKey (getName i)

nameToKey :: Name -> String
nameToKey n = 
  let occ = occNameString (getOccName n)
      modStr = case nameModule_maybe n of
                 Just m -> moduleNameString (moduleName m) ++ "."
                 Nothing -> ""
  in modStr ++ occ

-- Save the registry for a specific module to disk
saveRegistry :: String -> Registry -> IO ()
saveRegistry moduleName reg = do
  createDirectoryIfMissing True registryDir
  let filePath = registryDir </> (moduleName ++ ".reginfo")
  writeFile filePath (show (Map.toList reg))

-- Load all registry files from the directory
loadRegistry :: IO Registry
loadRegistry = do
  dirExists <- doesDirectoryExist registryDir
  if not dirExists
    then return Map.empty
    else do
      files <- listDirectory registryDir
      let regFiles = filter (\f -> takeExtension f == ".reginfo") files
      maps <- forM regFiles $ \f -> do
        content <- readFile (registryDir </> f)
        case reads content of
          [(pairs, "")] -> return (Map.fromList pairs)
          _ -> return Map.empty -- Ignore malformed files
      return (Map.unions maps)
