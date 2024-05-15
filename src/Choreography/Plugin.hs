module Choreography.Plugin (plugin) where
import GHC.Plugins
import Data.Generics

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  return (todo ++ [CoreDoPluginPass "Partitioning verification pass" pass])

-- | The actual pass, using syb to traverse a module and see if anything forbidden appears
pass :: ModGuts -> CoreM ModGuts
pass guts = do dflags <- getDynFlags
               let binds = mg_binds guts
                   modulename = moduleName $ mg_module guts
               putMsgS $ "*** Partitioning verification pass running on " ++ showSDoc dflags (ppr modulename)
               x <- mkM (checkForForbidden dflags modulename) `everywhereM` binds
               putMsgS $ "*** Verification for module " ++ showSDoc dflags (ppr modulename) ++ ": done"
               return $ guts { mg_binds = x }

-- | Function that inspects a core variable to figure out if it is one of the forbidden
-- keywords or not.
checkForForbidden :: DynFlags -> ModuleName -> Expr CoreBndr -> CoreM (Expr CoreBndr)
checkForForbidden dflags mname (Var id) = do
    let fs = occNameMangledFS $ getOccName id
    if show fs `elem` keywords
        then error $ eMsg (showSDoc dflags (ppr mname)) (showSDoc dflags (ppr id))
        else return ()
    return (Var id)
checkForForbidden _ _ e = return e

eMsg :: String -> String -> String
eMsg mod var = unlines
  [ "Found indications of partitioning not occurring when inspecting module: " ++ mod
  , "    The problematic symbol that was encountered: " ++ drop 2 var
  ]

-- | These are symbols which should not appear in the codebase if partitioning
-- happened the way it should have
keywords :: [String]
keywords = [ "\"$w~>\""
           , "\"$wlocally\""
           , "\"$wcond\""
           , "\"$w~~>\""
           , "\"$wcond'\""
           ]