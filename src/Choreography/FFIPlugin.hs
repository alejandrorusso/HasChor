module Choreography.FFIPlugin where

import GHC
import GHC.Plugins
import GHC.Types.CostCentre.State
import Data.Generics

import Control.Monad.State

plugin :: Plugin
plugin = defaultPlugin { parsedResultAction = const . ffi_stubgen_plugin
                       , latePlugin = install_ffi_stub_plugin
                       }

-- analyse source and create C file with stubs

ffi_stubgen_plugin :: [CommandLineOption] -> ParsedResult -> Hsc ParsedResult
ffi_stubgen_plugin _ pm = do
    let L _ hpm = hpm_module $ parsedResultModule pm
        decls = hsmodDecls hpm
    let x = execState ((mkM collectC) `everywhereM` decls) []
    liftIO $ createCFile x
    return pm

collectC :: HsDecl GhcPs -> State [String] (HsDecl GhcPs)
collectC e@(ForD _ (ForeignImport _ (L _ n) _ _)) = do
    modify $ \st -> st ++ [show (occNameMangledFS $ occName n)]
    return e
collectC x = return x

filename = "/tmp/ghctmp.c"

createCFile :: [String] -> IO ()
createCFile names =
    writeFile filename $ unlines $
      map (\name -> concat ["void ", init $ (drop 1) name, "(void) { return; }"]) names

-- late plugin for installing the new C source as a source

install_ffi_stub_plugin :: HscEnv -> [CommandLineOption] -> (CgGuts, CostCentreState) -> IO (CgGuts, CostCentreState)
install_ffi_stub_plugin _ _ (cg, cc) =
    return (cg { cg_foreign_files = cg_foreign_files cg ++ [(LangC, filename)]}, cc)