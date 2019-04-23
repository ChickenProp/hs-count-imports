{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GraphMod where

import           GhcPlugins
import           TcRnTypes
import           HsExtension
import           HsImpExp

import           Utils                         as GraphMod
import           Args

import           System.FilePath
import           System.Directory
import           System.IO

printStderr :: Show a => a -> IO ()
printStderr = hPutStrLn stderr . show

initBinMemSize :: Int
initBinMemSize = 1024 * 1024


-- Installing the plugin
plugin :: Plugin
plugin = defaultPlugin { typeCheckResultAction = install
                       , pluginRecompile       = impurePlugin
                       }

-- The main plugin function, it collects and serialises the import
-- information for a module.
install :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
install opts ms tc_gbl = do
    dynFlags <- getDynFlags
    let imps       = tcg_rn_imports tc_gbl
        gm_imps    = concatMap (convertImport dynFlags . unLoc) imps
        outdir     = mkOutdir opts
        path       = mkPath outdir (ms_mod ms)
        gm_modname = getModName ms
    liftIO $ do
        createDirectoryIfMissing False outdir
        writeImports path gm_imps
    return tc_gbl

mkOutdir :: [CommandLineOption] -> FilePath
mkOutdir []      = defaultLocation
mkOutdir (x : _) = x

writeImports :: FilePath -> [Import] -> IO ()
writeImports path imps = writeFile path (unlines $ map import2tsv imps)

mkPath :: FilePath -> Module -> FilePath
mkPath fp m =
    fp </> (moduleNameString (moduleName m) ++ (show (moduleUnitId m)))


-- Converting to GraphMod data types
--
-- The type we are going to serialise
type Payload = (GraphMod.ModName, [GraphMod.Import])

getModName :: ModSummary -> GraphMod.ModName
getModName ms =
    GraphMod.splitModName . moduleNameString . moduleName . ms_mod $ ms


convertImport :: DynFlags -> ImportDecl GhcRn -> [GraphMod.Import]
convertImport dynFlags (ImportDecl {..}) =
    let
        modName = convertModName ideclName
        modType =
            if ideclSource then GraphMod.SourceImp else GraphMod.NormalImp
    in
        case ideclHiding of
            Just (False, (L _ names)) -> map
                (\n -> GraphMod.Import { impMod    = modName
                                       , impType   = modType
                                       , impEntity = n
                                       }
                )
                (concatMap (lieToString dynFlags) names)
            _ -> []
convertImport _ _ = error "Unreachable"

convertModName :: Located ModuleName -> GraphMod.ModName
convertModName (L _ mn) = GraphMod.splitModName (moduleNameString mn)

lieToString :: DynFlags -> LIE GhcRn -> [String]
lieToString dynFlags (L _ ie) = case ie of
    IEVar      _ (unwrap -> name) -> [n2s name]
    IEThingAbs _ (unwrap -> name) -> [n2s name]
    IEThingAll _ (unwrap -> name) -> [n2s name, n2s name ++ "(..)"]
    IEThingWith _ (unwrap -> name) _ (map unwrap -> ns) _ ->
        n2s name : map (\n -> n2s name ++ "(" ++ n2s n ++ ")") ns

    -- IEModuleContents is actually unreachable. The others I'm not sure about,
    -- but I haven't been able to reach them.
    IEModuleContents _ _ -> error "IEModuleContents unreachable"
    IEGroup _ _ _        -> error "IEGroup unreachable?"
    IEDoc      _ _       -> error "IEDoc unreachable?"
    IEDocNamed _ _       -> error "IEDocNamed unreachable?"
    XIE _                -> error "XIE unreachable?"
  where
    n2s n = showSDoc dynFlags $ pprPrefixName n
    unwrap (L _ (IEName (L _ n))) = n
    unwrap _                      = error "type/pattern imports unimplemented"


--
-- Serialisation logic for GraphMod types

import2tsv :: Import -> String
import2tsv (GraphMod.Import { impMod, impEntity }) =
    joinModName impMod ++ "\t" ++ impEntity
