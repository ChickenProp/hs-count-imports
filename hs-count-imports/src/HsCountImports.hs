{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HsCountImports where

import           GhcPlugins
import           TcRnTypes
import           HsExtension
import           HsImpExp

import           Utils                         as HsCountImports
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


-- Converting to HsCountImports data types
--
-- The type we are going to serialise
type Payload = (HsCountImports.ModName, [HsCountImports.Import])

getModName :: ModSummary -> HsCountImports.ModName
getModName ms =
    HsCountImports.splitModName . moduleNameString . moduleName . ms_mod $ ms


convertImport :: DynFlags -> ImportDecl GhcRn -> [HsCountImports.Import]
convertImport dynFlags (ImportDecl {..}) =
    let modName = convertModName ideclName
        modType = if ideclSource
            then HsCountImports.SourceImp
            else HsCountImports.NormalImp
    in  case ideclHiding of
            Just (False, (L _ names)) -> map
                (\n -> HsCountImports.Import { impMod    = modName
                                             , impType   = modType
                                             , impEntity = n
                                             }
                )
                (concatMap (lieToString dynFlags) names)
            _ -> []
convertImport _ _ = error "Unreachable"

convertModName :: Located ModuleName -> HsCountImports.ModName
convertModName (L _ mn) = HsCountImports.splitModName (moduleNameString mn)

lieToString :: DynFlags -> LIE GhcRn -> [String]
lieToString dynFlags (L _ ie) = case ie of
    IEVar      _ (wn2s -> name) -> [name]
    IEThingAbs _ (wn2s -> name) -> [name]
    IEThingAll _ (wn2s -> name) -> [name, name ++ "(..)"]
    IEThingWith _ (wn2s -> name) _ (map wn2s -> ns) _ ->
        name : map (\n -> name ++ "(" ++ n ++ ")") ns

    -- IEModuleContents is actually unreachable. The others I'm not sure about,
    -- but I haven't been able to reach them.
    IEModuleContents _ _ -> error "IEModuleContents unreachable"
    IEGroup _ _ _        -> error "IEGroup unreachable?"
    IEDoc      _ _       -> error "IEDoc unreachable?"
    IEDocNamed _ _       -> error "IEDocNamed unreachable?"
    XIE _                -> error "XIE unreachable?"
  where
    n2s :: Name -> String
    n2s n = showSDoc dynFlags $ pprPrefixName n

    wn2s :: LIEWrappedName Name -> String
    wn2s (L _ (IEName    (L _ n))) = n2s n
    wn2s (L _ (IEType    (L _ n))) = "type " ++ n2s n
    wn2s (L _ (IEPattern (L _ n))) = "pattern " ++ n2s n


--
-- Serialisation logic for HsCountImports types

import2tsv :: Import -> String
import2tsv (HsCountImports.Import { impMod, impEntity }) =
    joinModName impMod ++ "\t" ++ impEntity
