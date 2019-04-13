{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module GraphMod where

import           GhcPlugins
import           TcRnTypes
import           HsExtension
import           HsImpExp
import           Binary

import           Data.Maybe
import           Data.List

import           Utils                         as GraphMod
import           Dot                           as GraphMod
import           Args
import qualified Trie

import qualified Data.Map                      as Map

import           System.FilePath
import           System.Directory
import           System.Console.GetOpt
import           System.Environment             ( getArgs )
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
    let imps       = tcg_rn_imports tc_gbl
        gm_imps    = concatMap (convertImport . unLoc) imps
        outdir     = mkOutdir opts
        path       = mkPath outdir (ms_mod ms)
        gm_modname = getModName ms
    liftIO $ do
        createDirectoryIfMissing False outdir
        writeBinary path (gm_modname, gm_imps)
    return tc_gbl

mkOutdir :: [CommandLineOption] -> FilePath
mkOutdir []      = defaultLocation
mkOutdir (x : _) = x

writeBinary :: Binary a => FilePath -> a -> IO ()
writeBinary path payload = do
    bh <- openBinMem initBinMemSize
    put_ bh payload
    writeBinMem bh path

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


convertImport :: ImportDecl GhcRn -> [GraphMod.Import]
convertImport (ImportDecl {..}) =
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
                (concatMap lieToString names)
            _ -> []
convertImport _ = error "Unreachable"

convertModName :: Located ModuleName -> GraphMod.ModName
convertModName (L _ mn) = GraphMod.splitModName (moduleNameString mn)

lieToString :: LIE GhcRn -> [String]
lieToString (L _ ie) = case ie of
    IEVar _ (L _ (IEName (L _ name))) -> [getOccString name]
    IEVar      _ _        -> error "type/pattern imports unimplemented"
    IEThingAbs _ (L _ (IEName (L _ name))) -> [getOccString name]
    IEThingAbs _ _        -> error "type/pattern imports unimplemented"
    IEThingAll _ (L _ (IEName (L _ name))) -> [getOccString name]
    IEThingAll _ _        -> error "type/pattern imports unimplemented"
    IEThingWith _ _ _ _ _ -> error "IEThingWith unimplemented"

    -- IEModuleContents is actually unreachable. The others I'm not sure about,
    -- I guess at least the IE_ ones are reachable and should just be ignored.
    IEModuleContents _ _  -> error "IEModuleContents unreachable"
    IEGroup _ _ _         -> error "IEGroup unreachable?"
    IEDoc      _ _        -> error "IEDoc unreachable?"
    IEDocNamed _ _        -> error "IEDocNamed unreachable?"
    XIE _                 -> error "XIE unreachable?"

--
-- Finalisation logic
-- We run this code at the end to gather up all the results and
-- output the dotfile.


readImports :: FilePath -> FilePath -> IO Payload
readImports outdir fp = do
    readBinMem (outdir </> fp) >>= get

collectImports :: IO ()
collectImports = do
    raw_opts <- getArgs
    printStderr raw_opts
    let (fs, _ms, _errs) = getOpt Permute options raw_opts
        opts             = foldr ($) default_opts fs

        outdir           = inputDir opts
    printStderr $ ("OutDir: ", outdir)
    files <- listDirectory outdir
    printStderr $ ("files:", concat files)
    usages <- mapM (readImports outdir) files
    printStderr usages
    let graph = buildGraph opts usages
    putStr (GraphMod.make_dot opts graph)



-- Get all the ModNames to make nodes for
modGraph :: [Payload] -> [GraphMod.ModName]
modGraph = nub . foldMap do_one
  where
    do_one (mn, is) = mn : map do_import is

    do_import (GraphMod.Import n _ _) = n

--
buildGraph :: Opts -> [Payload] -> (GraphMod.AllEdges, GraphMod.Nodes)
buildGraph opts payloads = maybePrune opts (aes, processNodes)
  where
    processNodes = collapseAll opts nodes (collapse_quals opts)

    nodeMapList  = zip (modGraph payloads) [0 ..]

    nodeMap      = Map.fromList nodeMapList

    nodes        = foldr insertMod Trie.empty nodeMapList

    aes          = foldr (makeEdges nodeMap)
                         GraphMod.noEdges
                         (concatMap (\(p, is) -> map (p, ) is) payloads)

    insertMod (n, k) t = GraphMod.insMod n k t

-- Make edges between the nodes
-- Invariant: All nodes already exist in the map
makeEdges
    :: Map.Map GraphMod.ModName Int
    -> (GraphMod.ModName, GraphMod.Import)
    -> GraphMod.AllEdges
    -> GraphMod.AllEdges
makeEdges nodeMap (m_from, m_to) aes = fromMaybe (error "makeEdges") $ do
    from_i <- Map.lookup m_from nodeMap
    to_i   <- Map.lookup (GraphMod.impMod m_to) nodeMap
    return $ case GraphMod.impType m_to of
        GraphMod.SourceImp -> aes
            { GraphMod.sourceEdges = GraphMod.insSet
                                         from_i
                                         to_i
                                         (GraphMod.sourceEdges aes)
            }
        GraphMod.NormalImp -> aes
            { GraphMod.normalEdges = GraphMod.insSet
                                         from_i
                                         to_i
                                         (GraphMod.normalEdges aes)
            }



--
-- Serialisation logic for GraphMod types

instance Binary GraphMod.Import where
    put_ bh (GraphMod.Import mn ip ent) =
        put_ bh mn >> put_ bh ip >> put_ bh ent
    get bh = GraphMod.Import <$> get bh <*> get bh <*> get bh
instance Binary GraphMod.ImpType where
    put_ bh c = case c of
        GraphMod.NormalImp -> putByte bh 0
        GraphMod.SourceImp -> putByte bh 1
    get bh = getByte bh >>= return . \case
        0 -> GraphMod.NormalImp
        1 -> GraphMod.SourceImp
        _ -> error "Binary:GraphMod"



