{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module Day25 where

import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV

import           Control.Monad           (filterM, forM, forM_, guard, unless,
                                          when)
import           Control.Monad.ST        (runST)
import qualified Data.Bifunctor
import qualified Data.HashTable.ST.Basic as HT
import           Data.List               (groupBy, sort, tails)
import           Data.Maybe              (fromJust, fromMaybe, isJust)
import qualified Data.Text.Lazy.Lens     as T
import           System.IO.Unsafe        (unsafePerformIO)
import           Utils                   (pairs)

parseLine line = (node1, nodes)
    where
        [node1, nodes_concat] = T.splitOn ": " line
        nodes = T.words nodes_concat


newtype Vertex = Vertex (Int, T.Text, V.Vector (Int, Vertex))

compileGraph :: [(T.Text, [T.Text])] -> V.Vector Vertex
compileGraph parsedGraph = runST $ mdo
    vertexNameToIndex <- HT.new
    forM parsedGraph $  \(vertexName, _) -> do
        currentSize <- HT.size vertexNameToIndex
        HT.insert vertexNameToIndex vertexName currentSize
    n <- HT.size vertexNameToIndex
    vertexIndexToName <- MV.replicate n ""
    (`HT.mapM_` vertexNameToIndex) $ \(name, index) -> do
        MV.write vertexIndexToName index name
    edgesByVertex <- forM parsedGraph $ \(vertex1Name, vertex2Names) -> do
        Just vertex1Index <- HT.lookup vertexNameToIndex vertex1Name
        Just vertex2Indices <- sequence <$> traverse (HT.lookup vertexNameToIndex) vertex2Names
        return $ do
            vertex2Index <- vertex2Indices
            [(vertex1Index, vertex2Index), (vertex2Index, vertex1Index)]
    let edges = ((`zip` [0..]) . sort . concat) edgesByVertex
    let groupedEdges = groupBy  (\((lh, _), _) ((rh, _), _) -> lh == rh) edges
    compiledGraph <- V.fromList <$> forM groupedEdges (\edgeGroup -> do
        let ((selfIndex, _), _): _ = edgeGroup
        selfName <- MV.read vertexIndexToName selfIndex
        let adjVertices = V.fromList $ map (\((_, adjIndex), edgeIndex) -> (edgeIndex, compiledGraph V.! adjIndex)) edgeGroup
        return $ Vertex (selfIndex, selfName, adjVertices))
    return compiledGraph

testCompileGraph = unvertex <$> cg
    where
        cg = compileGraph [("foo", ["bar", "baz"]), ("bar", ["baz"]), ("baz", ["foobar"]), ("foobar", [])]
        getName (index, Vertex (_, name, _)) = (index, name)
        unvertex (Vertex (index, name, adj)) = (index, name, getName  <$> adj)

-- >>> testCompileGraph
-- [(0,"foo",[(0,"bar"),(1,"baz")]),(1,"bar",[(2,"foo"),(3,"baz")]),(2,"baz",[(4,"foo"),(5,"bar"),(6,"foobar")]),(3,"foobar",[(7,"baz")])]



solve inputFilename = do
    simpleGraph <- fmap parseLine . T.lines <$> T.readFile inputFilename
    return simpleGraph

-- >>> solve "inputs/sample/25.txt"
-- [("jqt",["rhn","xhk","nvd"]),("rsh",["frs","pzl","lsr"]),("xhk",["hfx"]),("cmg",["qnr","nvd","lhk","bvb"]),("rhn",["xhk","bvb","hfx"]),("bvb",["xhk","hfx"]),("pzl",["lsr","hfx","nvd"]),("qnr",["nvd"]),("ntq",["jqt","hfx","bvb","xhk"]),("nvd",["lhk"]),("lsr",["lhk"]),("rzs",["qnr","cmg","lsr","rsh"]),("frs",["qnr","lhk","lsr"])]

-- >>> solve "inputs/real/25.txt"
-- [("tcs","vrk"),("mjn","vrk")]
