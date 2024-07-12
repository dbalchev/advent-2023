{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
module Day25 where

import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV
import qualified Data.HashMap            as HM

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


newtype Vertex = Vertex (Int, T.Text, V.Vector (Int, Vertex, Int))

compileGraph parsedGraph = V.fromList (HM.elems nameToVertex)
    where
        edgePairs = do
            (first, seconds) <- parsedGraph
            second <- seconds
            return (first, second)
        edgeIndexedPairs = do
            (i, (first, second)) <- zip [0..] edgePairs
            let directIndex = 2 * i
            let inverseIndex = 2 * i + 1
            [((first, second), (directIndex, inverseIndex)), ((second, first), (inverseIndex, directIndex))]
        isSameFirstVertex ((a, _), _) ((b, _), _) = a == b
        groupedEdges = groupBy isSameFirstVertex . sort $ edgeIndexedPairs
        makeEdge ((_, second), (directIndex, inverseIndex)) = (directIndex, nameToVertex HM.! second, inverseIndex)
        makeVertex name index edges = Vertex (index, name, V.fromList $ map makeEdge edges)
        nameToVertex = HM.fromList $ do
            (index, edgeGroup) <- zip [0..] groupedEdges
            let ((first, _), (_, _)) : _ = edgeGroup
            return (first, makeVertex first index edgeGroup)


testCompileGraph = unvertex <$> cg
    where
        cg = compileGraph [("foo", ["bar", "baz"]), ("bar", ["baz"]), ("baz", ["foobar"]), ("foobar", [])]
        getName (index, Vertex (_, name, _), _) = (index, name)
        unvertex (Vertex (index, name, adj)) = (index, name, getName  <$> adj)

-- >>> testCompileGraph
-- [(0,"bar",[(4,"baz"),(1,"foo")]),(1,"baz",[(5,"bar"),(3,"foo"),(6,"foobar")]),(2,"foo",[(0,"bar"),(2,"baz")]),(3,"foobar",[(7,"baz")])]



solve inputFilename = do
    simpleGraph <- fmap parseLine . T.lines <$> T.readFile inputFilename
    return simpleGraph

-- >>> solve "inputs/sample/25.txt"
-- [("jqt",["rhn","xhk","nvd"]),("rsh",["frs","pzl","lsr"]),("xhk",["hfx"]),("cmg",["qnr","nvd","lhk","bvb"]),("rhn",["xhk","bvb","hfx"]),("bvb",["xhk","hfx"]),("pzl",["lsr","hfx","nvd"]),("qnr",["nvd"]),("ntq",["jqt","hfx","bvb","xhk"]),("nvd",["lhk"]),("lsr",["lhk"]),("rzs",["qnr","cmg","lsr","rsh"]),("frs",["qnr","lhk","lsr"])]

-- >>> solve "inputs/real/25.txt"
-- [("tcs","vrk"),("mjn","vrk")]
