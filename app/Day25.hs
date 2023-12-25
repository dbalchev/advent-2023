{-# LANGUAGE OverloadedStrings #-}
module Day25 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V

import qualified Data.HashTable.IO as HT
import qualified Data.Bifunctor
import Control.Monad (forM_)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text.Lazy.Lens as T
import Data.List (sort)

parseLine line = (node1, nodes)
    where
        [node1, nodes_concat] = T.splitOn ": " line
        nodes = T.words nodes_concat

psort (a, b) = (sa, sb)
    where
        [sa, sb] = sort [a, b]

solve inputFilename = do
    simpleGraph <- fmap parseLine . T.lines <$> T.readFile inputFilename

    let 
        makeAdjMatrix = do
            protoAdjMatrix <- HT.new :: IO (HT.BasicHashTable T.Text [T.Text])
            edgeSet <- HT.new :: IO (HT.BasicHashTable (T.Text, T.Text) ())
            forM_ simpleGraph $ \(node1, nodes) -> do
                forM_ nodes $ \node2 -> do
                    HT.mutate protoAdjMatrix node1 (\o -> (Just $ node2: fromMaybe [] o, ()))
                    HT.mutate protoAdjMatrix node2 (\o -> (Just $ node1: fromMaybe [] o, ()))
                    HT.insert edgeSet (psort (node1, node2)) ()
            protoAdjMatrixList <- HT.toList protoAdjMatrix
            adjMatrix <- HT.fromList (map (Data.Bifunctor.second V.fromList) protoAdjMatrixList) :: IO (HT.BasicHashTable T.Text (V.Vector T.Text))
            return (adjMatrix, edgeSet)
    (adjMatrix, edgeSet) <- makeAdjMatrix
    
    nodeList <- V.fromList . map fst <$> HT.toList adjMatrix
    let 
        hasEdnge node1 node2 = do
            isJust <$> HT.lookup edgeSet (psort (node1, node2))
        find3clique = do
            

    let foo = 3
    return simpleGraph

-- >>> solve "inputs/sample/25.txt"
-- [("jqt",["rhn","xhk","nvd"]),("rsh",["frs","pzl","lsr"]),("xhk",["hfx"]),("cmg",["qnr","nvd","lhk","bvb"]),("rhn",["xhk","bvb","hfx"]),("bvb",["xhk","hfx"]),("pzl",["lsr","hfx","nvd"]),("qnr",["nvd"]),("ntq",["jqt","hfx","bvb","xhk"]),("nvd",["lhk"]),("lsr",["lhk"]),("rzs",["qnr","cmg","lsr","rsh"]),("frs",["qnr","lhk","lsr"])]
