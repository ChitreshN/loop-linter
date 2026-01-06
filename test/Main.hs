module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort)
import LoopLinter.Graph (detectLoops, breakEdges)
import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

-- Graph Construction Helper
mkGraph :: (Ord a) => [(a, [a])] -> Map.Map a (Set.Set a)
mkGraph edges = Map.fromList $ map (\(k, v) -> (k, Set.fromList v)) edges

-- Tests for detectLoops
testSimpleCycle :: Test
testSimpleCycle = TestCase $ do
  let graph = mkGraph [('A', ['B']), ('B', ['A'])]
      loops = detectLoops graph
  assertBool "Should detect loop" (not (null loops))
  assertEqual "Loop length should be 2" 2 (length (head loops))

testNoCycle :: Test
testNoCycle = TestCase $ do
  let graph = mkGraph [('A', ['B']), ('B', ['C']), ('C', [])]
      loops = detectLoops graph
  assertBool "Should not detect loop" (null loops)

testSelfLoop :: Test
testSelfLoop = TestCase $ do
  let graph = mkGraph [('A', ['A'])]
      loops = detectLoops graph
  assertEqual "Should detect 1 loop" 1 (length loops)
  assertEqual "Loop should be [A]" ['A'] (head loops)

testDisconnectedCycle :: Test
testDisconnectedCycle = TestCase $ do
  let graph = mkGraph [('A', ['B']), ('B', []), ('C', ['D']), ('D', ['C'])]
      loops = detectLoops graph
  assertEqual "Should detect 1 loop" 1 (length loops)
  assertBool "Loop should contain C and D" (all (`elem` head loops) ['C', 'D'])

testComplexCycle :: Test
testComplexCycle = TestCase $ do
  let graph = mkGraph 
        [ ('A', ['B'])
        , ('B', ['C', 'D'])
        , ('C', ['A'])
        , ('D', [])
        ]
      loops = detectLoops graph
  assertEqual "Should detect 1 loop" 1 (length loops)
  assertEqual "Loop length should be 3" 3 (length (head loops))
  assertBool "Loop should contain A, B, C" (all (`elem` head loops) ['A', 'B', 'C'])

-- -- Tests for breakEdges
-- testBreakEdges :: Test
-- testBreakEdges = TestCase $ do
--   -- Scenario: A -> f1, B -> f2.
--   -- f1 has register (True), f2 has no register (False).
--   -- A depends on B.
--   let producerMap = Map.fromList [("A", Right "f1"), ("B", Right "f2")]
--       calleeMap   = mkGraph [("A", ["B"]), ("B", [])]
--       hasReg      = Map.fromList [("f1", True), ("f2", False)]
--
--       -- Action
--       brokenGraph = breakEdges producerMap calleeMap hasReg
--
--       -- Verification
--       depsA = Map.findWithDefault Set.empty "A" brokenGraph
--
--   assertBool "Dependencies of A should be empty (broken at reg)" (Set.null depsA)
--
-- testBreakEdgesNoReg :: Test
-- testBreakEdgesNoReg = TestCase $ do
--   -- Scenario: A -> f1, B -> f2.
--   -- Both f1, f2 are combinational (False).
--   -- A depends on B.
--   let producerMap = Map.fromList [("A", Right "f1"), ("B", Right "f2")]
--       calleeMap   = mkGraph [("A", ["B"]), ("B", [])]
--       hasReg      = Map.fromList [("f1", False), ("f2", False)]
--
--       -- Action
--       brokenGraph = breakEdges producerMap calleeMap hasReg
--
--       -- Verification
--       depsA = Map.findWithDefault Set.empty "A" brokenGraph
--
--   assertEqual "Dependencies of A should remain" (Set.fromList ["B"]) depsA
--
-- testBreakEdgesAlias :: Test
-- testBreakEdgesAlias = TestCase $ do
--   -- Scenario: A is alias of B, B is produced by f1 (Reg).
--   -- A depends on C.
--   let producerMap = Map.fromList [("A", Left "B"), ("B", Right "f1")]
--       calleeMap   = mkGraph [("A", ["C"]), ("C", [])]
--       hasReg      = Map.fromList [("f1", True)]
--
--       -- Action
--       brokenGraph = breakEdges producerMap calleeMap hasReg
--
--       -- Verification
--       depsA = Map.findWithDefault Set.empty "A" brokenGraph
--
--   assertBool "Dependencies of A (alias) should be empty" (Set.null depsA)
--
-- Test Suite
tests :: Test
tests = TestList 
  [ TestLabel "Simple Cycle" testSimpleCycle
  , TestLabel "No Cycle" testNoCycle
  , TestLabel "Self Loop" testSelfLoop
  , TestLabel "Disconnected Cycle" testDisconnectedCycle
  , TestLabel "Complex Cycle" testComplexCycle
  -- , TestLabel "Break Edges (Reg)" testBreakEdges
  -- , TestLabel "Break Edges (No Reg)" testBreakEdgesNoReg
  -- , TestLabel "Break Edges (Alias)" testBreakEdgesAlias
  ]

main :: IO ()
main = do
  counts <- runTestTT tests
  if errors counts + failures counts == 0
    then exitSuccess
    else exitFailure
