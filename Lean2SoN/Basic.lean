import Lean2SoN.Node
import Lean2SoN.Lexer
import Lean2SoN.Parser
import Lean2SoN.GraphViz

open Node
-- PROOFS
def testSimpleProgram : M Unit := do
  let parser  ← Parser.ParserMK "return 1;"
  let (retNode) ← parser.parse

  let graph : String <- GraphViz.generateDotOutput parser
  IO.println graph

  let start ← Node.getNodeByRef retNode.inputs[0]!
  let start2 ← Node.getNodeByRef parser.startN.ref
  assert! (retNode.inputs.size == 2)
  assert! (NodeData.startData == start.data)

  IO.println s!"Start node outputs size: {start2.outputs.size}"
  IO.println s!"Start node outputs size: {start.outputs.size}"
  let expr ← Node.getNodeByRef (retNode.inputs[1]!)
  match expr.data with
  | NodeData.constantl v => assert! (v == 1)
  | _ => panic! "Expression is not a constant node"


def testZero : M Unit := do
  let parser  ← Parser.ParserMK "return 0;"
  let (retNode) ← parser.parse

  let start ← Node.getNodeByRef retNode.inputs[0]!
  assert! (retNode.inputs.size == 2)
  assert! (NodeData.startData == start.data)

  for out in start.outputs do
    let nodeOut ← Node.getNodeByRef out
    match nodeOut.data with
    | NodeData.constantl v => assert! (v == 0)
    | _ => continue


def testBad1 : M Unit := do
  let parser ← Parser.ParserMK "ret"
  try
    -- call something that should fail
    let retNode ← parser.parse
    -- if it didn't throw, fail the test
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    -- check the message
    let msg := e.toString
    assert! (msg == "Syntax error, expected a statement: ret")


def testBad2 : M Unit := do
  let parser ← Parser.ParserMK "return 0123;"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error: integer values cannot start with '0'")


def testBad3 : M Unit := do
  let parser ← Parser.ParserMK "return --12;"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, expected integer literal")


def testBad4 : M Unit := do
  let parser ← Parser.ParserMK "return 100"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, expected ;: ")


-- Negative numbers require unary operator support that is not in scope
def testBad5 : M Unit := do
  let parser ← Parser.ParserMK "return -100;"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, expected integer literal")


def testBad6 : M Unit := do
  let parser ← Parser.ParserMK "return100"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, expected a statement: return100")


def testBad7 : M Unit := do
  let parser ← Parser.ParserMK "return 1;}"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, unexpected }")



def testInputOutputBijection : M Unit := do
  let start ← Node.nodeMK #[] NodeData.startData
  let const1 ← Node.nodeMK #[start.ref] (NodeData.constantl 1)
  let startNode ← Node.getNodeByRef start.ref
  let constNode ← Node.getNodeByRef const1.ref

  -- Check inputs/outputs

#eval testSimpleProgram.run' { uniqueNodeId := 0, allNodes := #[] }

#eval testZero.run' { uniqueNodeId := 0, allNodes := #[] }

#eval testBad1.run' { uniqueNodeId := 0, allNodes := #[] }

#eval testBad2.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad3.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad4.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad5.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad6.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad7.run' { uniqueNodeId := 0, allNodes := #[] }

-- Basic static proofs without IO

-- Define NodeRef

-- Define NodeTest

-- inductive NodeDataTest where
--   | constantl (value: Int)
--   | nullData
--   | returnData
--   | startData
-- deriving Inhabited, Repr,  DecidableEq

-- structure NodeTest where
--   ref : Option NodeRef
--   data: NodeDataTest
--   inputs : Array (Option NodeRef) := #[]
--   outputs : Array (Option NodeRef) := #[]
-- deriving DecidableEq


-- def starttest1plus2 : NodeTest :=
--   { data := NodeDataTest.startData,
--     ref := some { nid := 0 },
--     inputs := #[],  -- start node has no inputs
--     outputs := #[some { nid := 1 }, some { nid := 2 }, some { nid := 3 }, some { nid := 4 }] }

-- -- Define your small example graph: 1 + 2
-- def test1plus2 : NodeTest :=
--   { data := NodeDataTest.nullData,
--     ref := some { nid := 1 },
--     inputs := #[some { nid := 0 }, some { nid := 2 }, some { nid := 3 }],
--     outputs := #[] }

-- def test1 : NodeTest :=
--   { data := NodeDataTest.nullData,
--     ref := some { nid := 2 },
--     inputs := #[some { nid := 0 }],
--     outputs := #[some { nid := 1 }] }

-- def test2 : NodeTest :=
--   { data := NodeDataTest.nullData,
--     ref := some { nid := 3 },
--     inputs := #[some { nid := 0 }],
--     outputs := #[some { nid := 1 }, some { nid := 4 }] }

-- def test3 : NodeTest :=
--   { data := NodeDataTest.nullData,
--     ref := some { nid := 4 },
--     inputs := #[some { nid := 0 }, some { nid := 3 }],
--     outputs := #[some { nid := 1 }] }

-- -- Put all nodes in an array
-- def testGraph : Array NodeTest :=
--   #[starttest1plus2, test1plus2, test1, test2, test3]


-- def isCfg(node: NodeTest) :=
--  match node.data with
--   | NodeDataTest.startData => true
--   | NodeDataTest.returnData => true
--   | _ => false



-- -- Consistency: if x is an input to a, then a must appear in x.outputs
-- def inputsOutputsConsistentTest (nodes : Array (Option NodeTest)) : Prop :=
--   ∀ (a x : NodeTest),
--     some a ∈ nodes →  -- wrap in some to match Option
--     some x ∈ nodes →
--     ∃ refA refX,
--       a.ref = some refA ∧ x.ref = some refX ∧
--       refX ∈ a.inputs.filterMap id ∧
--       refA ∈ x.outputs.filterMap id

-- -- Node control: every node's first input must be a CFG node (if it has inputs)
-- def nodeControlTest (nodes : Array (Option NodeTest)) : Prop :=
--   ∀ o ∈ nodes,
--     match o with
--     | none => True
--     | some n =>
--       (n.inputs.isEmpty ∧ isCfg n) ∨
--       (¬ n.inputs.isEmpty ∧
--         let firstRef := n.inputs[0]!
--         ∃ ctrl ∈ nodes,
--           match ctrl with
--           | some c => c.ref = firstRef ∧ isCfg c
--           | none => False)

-- -- No input cycles: x cannot be an input to a node b that has x as input
-- def noInputCycleTest (nodes : Array (Option NodeTest)) : Prop :=
--   ∀ oX ∈ nodes, ∀ oB ∈ nodes,
--     match oX, oB with
--     | some x, some b =>
--       ∀ refB, b.ref = some refB → refB ∈ x.inputs → ¬ (match x.ref with | some r => r ∈ b.inputs | none => False)
--     | _, _ => True

-- -- CFG nodes (except Start) must have a control input that is a CFG node
-- def cfgHasControlInputTest (nodes : Array (Option NodeTest)) : Prop :=
--   ∀ o ∈ nodes,
--     match o with
--     | some n =>
--       isCfg n ∧ n.data ≠ NodeDataTest.startData →
--         ∃ ctrl ∈ nodes,
--           match ctrl with
--           | some c => c.ref ∈ n.inputs.filterMap id ∧ isCfg c
--           | none => False
--     | none => True
-- deriving instance Decidable

-- -- Now prove the property for this small graph
-- example : inputsOutputsConsistentTest testGraph := by
--   unfold inputsOutputsConsistentTest testGraph starttest1plus2 test1plus2 test1 test2 test3
--   simp only [List.mem_toArray, List.mem_cons, List.mem_nil_iff, or_imp, forall_and,
--     forall_eq_apply_imp_iff, forall_eq]
--   simp

-- example : nodeControlTest testGraph := by decide

-- example : noInputCycleTest testGraph := by decide

-- example : cfgHasControlInputTest testGraph := by decide

def sumArray (xs : Array Nat) : Nat :=
  xs.foldl (fun acc x => acc + x) 0

#eval sumArray #[10, 20, 30]
