import Lean2SoN.Node
import Lean2SoN.Lexer
import Lean2SoN.Parser

def assert {α : Type} [BEq α] (a b : α) : Bool :=
  let bool := a == b
  assert! bool
  bool



def testSimpleProgram : M Unit := do
  let parser := Parser.ParserMK "return 1;"
  let (retNode) ← parser.parse

  let start ← Node.getNodeByRef retNode.inputs[0]!
  assert! (retNode.inputs.size == 2)
  assert! (NodeData.startData == start.data)

  -- The 1 as an expresssion
  IO.println s!"retNode ref: {retNode.inputs[1]!}"

  let expr ← Node.getNodeByRef (retNode.inputs[1]!)
  match expr.data with
  | NodeData.constantl v => assert! (v == 1)
  | _ => panic! "Expression is not a constant node"



#eval testSimpleProgram.run' { uniqueNodeId := 0, allNodes := #[] }
