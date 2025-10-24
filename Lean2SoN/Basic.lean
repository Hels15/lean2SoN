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

  let expr ← Node.getNodeByRef (retNode.inputs[1]!)
  match expr.data with
  | NodeData.constantl v => assert! (v == 1)
  | _ => panic! "Expression is not a constant node"



def testZero: M Unit := do
  let parser := Parser.ParserMK "return 0;"
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
  let parser := Parser.ParserMK "ret"
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
  let parser := Parser.ParserMK "return 0123;"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error: integer values cannot start with '0'")


def testBad3 : M Unit := do
  let parser := Parser.ParserMK "return --12;"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, expected integer literal")


def testBad4 : M Unit := do
  let parser := Parser.ParserMK "return 100"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, expected ;: ")


-- Negative numbers require unary operator support that is not in scope
def testBad5 : M Unit := do
  let parser := Parser.ParserMK "return -100;"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, expected integer literal")


def testBad6 : M Unit := do
  let parser := Parser.ParserMK "return100"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, expected a statement: return100")


def testBad7 : M Unit := do
  let parser := Parser.ParserMK "return 1;}"
  try
    let _ ← parser.parse
    throw (IO.userError "Expected failure, but parse succeeded")
  catch e =>
    let msg := e.toString
    assert! (msg == "Syntax error, unexpected }")


#eval testSimpleProgram.run' { uniqueNodeId := 0, allNodes := #[] }

#eval testZero.run' { uniqueNodeId := 0, allNodes := #[] }

#eval testBad1.run' { uniqueNodeId := 0, allNodes := #[] }

#eval testBad2.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad3.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad4.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad5.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad6.run' { uniqueNodeId := 0, allNodes := #[] }
#eval testBad7.run' { uniqueNodeId := 0, allNodes := #[] }
