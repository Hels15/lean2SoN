import Lean2SoN.Parser
import Lean2SoN.Node
import Std
open Std
open Node

namespace GraphViz


def nodes (sb: String) (all: List Node) : M String := do

  let mut sb := sb ++ "\tsubgraph cluster_Nodes {\n"

  for n in all do
    let lab := n.glabel
    sb := sb ++ "\t\t" ++ n.uniqueName ++ " [ "

    let nodeG ← getNodeByRef n.ref
    if nodeG.nodeIsCFG then
      sb := sb ++ "shape=box style=filled fillcolor=yellow "

    sb := sb ++ "label=\"" ++ lab ++ "\" ];\n"

  sb := sb ++ "\t}\n"
  return sb


def nodeedges(sb: String) (all: List Node) : M String := do
  let mut sb := sb ++ "\tedge [ fontname=Helvetica, fontsize=8 ];\n"
  for n in all do
    let mut i := 0
    for def1? in n.inputs do
        match def1? with
          | none =>
            pure ()
          | some def1 => do
            let defNode ← getNodeByRef def1
            sb := sb
             ++ "\t" ++ n.uniqueName ++ " -> " ++ defNode.uniqueName
             ++ "[taillabel=" ++ toString i

            if n.isConstant && defNode.isStart then
             sb := sb ++ " style=dotted"
            else if defNode.nodeIsCFG then
             sb := sb ++ " color=red"

           sb := sb ++ "];\n"

        i := i + 1 -- For debugging
  return sb

private partial def walk (all : Std.HashMap Nat Node) (n : Node) : M (Std.HashMap Nat Node) := do
  let some r := n.ref
    | return all

  if all.contains r.nid then return all
  let mut all := all.insert r.nid n
  for c in n.inputs do
    if let some c := c then
      let c1 ← getNodeByRef c
      all ← walk all c1
  for c in n.outputs do
    if let some c := c then
      let c1 ← getNodeByRef c
      all ← walk all c1
  return all

def findAll (parser : Parser) : M (List Node) := do
  let start ← Node.getNodeByRef parser.startN.ref
  let mut all : Std.HashMap Nat Node := {}

  IO.println s!"StartNode outputs {start.outputs.size}"
  for c in start.outputs do
    if let some c := c then
      let c1 ← getNodeByRef c
      all ← walk all c1

  return all.values




def generateDotOutput (parser : Parser) : M String := do

  let all ← findAll parser
  dbg_trace s!"all length for nodes: {all.length}"

  let mut sb: String := ""
  sb := sb ++ "digraph G {\n"

  sb := sb ++ "\trankdir=BT;\n"
  sb := sb ++ "\tordering=\"in\";\n"
  sb := sb ++ "\tconcentrate=\"true\";\n"

  let firstPart: String ← nodes sb all

  let mut newPart: String ← nodeedges firstPart all

  newPart := newPart ++ "}\n"
  return newPart


end GraphViz
