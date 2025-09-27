structure NodeRef where
    nid: Nat
deriving Inhabited, Repr

inductive NodeData where
  | constantl (value: Int64)
  | nullData
deriving Inhabited, Repr

structure Node where
  ref: NodeRef
  inputs: Array NodeRef := #[]
  outputs: Array NodeRef:= #[]
  data: NodeData
deriving Inhabited, Repr


structure ManyNodes where
  uniqueNodeId: Nat
  -- Indexed by nodeRef
  allNodes: Array Node

abbrev M := StateRefT ManyNodes IO


namespace Node

-- Loop through inputs and get their nodeRef
def nodeIn(n: Node) (idx: Nat) : M Node := do
 for n_idx in [0: n.inputs.size] do
   if(n_idx = idx) then do
     let stateOld ← get
     match stateOld.allNodes[n.inputs[n_idx]!.nid]? with
      | some node => return node
      | none => panic! "Invalid index {idx} in nodeIn"
  panic! "Invalid index {n_idx} in nodeIn"

def nodeNins(n: Node) : Nat :=
   n.inputs.size


def nodeNouts(n: Node) : Nat :=
   n.outputs.size

def nodeUnused(n: Node) : Bool :=
  n.outputs.size = 0

def nodeIsCFG(n: Node) : Bool :=
  false

-- Add a custom node to the arena
def addNode2 (newNode : Node) : M Unit := do
  -- update the arena: increment unique ID and store the new node
  modify fun arena =>
    { arena with
      uniqueNodeId := arena.uniqueNodeId + 1,
      allNodes     := arena.allNodes.push newNode }


def nodeMK(inputs: Array NodeRef := #[]) : M Node := do
  let uid := (← get).uniqueNodeId
  let ref: NodeRef := {nid := uid}
   let newNode : Node := { ref := ref, inputs := inputs, outputs := (#[] : Array NodeRef), data := NodeData.nullData }
      --   -- Update outputs of all input nodes
    let updatedInputs := inputs.map (fun n => { n with outputs := n.outputs.push newNode.ref })

    let newNode : Node := { newNode with
      ref := ref,
      inputs := updatedInputs
    }

    addNode2 newNode
    return newNode
end Node
