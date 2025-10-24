structure NodeRef where
    nid: Nat
deriving Inhabited, Repr

inductive NodeData where
  | constantl (value: Int)
  | nullData
  | returnData
  | startData
deriving Inhabited, Repr, BEq

-- Todo: need return and start
structure Node where
  ref: NodeRef
  inputs: Array NodeRef := #[]
  outputs: Array NodeRef:= #[]
  data: NodeData
deriving Inhabited, Repr

instance : ToString NodeRef where
  toString r := s!"NodeRef({r.nid})"

instance : ToString NodeData where
  toString
    | .constantl v => s!"Const({v})"
    | .nullData    => "Null"
    | .returnData  => "Return"
    | .startData   => "Start"

instance : ToString Node where
  toString n :=
    s!"Node(id={n.ref.nid}, inputs={n.inputs.map (·.nid)}, outputs={n.outputs.map (·.nid)}, data={n.data})"


structure ManyNodes where
  uniqueNodeId: Nat
  -- Indexed by nodeRef
  allNodes: Array Node

abbrev M := (StateRefT ManyNodes IO)


namespace Node

def getNodeByRef (ref : NodeRef) : M Node := do
  let state ← get
  match state.allNodes[ref.nid]? with
  | some node => return node
  | none      => panic! s!"Invalid NodeRef: {ref}"

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

-- ReturnNode true
-- StartNode true
-- for everything else false
-- def nodeIsCFG(n: Node) : Bool :=
--   false

-- Add a custom node to the arena
def addNode2 (newNode : Node) : M Unit := do
  -- update the arena: increment unique ID and store the new node
  modify fun arena =>
    { arena with
      uniqueNodeId := arena.uniqueNodeId + 1,
      allNodes     := arena.allNodes.push newNode }


def nodeMK (inputs : Array NodeRef := #[])(data: NodeData := NodeData.nullData) : M Node := do
  let uid := (← get).uniqueNodeId
  let ref : NodeRef := { nid := uid }

  -- Create base node
  let newNode : Node := {
    ref := ref,
    inputs := inputs,
    outputs := #[],
    data := data
  }

  -- Update each input node’s outputs to include this new node
  for iRef in inputs do
    let mut inputNode ← Node.getNodeByRef iRef
    inputNode := { inputNode with outputs := inputNode.outputs.push ref }
    modify fun st => { st with allNodes := st.allNodes.set! iRef.nid inputNode }

  addNode2 newNode
  return newNode

end Node
