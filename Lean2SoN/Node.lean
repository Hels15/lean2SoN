import Std.Data.HashMap

structure NodeRef where
    nid: Nat
deriving Inhabited, Repr,  DecidableEq

inductive NodeData where
  | constantl (value: Int)
  | nullData
  | returnData
  | startData
deriving Inhabited, Repr, BEq, DecidableEq

-- Todo: need return and start
structure Node where
  ref: Option NodeRef
  inputs: Array (Option NodeRef) := #[]
  outputs: Array (Option NodeRef) := #[]
  data: NodeData
deriving Inhabited, Repr

namespace Node

def hashN (n : Node) : UInt64 := Id.run do
  let mut hash : UInt64 := 0
  for optChild in n.inputs do
    match optChild with
    | some child =>
        let nid := UInt64.ofNat child.nid
        hash := hash ^^^ (hash <<< 17) ^^^ (hash >>> 13) ^^^ nid
    | none =>
        pure ()
  if hash == 0 then
    return 0xDEADBEEF
  else
    return hash


instance : Hashable Node where
  hash n := hashN n

-- ReturnNode true
-- StartNode true
-- for everything else false
def nodeIsCFG(n: Node) : Bool :=
  match n.data with
   | NodeData.returnData => true
   | NodeData.startData  => true
   | _ => false

-- Prove it here:
def inputsOutputsConsistent (nodes : Array Node) : Prop :=
  ∀ (a x : Node),
    a ∈ nodes →
    x ∈ nodes →
    x.ref ∈ a.inputs →
    a.ref ∈ x.outputs


def nodeControl (nodes : Array Node) : Prop :=
  ∀ n ∈ nodes,
    (n.inputs.isEmpty ∧ nodeIsCFG n) ∨
    (¬ n.inputs.isEmpty ∧
      let firstRef := n.inputs[0]!
      ∃ ctrl ∈ nodes, ctrl.ref = firstRef ∧ nodeIsCFG ctrl)
deriving Decidable


def noInputCycle (nodes : Array Node) : Prop :=
  ∀ x ∈ nodes, ∀ b ∈ nodes,
    b.ref ∈ x.inputs →
    ¬ (x.ref ∈ b.inputs)
deriving Decidable


def cfgHasControlInput (nodes : Array Node) : Prop :=
  ∀ n ∈ nodes,
    nodeIsCFG n →
      n.data ≠ NodeData.startData →
      ∃ ctrl ∈ nodes,
        ctrl.ref ∈ n.inputs ∧ nodeIsCFG ctrl
deriving Decidable

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
    let refStr := match n.ref with
      | some r => toString r.nid
      | none   => "none"
    let inputsStr := n.inputs.map (fun o => match o with
      | some r => toString r.nid
      | none   => "none")
    let outputsStr := n.outputs.map (fun o => match o with
      | some r => toString r.nid
      | none   => "none")
    s!"Node(id={refStr}, inputs={inputsStr}, outputs={outputsStr}, data={n.data})"


structure ManyNodes where
  uniqueNodeId: Nat
  -- Indexed by nodeRef
  allNodes: Array Node
  --invariant1: inputsOutputsConsistent allNodes
  --invariant2: nodeControl allNodes
  --invariant3: noInputCycle allNodes
  --invariant4: cfgHasControlInput allNodes

abbrev M := (StateRefT ManyNodes IO)


-- Easy reading label for debugger, e.g. "Add" or "Region" or "EQ"
def label (n: Node) : String :=
  match n.data with
  | NodeData.constantl v => s!"#{v}"
  | NodeData.nullData    => "Null"
  | NodeData.returnData  => "Return"
  | NodeData.startData   => "Start"

--  Unique label for graph visualization, e.g. "Add12" or "Region30" or "EQ99"
def uniqueName (n: Node) : String :=
  match n.ref with
  | some r => s!"N{r.nid}"
  | none   => "Nnone"


-- Graphical label, e.g. "+" or "Region" or "=="
def glabel (n: Node) : String :=
  match n.data with
  | _ => n.label


def isConstant (n: Node) : Bool :=
  match n.data with
  | NodeData.constantl _ => true
  | _                    => false

def isStart (n: Node) : Bool :=
  match n.data with
  | NodeData.startData => true
  | _                  => false

def getNodeByRef (ref : Option NodeRef) : M Node := do
  let state ← get
  match ref with
  | some r =>
    match state.allNodes[r.nid]? with
    | some node => return node
    | none      => panic! s!"Invalid NodeRef nid={r.nid}"
  | none =>
    panic! "Attempted to get node with none reference"


-- Gets the ith input
def nodeIn (n : Node) (idx : Nat) : M Node := do
  if h : idx < n.inputs.size then
    match n.inputs[idx] with
    | some ref =>
      let stateOld ← get
      match stateOld.allNodes[ref.nid]? with
      | some node => return node
      | none      => panic! s!"Invalid reference nid={ref.nid} in nodeIn"
    | none =>
      panic! s!"Input at index {idx} is none in nodeIn"
  else
    panic! s!"Invalid index {idx} in nodeIn"


def nodeNins(n: Node) : Nat :=
   n.inputs.size


def nodeNouts(n: Node) : Nat :=
   n.outputs.size

def nodeUnused(n: Node) : Bool :=
  n.outputs.size = 0

-- Add a custom node to the arena
def addNode2 (newNode : Node) : M Unit := do
  -- update the arena: increment unique ID and store the new node
  modify fun arena =>
    { arena with
      uniqueNodeId := arena.uniqueNodeId + 1,
      allNodes     := arena.allNodes.push newNode}


def nodeMK (inputs : Array (Option NodeRef) := #[])
            (data : NodeData := NodeData.nullData) : M Node := do
  let uid := (← get).uniqueNodeId
  let ref : NodeRef := { nid := uid }

  -- Create the base node
  let newNode : Node := {
    ref := some ref,
    inputs := inputs,
    outputs := #[],
    data := data
  }

  -- For each input node, update its outputs to include this new node
  for iRef in inputs do
    match iRef with
    | some r => do
        let mut inputNode ← getNodeByRef (some r)
        inputNode := { inputNode with outputs := inputNode.outputs.push (some ref) }
        modify fun st => { st with allNodes := st.allNodes.set! r.nid inputNode }
    | none =>
        pure ()  -- Skip empty input slots safely

  addNode2 newNode
  return newNode


end Node
