import Lean2SoN.Node
import Lean2SoN.Lexer
import Lean2SoN.Parser

-- def hello := "world"


-- def isWhiteSpace(ch: Char) : Bool :=
--   ch <= ' '

-- -- one with the exclamation mark

-- def charAt? (s : String) (n : Nat) : Option Char :=
--   go s.startPos n
-- where
--   go (pos : String.Pos) : Nat → Option Char
--   | 0 => s.get? pos
--   | (Nat.succ k) =>
--       match s.next? pos with
--       | some nextPos => go nextPos k
--       | none => none
-- -- optional with getd

-- #eval (some 42).getD 0
-- #eval (none : Option Nat).getD 0
-- #eval isWhiteSpace '→'

-- def findFirstEven(xs: List Nat) : Option Nat :=
--   xs.find? (fun x => x % 2 == 0)



-- def sumTo (n : Nat) : Nat :=
-- if n = 0 then
--   0
-- else
--   n + sumTo (n - 1)


-- def sub2 (n : Nat) : Nat :=
--   if n ≤ 1 then
--     0
--   else
--     sub2 (n - 2)



-- -- monad
-- def isPrime (n : Nat) : Bool := Id.run do
--   for i in [2:n] do
--     if i * i > n then return true
--     if n % i = 0 then return false
--   return true

--   -- why do we need partial here but not above
-- partial def nextPrime (n : Nat) : Nat :=
--   let n := n + 1
--   if isPrime n then n else nextPrime n


-- def advanceString(s:String) (pos: String.Pos) : String.Pos :=
--   s.next pos


-- def a : String := "test"

-- -- it returns endPos if it does not contain it
-- --#eval a.posOf 'a'  -- print the character at that position

-- def a1 : String := "string2"


-- #eval a1.extract
-- def isDigit (ch: Char) : Bool :=
--   ch.isDigit

-- structure Lexer where
--   source : String
--   pos    : String.Pos
-- deriving Repr

-- -- "Method" version — not stored in the struct
-- def Lexer.peek (self : Lexer) : Option Char :=
--   self.source.get? self.pos

-- def example1 : IO Unit := do
--   let l := { source := "abc", pos := 0 : Lexer }
--   IO.println s!"peek: {l.peek}"


-- #eval example1
-- #eval a1.get! (String.next a1 (String.next a1 (0 : String.Pos)))


-- structure Node where
--   id: Nat




-- def require(Option Node)(syn: String) :

-- def requireWd()

-- Test

def assert {α : Type} [BEq α] (a b : α) : Bool :=
  let bool := a == b
  assert! bool
  bool



def testSimpleProgram : M Unit := do
  let initArena : ManyNodes := { uniqueNodeId := 0, allNodes := #[] }
  let parser := Parser.ParserMK "return 1;"
  let (retNode, arena) ← parser.parse.run initArena
  let start ← parser.startN.run' initArena

  assert! (retNode.inputs.size == 2)
  assert! (retNode.inputs[0]!.nid == start.ref.nid)

  let expr ← Node.getNodeByRef (retNode.inputs[1]!)
  match expr.data with
  | NodeData.constantl v => assert! (v == 1)
  | _ => panic! "Expression is not a constant node"



#eval testSimpleProgram.run' { uniqueNodeId := 0, allNodes := #[] }
