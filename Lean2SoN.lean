import Lean2SoN.Basic



-- Either this
def testIdx (idx: Nat) (ar: Array Nat) (h : idx < ar.size := by decide)  : Nat :=
  match ar[idx]? with
    | some s => s
    | none => panic! "Panic"


#eval testIdx 1 #[1, 2]

-- Default values for parameters

def testIdxDefault(x: Nat := 0) := x


-- Default value for a struct


#eval testIdxDefault

-- Decide in lean


-- by introduces a proof block
