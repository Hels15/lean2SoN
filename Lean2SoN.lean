import Lean2SoN.Basic



-- Either this
def testIdx (idx: Nat) (ar: Array Nat) {h : idx < ar.size}  : Nat :=
  match ar[idx]? with
    | some s => s
    | none => panic! "Panic"


#eval testIdx 1 #[1, 2]
