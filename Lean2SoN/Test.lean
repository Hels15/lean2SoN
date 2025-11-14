
def a: Array (Option Nat) := #[some 1, none, some 3]

def test_drop_nulls (a: Array (Option Nat)): Array Nat :=
  a.filterMap id

#eval test_drop_nulls a  -- should be #[1, 3]
