def hello := "world"


def isWhiteSpace(ch: Char) : Bool :=
  ch <= ' '

-- one with the exclamation mark

def charAt? (s : String) (n : Nat) : Option Char :=
  go s.startPos n
where
  go (pos : String.Pos) : Nat → Option Char
  | 0 => s.get? pos
  | (Nat.succ k) =>
      match s.next? pos with
      | some nextPos => go nextPos k
      | none => none
-- optional with getd

#eval (some 42).getD 0
#eval (none : Option Nat).getD 0
#eval isWhiteSpace '→'

def findFirstEven(xs: List Nat) : Option Nat :=
  xs.find? (fun x => x % 2 == 0)



def sumTo (n : Nat) : Nat :=
if n = 0 then
  0
else
  n + sumTo (n - 1)


def sub2 (n : Nat) : Nat :=
  if n ≤ 1 then
    0
  else
    sub2 (n - 2)



-- monad
def isPrime (n : Nat) : Bool := Id.run do
  for i in [2:n] do
    if i * i > n then return true
    if n % i = 0 then return false
  return true

  -- why do we need partial here but not above
partial def nextPrime (n : Nat) : Nat :=
  let n := n + 1
  if isPrime n then n else nextPrime n


def advanceString(s:String) (pos: String.Pos) : String.Pos :=
  s.next pos


def a : String := "test"

-- it returns endPos if it does not contain it
--#eval a.posOf 'a'  -- print the character at that position

def a1 : String := "string2"

#eval a1.get! (String.next a1 (String.next a1 (0 : String.Pos)))
