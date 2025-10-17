structure Lexer where
   source: String
   position: String.Pos := 0
deriving Repr

namespace Lexer


def LexerMk(source1: String) : Lexer :=
    {source:= source1}

def isEOF(lexer: Lexer) : Bool :=
   lexer.position >= lexer.source.endPos

def peek (lexer : Lexer) : Option Char :=
  if isEOF lexer then
    none
  else
    some (lexer.source.get! lexer.position)

def nextChar (lexer : Lexer) : Lexer × Option Char :=
  match peek lexer with
  | some c =>
      let newPos := lexer.source.next lexer.position
      ({ lexer with position := newPos }, some c)
  | none =>
      (lexer, none)

-- Space has ASCII value of 32
-- Everyting below that should be handled as a whitespace
def isWhiteSpace(lexer: Lexer) : Bool :=
  match peek lexer with
     | some ch => ch <= ' '
     | none => false

-- if its a whitespace change lexer and do it with recursion again
-- if its not a whitespace just return the current lexer

partial def skipWhiteSpace (lexer : Lexer) : Lexer :=
  if isWhiteSpace lexer then
    skipWhiteSpace { lexer with position := String.next lexer.source lexer.position }
  else
    lexer

-- Return true, if we find "syntax" after skipping whitespace, also then
-- advance cursor past syntax
-- Return false otherwise and do not advnace the cursor

partial def matchLex (lexer : Lexer) (syn : String) : Lexer × Bool :=
  let lexer := skipWhiteSpace lexer
  let rec check (pos : String.Pos) (i : String.Pos) : Bool :=
    if i = syn.endPos then true                     -- matched all characters
    else match lexer.source.get? pos with
         | none => false                            -- ran out of input
         | some ch =>
             if ch ≠ syn.get i then false
             else check (String.next lexer.source pos) (String.next syn i)
  let success := check lexer.position 0
  if success then
    ({ lexer with position := (String.next lexer.source lexer.position) }, true)
  else
    (lexer, false)

def isIdLetter (ch : Char) : Bool :=
  ch.isAlpha || ch = '_' || ch.isDigit

-- We need to undo the lexer's position advancing that we did in matchLex
-- If match is true and the next character is not an underscore, identier and digit then return true
-- If it is not then just take away the suntax's length from position
def matchx (lexer : Lexer) (syn : String) : Lexer × Bool :=
  let (lexer1, matched) := matchLex lexer syn
  if !matched then
    (lexer1, false)
  else
    match peek lexer1 with
    | some ch =>
        if !isIdLetter ch then
          (lexer1, true)
        else
          -- backtrack: undo advancing by syntax length
          let backPos := String.iterator (lexer.source.drop lexer1.position.byteIdx - syn.length)
          ({ lexer with position := lexer.position }, false)
    | none =>
        -- end of input after syntax ⇒ valid match
        (lexer1, true)



end Lexer
