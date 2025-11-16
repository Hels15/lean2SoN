import Mathlib.Logic.Function.Iterate
import Lean2SoN.Node

structure Lexer where
   source : String
   position: String.Pos := 0
deriving Repr

open Node
namespace Lexer


def LexerMk(source1 : String) : Lexer :=
    {source:= source1}

def isEOF(lexer : Lexer) : Bool :=
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
def isWhiteSpace(lexer : Lexer) : Bool :=
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
    ({ lexer with position := ((lexer.source.next^[syn.length] lexer.position)) }, true)
  else
    (lexer, false)

def isIdLetter (ch : Char) : Bool :=
  ch.isAlpha || ch = '_' || ch.isDigit

def isIdStart (ch : Char) : Bool :=
  ch.isAlpha || ch = '_'

def isNumber (ch : Char) : Bool :=
   ch.isDigit

def isNumberPeek(lexer : Lexer) : Bool :=
    isNumber (lexer.peek.getD ' ')

def isPunctuation (ch : Char) : Bool :=
  let punctuations := "=;[]<>(){}+-/*!"
  punctuations.contains ch

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
          ({ lexer1 with position := lexer.position - syn.endPos}, false)
    | none =>
        -- end of input after syntax ⇒ valid match
        (lexer1, true)



def parseNumberString (lexer : Lexer) : (Lexer ×  String) := Id.run do
  let start := lexer.position
  let mut lexer := lexer
  repeat
    if let (lexer1, some ch) := nextChar lexer then
      if !ch.isDigit then break
      lexer := lexer1
    else
      break
  return (lexer, lexer.source.extract start lexer.position)


def parseNumber(lexer : Lexer) : M (Lexer × Int) :=
   let (lexer, snum) := parseNumberString lexer
   if snum.length > 1 && snum.get! 0 = '0' then
    throw (IO.userError s!"Syntax error: integer values cannot start with '0'")
   else
     return (lexer, snum.toInt!)

def parseId(lexer : Lexer) : String := Id.run do
  let start:= lexer.position
  let mut lexer := lexer

    repeat
    if let (lexer1, some ch) := nextChar lexer then
      if !isIdLetter ch then break
      lexer := lexer1
    else
      break
  return lexer.source.extract start lexer.position

def parsePunctuation(lexer : Lexer) : String := Id.run do
    let start:= lexer.position
    return lexer.source.extract start (String.next lexer.source start)


def getAnyNextToken(lexer : Lexer) : Lexer × String :=
  if lexer.isEOF then
    (lexer, "")
  else if isIdStart ((peek lexer).getD ' ') then
    (lexer, parseId lexer)
  else if isNumber ((peek lexer).getD ' ') then
    parseNumberString lexer
  else if isPunctuation ((peek lexer).getD ' ') then
    (lexer, parsePunctuation lexer)
  else
    (lexer, String.singleton ((peek lexer).getD ' '))





end Lexer
