import Lean2SoN.Lexer
import Lean2SoN.Node

set_option trace.compiler.ir.result true

open Lexer
open Node

structure Parser where
   lexerL: Lexer
   startN: M Node


namespace Parser

def ParserMK(source: String) : Parser :=
 {lexerL := LexerMk source, startN := nodeMK #[]}



def error {α: Type}(lexer : Lexer) (syn : String) : IO α :=
  throw (IO.userError s!"{syn}")


def errorSyntax {α: Type}(lexer : Lexer) (syn : String) : IO α :=
  let (_, nextToken) := lexer.getAnyNextToken
  error lexer s!"Syntax error, expected {syn}: {nextToken}"

def matchx(parser: Parser)(syn: String) : Parser × Bool :=
  let (lexer', matched) := parser.lexerL.matchx syn
  ({ parser with lexerL := lexer' }, matched)


def matchL(parser: Parser)(syn: String) : Parser × Bool :=
  let (lexer', matched) := parser.lexerL.matchLex syn
  ({ parser with lexerL := lexer' }, matched)


@[inline]
def requireWN (parser : Parser) (syn : String) : IO Parser := do
  IO.println s!"Syntax in requireWN{syn}"
  IO.println s!"Lexer position in requireWN: {parser.lexerL.position}"
  let (parser', matched) := parser.matchL syn
  if matched then

  IO.println s!"Here WN"
    return parser'
  else
    IO.println s!"Else WN"
    errorSyntax parser.lexerL syn


def require (parser: Parser) (syn: String) : M (Parser) := do
  --It hast o get executed before in order for syn to be matching
  IO.println s!"Position in require123123: {parser.lexerL.position}"
  IO.println s!"syn: {syn}"

  let (parser', matched) := parser.matchL syn
  if matched then

    return (parser')
  else
    dbg_trace s!"Not matched"
    errorSyntax parser.lexerL syn


def parseIntegerLiteral(parser: Parser) : M (Parser × Node) := do
  let (lexer', intVal) ←  parser.lexerL.parseNumber
  let node ← Node.nodeMK #[] (NodeData.constantl intVal)
  return ({ parser with lexerL := lexer' }, node)

def parsePrimary(parser: Parser) : M (Parser × Node):= do
   let lexer:= parser.lexerL.skipWhiteSpace
   if(lexer.isNumberPeek)then parseIntegerLiteral parser
   else error lexer "Syntax error, expected integer literal"

def parseExpression(parser: Parser) : M (Parser × Node) := do
   IO.println s!"Ever called here{parser.lexerL.position}"
   parsePrimary parser


def parseReturn(parser: Parser) : M (Parser × Node) := do
  dbg_trace s!"in Parse Return{parser.lexerL.position}"

  -- get start node
  let start ← parser.startN

  IO.println s!"Before parse expression"
  -- parse expression
  let (parser, expr) <- parseExpression parser
  let (parser) ← require parser ";"

  IO.println s!"After parse expression"
  -- build return node
  let retNode ← Node.nodeMK #[start.ref, expr.ref] NodeData.returnData

  return (parser, retNode)


def parseStatement(parser: Parser) : M (Parser × Node) := do
  dbg_trace s!"parse statement position{parser.lexerL.position}"
  let (parser', matched) := parser.matchx "return"
  if matched then
    IO.println s!"If"
    parseReturn parser'  -- pass the updated parser
  else
    IO.println s!"Else"
    errorSyntax parser.lexerL "a statement"


def parse(parser: Parser) : M Node := do
  IO.println s!"Here12"
  let (parser, ret) ← parseStatement parser
  IO.println s!"Here2"
  if !parser.lexerL.isEOF then
    error parser.lexerL s!"Syntax error, unexpected {parser.lexerL.getAnyNextToken.2}"
  return ret

end Parser
