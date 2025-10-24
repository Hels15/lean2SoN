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
 {lexerL := LexerMk source, startN := nodeMK #[] NodeData.startData}



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
  let (parser', matched) := parser.matchL syn
  if matched then

    return parser'
  else
    errorSyntax parser.lexerL syn


def require (parser: Parser) (syn: String) : M (Parser) := do
  --It hast o get executed before in order for syn to be matching
  let (parser', matched) := parser.matchL syn
  if matched then

    return (parser')
  else
    errorSyntax parser.lexerL syn


def parseIntegerLiteral(parser: Parser) : M (Parser × Node) := do
  let (lexer', intVal) ←  parser.lexerL.parseNumber
  let node ← Node.nodeMK #[] (NodeData.constantl intVal)
  return ({ parser with lexerL := lexer' }, node)

def parsePrimary(parser: Parser) : M (Parser × Node):= do
   let lexer:= parser.lexerL.skipWhiteSpace
   if(lexer.isNumberPeek)then parseIntegerLiteral {parser with lexerL := lexer}
   else error lexer "Syntax error, expected integer literal"

def parseExpression(parser: Parser) : M (Parser × Node) := do
   parsePrimary parser


def parseReturn(parser: Parser) : M (Parser × Node) := do

  -- get start node
  let start ← parser.startN

  -- parse expression
  let (parser, expr) <- parseExpression parser
  let (parser) ← require parser ";"

  -- build return node
  let retNode ← Node.nodeMK #[start.ref, expr.ref] NodeData.returnData

  return (parser, retNode)


def parseStatement(parser: Parser) : M (Parser × Node) := do
  let (parser', matched) := parser.matchx "return"
  if matched then
    parseReturn parser'  -- pass the updated parser
  else
    errorSyntax parser.lexerL "a statement"


def parse(parser: Parser) : M Node := do
  let start ← parser.startN
  let (parser, ret) ← parseStatement parser
  if !parser.lexerL.isEOF then
    error parser.lexerL s!"Syntax error, unexpected {parser.lexerL.getAnyNextToken.2}"
  return ret

end Parser
