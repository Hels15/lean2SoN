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
  error lexer s!"Syntax error, expected {syn}: {lexer.getAnyNextToken}"

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


def require (parser: Parser) (n: M Node) (syn: String) : M (Parser × Node) := do
  let (parser', matched) := parser.matchL syn
  if matched then
    let node ← n
    return (parser', node)
  else
    errorSyntax parser.lexerL syn


def parseIntegerLiteral(parser: Parser) : M Node := do
   Node.nodeMK #[] (NodeData.constantl parser.lexerL.parseNumber)

def parsePrimary(parser: Parser) : M Node := do
   let lexer:= parser.lexerL.skipWhiteSpace
   if(lexer.isNumberPeek)then parseIntegerLiteral parser
   else error lexer "Syntax error, expected integer literal"

def parseExpression(parser: Parser) : M Node := do
   parsePrimary parser


def parseReturn(parser: Parser) : M (Parser × Node) := do
  -- consume "return"
  let parser ← parser.requireWN "return"

  -- get start node
  let start ← parser.startN

  -- parse expression
  let (parser, expr) ← require parser (parseExpression parser) ";"

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
  let (parser, ret) ← parseStatement parser
  if !parser.lexerL.isEOF then
    error parser.lexerL s!"Syntax error, unexpected {parser.lexerL.getAnyNextToken}"
  return ret

end Parser
