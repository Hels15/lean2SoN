import Lean2SoN.Lexer
import Lean2SoN.Node

open Lexer
open Node

structure Parser where
   lexerL: Lexer
   startN: M Node


namespace Parser

def ParserMK(source: String) : Parser :=
 {lexerL := LexerMk source, startN := nodeMK #[]}



def errorSyntax {α: Type}(lexer : Lexer) (syn : String) : IO α :=
  throw (IO.userError s!"Syntax error, expected {syn}: {lexer.getAnyNextToken}")

def parseReturn(parser: Parser) : M Node := do
   let expr:= require require


def parseStatement(parser: Parser) : M Node := do
   if lexer.matchx "return" then parseReturn parser
   else panic! "Unknown statement"


def parse(parser: Parser) : M Node := do
   let ret := parseStatement parser

def matchL(parser: Parser )(syn: String) : Bool :=
   let (lex, b) := parser.lexerL.matchLex syn
   b

def matchx(parser: Parser )(syn: String) : Bool :=
   let (lex, b) := parser.lexerL.matchx syn
   b

def requrieWN {α : Type}(parser: Parser)(syn: String) : Unit :=
   let _ := errorSyntax parser.lexerL syn
   ()

def require(parser: Parser)(n: M Node) (syn: String) : M Node := do
   if(matchL parser syn) then n
   else errorSyntax parser.lexerL syn

end Parser
