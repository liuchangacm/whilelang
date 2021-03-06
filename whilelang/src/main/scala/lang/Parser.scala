package lang

import java.io.InputStream
import java.io.BufferedReader
import java.io.InputStreamReader

/**
 * @author Chang Liu (liuchang@cs.umd.edu)
 */

object Lexer {
  sealed trait Token
  case class Iden(n:String) extends Token
  case class Num(n:Int) extends Token
  case object Lparen extends Token
  case object Rparen extends Token
  case object Lbracket extends Token
  case object Rbracket extends Token
  case object Lbrace extends Token
  case object Rbrace extends Token
  case object Add extends Token
  case object Sub extends Token
  case object Mul extends Token
  case object Div extends Token
  case object Eq extends Token
  case object Neq extends Token
  case object Lt extends Token
  case object Leq extends Token
  case object Gt extends Token
  case object Geq extends Token
  case object Neg extends Token
  case object And extends Token
  case object Or extends Token
  case object Semicolon extends Token
  case object Eof extends Token
  case object Unknown extends Token
}

class Lexer(is:InputStream) {
  var reader = new BufferedReader(new InputStreamReader(is))
  var curChar = reader.read()
  var nextChar = if(curChar >= 0) reader.read() else -1
  
  val ordinary=(('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ Array('_')).toSet
  val num = ('0' to '9').toSet
  
  var line = 0
  var col = 0
  
  def nextIdent() = {
    var sb = new StringBuffer
    while(ordinary.contains(curChar.toChar)) {
      sb.append(curChar.toChar)
      readNext
    }
    Lexer.Iden(sb.toString)
  }
  
  def nextNum() = {
    var sb = new StringBuffer
    while(num.contains(curChar.toChar)) {
      sb.append(curChar.toChar)
      readNext
    }
    Lexer.Num(sb.toString.toInt)
  }
  
  def readNext() = {
    curChar = if(curChar >= 0) nextChar else -1
    nextChar = if(curChar >= 0) reader.read() else -1
    if (curChar == '\n') {
      line += 1
      col = 0
    } else if (curChar == '\t') {
      col += 4
    } else
      col += 1
  }
  
  def nextToken():Lexer.Token = {
    if (curChar < 0)
      Lexer.Eof
    else {
      curChar match {
        case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g'
           | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n'
           | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u'
           | 'v' | 'w' | 'x' | 'y' | 'z' | 'A' | 'B'
           | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I'
           | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P'
           | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W'
           | 'X' | 'Y' | 'Z' | '_' => nextIdent()
        case '0' | '1' | '2' | '3' | '4' | '5' | '6'
           | '7' | '8' | '9' => nextNum()
        case ' ' | '\t' | '\r' | '\n' => { readNext; nextToken }
        case '(' => { readNext; Lexer.Lparen }
        case ')' => { readNext; Lexer.Rparen }
        case '[' => { readNext; Lexer.Lbracket }
        case ']' => { readNext; Lexer.Rbracket }
        case '{' => { readNext; Lexer.Lbrace }
        case '}' => { readNext; Lexer.Rbrace }
        case '+' => { readNext; Lexer.Add }
        case '-' => { readNext; Lexer.Sub }
        case '*' => { readNext; Lexer.Mul }
        case '/' => { 
          readNext
          if (curChar == '\\') {
            readNext
            Lexer.And
          } else {
            Lexer.Div 
          }
        }
        case '\\' => { 
          readNext
          if (curChar == '/') {
            readNext
            Lexer.Or
          } else {
            curChar = -1
            Lexer.Unknown
          }
        }
        case '=' => { readNext; Lexer.Eq }
        case ';' => { readNext; Lexer.Semicolon }
        case '!' => {
          readNext
          if (curChar == '=') {
            readNext
            Lexer.Neq
          } else {
            Lexer.Neg
          }
        }
        case '<' => {
          readNext
          if (curChar == '=') {
            readNext
            Lexer.Leq
          } else {
            Lexer.Lt
          }
        }
        case '>' => {
          readNext
          if (curChar == '=') {
            readNext
            Lexer.Geq
          } else {
            Lexer.Gt
          }
        }
        case -1 => Lexer.Eof
        case _ => {
          curChar = -1
          Lexer.Unknown
        }
      }
    }
  }
}

object Parser {
  import Lexer._
  
  def parse(lex:Lexer):Ast = {
    var token = lex.nextToken()
    parseCodeBlock(token, lex)._2
  }
  
  def parseCodeBlock(token:Token, lex:Lexer):(Token, Ast) = {
    var ast:Ast = Skip
    var curTok = token
    while(true) {
      parseStat(curTok, lex) match {
        case (tok, null) => {
          return (tok, ast) 
        }
        case (tok, stat) => {
          ast = ast match {
                  case Skip => stat
                  case _ => Seq(ast.asInstanceOf[Stat], stat.asInstanceOf[Stat])
                }
          curTok = tok
        }
      }
    }
    (curTok, ast)
  }
  
  def meet(cur:Token, token:Token, lex:Lexer) {
    if(cur != token) {
      throw new ParsingError(s"[Ln ${lex.line + 1}, Col ${lex.col + 1}] Unexpected token $cur ; expecting $token.")
    }
  }  
  
  def meet(token:Token, lex:Lexer) {
    val tok = lex.nextToken
    if(tok != token) {
      throw new ParsingError(s"[Ln ${lex.line + 1}, Col ${lex.col + 1}] Unexpected token $tok ; expecting $token.")
    }
  }
  
  def parseStat(token:Token, lex:Lexer):(Token, Ast) = {
    token match {
      case Iden("skip") => {
        meet(Semicolon, lex)
        (lex.nextToken, Skip)
      }
      case Iden("while") => {
        meet(Lparen, lex)
        val (tok, cond) = parseExpr(lex.nextToken, lex)
        meet(tok, Rparen, lex)
        val (tok1, body) = parseStat(lex.nextToken, lex)
        (tok1, While(cond.asInstanceOf[Expr], body.asInstanceOf[Stat]))
      }
      case Iden("if") => {
        meet(Lparen, lex)
        val (tok, cond) = parseExpr(lex.nextToken, lex)
        meet(tok, Rparen, lex)
        val (tok1, tb) = parseStat(lex.nextToken, lex)
        val (tok2, fb) = tok1 match {
          case Iden("else") => {
            parseStat(lex.nextToken, lex)
          }
          case _ => {
            (tok1, Skip) 
          }
        }
        (tok2, If(cond.asInstanceOf[Expr], tb.asInstanceOf[Stat], fb.asInstanceOf[Stat]))
      }
      case Iden("else") => {
        throw new ParsingError("Unexpected token else.")
      }
      case Iden("in") => {
        meet(Lparen, lex)
        lex.nextToken match {
          case Iden(x) => {
            meet(Rparen, lex)
            meet(Semicolon, lex)
            (lex.nextToken, In(x))
          }
          case tok => {
            throw new ParsingError("Unexpected token " + tok+".")
          }
        }
      }
      case Iden("print") => {
        meet(Lparen, lex)
        parseExpr(lex.nextToken, lex) match {
          case (tok, exp) => {
            meet(tok, Rparen, lex)
            meet(Semicolon, lex)
            (lex.nextToken, Out(exp.asInstanceOf[Expr]))
          }
          case tok => {
            throw new ParsingError("Unexpected token " + tok+".")
          }
        }
      }
      case Iden(x) => {
        meet(Eq, lex)
        val (tok, expr) = parseExpr(lex.nextToken, lex)
        meet(tok, Semicolon, lex)
        (lex.nextToken, Assign(x, expr.asInstanceOf[Expr]))
      }
      case Lbrace => {
        val (tok, ast) = parseCodeBlock(lex.nextToken, lex)
        meet(tok, Rbrace, lex)
        (lex.nextToken, ast)
      }
      case _ => {
        (token, null)
      }
    }
  }
  
  def parseExpr(token:Token, lex:Lexer):(Token, Ast) = {
    parseOrExpr(token, lex)
  }
  
  def parseOrExpr(token:Token, lex:Lexer):(Token, Ast) = {
    var (curTok, ast) = parseAndExpr(token, lex)
    while(curTok == Or) {
      var (tok, ast2) = parseAndExpr(lex.nextToken, lex)
      ast = Op(ast.asInstanceOf[Expr], lang.Or, ast2.asInstanceOf[Expr])
      curTok = tok
    }
    (curTok, ast)
  }
  
  def parseAndExpr(token:Token, lex:Lexer):(Token, Ast) = {
    var (curTok, ast) = parseCompExpr(token, lex)
    while(curTok == And) {
      var (tok, ast2) = parseCompExpr(lex.nextToken, lex)
      ast = Op(ast.asInstanceOf[Expr], lang.And, ast2.asInstanceOf[Expr])
      curTok = tok
    }
    (curTok, ast)
  }
  
  def parseCompExpr(token:Token, lex:Lexer):(Token, Ast) = {
    var (curTok, ast) = parseAddSubExpr(token, lex)
    var bop: lang.Bop = lang.Eq
    curTok match {
      case Eq => bop = lang.Eq
      case Neq => bop = lang.Ne
      case Lt => bop = lang.Lt
      case Gt => bop = lang.Gt
      case Geq => bop = lang.Geq
      case Leq => bop = lang.Leq
      case _ => return (curTok, ast)
    }
    var (tok, ast2) = parseAddSubExpr(lex.nextToken, lex)
    (tok, Op(ast.asInstanceOf[Expr], bop, ast2.asInstanceOf[Expr]))
  }
  
  def parseAddSubExpr(token:Token, lex:Lexer):(Token, Ast) = {
    var (curTok, ast) = parseMulDivExpr(token, lex)
    while(curTok == Add || curTok == Sub) {
      var (tok, ast2) = parseMulDivExpr(lex.nextToken, lex)
      curTok match {
        case Add => ast = Op(ast.asInstanceOf[Expr], lang.Add, ast2.asInstanceOf[Expr])
        case Sub => ast = Op(ast.asInstanceOf[Expr], lang.Sub, ast2.asInstanceOf[Expr])
        case _ => {
          throw new ParsingError("Impossible to reach here")
        }
      }
      curTok = tok
    }
    (curTok, ast)
  }
  
  def parseMulDivExpr(token:Token, lex:Lexer):(Token, Ast) = {
    var (curTok, ast) = parseUnaryExpr(token, lex)
    while(curTok == Mul || curTok == Div) {
      var (tok, ast2) = parseUnaryExpr(lex.nextToken, lex)
      curTok match {
        case Mul => ast = Op(ast.asInstanceOf[Expr], lang.Mul, ast2.asInstanceOf[Expr])
        case Div => ast = Op(ast.asInstanceOf[Expr], lang.Div, ast2.asInstanceOf[Expr])
        case _ => {
          throw new ParsingError("Impossible to reach here")
        }
      }
      curTok = tok
    }
    (curTok, ast)
  }
  
  def parseUnaryExpr(token:Token, lex:Lexer):(Token, Ast) = {
    token match {
      case Sub => {
        val (tok, exp) = parseUnitExpr(lex.nextToken, lex)
        (tok, UnaryOp(lang.Neg, exp.asInstanceOf[Expr]))
      }
      case Neg => {
        val (tok, exp) = parseUnitExpr(lex.nextToken, lex)
        (tok, UnaryOp(Not, exp.asInstanceOf[Expr]))
      }
      case _ => {
        parseUnitExpr(token, lex)
      }
    }
  }
    
  def parseUnitExpr(token:Token, lex:Lexer):(Token, Ast) = {
    token match {
      case Iden(x) => (lex.nextToken, Var(x))
      case Num(n) => (lex.nextToken, Const(n))
      case Lparen => {
        val (tok, exp) = parseExpr(lex.nextToken, lex)
        meet(tok, Rparen, lex)
        (lex.nextToken, exp)
      }
      case _ => {
        throw new ParsingError("Unexpected token "+token+".")
      }
    }
  }

}

class ParsingError(msg:String) extends Exception("Parsing Error: " + msg)
