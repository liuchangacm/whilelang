package lang

import java.io.FileInputStream
import java.io.File

/**
 * @author Chang Liu (liuchang@cs.umd.edu)
 */
object TestParser {
  def main(args:Array[String]) = {
    val is = new FileInputStream(new File("exmp/parsertest/test1.smp"))
    val lex = new Lexer(is)
    val ast = Parser.parse(lex)
    println(ast)
  }
}