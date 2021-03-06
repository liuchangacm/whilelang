package lang

/**
 * @author Chang Liu (liuchang@cs.umd.edu)
 */
object Formatter {
  def indent(n:Int) = {
    var sb = new StringBuffer
    for(_ <- 0 until n) sb.append("    ")
    sb.toString
  }
  
  def toString(n:Int, ast:Ast):String = {
    ast match {
      case Skip => indent(n) + "skip;\n"
      case Assign(x, e) => indent(n) + s"$x = $e;\n"
      case Seq(s1, s2) => toString(n, s1) + toString(n, s2)
      case If(cond, tbranch, fbranch) =>
              indent(n) + s"if ($cond) {\n" +
              toString(n+1, tbranch) + indent(n) + "}" +
              (if (fbranch == Skip) 
                  "\n" 
               else 
                  " else {\n" + toString(n+1, fbranch) 
                  + indent(n) + "}\n")
      case While(cond, body) =>
              indent(n) + s"while ($cond) {\n" +
              toString(n+1, body) + indent(n) + "}\n"
      case In(v) => indent(n) + s"in($v);\n"
      case Out(e) => indent(n) + s"print($e);\n"
      
      case Var(v) => v
      case Const(n) => n.toString
      case Op(e1, op, e2) => s"$e1 $op $e2"
      case UnaryOp(op, e) => s"$op $e"
    }
  }
  
  def toString(bop:Bop) = {
    bop match {
      case Add => "+"
      case Sub => "-"
      case Mul => "*"
      case Div => "/"
      case Eq => "="
      case Ne => "!="
      case Lt => "<"
      case Gt => ">"
      case Leq => "<="
      case Geq => ">="
      case And => "/\\"
      case Or => "\\/"
    }
  }
  
  def toString(uop:Uop) = {
    uop match {
      case Not => "!"
      case Neg => "-"
    }
  }
}