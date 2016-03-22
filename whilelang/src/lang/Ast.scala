/**
 * Copyright Chang Liu (liuchang@cs.umd.edu)
 */
package lang

/**
 * @author Chang Liu (liuchang@cs.umd.edu)
 */
trait Ast {
  def indent(n:Int) = {
    var sb = new StringBuffer
    for(_ <- 0 until n) sb.append("    ")
    sb.toString
  }
}
sealed trait Stat extends Ast {
  def toString(n:Int):String = toString
}
sealed trait Expr extends Ast

case object Skip extends Stat {
  override def toString = toString(0)
  override def toString(n:Int) = indent(n) + "skip;\n"
}
case class Assign(v:String, e:Expr) extends Stat {
  override def toString = toString(0)
  override def toString(n:Int) = indent(n) + s"$v = $e;\n"
}
case class Seq(s1:Stat, s2:Stat) extends Stat {
  override def toString = toString(0)
  override def toString(n:Int) = s1.toString(n) + s2.toString(n)
}
case class If(cond:Expr, tbranch:Stat, fbranch:Stat) extends Stat {
  override def toString = toString(0)
  override def toString(n:Int) = 
      indent(n) + s"if ($cond) {\n" +
      tbranch.toString(n+1) + indent(n) + "}" +
      (if (fbranch == Skip) "\n" else " else {\n" + fbranch.toString(n+1) + indent(n) + "}\n")
}
case class While(cond:Expr, body:Stat) extends Stat {
  override def toString = toString(0)
  override def toString(n:Int) = 
      indent(n) + s"while ($cond) {\n" +
      body.toString(n+1) + indent(n) + "}\n"
}
case class In(v:String) extends Stat {
  override def toString = toString(0)
  override def toString(n:Int) = indent(n) + s"in($v);\n"
}
case class Out(e:Expr) extends Stat {
  override def toString = toString(0)
  override def toString(n:Int) = indent(n) + s"print($e);\n"
}

case class Var(v:String) extends Expr {
  override def toString = v
}
case class Const(n:Int) extends Expr {
  override def toString = n.toString
}
case class Op(e1:Expr, op:Bop, e2:Expr) extends Expr {
  override def toString = s"$e1 $op $e2"
}
case class UnaryOp(op:Uop, e:Expr) extends Expr {
  override def toString = s"$op $e"
}

sealed trait Bop
case object Add extends Bop {
  override def toString = "+"
}
case object Sub extends Bop {
  override def toString = "-"
}
case object Mul extends Bop {
  override def toString = "*"
}
case object Div extends Bop {
  override def toString = "/"
}

case object Eq extends Bop {
  override def toString = "="
}
case object Ne extends Bop {
  override def toString = "!="
}
case object Lt extends Bop {
  override def toString = "<"
}
case object Gt extends Bop {
  override def toString = ">"
}
case object Leq extends Bop {
  override def toString = "<="
}
case object Geq extends Bop {
  override def toString = ">="
}

case object And extends Bop {
  override def toString = "/\\"
}
case object Or extends Bop {
  override def toString = "\\/"
}

sealed trait Uop

case object Not extends Uop {
  override def toString = "!"
}
case object Neg extends Uop {
  override def toString = "-"
}