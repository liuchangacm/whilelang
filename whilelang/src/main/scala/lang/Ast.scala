/**
 * Copyright Chang Liu (liuchang@cs.umd.edu)
 */
package lang

/**
 * @author Chang Liu (liuchang@cs.umd.edu)
 */
trait Ast {
  override def toString = Formatter.toString(0, this)
}
sealed trait Stat extends Ast 
sealed trait Expr extends Ast

case object Skip extends Stat 
case class Assign(v:String, e:Expr) extends Stat 
case class Seq(s1:Stat, s2:Stat) extends Stat 
case class If(cond:Expr, tbranch:Stat, fbranch:Stat) extends Stat 
case class While(cond:Expr, body:Stat) extends Stat
case class In(v:String) extends Stat
case class Out(e:Expr) extends Stat

case class Var(v:String) extends Expr 
case class Const(n:Int) extends Expr
case class Op(e1:Expr, op:Bop, e2:Expr) extends Expr 
case class UnaryOp(op:Uop, e:Expr) extends Expr 

sealed trait Bop {
  override def toString = Formatter.toString(this)
}

case object Add extends Bop
case object Sub extends Bop
case object Mul extends Bop
case object Div extends Bop

case object Eq extends Bop 
case object Ne extends Bop 
case object Lt extends Bop 
case object Gt extends Bop 
case object Leq extends Bop
case object Geq extends Bop

case object And extends Bop
case object Or extends Bop 

sealed trait Uop {
  override def toString = Formatter.toString(this)
}

case object Not extends Uop 
case object Neg extends Uop 
