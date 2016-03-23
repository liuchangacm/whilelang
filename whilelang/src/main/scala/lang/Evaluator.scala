package lang

trait InStream {
  def read:Int
}

class StdinStream extends InStream {
  lazy val input = io.Source.stdin.getLines.map(_.toInt)

  override def read = {
    input.next()
  }
}

/**
  * Created by simpl_000 on 3/22/2016.
  */
object Evaluator {
  type Heap = Map[String, Int]

  var heap:Heap = Map()

  var is:InStream = new StdinStream()

  def initializeHeap(heap:Heap) = {
    this.heap = heap
  }

  def setInputStream(is:InStream) = this.is = is

  def evalOneStep(prog:Stat):Stat = {
    prog match {
      case Skip => Skip
      case Assign(v, e) => {
        heap += (v -> eval(e))
        Skip
      }
      case Seq(s1, s2) => {
        val s1p = evalOneStep(s1)
        if (s1p == Skip) s2
        else Seq(s1p, s2)
      }
      case If(cond, t, f) => if(eval(cond) != 0) t else f
      case While(cond, body) => if (eval(cond) != 0) Seq(body, prog) else Skip
      case In(v) => {
        heap += (v -> is.read)
        Skip
      }
      case Out(e) => {
        println(eval(e))
        Skip
      }
    }
  }

  def evalProg(prog:Stat):Unit = {
    if(prog != Skip) evalProg(evalOneStep(prog))
  }

  def eval(expr:Expr):Int = {
    expr match {
      case Var(x) => heap.getOrElse(x, throw new Exception(s"divided by zero"))
      case Const(n) => n
      case Op(e1, op, e2) => {
        val v1 = eval(e1)
        val v2 = eval(e2)
        op match {
          case Add => v1 + v2
          case Sub => v1 - v2
          case Mul => v1 * v2
          case Div => if(v2 != 0) v1 / v2 else throw new Exception(s"divided by zero")
          case Eq => if (v1 == v2) 1 else 0
          case Ne => if (v1 != v2) 1 else 0
          case Lt => if (v1 < v2) 1 else 0
          case Leq => if (v1 <= v2) 1 else 0
          case Gt => if (v1 > v2) 1 else 0
          case Geq => if (v1 >= v2) 1 else 0
          case And => if (v1 != 0 && v2 != 0) 1 else 0
          case Or => if (v1 != 0 || v2 != 0) 1 else 0
        }
      }
      case UnaryOp(op, e) => {
        val v = eval(e)
        op match {
          case Not => if (v == 0) 1 else 0
          case Neg => - v
        }
      }
    }
  }

  def throwExcept(msg:String) = {
    println(msg)
  }
}
