package hw3

import scala.collection.immutable.HashMap 
import hw3._


package object hw3 {
  type Env = HashMap[Var,Val]
  type Loc = Int
  
}

case class Mem(m: HashMap[Loc,Val], top: Loc) {
  def apply(location: Loc): Val = m(location)
  def exists(l: Loc): Boolean = 
    m.exists((a: (Loc, Val)) => a._1 == l)
  def add(value: Val) = Mem(m + (top -> value), top+1)
  def update(l: Loc, v: Val) = Mem(m + (l -> v), top)
}

sealed trait Val
case class IntVal(n: Int) extends Val
case class IntListVal(n: List[IntVal]) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, env: Env) extends Val
case class LocVal(l: Loc) extends Val


sealed trait Program
sealed trait Expr extends Program
case class ConstI(n: Int) extends Expr
case class ConstB(n: Boolean) extends Expr
case class ConstIL(n: List[IntVal]) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class Rem(l: Expr, r: Expr) extends Expr
case class Cons(l: Expr, r: Expr) extends Expr
case class GTExpr(l: Expr, r: Expr) extends Expr
case class GEQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class ValExpr(name: Var, value: Expr, body: Expr) extends Expr
case class VarExpr(name: Var, value: Expr, body: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class DefExpr(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr







object MiniScalaInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
  
  def existtEnv(env:HashMap[Var,Val], v: Var): Boolean = {
    env.exists((a: (Var, Val)) => a._1 == v)
  }
  
  def eval(env: Env, mem: Mem, expr: Expr): Result = {
    // println(env, mem, expr)
    expr match {
      case ConstI(n) => Result(IntVal(n), mem)

      case ConstB(n) => Result(BoolVal(n), mem)

      case ConstIL(n) => Result(IntListVal(n), mem)

      case Var(s) => if(existtEnv(env, Var(s))) env(Var(s)) match {
                      case LocVal(l) => if(mem.exists(l)) Result(mem(l), mem) else throw new Exception("empty")
                      case _ => Result(env(Var(s)), mem)
                    } else throw new UndefinedSemantics(s"No value is mapped to variable ${Var(s)} in env")

      case Add(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => Result(IntVal(x.n + y.n), my)
          case _ => throw new UndefinedSemantics(s"Type error ${vx} or ${vy} is not Integer")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Sub(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => Result(IntVal(x.n - y.n), my)
          case _ => throw new UndefinedSemantics(s"Type error ${vx} or ${vy} is not Integer")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Mul(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => Result(IntVal(x.n * y.n), my)
          case _ => throw new UndefinedSemantics(s"Type error ${vx} or ${vy} is not Integer")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Div(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => if(y.n != 0) Result(IntVal(x.n / y.n), my) else throw new UndefinedSemantics(s"Can't be divided by 0")
          case _ => throw new UndefinedSemantics(s"Type error ${vx} or ${vy} is not Integer")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Rem(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => if(y.n != 0) Result(IntVal(x.n % y.n), my) else throw new UndefinedSemantics(s"Can't be modulated by 0")
          case _ => throw new UndefinedSemantics(s"Type error ${vx} or ${vy} is not Integer")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Cons(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => Result(IntListVal(List(x) ++List(y)), my)
          case (x: IntVal, y: IntListVal) => Result(IntListVal(List(x) ++ y.n), my)
          case (x: IntListVal, y: IntVal) => Result(IntListVal(x.n ++ List(y)), my)
          case (x: IntListVal, y: IntListVal) => Result(IntListVal(x.n ++ y.n), my)
          case _ => throw new UndefinedSemantics(s"Type error ${vx} or ${vy} is not Integer or IntList")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case GTExpr(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => Result(BoolVal(x.n>y.n), my)
          case _ => throw new UndefinedSemantics(s"Type error ${vx} or ${vy} is not Integer")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case GEQExpr(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => Result(BoolVal(x.n>=y.n), my)
          case _ => throw new UndefinedSemantics(s"Type error ${vx} or ${vy} is not Integer")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Iszero(c: Expr) => eval(env, mem, c) match {
        case Result(v, m) => v match {
          case IntVal(n) => Result(BoolVal(n==0), m)
          case _ => throw new UndefinedSemantics(s"Type error ${v} is not Integer")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Ite(c: Expr, t: Expr, f: Expr) => eval(env, mem, c) match {
        case Result(v, m) => v match {
          case BoolVal(b) => if(b) eval(env, m, t) else eval(env, m, f)
          case _ => throw new UndefinedSemantics(s"Type error ${v} is not Boolean")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case ValExpr(name: Var, value: Expr, body: Expr) => eval(env, mem, value) match {
        case Result(v, m) => eval(env + (name -> v), m, body)
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case VarExpr(name: Var, value: Expr, body: Expr) => eval(env, mem, value) match {
        case Result(v, m) => eval(env + (name -> LocVal(m.top)), m.add(v), body)
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Proc(v: Var, expr: Expr) => Result(ProcVal(v, expr, env), mem)
      
      case DefExpr(fname: Var, aname: Var, fbody: Expr, ibody: Expr) => eval(env + (fname -> RecProcVal(fname, aname, fbody, env)), mem, ibody)
      
      case Asn(v: Var, e: Expr) => eval(env, mem, e) match {
        case Result(val1, mem1) => if(existtEnv(env, v)) env(v) match {
          case LocVal(l) => Result(val1, mem1.update(l, val1))
          case _ => throw new UndefinedSemantics(s"Type error ${env(v)} is not LocVal")
        } else throw new UndefinedSemantics(s"No value is mapped to variable ${v} in env")
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }
      
      case Paren(expr: Expr) => eval(env, mem, expr)
      
      case Block(f: Expr, s: Expr) => eval(env, mem, f) match {
        case Result(val1, mem1) => eval(env, mem1, s)
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }
      
      case PCall(ftn: Expr, arg: Expr) => eval(env, mem, ftn) match {
        case Result(val1, mem1) => val1 match {
          case ProcVal(v, expr2, env2) => eval(env, mem1, arg) match {
            case Result(val2, mem2) => eval(env2 + (v -> val2), mem2, expr2)
            case _ => throw new UndefinedSemantics(s"Can't evaluate Proc")
          }

          case RecProcVal(fv, av, body, env2) => eval(env, mem1, arg) match {
            case Result(val2, mem2) => eval(env2 + (fv -> val1) + (av -> val2), mem2, body)
            case _ => throw new UndefinedSemantics(s"Can't evaluate RecProc")
          }

          case _ => throw new UndefinedSemantics(s"Type error ${val1} is not ProcVal or RecProcVal")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }
    }
  }
  
  def apply(program: String): Val = {
    val parsed = MiniScalaParserDriver(program)
    eval(new Env(), Mem(new HashMap[Loc,Val],0), parsed).v
  }

}


object Hw3App extends App {
  
  println("Hello from Hw3!")

}