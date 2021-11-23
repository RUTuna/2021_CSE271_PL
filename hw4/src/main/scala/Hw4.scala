package hw4

import scala.collection.immutable.HashMap 
import hw4._


package object hw4 {
  type Env = HashMap[Var,LocVal]
}

case class Mem(m: HashMap[LocVal,Val], top: Int) {
  def extended(v: Val): (Mem, LocVal) = {
    val new_mem = Mem(m.updated(LocVal(top),v), top+1)
    (new_mem,LocVal(top))
  }
  def updated(l: LocVal, new_val: Val): Option[Mem] = {
    m.get(l) match {
      case Some(v) => Some(Mem(m.updated(l, new_val), top))
      case None => None
    }
  }
  def get(l: LocVal): Option[Val] = m.get(l)
  def getLocs(): List[LocVal] = m.keySet.toList
}

sealed trait Val
case object SkipVal extends Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(args: List[Var], expr: Expr, env: Env) extends Val
case class LocVal(l: Int) extends Val
sealed trait RecordValLike extends Val
case object EmptyRecordVal extends RecordValLike
case class RecordVal(field: Var, loc: LocVal, next: RecordValLike) extends RecordValLike


sealed trait Program
sealed trait Expr extends Program
case object Skip extends Expr
case object False extends Expr
case object True extends Expr
case class NotExpr(expr: Expr) extends Expr
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr {
  override def toString = s"Var(${"\""}${s}${"\""})"
}
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class LTEExpr(l: Expr, r: Expr) extends Expr
case class EQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(i: Var, v: Expr, body: Expr) extends Expr
case class Proc(args: List[Var], expr: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class BeginEnd(expr: Expr) extends Expr
case class FieldAccess(record: Expr, field: Var) extends Expr
case class FieldAssign(record: Expr, field: Var, new_val: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCallV(ftn: Expr, arg: List[Expr]) extends Expr
case class PCallR(ftn: Expr, arg: List[Var]) extends Expr
case class WhileExpr(cond: Expr, body: Expr) extends Expr
sealed trait RecordLike extends Expr
case object EmptyRecordExpr extends RecordLike
case class RecordExpr(field: Var, initVal: Expr, next: RecordLike) extends RecordLike








object MiniCInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
    
  def existEnv(env:HashMap[Var,LocVal], v: Var): Boolean = {
    env.exists((a: (Var, LocVal)) => a._1 == v)
  }

  def existRec(rec: RecordValLike, field: Var): Option[RecordVal] = rec match {
    case EmptyRecordVal => None
    case (rec: RecordVal) => if (rec.field.s.equals(field.s)) Some(rec) else existRec(rec.next, field)
  }

  def evalPcallV(exprs: List[Expr], vars: List[Var], env: Env, mem: Mem, oriEnv: Env): (Env, Mem) = {
    (exprs, vars) match {
      case (hExpr :: tExpr, hVar :: tVar) => eval(oriEnv, mem, hExpr) match {
        case Result(v,m) => mem.extended(v) match {
          case (m2: Mem, l: LocVal) => evalPcallV(tExpr, tVar, env + (hVar -> l), m2, oriEnv)
          case _ => throw new UndefinedSemantics(s"Can't extend in memory")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }
      case (Nil, Nil) => (env, mem)
      case _ => throw new UndefinedSemantics(s"doesn't matched")
    }
  }

  def evalPcallR(refs: List[Var], vars: List[Var], env: Env, oriEnv: Env): Env = {
    (refs, vars) match {
      case (hRef :: tRef, hVar :: tVar) => if(existEnv(oriEnv, hRef)) oriEnv(hRef) match {
        case LocVal(l) => evalPcallR(tRef, tVar, env + (hVar -> LocVal(l)), oriEnv)
        case _ => throw new UndefinedSemantics(s"No value is mapped to variable ${hRef} in env")
      } else throw new UndefinedSemantics(s"No value is mapped to variable ${hRef} in env")
      case (Nil, Nil) => env
      case _ => throw new UndefinedSemantics(s"doesn't matched")
    }
  }

  def eval(env: Env, mem: Mem, expr: Expr): Result = {
    // println(env, " // ", mem, " // ", expr)
    expr match {
      case Skip => Result(SkipVal, mem)
      
      case False => Result(BoolVal(false), mem)
      
      case True => Result(BoolVal(true), mem)
      
      case NotExpr(expr: Expr) => eval(env, mem, expr) match {
        case Result(v, m) => v match {
          case BoolVal(b: Boolean) => Result(BoolVal(!b), mem)
          case _ => throw new UndefinedSemantics(s"Type error ${expr} is not Boolean")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }
      
      case Const(n: Int) => Result(IntVal(n), mem)
      
      case Var(s: String) => if(existEnv(env, Var(s))) env(Var(s)) match { 
        case (loc: LocVal) => mem.get(loc) match {
          case Some(v) => Result(v, mem)
          case None => throw new Exception(s"No value is mapped to variable ${loc} in mem")
        }
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

      case LTEExpr(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => Result(BoolVal(x.n<=y.n), my)
          case _ => throw new UndefinedSemantics(s"Type error ${vx} or ${vy} is not Integer")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case EQExpr(l: Expr, r: Expr) => (eval(env, mem, l), eval(env, eval(env, mem, l).m, r)) match {
        case (Result(vx, mx), Result(vy, my)) => (vx, vy) match {
          case (x: IntVal, y: IntVal) => Result(BoolVal(x.n==y.n), my)
          // case (x: RecordValLike, y: RecordValLike) => Result(BoolVal(x==y), my)
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

      case Let(i: Var, v: Expr, body: Expr) => eval(env, mem, v) match {
        case Result(v1, m1) => m1.extended(v1) match {
          case (m2: Mem, l: LocVal) => eval(env + (i -> l), m2, body)
          case _ => throw new UndefinedSemantics(s"Can't extend in memory")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Proc(args: List[Var], expr: Expr) => Result(ProcVal(args, expr, env), mem)

      case Asn(v: Var, e: Expr) => eval(env, mem, e) match {
        case Result(v1, m1) => if(existEnv(env, v)) env(v) match {
          case (loc: LocVal) => m1.updated(loc, v1) match {
            case Some(m2) =>  Result(v1, m2)
            case None => throw new UndefinedSemantics(s"empty")
          }
          case _ => throw new UndefinedSemantics(s"Type error ${env(v)} is not LocVal")
        } else throw new UndefinedSemantics(s"No value is mapped to variable ${v} in env")
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case BeginEnd(expr2: Expr) => eval(env, mem, expr2)

      case FieldAccess(record: Expr, field: Var) => eval(env, mem, record) match {
        case Result(v, m) => v match {
          case (rec: RecordValLike) => existRec(rec, field) match {
            case Some(rec) => m.get(rec.loc) match {
              case Some(v2) => Result(v2, m)
              case None => throw new Exception(s"No value is mapped to variable ${rec.loc} in mem")
            }
            case None => throw UndefinedSemantics(s"No value is mapped to variable ${field} in rec")
          }
          case _ => throw new UndefinedSemantics(s"Type error ${v} is not RecordVal")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case FieldAssign(record: Expr, field: Var, new_val: Expr) => eval(env, mem, record) match {
        case Result(v, m) => v match {
          case (rec: RecordValLike) => existRec(rec, field) match {
            case Some(rec) => eval(env, m, new_val) match {
              case Result(v2, m2) => m2.updated(rec.loc, v2) match {
                case Some(mem) => Result(v2, mem)
                case None => throw new Exception(s"Can't update memory")
              }
              case _ => throw new UndefinedSemantics(s"Can't evaluate")
            }
            case None => throw UndefinedSemantics(s"No value is mapped to variable ${field} in rec")
          }
          case _ => throw new UndefinedSemantics(s"Type error ${v} is not RecordVal")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case Block(f: Expr, s: Expr) => eval(env, mem, f) match {
        case Result(val1, mem1) => eval(env, mem1, s)
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case PCallV(ftn: Expr, arg: List[Expr]) => eval(env, mem, ftn) match {
        case Result(v, m) => v match {
          case ProcVal(args2, expr2, env2) => (arg, args2) match {
            case (arg: List[Expr], args2: List[Var]) => evalPcallV(arg, args2, env2, m, env) match {
              case (new_e: Env, new_m: Mem) => eval(new_e, new_m, expr2)
              case _ => throw new UndefinedSemantics(s"Can't add")
            }
            case _ => throw new UndefinedSemantics(s"it not list")
          }
          case _ => throw new UndefinedSemantics(s"Type error ${v} is not ProcVal")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case PCallR(ftn: Expr, arg: List[Var]) => eval(env, mem, ftn) match {
        case Result(v, m) => v match {
          case ProcVal(args2, expr2, env2) => (arg, args2) match {
            case (arg: List[Var], args2: List[Var]) => evalPcallR(arg, args2, env2, env) match {
              case new_e: Env => eval(new_e, m, expr2)
              case _ => throw new UndefinedSemantics(s"Can't add")
            }
            case _ => throw new UndefinedSemantics(s"it not list")
          }
          case _ => throw new UndefinedSemantics(s"Type error ${v} is not ProcVal")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case WhileExpr(cond: Expr, body: Expr) => eval(env, mem, cond) match {
        case Result(v, m) => v match {
          case BoolVal(b: Boolean) => b match {
            case true => eval(env, eval(env, m, body).m, WhileExpr(cond, body)) match {
              case Result(v2, m2) => Result(SkipVal, m2)
              case _ => throw new UndefinedSemantics(s"Can't evaluate")
            }
            case false => Result(SkipVal, m)
          }
          case _ => throw new UndefinedSemantics(s"Type error ${v} is not Boolean")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }

      case EmptyRecordExpr => Result(EmptyRecordVal, mem)
    
      case RecordExpr(field: Var, initVal: Expr, next: RecordLike) => eval(env, mem, initVal) match {
        case Result(v, m) => m.extended(v) match {
          case (m2: Mem, l: LocVal) => eval(env, m2, next) match {
            case Result(v3, m3) => v3 match {
              case EmptyRecordVal => Result(RecordVal(field, l, EmptyRecordVal), m3)
              case RecordVal(field2: Var, loc2: LocVal, next2: RecordValLike) => Result(RecordVal(field, l, RecordVal(field2, loc2, next2)), m3)
              case _ => throw new UndefinedSemantics(s"Type error ${v3} is not RecordVal")
            }
            case _ => throw new UndefinedSemantics(s"Can't evaluate")
          }
          case _ => throw new UndefinedSemantics(s"Can't extend in memory")
        }
        case _ => throw new UndefinedSemantics(s"Can't evaluate")
      }
      
      case _ => throw new UndefinedSemantics(s"Is not expr")
    }
  }

  def gc(env: Env, mem: Mem): Mem = {
    Mem(gcfor(env.toList, new HashMap[LocVal,Val], mem), mem.top)
  }

  def gcfor(envList: List[(Var, LocVal)], map: HashMap[LocVal,Val], oriMem: Mem): HashMap[LocVal,Val] = {
    envList match {
      case (head :: tail) => head match {
        case (hVar: Var, hLoc: LocVal) => oriMem.get(hLoc) match {
          case Some(v) => {
            println("hloc: ", hLoc, "\n v: ", v)
            v match {
            case (loc2: LocVal) => oriMem.get(loc2) match {
              case Some(v2) => gcfor(tail, gcRec(v2, map, oriMem) + (hLoc -> v) + (loc2 -> v2), oriMem)
              case None => gcfor(tail, map, oriMem)
            }

            case ProcVal(args: List[Var], expr: Expr, env: Env) => gcfor(tail:::env.toList, map + (hLoc -> v), oriMem)

            case RecordVal(field: Var, loc2: LocVal, next: RecordValLike) => oriMem.get(loc2) match {
              case Some(v2) => gcfor(tail, gcRec(v2, map, oriMem) + (hLoc -> v) + (loc2 -> v2), oriMem)
              case None => gcfor(tail, map, oriMem)
            }

            case _ => gcfor(tail, map + (hLoc -> v), oriMem)
          }}
          case None => gcfor(tail, map, oriMem)
        }
        case _ => map
      }
      case _ => map
    }
  }

  def gcRec(v: Val, map: HashMap[LocVal,Val], oriMem: Mem): HashMap[LocVal,Val] = v match {
    case (loc2: LocVal) => oriMem.get(loc2) match {
      case Some(v2) => gcRec(v2, map + (loc2 -> v2), oriMem)
      case None => map
    }

    case RecordVal(field: Var, loc2: LocVal, next: RecordValLike) => oriMem.get(loc2) match {
      case Some(v2) => gcRec(v2, map + (loc2 -> v2), oriMem)
      case None => map
    }
    case _ => map
  }
  
  def apply(program: String): (Val, Mem) = {
    val parsed = MiniCParserDriver(program)
    val res = eval(new Env(), Mem(new HashMap[LocVal,Val],0), parsed)
    (res.v, res.m)
  }

}


object Hw4App extends App {
  
  println("Hello from Hw4!")

}