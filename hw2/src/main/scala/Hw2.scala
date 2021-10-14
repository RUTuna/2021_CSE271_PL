package Hw2

import fastparse._
import MultiLineWhitespace._
import scala.collection.immutable.HashMap 

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, expr: Expr, env: Env) extends Val

case class Env(hashmap: HashMap[Var,Val]) {
  def apply(variable: Var): Val = hashmap(variable)
  def exists(v: Var): Boolean = 
    hashmap.exists((a: (Var, Val)) => a._1 == v)
  def add(v: Var, value: Val) = Env(hashmap + (v -> value))
}

sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Equal(l: Expr, r: Expr) extends Expr
case class Less(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(name: Var, value: Expr, body: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr
case class LetRec(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr

sealed trait IntExpr
case class IntConst(n: Int) extends IntExpr
case object IntVar extends IntExpr
case class IntAdd(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSub(l: IntExpr, r: IntExpr) extends IntExpr
case class IntMul(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSigma(f: IntExpr, t: IntExpr, b: IntExpr) extends IntExpr
case class IntPi(f: IntExpr, t: IntExpr, b: IntExpr) extends IntExpr
case class IntPow(b: IntExpr, e: IntExpr) extends IntExpr



package object Hw2 {

  

}

object IntInterpreter {
  def evalInt(expr: IntExpr, env: Option[Int]): Int = {
    expr match {
      case IntConst(n) =>  n
      case IntVar => env match {
                      case Some(n) => n
                      case None => throw new Exception("empty")
                    }
      case IntAdd(l, r) => evalInt(l, env) + evalInt(r, env)
      case IntSub(l, r) => evalInt(l, env) - evalInt(r, env)
      case IntMul(l, r) => evalInt(l, env) * evalInt(r, env)
      case IntSigma(f, t, b) => if(evalInt(f, env)>evalInt(t, env)) 0 else evalInt(IntSigma(IntAdd(f, IntConst(1)), t, b), env) + evalInt(b, Some(evalInt(f, env)))
      case IntPi(f, t, b) => if(evalInt(f, env)>evalInt(t, env)) 1 else evalInt(IntPi(IntAdd(f, IntConst(1)), t, b), env) * evalInt(b, Some(evalInt(f, env)))
      case IntPow(b, e) => if(evalInt(e, env)==0) 1 else if(evalInt(e, env)>0) evalInt(IntMul(IntPow(b, IntSub(e, IntConst(1))), b), env) else throw new Exception("negative integer")
    }
  }

  def apply(s: String): Int = {
    val parsed = IntParser(s)
    evalInt(parsed, None)
  }
}

object LetRecInterpreter {
  def eval(env: Env, expr: Expr): Val = {
    expr match {
      case Const(n) => IntVal(n)
      case Var(s) => if(env.exists(Var(s))) env(Var(s)) else throw new Exception("empty")
      case Add(l, r) => (eval(env, l), eval(env, r)) match {
                          case (x: IntVal, y: IntVal) => IntVal(x.n + y.n)
                          case _ => throw new Exception("Type Error")
                        }
      case Sub(l, r) => (eval(env, l), eval(env, r)) match {
                          case (x: IntVal, y: IntVal) => IntVal(x.n - y.n)
                          case _ => throw new Exception("Type Error")
                        }
      case Equal(l, r) => (eval(env, l), eval(env, r)) match {
                          case (x: IntVal, y: IntVal) => BoolVal(x.n == y.n)
                          case _ => throw new Exception("Type Error")
                        }
      case Less(l, r) => (eval(env, l), eval(env, r)) match {
                          case (x: IntVal, y: IntVal) => BoolVal(x.n < y.n)
                          case _ => throw new Exception("Type Error")
                        }
      case Iszero(c) => eval(env, c) match {
                          case (x: IntVal) => BoolVal(x.n == 0)
                          case _ => BoolVal(false)
                        }
      case Ite(c, t, f) => eval(env,c) match {
                          case v: BoolVal => if (v.b) eval(env,t) else eval(env,f)
                          case _ => throw new Exception("Type Error")
                        }
      case Let(name, value, body) => eval(env.add(name, eval(env, value)), body)
      case Paren(expr) => eval(env, expr)
      case Proc(v, expr) => ProcVal(v, expr, env)
      case PCall(ftn, arg) => eval(env, ftn) match {
        case ProcVal(v, expr, env) => eval(env.add(v, eval(env, arg)), expr)
        case RecProcVal(fv, av, body, expr, env2) => eval(env.add(fv, eval(env, ftn)), PCall(Proc(av, body), arg))
        case _ => throw new Exception("Type Error")
      }
      //                   => ftn match {
      //                           case Var(s) => if(env.exists(Var(s))) env(Var(s)) match {
      //                             case ProcVal(v, expr, env2) => eval(env, PCall(Proc(v, expr), arg))
      //                           } else throw new Exception("empty")
      //                           case Proc(v, expr) => eval(env.add(v, eval(env, arg)), expr)
      //                         }
      case LetRec(fname, aname, fbody, ibody) => RecProcVal(fname, aname, fbody, ibody, env)
      // eval(env.add(fname, ProcVal(aname, fbody, env)), ibody)
      // "letrec f(x) = if iszero x then 0 else x + (f x-1) in (f 1)" 이러한 형식으로 들어올 때를 위한.
    }
  }
  
  def apply(program: String): Val = {
    val parsed = LetRecParserDriver(program)
    eval(Env(new HashMap[Var,Val]()), parsed)
  }

}

object LetRecToString {
  def ws: String = " "

  def apply(expr: Expr): String = {
    expr match {
      case Const(n) => s"${n}"
      case Var(s) => s
      case Add(l, r) => s"${apply(l)} + ${apply(r)}"
      case Sub(l, r) => s"${apply(l)} - ${apply(r)}"
      case Equal(l, r) => s"${apply(l)} == ${apply(r)}"
      case Less(l, r) => s"${apply(l)} < ${apply(r)}"
      case Iszero(c) => s"iszero ${apply(c)}"
      case Ite(c, t, f) => s"if ${apply(c)} then ${apply(t)} else ${apply(f)}"
      case Let(name, value, body) =>  s"let ${apply(name)}${apply(value)} in ${apply(body)}"
      case Paren(expr) => s"(${apply(expr)})"
      case Proc(v, expr) => s"proc ${apply(v)} ${apply(expr)}"
      case PCall(ftn, arg) => s"${apply(ftn)} ${apply(arg)}"
      case LetRec(fname, aname, fbody, ibody) => s"letrec ${apply(fname)}${apply(Paren(aname))} = ${apply(fbody)} in ${apply(ibody)}"
      case _ => throw new Exception("empty")
    }
  }
}

object Hw2App extends App {
  
  println("Hello from Hw2!")

  val int_prog = """x + 1"""
  val parsed = IntParser(int_prog)
  println(parsed)


}
