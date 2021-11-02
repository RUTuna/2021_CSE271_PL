package hw3

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.{TerminalNode}
import MiniScalaParser._

import scala.collection.immutable.HashMap 
import scala.collection.JavaConverters._

case class SyntaxError (msg: String = "", cause: Throwable = None.orNull) extends Exception("Syntax Error: " ++ msg, cause)

class MiniScalaVisitor extends MiniScalaParserBaseVisitor[Expr] {

  

  override def visitInteger(ctx: IntegerContext): ConstI = {
    val exprText = ctx.getText
    ConstI(exprText.toInt)
  }
  override def visitBool(ctx: BoolContext): ConstB = {
    val exprText = ctx.getText
    ConstB(exprText.toBoolean)
  }
  override def visitIntlist(ctx: IntlistContext): ConstIL = {
    def toIntVal(ctx: IntegerContext) : IntVal = {
      IntVal(ctx.getText.toInt)
    }
    def toIntListVal(ctx: IntlistContext): List[IntVal] = {
      ctx.getChildCount match {
        case 1 => toIntVal(ctx.integer) :: List[IntVal]()
        case 3 => toIntVal(ctx.integer) :: toIntListVal(ctx.intlist)
      }
    }
    ConstIL(toIntListVal(ctx))
  }
  override def visitCons(ctx: ConsContext): Cons = 
    Cons(visit(ctx.expr(0)), visit(ctx.expr(1)))
  override def visitId(ctx: IdContext): Var = {
    val exprText = ctx.getText
    // println(exprText)
    Var(exprText)
  }
  override def visitIszero(ctx: IszeroContext): Iszero = 
    Iszero(visit(ctx.expr()))
  override def visitIte(ctx: IteContext): Ite = 
    Ite(visit(ctx.expr(0)), visit(ctx.expr(1)), visit(ctx.expr(2)))
  
  override def visitProc(ctx: ProcContext): Proc = 
    Proc(visitId(ctx.id()), visitExpr(ctx.expr()))
  
  override def visitVal(ctx: ValContext): ValExpr = 
    ValExpr(visitId(ctx.id()), visit(ctx.expr(0)), visit(ctx.expr(1)))
  override def visitDef(ctx: DefContext): DefExpr = 
    DefExpr(visitId(ctx.id(0)),visitId(ctx.id(1)), visit(ctx.expr(0)), visit(ctx.expr(1)))
  override def visitVar(ctx: VarContext): VarExpr = 
    VarExpr(visitId(ctx.id()), visit(ctx.expr(0)), visit(ctx.expr(1)))


  override def visitExpr(ctx: ExprContext): Expr = {
    // println(ctx.getText)
    ctx.getChildCount match {
      case 1 => ctx.getChild(0).toString match{
        case "Nil" => ConstIL(List[IntVal]())
        case _ => visit(ctx.getChild(0))
      }
      case 2 => (ctx.getChild(0).toString,ctx.getChild(1).toString) match {
        case ("[","]") => ConstIL(List[IntVal]())
        case _ => PCall(visit(ctx.expr(0)),visit(ctx.expr(1)))
      }
      case 3 =>  (ctx.getChild(0).toString,ctx.getChild(2).toString) match {
        case ("(",")") => visit(ctx.expr(0))
        case ("{","}") => visit(ctx.expr(0))
        case ("[","]") => visit(ctx.getChild(1))
        case _ => {
            ctx.getChild(1).toString match {
            case "+" => Add(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case "-" => Sub(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case "*" => Mul(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case "/" => Div(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case "mod" => Rem(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case ">" => GTExpr(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case ">=" => GEQExpr(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case ":=" => Asn(visitId(ctx.id()), visit(ctx.getChild(2)))
            case ";" => Block(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case unknown => throw new SyntaxError(s"unknown token on binary op: ${unknown} @ ${ctx.getText()}")
          }
        }
      }
      case _ => throw new SyntaxError(ctx.getText())
    }
    /*
    ctx.getChild(0).toString match {
      case "iszero" => Iszero(visit(ctx.getChild(1)))
      case "if" => 
      case "val" => 
      //case "letrec" => 
      //case "proc" => Proc(visitId(ctx.id(0)), visitExpr(ctx.expr(0)))
      //case "(" => Paren(visit(ctx.expr(0)))
      case _ => 
        ctx.getChildCount match {
          /*
          case 1 => visit(ctx.getChild(0))
          
          case 3 => 
            ctx.getChild(1).toString match {
            case "+" => Add(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case "-" => Sub(visit(ctx.getChild(0)), visit(ctx.getChild(2)))
            case unknown => throw new Exception(s"unknown token: ${unknown}")
          }*/
          case _ => throw new SyntaxError("")
        }
    }
    */
  }

  override def visitProgram(ctx: ProgramContext): Expr = {
    visit(ctx.expr())
  }
} // MiniScalaVisitor

object MiniScalaParserDriver {
  

  def apply(program: String): Expr = {
    val charStream  = new ANTLRInputStream(program)
    val lexer       = new MiniScalaLexer(charStream)
    val tokens      = new CommonTokenStream(lexer)
  
    val parser      = new MiniScalaParser(tokens)
    
    val visitor     = new MiniScalaVisitor()
    val res         = visitor.visit(parser.program())

    val r = res.asInstanceOf[Expr]
    // println(r)
    r
  }
}
