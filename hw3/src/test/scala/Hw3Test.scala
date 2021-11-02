import hw3._
import hw3._
import scala.collection.immutable.HashMap
import scala.collection.JavaConverters._


class HweTest extends org.scalatest.funsuite.AnyFunSuite {
val fibo = """def fibo(x) = 
        if 0 >= x then {
          0
        } else {
          if iszero x-1 then {
            0 
          } 
          else {
            if iszero x-2 then {
              1
            } else {
              (fibo x-1) + (fibo (x-2))
            }
          }
        }"""
val miniScalaTestCases = List(
      ("1", IntVal(1))
      ,("true",BoolVal(true))
      ,("false",BoolVal(false))
      ,("[1, 2]",IntListVal(List[IntVal](IntVal(1),IntVal(2))))
      ,("[]",IntListVal(List[IntVal]()))
      ,("Nil",IntListVal(List[IntVal]()))
      ,("Cons(1, Cons(2, Cons(3, Nil)))",IntListVal(List[IntVal](IntVal(1),IntVal(2),IntVal(3))))
      ,("1 + 1",IntVal(2))
      ,("2 - 3",IntVal(-1))
      ,("2 * 3",IntVal(6))
      ,("15 / 5",IntVal(3))
      ,("0 / 5",IntVal(0))
      ,("15 mod 5",IntVal(0))
      ,("3 mod 5",IntVal(3))
      ,("0 mod 5",IntVal(0))
      ,("0 > 5",BoolVal(false))
      ,("10 > 5",BoolVal(true))
      ,("0 >= 5",BoolVal(false))
      ,("10 >= 5",BoolVal(true))
      ,("iszero 0",BoolVal(true))
      ,("iszero 1",BoolVal(false))
      ,("if iszero 0 then 1 else 2",IntVal(1))
      ,("{ val x = 1 ; x+1 }",IntVal(2))
      ,("{ var x = 1 ; x+1 }",IntVal(2))
      ,("(x) => x + 10",ProcVal(Var("x"),Add(Var("x"),ConstI(10)),new Env()))
      ,("((x) => x + 1) 1",IntVal(2))
      ,("{def f(x) = if iszero x then 0 else x + (f x-1) ; f 1}",IntVal(1))
      ,("((x) => x - 8) 5",IntVal(-3))
      ,("{val x = 1 ; {def f(y) = if iszero (x - y) then 0 else 1 ; { val x = 2 ; (f 2)}}}",IntVal(1))
      ,("{var x = 1; (x := 2; x)}", IntVal(2))
      ,("if 0 > 1 then 1 else 2",IntVal(2))
      ,("{val x = 1; if 0 > x then 1 else 2}",IntVal(2))
      ,("""{def x(y) = 
      y;
      (if 0 > (x 1) then 1 else 2)}""",IntVal(2))
      ,(s"{${fibo}; (fibo 0)}",IntVal(0))
      ,(s"{${fibo}; (fibo 10)}",IntVal(34))
    )

    val miniScalaErrorCases = List(
      "(iszero 1) + 1"
      ,"{def sq(x) = y := x * x; {var y = 3; (sq y)}}"
      ,"x + 1"
      ,"1 + true"
      ,"1 - x"
      ,"true - 1"
      ,"x * 1"
      ,"1 * true"
      ,"1 / x"
      ,"true / 1"
      ,"1 / 0"
      ,"1 mod x"
      ,"true mod 1"
      ,"1 mod 0"
      ,"x > 1"
      ,"true > 1"
      ,"x >= 1"
      ,"true >= 1"
      ,"if 0 then 0 else 1"
      ,"iszero true"
      ,"{val x = 1; (x := 2; x)}"
      ,"{var x = 1; (y := 2; x)}"
      ,"Cons(1, Cons(2, Cons(3, true)))"
    )

  test("Hw3.parser") {  
    miniScalaTestCases.foreach((a: (String, Val)) => {
      val result = MiniScalaInterpreter(a._1)
      // println(result)
      // println("")
      assert(result === a._2)
    } )
    miniScalaErrorCases.foreach((a: String) => {
      val thrown = intercept[Exception]{
        MiniScalaInterpreter(a)
      }
      // println(thrown)
      // println("")
      assert(thrown.getMessage contains "Undefined Semantics:")
    })
  }

}
