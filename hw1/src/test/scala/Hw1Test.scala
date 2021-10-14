class Hw1Test extends org.scalatest.funsuite.AnyFunSuite {
  test("Hw1.gcd") {
    assert(Hw1.gcd(16273,12873) === 1)
    assert(Hw1.gcd(12873,16273) === 1)
    assert(Hw1.gcd(281333, 72267) === 221)
    assert(Hw1.gcd(172, 54) === 2)
  }

  test("Hw1.oddSum") {
    def constant(x: Int) = x
    def square(x: Int) = x * x
    assert(Hw1.oddSum(constant,10) === 25)
    assert(Hw1.oddSum(square,20) === 1330)
  }

  test("Hw1.foldRight") {
    assert(Hw1.foldRight(100, (x: Int, y:Int) => x % y, Cons(5,Cons(3,Nil))) === 1)
    assert(Hw1.foldRight(100, (x: Int, y:Int) => x % y, Cons(3,Cons(5,Nil))) === 0)
  }

  test("Hw1.map") {
    
    assert(Hw1.map((x: Int) => x % 3, Cons(5,Cons(3,Cons(6,Nil)))) === Cons(2,Cons(0,Cons(0,Nil))))
    assert(Hw1.map((x: Int) => x % 5, Cons(5,Cons(3,Cons(6,Nil)))) === Cons(0,Cons(3,Cons(1,Nil))))
  }

  test("Hw1.iter") {
    def constant(x: Int) = x
    def square(x: Int) = x * x
    assert(Hw1.iter[Int](constant, 10)(10) === 10)
    assert(Hw1.iter[Int](square, 3)(2) === 256)
  }

  test("Hw1.insert") {
    val step_0 = IntNode(10,IntNode(9,IntNode(8,Leaf,Leaf),Leaf),Leaf)
    val step_1 = Hw1.insert(step_0, 11)
    val step_2 = Hw1.insert(step_1, 12)
    assert(step_1 === IntNode(10,IntNode(9,IntNode(8,Leaf,Leaf),Leaf),IntNode(11,Leaf,Leaf)))
    assert(step_2 === IntNode(10,IntNode(9,IntNode(8,Leaf,Leaf),Leaf),IntNode(11,Leaf,IntNode(12,Leaf,Leaf))))
  }

  test("Hw1.eval") {
    assert(Hw1.eval(True) === true)
    assert(Hw1.eval(Andalso(Orelse(True,False),True)) === true)
  }


}
