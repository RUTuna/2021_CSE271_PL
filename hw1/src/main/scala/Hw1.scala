sealed trait IntList
case object Nil extends IntList
case class Cons(v: Int, t: IntList) extends IntList

sealed trait BTree
case object Leaf extends BTree
case class IntNode(v: Int, left: BTree, right: BTree)
extends BTree

sealed trait Formula
case object True extends Formula
case object False extends Formula
case class Not(f: Formula) extends Formula
case class Andalso(left: Formula, right: Formula) extends Formula
case class Orelse(left: Formula, right: Formula)  extends Formula
case class Implies(left: Formula, right: Formula) extends Formula

object Hw1 extends App {

  println("Hw1!")

  def gcd(a: Int, b: Int): Int = {
    if(b==0) a else gcd(b, a%b) // 최대공약수 출력, a<b 라도 실행 후에 a,b가 서로 변경 되기에 문제 없음
  }

  def oddSum(f: Int=>Int, n: Int): Int = {
    if(n==0) 0 else if(n%2==1) f(n)+oddSum(f, n-1) else oddSum(f, n-1) // 홀수면 f 결과 값 포함해서, 짝수면 다음
  }

  def foldRight(init: Int, ftn: (Int, Int)=>Int, list: IntList): Int = {
    list match {
      case Nil => init // 제일 마지막 tail 이랑 init 계산
      case Cons(h, t) => ftn(foldRight(init, ftn, t), h) // head에 가까울 수록 나중에 계산
    }
  }

  def map(f: Int=>Int, list: IntList): IntList = {
    list match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(f, t)) // head 하니씩 뽑아서 계산하고 다시 list 안으로
    }
  }

  def iter[A](f: A => A, n: Int): A => A = {
    if(n==1) f else f andThen iter(f,n-1) // 합성 함수
  }
  
  def insert(t: BTree, a: Int): BTree = {
    t match {
      case Leaf => IntNode(a, Leaf, Leaf) // Leaf면 값이 a 이고 좌우 하단 Node가 Leaf인 Node로 변경
      case IntNode(v, left, right) => if(v<a) IntNode(v, left, insert(right, a)) else if(a<v) IntNode(v, insert(left, a), right) else IntNode(v, left, right)
      // a와 v의 값 크기를 비교하여 left Node와 right Node 중 어디로 이동할 지 결정
    }
  }

  def eval(f: Formula): Boolean = {
    f match {
      case True => true
      case False => false // 기본 ture, false 상태에 따른 bool 값
      case Not(f) => !eval(f)
      case Andalso(left, right) => eval(left)&&eval(right)
      case Orelse(left, right) => eval(left)||eval(right)
      case Implies(left, right) => !eval(left)||eval(right)
    }
  }
}