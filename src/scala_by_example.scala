/**
 * Created by IntelliJ IDEA.
 * User: etuka
 * Date: 30/01/2012
 * Time: 21:45
 * To change this template use File | Settings | File Templates.
 */
import sys._

 object ScalaByExample {
   def main(args: Array[String]) {
     println(fib(10))
     println(factorial(8))
     println(factorial2(8))
     println(sum(x => x)(0,10))
     println(product(x => x*x)(1,3))

     val mt = new EmptySet
     println(mt.size)
     var notmt = mt.add(3)
     notmt = notmt.add(4)
     notmt = notmt.add(10)
     notmt = notmt.add(40)
     println(notmt.size)
     val newmt = mt.intersect(notmt)
     println(newmt.size)
     val newnotmt = new NonEmptySet(17, new NonEmptySet(10, new EmptySet, new EmptySet), new NonEmptySet(19, new EmptySet, new EmptySet))
     println(newnotmt.size)
     val intnotmt = newnotmt.intersect(notmt)
     println(intnotmt.size)
     val unionnotmy = newnotmt.union(notmt)
     println(unionnotmy.size)
     
     println(notmt.remove(54).size)
     println(mt.remove(54).size)
     println(notmt.remove(10).size)
     println(notmt.remove(40).remove(4).size)

   }
   //chapter 4
   def factorial(n: Int): Int = {
     def factorialImpl(lvl: Int, multiplier: Int): Int = lvl match {
       case 0 => multiplier
       case _ => factorialImpl(lvl - 1, multiplier * lvl)
     }
     factorialImpl(n,1)
   }

   def fib(n : Int) : Int = {
     def fibImpl(n : Int, fib_last: Int,  fib_next_last: Int) : Int = n match {
       case 1 => fib_last
       case _ => fibImpl(n-1, fib_last + fib_next_last, fib_last)
     }
     fibImpl(n, 1, 1)
   }

   //chapter 5

   def sum(f: Int => Int)(a: Int, b: Int): Int = {
     def iter(a: Int, result: Int): Int = {
      if (a>b) result
      else iter(a+1, result + f(a))
     }
     iter(a, 0)
   }

   def product(f: Int => Int)(a: Int, b: Int): Int = {
     def iter(a: Int, result: Int): Int = {
       if (a>b) result
       else iter(a+1, result * f(a))
     }
     iter(a, 1)
   }
   
   def factorial2(n:Int):Int = {
     product(x => x)(1,n)
   }
}

//chapter 6
trait IntSet {
  def add(x: Int): IntSet
  def remove(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(set:IntSet): IntSet
  def intersect(set: IntSet): IntSet
  def size: Int
  def empty: Boolean
}

class EmptySet extends IntSet {
  def contains(x: Int): Boolean = false
  def add(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
  def remove(x:Int): IntSet = this
  def intersect(set:IntSet): IntSet = this
  def union(set: IntSet): IntSet = set
  def size: Int = 0
  def empty: Boolean = true
}

class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def add(x: Int): IntSet =
    if (x < elem) new NonEmptySet(elem, left add x, right)
    else if (x > elem) new NonEmptySet(elem, left, right add x)
    else this

  def remove(x:Int): IntSet = {
    if (elem == x) left.union(right)
    else if (elem > x) new NonEmptySet(elem, left.remove(x), right)
    else new NonEmptySet(elem, left, right.remove(x))
  }

  def intersect(set: IntSet): IntSet = {
    val lhs = left.intersect(set)
    val rhs = right.intersect(set)

    if(set.contains(elem))
      new NonEmptySet(elem,lhs, rhs)
    else
      lhs.union(rhs)
  }

  def union(set: IntSet): IntSet = {
    val lhs_and_set = left.union(set)
    val rhs_and_lhs_and_set = right.union(lhs_and_set)
    rhs_and_lhs_and_set.add(elem)
  }
  
  def size : Int = {
    left.size + right.size + 1
  }

  def empty : Boolean = false
}

abstract class Nat {
  def zero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

class Succ(x: Nat) extends Nat {
  def zero: Boolean = false
  def predecessor: Nat = x
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat = x + that.successor
  def - (that: Nat): Nat = {
    if (that.zero) this
    else x - that.predecessor
  }
}

//FIXME - needs work
class Integer(x: Nat) extends Nat {
  def zero: Boolean = false
  def positive: Boolean = true
  def predecessor: Nat = x
  def successor: Nat = new Succ(this)
  def negate: Integer = this
  def + (that: Nat): Nat = x + that.successor
  def - (that: Nat): Nat = {
    if (that.zero) this
    else x - that.predecessor
  }
}

object Zero extends Nat {
  def zero: Boolean = true
  def predecessor: Nat = error("negative number")
  def successor: Nat = new Succ(Zero)
  def + (that: Nat): Nat = that
  def - (that: Nat): Nat = {
    if (that.zero) Zero
    else error("negative number")
  }
}

abstract class IntTree {
  def contains(v: Int): Boolean
  def insert(v: Int): IntTree
}
//case object EmptyTree extends IntTree
//case class Node(elem: Int, left: IntTree, right: IntTree) extends IntTree