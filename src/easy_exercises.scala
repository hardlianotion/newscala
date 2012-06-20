/**
 * Created by IntelliJ IDEA.
 * User: etuka
 * Date: 01/01/2012
 * Time: 03:12
 * To change this template use File | Settings | File Templates.
 */

// You are not permitted to use these List methods:
// * length
// * map
// * filter
// * ::: (and variations such as ++)
// * flatten
// * flatMap
// * reverse (and variations i.e. reverseMap, reverse_:::)
// This also means you are not permitted to use for-comprehensions on Lists.
// You are permitted to use the functions you write yourself. For example, Exercise 2 may use Exercise 1 or Exercise 3.
// Using permitted existing methods where appropriate will attract marks for elegance.

// TOTAL marks:    /66

import sys._

object EasyExercises {

  def main(args: Array[String]) {
    val arg = List(1,2,3,4,5)
    println(add(4,5))
    println(sum(arg))
    println(length(arg))
    println(map(arg, (x: Int) => 2 * x))
    println(filter(arg, (x:Int) => x % 2 == 0))
    println(append(arg, arg))
    println(concat(List(arg, arg, arg)))
    println(concatMap(arg,(x:Int) => List(x * 2.5, -x)))
    println(maximum(arg))
    println(reverse(arg))
  }

  def succ(n: Int) = n + 1
  def pred(n: Int) = n - 1

  // Exercise 1
  // Relative Difficulty: 1
  // Correctness: 2.0 marks
  // Performance: 0.5 mark
  // Elegance: 0.5 marks
  // Total: 3
  def add(x: Int, y: Int): Int = y match {
    case 0 => x
    case _ => add(succ(x), pred(y))
  }

  // Exercise 2
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def sum(x: List[Int]): Int = {
    x.reduceLeft(add(_, _))
  }

  // Exercise 3
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def length[A](x: List[A]): Int = x match {
    case Nil => 0
    case head :: Nil => 1
    case head :: tail => 1 + length(tail)
  }

  // Exercise 4
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.0 mark
  // Elegance: 1.5 marks
  // Total: 7
  def map[A, B](x: List[A], f: A => B): List[B] = {
    x.foldRight(List[B]())((x, list)=>f(x) :: list)
  }

  // Exercise 5
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def filter[A](x: List[A], f: A => Boolean): List[A] = x match {
    case Nil => List()
    case head :: tail  if f(x.head) == true => head :: filter (tail,f)
    case head :: tail => filter (tail,f)
  }

  // Exercise 6
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def append[A](x: List[A], y: List[A]): List[A] = (x, y) match {
    case (rhs, Nil) => rhs
    case (rhs, head :: tail) => append(rhs :+ head, tail)
  }

  // Exercise 7
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def concat[A](x: List[List[A]]): List[A] = x match {
    case Nil => List()
    case head :: tail => append(head, concat(tail))
  }

  // Exercise 8
  // Relative Difficulty: 7
  // Correctness: 5.0 marks
  // Performance: 1.5 marks
  // Elegance: 1.5 mark
  // Total: 8
  def concatMap[A, B](x: List[A], f: A => List[B]): List[B] = x match {
    case Nil => List()
    case head :: tail => concat(List(f(head), concatMap (tail, f)))
  }

  // Exercise 9
  // Relative Difficulty: 8
  // Correctness: 3.5 marks
  // Performance: 3.0 marks
  // Elegance: 2.5 marks
  // Total: 9
  def maximum(x: List[Int]): Int = {
    x.reduceLeft((a, b) => if (a > b) {a} else {b})
  }

  // Exercise 10
  // Relative Difficulty: 10
  // Correctness: 5.0 marks
  // Performance: 2.5 marks
  // Elegance: 2.5 marks
  // Total: 10
  def reverse[A](x: List[A]): List[A] =  {
    x.foldLeft(List[A]())((list, x) => x :: list)
  }
}