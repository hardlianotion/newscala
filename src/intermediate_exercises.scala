/**
 * Created by IntelliJ IDEA.
 * User: etuka
 * Date: 30/01/2012
 * Time: 12:49
 * To change this template use File | Settings | File Templates.
 */

import scala.Either.{RightProjection, LeftProjection}
import sys._

trait PartialType[T[_, _], A] {
  type Apply[B] = T[A, B]
  type Flip[B] = T[B, A]
}

trait Fluffy[F[_]] {
  def furry[A, B](f: A => B, fa: F[A]): F[B]
}

object Fluffy {
  // Exercise 1
  // Relative Difficulty: 1
  def ListFluffy: Fluffy[List] = new Fluffy[List] {
    def furry[A, B](f: (A) => B, fa: List[A]): List[B] = fa map f
  }

  // Exercise 2
  // Relative Difficulty: 1
  def OptionFluffy: Fluffy[Option] = new Fluffy[Option] {
    def furry[A, B](f: (A) => B, fa: Option[A]): Option[B] = fa map f
  }

  // Exercise 3
  // Relative Difficulty: 1
  def StreamFluffy: Fluffy[Stream] = new Fluffy[Stream] {
    def furry[A, B](f: (A) => B, fa: Stream[A]): Stream[B] = fa map f
  }

  // Exercise 4
  // Relative Difficulty: 1
  def ArrayFluffy: Fluffy[Array] = new Fluffy[Array] {
    def furry[A, B](f: (A) => B, fa: Array[A]): Array[B] =  null
  }

  // Exercise 5
  // Relative Difficulty: 5
  def Function1Fluffy[X]: Fluffy[PartialType[Function1, X]#Apply] = new Fluffy[PartialType[Function1, X]#Apply] {
    def furry[A,B](f:(A) => B, fa: Function1[X,A]): Function1[X,B] = fa andThen f
  }


  // Exercise 6
  // Relative Difficulty: 6
  def EitherLeftFluffy[X]: Fluffy[PartialType[Either.LeftProjection, X]#Flip] = new Fluffy[PartialType[LeftProjection, X]#Flip] {
    def furry[A, B](f: (A) => B, fa: LeftProjection[A, X]): LeftProjection[B, X] = (fa map f).left
  }

  // Exercise 7
  // Relative Difficulty: 4
  def EitherRightFluffy[X]: Fluffy[PartialType[Either.RightProjection, X]#Apply] = new Fluffy[PartialType[RightProjection, X]#Apply] {
    def furry[A, B](f: (A) => B, fa: RightProjection[X,A]): RightProjection[X,B] = (fa map f).right
  }

}

trait Misty[M[_]] extends Fluffy[M] {
  def banana[A, B](f: A => M[B], ma: M[A]): M[B]

  def unicorn[A](a: A): M[A]

  // Exercise 8
  // Relative Difficulty: 3
  // (use banana and/or unicorn)
  def furry[A, B](f: A => B, ma: M[A]): M[B] = banana(f andThen unicorn, ma)
}

object Misty {
  // Exercise 9
  // Relative Difficulty: 2
  def ListMisty: Misty[List] = new Misty[List] {
    def banana[A, B](f: (A) => List[B], ma: List[A]): List[B] = null

    def unicorn[A](a: A): List[A] = List(a)
  }

  // Exercise 10
  // Relative Difficulty: 2
  def OptionMisty: Misty[Option] = error("todo")

  // Exercise 11
  // Relative Difficulty: 2
  def StreamMisty: Misty[Stream] = error("todo")

  // Exercise 12
  // Relative Difficulty: 2
  def ArrayMisty: Misty[Array] = error("todo")

  // Exercise 13
  // Relative Difficulty: 6
  def Function1Misty[X]: Misty[PartialType[Function1, X]#Apply] =
    error("todo")

  // Exercise 14
  // Relative Difficulty: 7
  def EitherLeftMisty[X]: Misty[PartialType[Either.LeftProjection, X]#Flip] =
    error("todo")

  // Exercise 15
  // Relative Difficulty: 5
  def EitherRightMisty[X]: Misty[PartialType[Either.RightProjection, X]#Apply] =
    error("todo")

  // Exercise 16
  // Relative Difficulty: 3
  def jellybean[M[_], A](ma: M[M[A]], m: Misty[M]): M[A] = error("todo")

  // Exercise 17
  // Relative Difficulty: 6
  def apple[M[_], A, B](ma: M[A], mf: M[A => B], m: Misty[M]): M[B] =
    error("todo")

  // Exercise 18
  // Relative Difficulty: 6
  def moppy[M[_], A, B](as: List[A], f: A => M[B], m: Misty[M]): M[List[B]] =
    error("todo")
}

object AdvancedFun {
  case class State[S, A](f: S => (S, A))

  // Exercise 19
  // Relative Difficulty: 9
  def StateFluffy[S]: Fluffy[PartialType[State, S]#Apply] = error("todo")

  // Exercise 20
  // Relative Difficulty: 10
  def StateMisty[S]: Misty[PartialType[State, S]#Apply] = error("todo")
}

object IntermediateExercises extends App{
  println("Start here.")
}