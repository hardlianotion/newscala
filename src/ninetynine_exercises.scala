/**
 * Created by IntelliJ IDEA.
 * User: etuka
 * Date: 22/06/2012
 * Time: 13:18
 * To change this template use File | Settings | File Templates.
 */

//1. Find the last element of a list
def lastMember[A](ls: List[A]) = ls.last

//2. Find the last-but-one element of a list
def penultimate[A](ls: List[A]):A = {
  if (ls.length <2) throw new NoSuchElementException
  ls.init.last
}

//3. Find the k'th element of a list
def kth[A](ls: List[A], k:Int):A  = {
  if (ls.length < k) throw new NoSuchElementException
  k match {
    case 0 => ls.head
    case _ => kth(ls.tail, k - 1)
  }
}

//4 Find the number of elements in a list
def length[A](ls: List[A]) = ls.length

//5 Reverse a list
def reverse[A](ls: List[A]) = ls.reverse

//6 Find out is a list is a palindrome
def isPalindrome[A](ls: List[A]) = ls == ls.reverse

//7
def flatten(ls: List[_]): List[_] = {
  ls match {
    case Nil => List()
    case (a: List[_]) :: Nil => flatten(a)
    case a :: Nil => List(a)
    case (a: List[_]) :: (tail: List[_]) => flatten(a) ::: flatten(tail)
    case a :: tail => a :: flatten(tail)
  }
}

//8
def compress[A](ls: List[A]): List[A] = ls match {
  case Nil => List()
  case head :: Nil => List(head)
  case head :: tail if (head == tail.head) => compress(tail)
  case head :: tail => head::compress(tail)
}

//9 Pack consecutive duplicates of list elements into sublists: not pretty.
def pack[A](ls: List[A]): List[List[_]] = ls match {
  case Nil => List[List[A]]()
  case (head: List[A]) :: Nil => List(head)
  case head :: Nil => List(List(head))
  case (head: List[A]) :: tail if (head.last == tail.head) => pack((head ::: (tail.head :: Nil)) :: tail.tail)
  case (head: List[A]) :: tail => head :: pack(tail)
  case head :: tail if (head == tail.head) =>  pack((head :: (tail.head :: Nil)) :: tail.tail)
  case head :: tail => List(head) :: pack(tail)
}

//10 Run-length encoding of a list.
def encode[A](ls: List[A]): List[(Int,_)] = pack(ls).map(a => (a.length, a.head))

//11 Modified run-length encoding.
def encode2[A](ls: List[A]): List[_] = pack(ls).map(a => if(a.length == 1) a.head else (a.length, a.head))

//12 Decode a run-length encoded list.
def decode[A](ls: List[(Int, A)]): List[_] = ls.foldLeft(List[A]()) {(lst, a) => lst ::: (for(i <- 0 to a._1) yield a._2).toList}

//13 Run-length encoding of a list (direct solution).
def encode3[A](ls: List[A]): List[(Int, A)] = {
  def allocate (i: Int, ls: List[A]): List[(Int, A)] = {
    (i,ls) match {
      case (i, Nil) => Nil
      case (i, head::Nil) => List((i, head))
      case (i, head::tail) if (head == tail.head) => allocate(i+1, tail)
      case (i, head::tail) => (i, head) :: allocate(1,tail)
    }
  }
  allocate(1,ls)
}

//14 Duplicate the elements of a list.
def duplicate[A](ls: List[A]): List[A] = ls.foldLeft(List[A]())((l, x) => l ::: (x::x::Nil))

//15 Duplicate the elements of a list a given number of times.
def duplicateK[A](k: Int, ls: List[A]) = ls.flatMap(List.fill(k)(_))
