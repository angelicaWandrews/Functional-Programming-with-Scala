package h2.p1

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //  switches the 1st and 2nd elements from lst.
  //  If the list has fewer than two elements throw an exception using function call sys.error(“too short”).
  //  Uses pattern matching.
  def switchHead[A](lst: List[A]): List[A] = lst match {
    case Cons(h, Cons(x, t)) => Cons(x, Cons(h, t))
    case Cons(h, Nil) => sys.error("too short")
    case Nil => sys.error("too short")
  }

  //  Write in module p1 a polymorphic method (with type parameter A) called get that takes as argument a list lst and an integer n.
  //  The function returns the element from lst with index n, considering the first element is at index 0.
  //  If the index is invalid (<0 or >= size of lst) then get throws an exception using function call sys.error(“wrong  index”).
  //  Use pattern matching.
  def get[A](lst: List[A], n: Int): A = lst match {
    case Cons( _ , _ ) if (n < 0) => sys.error("wrong index")
    case Cons(h, Nil) if (n > 0) => sys.error("wrong index")
    case Nil =>  sys.error("wrong index")
    case Cons(h, _ ) if (n == 0) => h
    case Cons(h,t) => get(t,n-1)
  }
  //  Write in module p1 a polymorphic method (with type parameter A) called set that takes as argument a list lst, an integer n, and x of type A.
  //  The function returns a new list identical to lst except for the element from lst with index n that is replaced by x.
  //  The first list element has index 0.
  //  If the given index is invalid (n<0 or >= size of lst) then set throws an exception using function call sys.error(“wrong  index”).
  //  Use pattern matching.
  def set[A](lst:List[A],n:Int,x:A): List[A] =lst match {
    case Cons( _ , _ ) if (n < 0) => sys.error("wrong index")
    case Cons(h, Nil) if (n > 0) => sys.error("wrong index")
    case Nil =>  sys.error("wrong index")
    case Cons(h,t) if (n==0) => Cons(x,t)
    case Cons(h,t) if (n>0) => Cons(h,set(t,n-1,x))
  }
  //  Write in module p1 a polymorphic method (with type parameter A) called takeN that takes as argument a list lst: List[A] and an integer n.
  //  The function returns a new list with the first n elements from lst. If n is invalid (<0 or >= size of lst)
  //  then takeN throws an exception using function call sys.error(“wrong  index”). Use pattern matching. This function is quite similar to drop.
  def takeN[A](lst:List[A],n:Int):List[A]=lst match {
    case Cons( _ , _ ) if (n < 0) => sys.error("wrong index")
    case Cons(h, Nil) if (n >= 0) => sys.error("wrong index")
    case Nil =>  sys.error("wrong index")
    case Cons(h,t) if (n==0) => Cons(h,Nil)
    case Cons(h,t) => Cons(h, takeN(t,n-1))
  }

  //  mapsq returns a new list where each element x from the original list lst is replaced by expression f(f(x)). Use the map function.
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))

  def mapsq[A](lst: List[A])(f: A => A) : List[A] =
    map(map(lst)(f:A => A))(f:A=>A)
}


object p1{
  def main(args: Array[String]): Unit= {
    val lst = List('b','a')
    val lst2 = List()
    println(List.switchHead(lst)) //Cons(a,Cons(b,Nil))
//    println(List.switchHead(lst2)) // raises java.lang.RuntimeException: too short

    val lst3 = List('a','b','c','d','f','w')
    println(List.get(lst3, 4)) // prints f
//    println(List.get(lst3, 8)) // raises java.lang.RuntimeException: wrong index

    val lst4 = List('a','b','c','e','e','f')
    println(List.set(lst4, 3, 'd'))  // prints Cons(a,Cons(b,Cons(c,Cons(d,Cons(e,Cons(f,Nil))))))
//    println(List.set(lst4,-1,'p')) // raises java.lang.RuntimeException: wrong index

    val lst5 = List(0.25, 0.75, 1.25, 1.75, 2.9)
    println(List.takeN(lst5, 3))   // prints Cons(0.25,Cons(0.75,Cons(1.25,Cons(1.75,Nil))))

    //  Write in main an expression with mapsq that obtains a list of elements x * 4 for each element x of List(1,2,3,4), i.e. the same list as List(4,8,12,16).
    //  Use the _ notation for the  lambda expression.
    println(List.mapsq(List(1,2,3,4))(_*2))
    //    Write in main an expression with mapsq that obtains a list of elements x + 6 for each element x of List(1,2,3,4), i.e. the same list as List(7,8,9,10).
    //    Use the _ notation for the lambda expression.
    println(List.mapsq(List(1,2,3,4))(_+3))
  }
}