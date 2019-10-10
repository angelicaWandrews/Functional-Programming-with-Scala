package h2.p2
import h2.p2.List._
import java.lang.String

import scala.collection.immutable.List
object List{
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else as.head :: apply(as.tail: _*)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case x::xs => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case h::t => foldLeft(t, f(z,h))(f)
  }

  //  def map[A,B](l: List[A])(f: A => B): List[B] =
  //    foldRight(l, Nil:List[B])((h,t) => f(h)::t)

  //  def filter[A](as: List[A])(f: A => Boolean): List[A] =
  //    foldRight(as, List[A]())((h,t) => if (f(h)) h::t else t)
  //
  //  def filterNot[A](as: List[A])(f: A => Boolean): List[A] =
  //    foldRight(as, List[A]())((h,t) => if (!f(h)) h::t else t)

}
import scala.collection.immutable.List
object p2{
  //  called toStrings that takes a parameter lst of type List[A] and returns a List[String] where each element x of lst is replaced by x.toString. Use the map method.
  def toStrings[A](lst:List[A]):List[String] =
    lst.map(_.toString())
  //      map(lst)(_.toString())

  //  Write in module p2 a polymorphic method function called compute using foldLeft, that takes in the first parameter list a List[A],
  //  in the second parameter list a binary operator f of type (A, A) => A, and that applies f to all elements of this sequence, going left to right.
  //  So, compute(List(a, b, c, d))(f) == f( f( f( a, b), c) , d)
  def compute[A](lst:List[A])(f:(A, A)=> A):A = lst match {
    case h::x::t  => foldLeft(t, f(h,x))(f)
    case h::Nil => h
  }

  // splitSum has two parameter lists to assist with type inference.
  // It returns a tuple where the first element is the sum of all elements x from lst for which p(x) is true and the second
  // element is the sum of all elements x from lst where for which p(x) is not true, i.e. expression (∑x∈lst∧p(x) x,∑x∈lst∧!p(x) x)
  // Write in method p2.main an example on a List[Int] that prints the tuple with the sum of even numbers and the sum of odd numbers from that list.
  def splitSum(lst: List[Int])(p: Int => Boolean) : (Int, Int) =
    (foldLeft(lst.filter(p),0)(_+_), foldLeft(lst.filterNot(p),0)(_+_))

  //    returns a partition of list xs based on predicate p: the first element of the returned tuple is a list with elements x from xs where p(x) is true
  //    and the second element of the returned tuple is a list with elements x from xs where p(x) is false.
  //    The order of elements in these lists must be preserved. Use one of the fold methods
  def split[A](xs: List[A])(p: A => Boolean) : (List[A], List[A]) =
    ( foldRight(xs, List[A]())((h,t) => if (p(h)) h::t else t),  foldRight(xs, List[A]())((h,t) => if (!p(h)) h::t else t))

  //    using pattern matching that applies the binary operator f in a right-to-left order only to elements x of list xs for which p(x) is true.
  //    Write a second version called filterFoldRight_1 that does the same thing using the foldRight function.
  //    Write in method p2.main an example of using each of this function.
  def filterFoldRight[A, B](xs: List[A], z: B)(p: A => Boolean)(f: (A, B) => B) : B = xs match {
    case h::t if p(h)=> f(h, filterFoldRight(t, z)(p)(f))
    case h::t if !p(h)=> filterFoldRight(t, z)(p)(f)
    case Nil => z
  }
  def filterFoldRight_1[A, B](xs: List[A], z: B)(p: A => Boolean)(f: (A, B) => B) : B =
    foldRight(xs.filter(p), z)(f)

  def main(args: Array[String]): Unit={
    val lst = List(13,-1,0,2)
    println(List.foldRight(lst.filter(_%2!=0),0)(_+_)) //prints 12

    val numbers = List(-3.0,9.0,13.5,9.5,27.7)
    println(toStrings(numbers))   // prints List(-3.0, 9.0, 13.5, 9.5, 27.7), a list of String

    val numbers1 = List(64.0,4.0,2.0)
    println(compute(numbers1)(_/_))   // prints 8.0

    val lst2 = List(1,2,3,4,5,6,7,8)
    println(splitSum(lst2)(_%2==0)) //prints(20,16)

    val lst3 = List("Monday","Weekend","Month","Weekday","Year")
    println(split(lst3)(_.endsWith("day")))     // prints (List(Monday, Weekday),List(Weekend, Month, Year))

    val lst4 = List(13,12,11,1,2,3,4,5)
    println(filterFoldRight(lst4,0)(_%4==0)(_+_)) //prints 16

    println(filterFoldRight_1(lst4,0)(_%4==0)(_+_)) //prints 16

  }
}
