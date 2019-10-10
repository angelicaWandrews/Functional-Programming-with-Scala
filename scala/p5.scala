package h2.p5

import fpinscala.errorhandling.Either._

//Consider the following Student class:
sealed case class Student(name: String, id: Int, grades: List[Double])
import scala.collection.immutable.List
object p5{
  //Write a function called mkStudent that parses those Strings parameters and produces a new Student class as a Right value if
  // input parsing is successful or a Left(exception) if name is an empty or string parsing failed for id or any of the strings in the grades list.
  // Use a for comprehension. The function’s signature is:
  def mkStudent(name: String, idstr: String, gradesstr: List[String]): Either[Exception, Student]={
    for{
      f <-Either.Try(name)
      g <-Either.Try(idstr.toInt)
      h<-Either.Try(gradesstr.map(_.toDouble))
    }yield Student(f,g,h)
  }
  def main(args: Array[String]): Unit={
    //    Write in p5.main different 2 examples with mkStudent that produce (and print) a Right value and a Left value.
    val studentA = mkStudent("Clarke", "7890",List("100.0","80.0","95.0","99.0"))
    println(studentA)
    //prints Right(Student(Clarke,7890,List(100.0, 80.0, 95.0, 99.0)))
    val studentB = mkStudent("Kent", "56W78", List("30.0","90.0","85.0","00.0"))
    println(studentB)
    //prints Left(java.lang.NumberFormatException: For input string: "56W78")

  }
}
