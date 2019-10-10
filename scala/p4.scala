package h2.p4
import fpinscala.errorhandling.Option._
import fpinscala.errorhandling.Either._
object p4{
  def testFunction2[A,B,C](testname: String, a: A, b: B, expected: C)(f: (A,B) => C):Either[String, String] = {
    if (f(a,b) == expected)
        Right(testname + "passed")
    else
        Left(testname+"failed")
    }
  // a bit later in the chapter we'll learn nicer syntax for
  // writing functions like this
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
//  takes as arguments x and y (two Option[Int] objects) and returns the sum of the values in x and y in a Some constructor or None,
//  if at least one of x and y is None. Use the map2 function to get credit.
  def sumO(a:Option[Int],b:Option[Int]):Option[Int]  = {
    if (a == None | b==None) None
    else map2(a,b)(_+_)
    }
//  using flatMap and map
  def sumO_1(a:Option[Int],b:Option[Int]):Option[Int] = {
    if (a==None | b==None) None
    else a.flatMap(aa=>b map(bb=>aa+bb))
  }
//  using a for comprehension
  def sumO_2(a:Option[Int], b:Option[Int]):Option[Int] = {
//    if(a==None|b==None) None
//    else
//      for {
//        aa <- a
//        bb <- b
//      } yield aa + bb
    for {
      aa <- a
      bb <- b
    } yield aa + bb
  }
//  converts an (A,B)=>C function into an (Option[A], Option[B]) => Option[C] function.
//  Don’t do pattern matching in this function. Instead, use the utility functions from the Option object or a for comprehension.
  def lift2[A,B,C](f:(A,B)=>C):(Option[A],Option[B])=>Option[C] = (a,b)=>map2(a,b)(f)
  def main(args: Array[String]): Unit={
    def sample_add(x: Int, y: Int): Int = x + y    // function we want to test
    // we set up a “test vector” with values 10, 20, 30, where 30 is the expected return value:
    val add_test_result = testFunction2("sample_add", 10, 20, 10 + 20)(sample_add)
    println(add_test_result)     // a Right value means the test passed: prints Right(sample_add passed)
    val a:Option[Int] = None
    val b:Option[Int] = Some(5)
    val c:Option[Int] = Some(6)
    val sumTest= testFunction2("sumO return 11 ",b,c, sumO(b,c))(sumO)
    println(sumTest)
    val sumTest1= testFunction2("sumO return None ",a,b, sumO(a,b))(sumO)
    println(sumTest1)
    val sumTest2= testFunction2("sumO_1 return 15 ",Some(7),Some(8), sumO_1(Some(7),Some(8)))(sumO_1)
    println(sumTest2)
    val sumTest3= testFunction2("sumO_2 return None Fail ",None,Some(9),sumO_2(Some(11),Some(9)))(sumO_2)
    println(sumTest3)
    val sumTest4= testFunction2("sumO_2 return None ",None,Some(9),sumO_2(None,Some(9)))(sumO_2)
    println(sumTest4)
    val add_lifted = lift2(sample_add)
    println(add_lifted(Some(10), Some(20)))  // prints Some(30)
  }
}

