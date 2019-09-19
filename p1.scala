object ob2{
  def crossProd(x:Array[Int], y:Array[Int]): Array[Int]={
    val a = (x(1)*y(2))-(x(2)*y(1))
    val b = (x(2)*y(0))-(x(0)*y(2))
    val c = (x(0)*y(1))-(x(1)*y(0))
    val z = Array(a,b,c)
    z
  }
  def main(args: Array[String]): Unit={
    val x = Array(1,2,3)
    val y = Array(4,5,6)
    val d = crossProd(x,y)
    println(d.mkString(" "))
    val e = Array((x(1)*y(2))-(x(2)*y(1)),(x(2)*y(0))-(x(0)*y(2)),(x(0)*y(1))-(x(1)*y(0)))
    println(e.mkString(" "))
    val g = Array((2*6)-(3*5),(3*4)-(1*6),(1*5)-(2*4))
    println(g.mkString(" "))
  }
}