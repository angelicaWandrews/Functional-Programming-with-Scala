import scala.math._
trait Shape{
  def area:Double
  def perimeter:Double
}
class Circle(x:Double) extends Shape{
  override def area: Double ={
    Math.PI*(x*x)
  }
  override def perimeter: Double={
    2*Math.PI*x
  }
}
class Rectangle(x: Double, y: Double) extends Shape{
  override def area:Double={
    x*y
  }
  override def perimeter: Double = {
    2*(x+y)
  }
}
object p5{
  def main(args:Array[String]): Unit={
    val circleShape = new Circle(5)
    val rectangleShape = new Rectangle(8,9)
    println(circleShape.area)
    println(circleShape.perimeter)
    println(rectangleShape.area)
    println(rectangleShape.perimeter)
    val express = new Shape{
      def area=15
      def perimeter=16
    }
    println(express.area)
    println(express.perimeter)
  }
}