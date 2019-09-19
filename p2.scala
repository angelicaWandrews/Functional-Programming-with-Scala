object p1 {
  // format a multiline string with product UPC and name
  def addProduct(upc: Int, prodName: String, prevProds: String) : String = {
    val line = "UPC:%04d Product:%20s".format(upc, prodName)
    prevProds + "\n" + line
  }
  def main(args: Array[String]): Unit = {
    val prod0 = "Apple 2"
    val prod1 = "IBM PC"
    val productLines0 = addProduct(325, prod0, "")
    val productLines1 = addProduct(103, prod1, productLines0)
    // non FP code to display the result:
    println("Products: \n" + productLines1 + "\n")
    println("Products: \n " + " \n UPC:325 Product: Apple 2 \n UPC: 103 Product: IBM PC")
  }
}

import java.time.LocalDate // a date class in the local timezone
// University Registrar class.
class Registrar(val univName: String) {
  private var allStudents = List[Student]() // a MUTABLE instance variable
  // add student to the list of students registered
  // Return the number of students already registered
  def register(s: Student) : Int = {
    allStudents = allStudents :+ s
    // operator :+ appends something at the end of a list and returns the new list
    allStudents.size
  }
  def hasRegistered(s: Student) : Boolean = {
    @annotation.tailrec
    def go(lst: List[Student]) : Boolean = {
      if (lst == Nil)
        false
      else {
        if (lst.head.name == s.name)
          true
        else
          go(lst.tail)
      }
    }
    go(this.allStudents)
  }
}
// A student class.
class Student(val name:String) {
  // putting 'val' in front of parameter makes name an immutable instance variable
  val startedAt : LocalDate = LocalDate.now()
}
object p2 {
  def main(args: Array[String]): Unit = {
    val univ = new Registrar("FAU")
    val alice = new Student("Alice")
    val bob = new Student("Bob")
    val count1 = univ.register(alice)
    val count2 = univ.register(bob)
    val didBobRegister = univ.hasRegistered(bob)
    println("%d students registered".format(count2))
    println("Did Bob register? %b.\n\n".format(didBobRegister))
  }
}