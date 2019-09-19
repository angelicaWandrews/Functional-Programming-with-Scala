import java.time.LocalDate // a date class in the local timezone
// University Registrar class.
class allStudents(allS:List[Student]){
  def length_register(s: Student) : Int = {
    allS.size
  }
  def hasRegister(s: Student) : Boolean = {
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
    go(this.allS)
  }
}
class Registrar(val univName: String) {
  private val allStudent = List[Student]() // a MUTABLE instance variable
  // add student to the list of students registered
  // Return the number of students already registered
  def register(s: Student) : Unit = {
    val temp =new allStudents(allStudent:+s)
  }
  def hasRegistered(s:Student): Boolean ={
    val temp =new allStudents(allStudent:+s)
    temp.hasRegister(s)
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

