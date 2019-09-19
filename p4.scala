object p4{
  def partial1[A,B,C](a:A,f:(A,B)=>C):B=>C = b=>f(a,b)
  def partial2[A,B,C,D](a:A, f:(A,B,C)=>D): (B,C) => D = (b,c) => f(a,b,c)
  def formatStudentInfo(isGraduate: Boolean, year: Int, name: String) = {
    val grad = if (isGraduate) "graduate" else "undergraduate"
    "%s student %s enrolled in year %d".format(grad, name, year)
  }
  def htmlTag(elem: String, inner: String) = "<%s>%s</%s>\n".format(elem, inner, elem)
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)
  def curry2[A,B,C,D](f:(A,B,C)=>D): A=>((B,C)=>D)=
    a=>(b,c)=>f(a,b,c)

  def main(args:Array[String]):Unit={
    val formatGraduateStd = partial2(true, formatStudentInfo)
    println(formatGraduateStd(2020,"Angelica"))
    val mkBold = (inner: String) => htmlTag("B", inner)
    val mkItalic = (inner:String) => htmlTag("I",inner)
    println(mkBold("hello"))
    println(mkItalic("hello"))
    val mkBoldItalic = mkItalic.compose(mkBold)
    println(mkBoldItalic("Hello"))
    val mkItalicBold =mkBold.andThen(mkItalic)
    println(mkItalicBold("Hello"))
    val mkStdInfo = curry2(formatStudentInfo):Boolean => (Int, String) => String
    val formatGradStdInfo = mkStdInfo(true) //good for graduate student info
    println(formatGradStdInfo(2020,"Harry Potter")) //displays: graduate student Harry Potter enrolled in year 2020
    val formatUndergradStdInfo =mkStdInfo(false)
    println(formatUndergradStdInfo(2020,"Ronald Weasley")) //displays: undergraduate student Ronald Weasley enrolled in year 2020
  }
}