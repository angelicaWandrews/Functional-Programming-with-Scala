object p3{
  //tail recursive pure version
  def trueForAll(bools: List[Boolean]): Boolean ={
    @annotation.tailrec
    def iter(head: Boolean, tail:List[Boolean]):Boolean={
      if(!head) {
        false
      }
      else {
        if(tail==Nil){
          true
        }
        else{
          iter(tail.head, tail.tail)
        }
      }
    }
    iter(bools.head,bools.tail)
  }


  // imperative, impure version: don't do like this
  def trueForAll_imperative(bools: List[Boolean]) : Boolean = {
    var lst = bools
    while (lst != Nil) {
      if (! lst.head) {
        return false // CAUTION: non-functional, bad programming style to use return
      }
      else {
        lst = lst.tail
      }
    }
    true
  }

  //tail recursive polymorphic function
  def countWithProperty[T](xs: List[T], p: T => Boolean) : Int={
    @annotation.tailrec
    def go(n: Int, count: Int): Int={
      if(n >= xs.length){
        count
      }
      else if (p(xs(n))) {
        go(n+1,count+1)
      }
      else{
        go(n+1, count)
      }
    }
    go(0, 0)
  }

  def scalarProduct(x: Array[Double], y: Array[Double]) : Double={
    @annotation.tailrec
    def pass(n: Int, m: Double): Double ={
      if(x.length != y.length){
        m-1
      }
      else if (n >= x.length){
        m
      }
      else{
        pass(n+1, m+(x(n)*y(n)))
      }
    }
    pass(0,0)
  }

  def countCommonElements[T](xs: Array[T], ys: Array[T], p: (T, T) => Boolean): Int = {
    @annotation.tailrec
    def goin(n:Int, m:Int, count:Int): Int ={
      if (n>=xs.length) return count
      else if(m>=ys.length) goin(n+1,0,count)
      else if(p(xs(n),ys(m))) goin(n,m+1,count+1)
      else goin(n,m+1,count)
    }
    goin(0, 0, 0)
  }


  def main(args: Array[String]): Unit= {
    val refalse =  List(true,false,true,true,true,true)
    val retrue = List(true,true,true,true,true,true)

    //non FP to display result
    println(trueForAll(refalse))
    println(trueForAll_imperative(refalse))
    println(trueForAll(retrue))
    println(trueForAll_imperative(retrue))

    val luckyNumbers = List(2,7,9,8,3,10,99,55,20)
    val oddCount = countWithProperty(luckyNumbers, (x: Int) => x % 2 != 0)
    //oddCount == 5, (7,9,3,99,55)

    //non FP to display result
    println(oddCount)

    val xvec = Array(-1.5, -2.75, 3.5)
    val yvec = Array(4.0, 5, 6.0)
    val prod = scalarProduct(xvec, yvec) // prod == 1.25, (-6 - 13.75 + 21)

    //non FP to display result
    println(prod)
    val as = Array(1,2,4,8,23,26,28,33,95)
    val bs = Array(2,6,8,10,11,13,14)
    val divisibleBy = countCommonElements(as, bs, (x: Int, y: Int) => x % y == 0)
    //     divisibleBy == 9, for 2 divides into  2,4,8,26,28; 8 divides into 8;  11 divides into 33;
    //    13 divisible by 26; 14 divisible by 28 (as/bs, bs(n) is divides into as(n))
    //non FP to display result
    println(divisibleBy)
  }
}

