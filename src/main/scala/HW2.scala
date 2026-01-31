object HW2 {
  def MySum(a: List[Int]): Int = {
    a.foldLeft(0)((acc, x) => acc + x)
  }

  def MyRange(n: Int, m: Int): List[Int] = (n, m) match {
    case (x, y) if x > y => Nil
    case (x, y) if x == y => List(x)
    case _ => n :: MyRange(n + 1, m)
  }

  def main(args: Array[String]): Unit = {

    println(MySum(List(1, 2, 3, 4))) // 10
    println(MySum(List(-2, 5, 0))) // 3

    println(MyRange(1, 5)) // List(1, 2, 3, 4, 5)
    println(MyRange(3, 3)) // List(3)
    println(MyRange(5, 1)) // List()
  }
}
