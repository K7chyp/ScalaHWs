object TopNSort {
  def topNSort(vector: Vector[Int], n: Int): Vector[Int] = {
    vector
      .foldLeft(Vector.empty[Int]) { (acc, elem) =>
        if (acc.size < n) (acc :+ elem).sorted
        else if (elem < acc.last) (acc.init :+ elem).sorted
        else acc
      }
  }

  def main(args: Array[String]): Unit = {
    val testVector = Vector(5, 3, 8, 1, 2, 7, 3, 9, 4, 6, 1, 5)
    val n = 4

    println(s"Исходный вектор: $testVector")
    println(s"Ищем $n наименьших элементов")

    val result1 = topNSort(testVector, n)
    println(s"Отсортированный: $result1")

    println("\nГраничные случаи:")
    println(s"n больше размера вектора (n=20): ${topNSort(testVector, 20)}")
    println(s"Пустой вектор: ${topNSort(Vector.empty, 3)}")
    println(s"Все элементы одинаковые: ${topNSort(Vector.fill(10)(5), 3)}")
  }
}