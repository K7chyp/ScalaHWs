object UniqueMergeSort {
  def uniqueMergeSort(vector: Vector[Int]): Vector[Int] = {
    def mergeUnique(left: Vector[Int], right: Vector[Int]): Vector[Int] = {
      def removeDuplicates(vec: Vector[Int]): Vector[Int] = {
        vec.foldLeft(Vector.empty[Int]) { (acc, elem) =>
          if (acc.isEmpty || acc.last != elem) acc :+ elem
          else acc
        }
      }

      def merge(left: Vector[Int], right: Vector[Int]): Vector[Int] = (left, right) match {
        case (Vector(), _) => right
        case (_, Vector()) => left
        case (lHead +: lTail, rHead +: rTail) =>
          if (lHead <= rHead) lHead +: merge(lTail, right)
          else rHead +: merge(left, rTail)
      }

      removeDuplicates(merge(left, right))
    }

    def split(vec: Vector[Int]): (Vector[Int], Vector[Int]) = {
      val mid = vec.length / 2
      vec.splitAt(mid)
    }

    if (vector.length <= 1) vector
    else {
      val (left, right) = split(vector)
      mergeUnique(uniqueMergeSort(left), uniqueMergeSort(right))
    }
  }

  def main(args: Array[String]): Unit = {
    val unsorted = Vector(38, 27, 43, 3, 9, 82, 10, 3, 9, 9, 27, 43, 38, 10)
    println(s"Исходный вектор: $unsorted")

    val sorted = uniqueMergeSort(unsorted)
    println(s"Отсортированный без дубликатов: $sorted")

    println("\nТестирование на различных случаях:")

    val test1 = Vector(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
    println(s"Все дубликаты: ${test1} -> ${uniqueMergeSort(test1)}")

    val test2 = Vector(5, 4, 3, 2, 1, 5, 4, 3, 2, 1)
    println(s"Перевернутый с дубликатами: ${test2} -> ${uniqueMergeSort(test2)}")

    val test3 = Vector(1)
    println(s"Один элемент: ${test3} -> ${uniqueMergeSort(test3)}")

    val test4 = Vector()
    println(s"Пустой вектор: ${test4} -> ${uniqueMergeSort(test4)}")

    val test5 = Vector(7, 7, 7, 7, 7)
    println(s"Все одинаковые: ${test5} -> ${uniqueMergeSort(test5)}")
  }
}