object MergeSort {
  def mergeSort(vector: Vector[Int]): Vector[Int] = {
    def merge(left: Vector[Int], right: Vector[Int]): Vector[Int] = (left, right) match {
      case (Vector(), _) => right
      case (_, Vector()) => left
      case (lHead +: lTail, rHead +: rTail) =>
        if (lHead <= rHead) lHead +: merge(lTail, right)
        else rHead +: merge(left, rTail)
    }

    def split(vec: Vector[Int]): (Vector[Int], Vector[Int]) = {
      val mid = vec.length / 2
      vec.splitAt(mid)
    }

    if (vector.length <= 1) vector
    else {
      val (left, right) = split(vector)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def main(args: Array[String]): Unit = {
    val unsorted = Vector(38, 27, 43, 3, 9, 82, 10)
    println(s"Исходный вектор: $unsorted")

    val sorted1 = mergeSort(unsorted)
    println(s"Отсортированный: $sorted1")

  }
}