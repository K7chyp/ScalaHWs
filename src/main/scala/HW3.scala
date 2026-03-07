import play.api.libs.json.*

import scala.language.postfixOps

object HW3 {

  def fibonacci: LazyList[BigInt] = {
    def loop(a: BigInt, b: BigInt): LazyList[BigInt] = a #:: loop(b, a + b)

    loop(0, 1)
  }

  val first10FibDivBy3: Vector[BigInt] =
    fibonacci.filter(_ % 3 == 0).take(10).toVector

  case class User(id: Int, name: String)

  case class Order(id: Int, amount: Double)

  private val users = Map(1 -> User(1, "Alice"), 2 -> User(2, "Bob"))
  private val orders = Map(
    1 -> Order(101, 200.0),
    2 -> Order(102, 600.0)
  )

  def findUser(id: Int): Option[User] = users.get(id)

  def getActiveOrder(user: User): Option[Order] = orders.get(user.id)

  def calculateDiscount(order: Order): Either[String, Double] =
    if (order.amount >= 500) Right(order.amount * 0.1)
    else Left("Слишком маленькая сумма для скидки")

  def getUserDiscount(userId: Int): Either[String, Double] = {
    val result = for {
      user <- findUser(userId).toRight("Пользователь не найден")
      order <- getActiveOrder(user).toRight("Активный заказ не найден")
      discount <- calculateDiscount(order)
    } yield discount
    result
  }

  trait Validator[T] {
    def validate(value: T): Boolean
  }

  implicit val stringValidator: Validator[String] = _.nonEmpty
  implicit val intValidator: Validator[Int] = _ > 0

  def check[T](value: T)(implicit v: Validator[T]): Unit = {
    if (v.validate(value)) println("OK") else println("Error")
  }

  implicit class ValidatorOps[T](value: T) {
    def isValid(implicit v: Validator[T]): Boolean = v.validate(value)
  }

  case class Product(id: Long, name: String, price: Double, tags: List[String])

  implicit val productFormat: Format[Product] = Json.format[Product]

  def jsonTest(): Unit = {
    val jsonStr = """{"id": 1, "name": "Laptop", "price": 999.99, "tags": ["tech", "work"]}"""
    Json.parse(jsonStr).validate[Product] match {
      case JsSuccess(p, _) =>
        val updated = p.copy(price = p.price * 1.1)
        println("Original: " + jsonStr)
        println("Updated:  " + Json.stringify(Json.toJson(updated)))
      case JsError(e) =>
        println("Parse error: " + e)
    }
  }

  def main(args: Array[String]): Unit = {
    println("=== Задание 1 ===")
    println(first10FibDivBy3.mkString(", "))

    println("\n=== Задание 2 ===")
    println("User 2: " + getUserDiscount(2))
    println("User 1: " + getUserDiscount(1))

    println("\n=== Задание 3 ===")
    check("hello")
    check("")
    check(5)
    check(-3)
    println("'hello'.isValid = " + "hello".isValid)
    println("''.isValid = " + "".isValid)

    println("\n=== Задание 4 ===")
    jsonTest()
  }
}