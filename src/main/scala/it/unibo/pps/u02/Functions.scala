package it.unibo.pps.u02

object Functions:
  /** 3.a */
  def positive: Int => String = _ match
    case value if value >= 0 => "positive"
    case _ => "negative"

  /** 3.b */
  def neg: (predicate: String => Boolean) => String => Boolean =
    predicate => string => !predicate(string)

  /** 4 */
  val curryingP1: Int => Int => Int => Boolean =
    x => y => z => x <= y && y == z

  val curryingP2: (Int, Int, Int) => Boolean =
    (x, y, z) => x <= y && y == z

  def curryingP3(x: Int)(y: Int)(z: Int): Boolean =
    x <= y && y == z

  def curryingP4(x: Int, y: Int, z: Int): Boolean =
    x <= y && y == z

  /** 5 */
  def compose(outer: Int => Int, inner: Int => Int ): Int => Int =
    input => outer(inner(input))

  /** 6 */
  def power(base: Double, exponent: Int): Double = exponent match
    case 0 => 1
    case exponent if exponent % 2 == 0 =>
      val half = power(base, exponent / 2)
      half * half
    case exponent =>
      val half = power(base, exponent / 2)
      half * half * base

  /** 7 */
  def reverseNumber(number: Int): Int =
    def numberAccumulator(remaining: Int, reversed: Int): Int = remaining match
      case 0 => reversed
      case _ =>
        val digit = remaining % 10
        numberAccumulator(remaining / 10, reversed * 10 + digit)

    numberAccumulator(number, 0)

  /** 8 */
  enum Expr:
    case Literal(value: Int)
    case Add(left: Expr, right: Expr)
    case Multiply(left: Expr, right: Expr)

  object ExprOps:
  
    def evaluate(expr: Expr): Int = expr match
      case Expr.Literal(v) => v
      case Expr.Add(left, right) => evaluate(left) + evaluate(right)
      case Expr.Multiply(left, right) => evaluate(left) * evaluate(right)
  
    def show(expr: Expr): String = expr match
      case Expr.Literal(v) => v.toString
      case Expr.Add(left, right) => "(" + show(left) + " + " + show(right) + ")"
      case Expr.Multiply(left, right) => "(" + show(left) + " * " + show(right) + ")"