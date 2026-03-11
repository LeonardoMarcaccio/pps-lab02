package it.unibo.pps.u02

object WarmUpCode extends App:

  def altMultiplication1: (Double, Double) => Double = (x, y) => x * y
  def altMultiplication2: Double => Double => Double = x => y => x * y
  val mulBy3Variant1 = altMultiplication1(3, _)
  val mulBy3Variant2 = altMultiplication2(3)(_)

  println(altMultiplication1(10, 2)) // 20
  println(altMultiplication2(10)(2)) // 20
  println(mulBy3Variant1)
  println(mulBy3Variant2)
  println(mulBy3Variant1(10)) // 30
  println(mulBy3Variant2(10)) // 30

  def division(x: Double, y: Double): Double = x / y
  def curriedDivision(x: Double)(y: Double): Double = x / y
  def divisionDoubleFloat(x: Double, y: Float): Double = x / y
  def curriedDivisionDoubleFloat(x: Double)(y: Float): Double = x / y

  println(division(4, 2)) // 2
  println(curriedDivision(4)(2)) // 2

  /** Corner Cases */
  /** Dividing by 0, should be impossible */
  println(division(4, 0)) // Returns Infinite
  println(curriedDivision(4)(0)) // Returns Infinite
  println(division(-4, 0)) // Returns -Infinite
  println(curriedDivision(-4)(0)) // Returns -Infinite
  /** Periodic Result */
  println(division(1, 3)) // Returns the first 16 decimal values of the number
  println(curriedDivision(1)(3)) // Returns the first 16 decimal values of the number
  /** Dividing 0 by 0, should be impossible */
  println(division(0, 0)) // Returns Nan
  println(curriedDivision(0)(0)) // Returns Nan
  /** Different Type Division */
  println(divisionDoubleFloat(4, 2)) // 2
  println(curriedDivisionDoubleFloat(4)(2)) // 2
  /** Different Type Division by 0*/
  println(divisionDoubleFloat(4, 0)) // Returns Infinite
  println(curriedDivisionDoubleFloat(4)(0)) // Returns Infinite
  println(divisionDoubleFloat(-4, 0)) // Returns -Infinite
  println(curriedDivisionDoubleFloat(-4)(0)) // Returns -Infinite
  /** Different Type Division with Periodic Result */
  println(divisionDoubleFloat(1, 3)) // Returns the first 16 decimal values of the number
  println(curriedDivisionDoubleFloat(1)(3)) // Returns the first 16 decimal values of the number
  /** Different Type Division 0 by 0*/
  println(divisionDoubleFloat(0, 0)) // NaN
  println(curriedDivisionDoubleFloat(0)(0)) // NaN