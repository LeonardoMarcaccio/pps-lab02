package functions

import it.unibo.pps.u02.Functions.*
import it.unibo.pps.u02.Functions.Expr.*
import it.unibo.pps.u02.Functions.ExprOps.*
import org.junit.Assert.*
import org.junit.Test

import scala.math.pow
import scala.quoted.Type.show

class FunctionTest:
  @Test
  def testPositiveWithPositiveNumber(): Unit =
    val x: Int = 1
    assertEquals("positive", positive(x))

  @Test
  def testPositiveWithNegativeNumber(): Unit =
    val x: Int = -1
    assertEquals("negative", positive(x))

  @Test
  def testPositiveZeroShouldBePositive(): Unit =
    val x: Int = 0
    assertEquals("positive", positive(x))

  @Test
  def testNeg(): Unit =
    val predicate: String => Boolean = string => string == ""
    val emptyString = ""
    val nonEmptyString = "foo"

    assertTrue(neg(predicate)(nonEmptyString))
    assertFalse(neg(predicate)(emptyString))

  @Test
  def testCurryingValid(): Unit =
    val x: Int = 1
    val y: Int = 2
    val z: Int = y

    assertTrue(curryingP1(x)(y)(z))
    assertTrue(curryingP2(x, y, z))
    assertTrue(curryingP3(x)(y)(z))
    assertTrue(curryingP4(x, y, z))

  @Test
  def testCurryingFalse(): Unit =
    val x: Int = 1
    val y: Int = 2
    val z: Int = y + 1

    assertFalse(curryingP1(x)(y)(z))
    assertFalse(curryingP2(x, y, z))
    assertFalse(curryingP3(x)(y)(z))
    assertFalse(curryingP4(x, y, z))

  @Test
  def testCompose(): Unit =
    val outer: Int => Int = input => input - 1
    val inner: Int => Int = input => input * 2
    val input: Int = 5
    val expected: Int = (input * 2) - 1

    assertEquals(expected, compose(outer, inner)(input))

  @Test
  def testRecursivePower(): Unit =
    val firstPow: (base: Double, exp: Int) = (2.0, 3)
    val firstExpectedPow: Double = pow(firstPow.base, firstPow.exp)
    val secondPow: (base: Double, exp: Int) = (5.0, 2)
    val secondExpectedPow: Double = pow(secondPow.base, secondPow.exp)
    val delta: Double = 0.01

    assertEquals(
      firstExpectedPow,
      power(firstPow.base, firstPow.exp),
      delta
    )
    assertEquals(
      secondExpectedPow,
      power(secondPow.base, secondPow.exp),
      delta
    )

  @Test
  def testReverseNumber(): Unit =
    val number: Int = 12345
    val reverse: Int = 54321

    assertEquals(
      reverse,
      reverseNumber(number)
    )

  @Test
  def testLiteral(): Unit =
    val expr = Expr.Literal(5)
    assertEquals(5, evaluate(expr))
    assertEquals("5", ExprOps.show(expr))

  @Test
  def testAddition(): Unit =
    val expr = Expr.Add(Expr.Literal(2), Expr.Literal(3))
    assertEquals(5, evaluate(expr))
    assertEquals("(2 + 3)", ExprOps.show(expr))

  @Test
  def testMultiplication(): Unit =
    val expr = Expr.Multiply(Expr.Literal(4), Expr.Literal(3))
    assertEquals(12, evaluate(expr))
    assertEquals("(4 * 3)", ExprOps.show(expr))

  @Test
  def testNestedExpression(): Unit =
    val expr = Expr.Add(
      Expr.Literal(2),
      Expr.Multiply(Expr.Literal(3), Expr.Literal(4))
    )
    assertEquals(14, evaluate(expr))
    assertEquals("(2 + (3 * 4))", ExprOps.show(expr))

  @Test
  def testDeeplyNestedExpression(): Unit =
    val expr = Expr.Multiply(
      Expr.Add(Expr.Literal(1), Expr.Literal(2)),
      Expr.Add(Expr.Literal(3), Expr.Literal(4))
    )
    assertEquals(21, evaluate(expr))
    assertEquals("((1 + 2) * (3 + 4))", ExprOps.show(expr))




