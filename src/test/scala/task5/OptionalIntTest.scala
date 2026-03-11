package task5

import org.junit.*
import org.junit.Assert.*
import Optionals.*
import Optionals.OptionalInt.*

class OptionalIntTest:
  @Test def emptyOptionalShouldBeEmpty(): Unit =
    val empty = Empty()
    assertTrue(isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    val nonEmpty = Just(0)
    assertFalse(isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    val nonEmpty = Just(0)
    assertEquals(0, orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    val empty = Empty()
    assertEquals(1, orElse(empty, 1))

  /** Task 5: do test for map **/
  @Test def testMapIntOnEmpty(): Unit =
    val function: Int => Int = _ + 1
    val empty = Empty()

    assertTrue(
      isEmpty(
        mapInt(empty)(function)
      )
    )

  @Test def testMapIntOnNonEmpty(): Unit =
    val startingValue: Int = 0
    val invalidValue: Int = -1
    val function: Int => Int = _ + 1
    val expectedValue: Int = function(startingValue)
    val nonEmpty = Just(startingValue)

    assertFalse(
      isEmpty(
        mapInt(nonEmpty)(function)
      )
    )
    assertEquals(
      expectedValue,
      orElse(
        mapInt(nonEmpty)(function),
        invalidValue
      )
    )

  @Test def testFilterOnNonEmptyPassingThePredicate(): Unit =
    val threshold: Int = 1
    val correctValue: Int = 2
    val invalidValue: Int = -1
    val predicate: Int => Boolean = _ > threshold
    val nonEmpty: Just = Just(correctValue)
    
    assertFalse(
      isEmpty(
        filter(nonEmpty)(predicate)
      )
    )
    
    assertEquals(
      correctValue,
      orElse(
        filter(nonEmpty)(predicate),
        invalidValue
      )
    )

  @Test def testFilterOnNonEmptyNonPassingThePredicate(): Unit =
    val threshold: Int = 2
    val wrongValue: Int = 1
    val predicate: Int => Boolean = _ > threshold
    val nonEmpty: Just = Just(wrongValue)

    assertTrue(
      isEmpty(
        filter(nonEmpty)(predicate)
      )
    )

  @Test def testFilterOnEmpty(): Unit =
    val threshold: Int = 2
    val predicate: Int => Boolean = _ > threshold
    val nonEmpty: Empty = Empty()

    assertTrue(
      isEmpty(
        filter(nonEmpty)(predicate)
      )
    )

