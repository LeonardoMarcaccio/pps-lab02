package task5

// overall module
object Optionals:

  // type "public" definition, exposing structure
  enum OptionalInt:
    case Just(value: Int)
    case Empty()

  // operations (/algorithms)
  object OptionalInt:
    def isEmpty(optional: OptionalInt): Boolean =
      optional match
        case Empty() => true
        case _       => false

    def orElse(optional: OptionalInt, orElse: Int): Int =
      optional match
        case Just(value) => value
        case _       => orElse

    def mapInt(optional: OptionalInt)(mapFunction: Int => Int): OptionalInt =
      optional match
        case Just(value) => Just(mapFunction(value))
        case Empty() => Empty()

    def filter(optional: OptionalInt)(predicate: Int => Boolean): OptionalInt =
      optional match
        case Just(value) if predicate(value) => Just(value)
        case Just(_) => Empty()
        case Empty() => Empty()


@main def tryOptionals(): Unit =
  import Optionals.* // to work with Optionals (to see OptionalInt type)
  import OptionalInt.* // to directly access algorithms

  val s1: OptionalInt = Just(1)
  val s2: OptionalInt = Empty()

  println(s1) // Some(1)
  println(isEmpty(s1)) // false
  println(orElse(s1, 0)) // 1
  println(orElse(s2, 0)) // 0
