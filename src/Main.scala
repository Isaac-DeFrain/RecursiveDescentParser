import scala.io.StdIn.readLine
import scala.util.matching.Regex

// Tree class for descent parsing:
/*
Grammar without left recursion:
S  -: E$
E  -: T E2     -- expression
E2 -: '|' E3   -- alternative
E2 -: NIL      -- alternative
E3 -: T E2
T  -: F T2
T2 -: F T2
T2 -: NIL
F  -: A F2     -- string or (expression)
F2 -: '?' F2   -- optional
F2 -: NIL      -- optional
A  -: C        -- string
A  -: '(' A2
A2 -: E ')'
*/

// no environment is required because we are not assigning values to variables

// Two steps:
// 1. Create pattern AST
// 2. For each input string, check if it matches the pattern AST

abstract class S {
  def matches(input: S): Boolean
}
// right.isDefined == true if alternative
case class E(left: T, right: Option[E2]) extends S {
  def matches(input: S): Boolean = {
    if (right.isEmpty) {
      // Not an ALTERNATIVE
      input match {
        case E(t,None) => left.matches(t)
        case _ => false
      }
    } else {
      // ALTERNATIVE
      input match {
        case E(t,Some(e2)) =>
          left.matches(t) || right.get.left.left.left.matches(t.left) ||
            right.get.matches(e2) || right.get.left.left.matches(t)
        case E(t,None) => left.matches(t) || right.get.left.left.left.matches(t.left)
        case _ => false
      }
    }
  }
}
// Alternative branch
case class E2(left: E3) extends S {
  def matches(input: S): Boolean = input match {
    case E2(e3)    => left.matches(e3)
    case E(t,None) => left.matches(t)
    case t: T      => left.matches(t)
    case _         => false
  }
}
// nested within alternative
case class E3(left: T, right: Option[E2]) extends S {
  def matches(input: S): Boolean = {
    if (right.isEmpty)
    input match {
      case E3(t,None) => left.matches(t)
      case t: T => left.matches(t)
      case _ => false
    } else {
      input match {
        case E3(t,None) => left.matches(t) || right.get.matches(t)
        case t: T => left.matches(t) || right.get.matches(t)
        case _ => false
      }
    }
  }
}
// several terms if right.isDefined
case class T(left: F, right: Option[T2]) extends S {
  def matches(input: S): Boolean = {
    if (right.isEmpty) {
      // only one term, more than one can match on left
      input match {
        case T(f,None) => left.matches(f)
        case t: T => left.left.matches(t)
        case _ => false
      }
    } else {
      // several terms
      if (left.right.isEmpty) {
        // left is not optional
        input match {
          // several chars, some may be optional
          case T(f,Some(t2)) => left.matches(f) && right.get.matches(t2)
          case _ => false
        }
      } else {
        if (right.get.right.isEmpty) {
          input match { //TODO
            // right is optional
            case x@T(f, Some(t2)) => (left.matches(f) && right.get.matches(t2)) || (!right.get.matches(t2) && left.matches(x))
            case T(f, None) => left.matches(f)
            case _ => false
          }
        } else {
          // left is optional, right is also optional
          if (right.get.left.right.isEmpty) {
            input match { //TODO
              // several chars, some may be optional though
              case x@T(f, Some(t2)) => (left.matches(f) && right.get.matches(t2)) || (!left.matches(f) && right.get.matches(x))
              case T(f, None) => right.get.left.matches(f)
              case _ => false
            }
          } else {
            // left is optional, right is not optional
            input match {
              // several chars, some may be optional though
              case x@T(f, Some(t2)) => (left.matches(f) && right.get.matches(t2)) || (!left.matches(f) && right.get.matches(x))
              case T(f, None) => right.get.left.matches(f)
              case _ => false
            }
          }
        }
      }
    }
  }
}
case class T2(left: F, right: Option[T2]) extends S {
  def matches(input: S): Boolean = {
    if (right.isEmpty) {
      input match {
        case T2(f,None) => left.matches(f)
        case _ => false
      }
    } else {
      input match {
        case T2(f,Some(t2)) => left.matches(f) && right.get.matches(t2)
        case T2(f,None) => left.matches(f) && right.get.left.matches(f) //TODO
        case _ => false
      }
    }
  }
}
// right.isDefined == true if optional
case class F(left: A, right: Option[F2]) extends S {
  def matches(input: S): Boolean = {
    if (right.isEmpty) {
      // left is not optional
      input match {
        case F(a,None) => left.matches(a)
        case _ => false
      }
    } else {
      // A is OPTIONAL => it doesn't need to be matched
      input match {
        case F(a,Some(f2)) => left.matches(a) && right.get.matches(f2)
        case F(a,None) => left.matches(a)
        case _ => false
      }
    }
  }
}
// optional
case class F2(left: Option[F2]) extends S {
  def matches(input: S): Boolean = input match {
    case F2(None)     => left.isEmpty
    case F2(Some(f2)) => left.get.matches(f2)
    case _            => true
  }
}
abstract class A extends S
case class C(left: String) extends A {
  def matches(input: S): Boolean = input match {
    case C(c) => left == c
    case _    => false
  }
}
// Parentheses
// E(T, Option[E2])
case class A2(left: E) extends A { //FIXME
  def matches(input: S): Boolean = input match {
//    case c: C  => {
//      if (left.right.isEmpty) {
//        left.left.left.left.matches(c)
//      } else {
//        left.left.left.left.matches(c) || left.right.get.left.left.left.left.matches(c)
//      }
//    }
    case A2(e) => left.matches(e)
    case t: T =>left.left.matches(t)
    case _ => false
  }
}

// recursive top-down descent parsing class
class RecursiveDescentParser(input: String) {
  var index: Int = 0
  val charsRegex: Regex = "^[0-9a-zA-Z. ]".r

  // Parsing
  // S - top level
  def parseS(): S = parseE()

  // E(T, Option[E2])
  def parseE(): E = E(parseT(), parseE2())

  // Alternation - |
  // E2(E3)
  def parseE2(): Option[E2] = {
    if (index < input.length && input(index) == '|') {
      index += 1
      Some(E2(parseE3()))
    } else None
  }

  // E3(T, E2)
  def parseE3(): E3 = E3(parseT(), parseE2())

  // T(F, Option[T2])
  def parseT(): T = T(parseF(), parseT2())

  // T2(F, Option[T2])
  def parseT2(): Option[T2] = {
    if (index < input.length &&
      (input(index) == '(' || input(index).isLetterOrDigit || input(index) == ' ' || input(index) == '.')) {
      Some(T2(parseF(), parseT2()))
    } else None
  }

  // F(A, Option[F2])
  def parseF(): F = {
    if (index < input.length && input(index) == '(') {
      // if A is char, then don't increment
      // if A = (E), then increment
      F(parseA(), parseF21())
    } else F(parseA(), parseF2())
  }

  // Optional - ?
  // F2(Option[F2])
  def parseF2(): Option[F2] = {
    if (index < input.length && input(index) == '?') {
      index += 1
      Some(F2(parseF2()))
    } else None
  }

  def parseF21(): Option[F2] = {
    index += 1
    parseF2()
  }

  // A -: ( A2
  // A -: C
  def parseA(): A = {
    if (input(index) == '(') {
      index += 1
      parseA2()
    } else {
      val currStr = input.substring(index)
      val chars = charsRegex.findAllIn(currStr)
      val char = chars.next()
      index += char.length()
      C(char)
    }
  }

  // A2(E)
  def parseA2(): A2 = A2(parseE())
}

object Main {
  def main(args: Array[String]): Unit = {
    println("Please enter a pattern.")
    println()
    val patternInput = readLine("pattern? ")
    val patternParser = new RecursiveDescentParser(patternInput)
    val pattern = patternParser.parseS()
    println(pattern)
    var stringInput = readLine("string? ")

    while (stringInput != "(end)" && stringInput != "(exit)") {
      if (stringInput.isEmpty) {
        println("no match")
        stringInput = readLine("string? ")
      } else {
        val stringParser = new RecursiveDescentParser(stringInput)
        val parsedStr = stringParser.parseS()
        println(parsedStr)
        val isMatch = pattern.matches(parsedStr)
        if (isMatch) {
          println("match")
        } else println("no match")
        stringInput = readLine("string? ")
      }
    }
  }
}
