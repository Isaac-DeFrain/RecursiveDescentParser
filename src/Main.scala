import scala.io.StdIn.readLine
import scala.util.matching.Regex

/*
Grammar without left recursion:
S  -: E$
E  -: T E2
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

// Two steps:
// 1. Create pattern AST
// 2. For each input string, check if its AST matches the pattern's AST

abstract class S {
  def matches(input: S): Boolean
}
// right.isDefined == true if alternative
// E2: Some => match left or right.get.left.left
// E2: None => no other alternatives
// E3: an alternative to potentially match
case class E(left: T, right: Option[E2]) extends S {
  def matches(input: S): Boolean = {
    if (right.isEmpty) {
      // No ALTERNATIVES -- must match left
      input match {
        case E(t,None) => left.matches(t)
        case t: T2 => left.matches(t)
        case t: T => left.matches(t)
        case F(a,None) => left.matches(a)
        case a: A => left.matches(a)
        case _ => false
      }
    } else {
      // ALTERNATIVES exist -- must match one of them
      input match {
        case E(t,None) => left.matches(t) || right.get.left.matches(t)
        case t: T2 => left.matches(t) || right.get.left.matches(t)
        case t: T => left.matches(t) || right.get.left.matches(t)
        case F(a,None) => left.matches(a) || right.get.left.matches(a)
        case a: A => left.matches(a) || right.get.left.matches(a)
        case _ => false
      }
    }
  }
}
// Alternative branch
case class E2(left: E3) extends S {
  def matches(input: S): Boolean = input match {
    case E(t,None) => left.matches(t)
    case t: T2 => left.matches(t)
    case t: T => left.matches(t)
    case F(a,None) => left.matches(a)
    case a: A => left.matches(a)
    case _ => false
  }
}
// nested within alternative
case class E3(left: T, right: Option[E2]) extends S {
  def matches(input: S): Boolean = {
    if (right.isEmpty) {
      // No ALTERNATIVES -- must match left
      input match {
        case E(t,None) => left.matches(t)
        case t: T2 => left.matches(t)
        case t: T => left.matches(t)
        case F(a,None) => left.matches(a)
        case a: A => left.matches(a)
        case _ => false
      }
    } else {
      // ALTERNATIVES exist -- only need to match one of them
      input match {
        case E(t,None) => left.matches(t) || right.get.left.matches(t)
        case t: T2 => left.matches(t) || right.get.left.matches(t)
        case t: T => left.matches(t) || right.get.left.matches(t)
        case F(a,None) => left.matches(a) || right.get.left.matches(a)
        case a: A => left.matches(a) || right.get.left.matches(a)
        case _ => false
      }
    }
  }
}
// several terms if right.isDefined
// may be part of a nested expression so we need to match simpler expressions
// T2: Some => must match iff left.right.isEmpty (i.e. the F is not optional)
//             otherwise matching is optional
// T2: None => no more characters to match
case class T(left: F, right: Option[T2]) extends S {
  def matches(input: S): Boolean = {
    val thisT2 = T2(left, right)
    if (right.isEmpty) { // only left
      input match {
        case t: T2 => left.matches(t)
        case t: T => left.matches(t)
        case F(a, None) => left.matches(a)
        case a: A => left.matches(a)
        case _ => false
      }
    } else { // several terms (may be optional or required)
      if (left.right.isEmpty) { // left is required
        if (right.get.required) { // something in right is required
          input match {
            case t@T2(_, Some(_)) => thisT2.traverseMatch(None, Some(t))
            case t@T(_, Some(_)) => thisT2.traverseMatch(None, Some(T2(t.left, t.right)))
            case _ => false
          }
        } else { // left is required, right terms are all optional
          input match {
            case t@T2(_, Some(_)) =>
              left.matches(t) || thisT2.traverseMatch(None, Some(t))
            case t@T(_, Some(_)) =>
              left.matches(t) || thisT2.traverseMatch(None, Some(T2(t.left, t.right)))
            case T2(f, None) => left.matches(f)
            case T(f, None) => left.matches(f)
            case F(a, None) => left.matches(a)
            case a: A => left.matches(a)
            case _ => false
          }
        }
      } else { // left is optional
        if (right.get.required) { // something in right is required
          input match {
            case t@T2(_, Some(_)) =>
              right.get.matches(t) || thisT2.traverseMatch(None, Some(t))
            case t@T(_, Some(_)) =>
              right.get.matches(t) || thisT2.traverseMatch(None, Some(T2(t.left, t.right)))
            case T2(f, None) => right.get.matches(f)
            case T(f, None) => right.get.matches(f)
            case F(a, None) => right.get.matches(a)
            case a: A => right.get.matches(a)
            case _ => false
          }
        } else { // left is optional, everything in right is optional
          input match {
            case t@T2(_, Some(_)) =>
              left.matches(t) || right.get.matches(t) || thisT2.traverseMatch(None, Some(t))
            case t@T(_, Some(_)) =>
              left.matches(t) || right.get.matches(t) || thisT2.traverseMatch(None, Some(T2(t.left, t.right)))
            case T2(f, None) => left.matches(f) || right.get.matches(f)
            case T(f, None) => left.matches(f) || right.get.matches(f)
            case F(a, None) => left.matches(a) || right.get.matches(a)
            case a: A => left.matches(a) || right.get.matches(a)
            case _ => false
          }
        }
      }
    }
  }
  def push(t2: T2): Option[T2] = this match {
    case T(f, None) => Some(T2(f, Some(t2)))
    case T(f, Some(t)) => Some(T2(f, Some(t.push(t2))))
  }
}
case class T2(left: F, right: Option[T2]) extends S {
  def matches(input: S): Boolean = {
    if (right.isEmpty) { // only left
      input match {
        case t: T2 => left.matches(t)
        case t: T => left.matches(t)
        case F(a, None) => left.matches(a)
        case a: A => left.matches(a)
        case _ => false
      }
    } else { // several terms (may be optional or required)
      if (left.right.isEmpty) { // left is required
        if (right.get.required) { // something in right is required
          input match {
            case t@T2(_, Some(_)) => this.traverseMatch(None, Some(t))
            case t@T(_, Some(_)) => this.traverseMatch(None, Some(T2(t.left, t.right)))
            case _ => false
          }
        } else { // left is required, right terms are all optional
          input match {
            case t@T2(_, Some(_)) =>
              left.matches(t) || this.traverseMatch(None, Some(t))
            case t@T(_, Some(_)) =>
              left.matches(t) || this.traverseMatch(None, Some(T2(t.left, t.right)))
            case T2(f, None) => left.matches(f)
            case T(f, None) => left.matches(f)
            case F(a, None) => left.matches(a)
            case a: A => left.matches(a)
            case _ => false
          }
        }
      } else { // left is optional
        if (right.get.required) { // something in right is required
          input match {
            case t@T2(_, Some(_)) =>
              right.get.matches(t) || this.traverseMatch(None, Some(t))
            case t@T(_, Some(_)) =>
              right.get.matches(t) || this.traverseMatch(None, Some(T2(t.left, t.right)))
            case T2(f, None) => right.get.matches(f)
            case T(f, None) => right.get.matches(f)
            case F(a, None) => right.get.matches(a)
            case a: A => right.get.matches(a)
            case _ => false
          }
        } else { // left is optional, everything in right is optional
          input match {
            case t@T2(_, Some(_)) =>
              left.matches(t) || right.get.matches(t) || this.traverseMatch(None, Some(t))
            case t@T(_, Some(_)) =>
              left.matches(t) || right.get.matches(t) || this.traverseMatch(None, Some(T2(t.left, t.right)))
            case T2(f, None) => left.matches(f) || right.get.matches(f)
            case T(f, None) => left.matches(f) || right.get.matches(f)
            case F(a, None) => left.matches(a) || right.get.matches(a)
            case a: A => left.matches(a) || right.get.matches(a)
            case _ => false
          }
        }
      }
    }
  }
  def required: Boolean = this match {
    case T2(f, Some(t2)) => f.right.isEmpty || t2.required
    case T2(f, None) => f.right.isEmpty
  }
  def push(t2: T2): T2 = this match {
    case T2(f, None) => T2(f, Some(t2))
    case T2(f, Some(t)) => T2(f, Some(t.push(t2)))
  }
  // *horrible* several character traversal
  def traverseMatch(prev: Option[T2], t2opt: Option[T2]): Boolean = {
    val newLeft = T2(left, None)
    if (prev.isEmpty) { // first pass
      t2opt match { // t2opt is Some
        case Some(T2(f1, Some(T2(f, None)))) =>
          newLeft.matches(T2(f1, None)) && right.get.matches(f)
        case Some(T2(f1, t)) =>
          val newPrev = Some(T2(f1, None))
          newLeft.matches(newPrev.get) && right.get.matches(t.get) ||
            this.traverseMatch(newPrev, t)
        case _ => false
      }
    } else {
      t2opt match { // next passes
        case Some(T2(f1, Some(T2(f, None)))) => // last possible check
          val current = prev.get.push(T2(f1, None))
          val t = T2(f, None)
          newLeft.matches(current) && right.get.matches(t)
        case Some(T2(f1, Some(T2(f, t2)))) => // might need more checks
          val current = Some(prev.get.push(T2(f1, None)))
          val t = Some(T2(f, t2))
          newLeft.matches(current.get) && right.get.matches(t.get) ||
            this.traverseMatch(current, t)
        case _ => false
      }
    }
  }
}
// right.isDefined == true if optional
// F2: None => left is required
// F2: Some => left is optional
case class F(left: A, right: Option[F2]) extends S {
  def matches(input: S): Boolean = {
    input match {
      case F(a, None) => left.matches(a)
      case t: T2 => left.matches(t)
      case t: T => left.matches(t)
      case a: A => left.matches(a)
      case _ => false
    }
  }
}
// OPTIONAL -- nothing ever checks this
case class F2(left: Option[F2]) extends S {
  def matches(input: S): Boolean = false
}
abstract class A extends S
case class C(left: String) extends A {
  def matches(input: S): Boolean = input match {
    case T2(f,None) => this.matches(f)
    case T(f,None) => this.matches(f)
    case F(a,None) => this.matches(a)
    case C(c) => left == "." || left == c
    case _ => false
  }
}
// Parentheses
case class A2(left: E) extends A {
  def matches(input: S): Boolean = input match {
    case t: T2 => left.matches(t)
    case t: T => left.matches(t)
    case f: F => left.matches(f)
    case a: A => left.matches(a)
    case _ => false
  }
}

// recursive top-down descent parsing class
class RecursiveDescentParser(input: String) {
  var index: Int = 0
  val charsRegex: Regex = "^[0-9a-zA-Z. ]".r

  // S -: E $
  def parseS(): S = parseE()

  // E -: T E2
  def parseE(): E = E(parseT(), parseE2())

  // E2 -: '|' E2
  def parseE2(): Option[E2] = {
    if (index < input.length && input(index) == '|') {
      index += 1
      Some(E2(parseE3()))
    } else None
  }

  // E3 -: T E2
  def parseE3(): E3 = E3(parseT(), parseE2())

  // T -: F T2
  def parseT(): T = T(parseF(), parseT2())

  // T2 -: F T2
  // T2 -: NIL
  def parseT2(): Option[T2] = {
    if (index < input.length &&
      (input(index) == '(' || input(index).isLetterOrDigit || input(index) == ' ' || input(index) == '.')) {
      Some(T2(parseF(), parseT2()))
    } else None
  }

  // F -: A F2
  def parseF(): F = {
    if (index < input.length && input(index) == '(') {
      // if A is char, then don't increment
      // if A = (E), then increment
      F(parseA(), parseF21())
    } else F(parseA(), parseF2())
  }

  // F2 -: '?' F2
  // F2 -: NIL
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

  // A -: '(' A2
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

  // A2 -: E ')'
  def parseA2(): A2 = A2(parseE())
}

object Main {
  def main(args: Array[String]): Unit = {
    println("Enter a pattern (if you dare)")
    println()
    val patternInput = readLine("pattern? ")
    val patternParser = new RecursiveDescentParser(patternInput)
    val pattern = patternParser.parseS()
    println(pattern)
    var stringInput = readLine("string? ")

    while (stringInput != "/") {
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
