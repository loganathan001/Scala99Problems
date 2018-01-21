import scala.annotation.tailrec

//// P01 (*) Find the last element of a list.
// My direct approach
def last[A](xs: List[A]) : A = {
  if(xs.length == 1) xs(0)
  else last(xs.tail)
}

last(List(1, 1, 2, 3, 5, 8))

//Predef
List(1, 1, 2, 3, 5, 8).last

//Suggested implementation
@tailrec
def lastRec[A] (xs: List[A]) : A = xs match {
  case h::Nil => h
  case _::tail => lastRec(tail)
  case _ => throw new NoSuchElementException
}

lastRec(List(1, 1, 2, 3, 5, 8))


////P02 (*) Find the last but one element of a list.
//My Approach
@tailrec
def penultimate[A] (xs: List[A]) : A = xs match {
  case x::y::Nil => x
  case _::tail => penultimate(tail)
  case _ => throw new NoSuchElementException
}

penultimate(List(1, 1, 2, 3, 5, 8))
//Builtin
List(1, 1, 2, 3, 5, 8).init.last
List(1, 1, 2, 3, 5, 8).takeRight(1+1).head

//Suggested
@tailrec
def penultimateRec[A] (xs: List[A]) : A = xs match {
  case x::_::Nil => x
  case _::tail => penultimateRec(tail)
  case _ => throw new NoSuchElementException
}

penultimateRec(List(1, 1, 2, 3, 5, 8))
// To find nth last element
//My Approach
def lastNthElememnt[A] (ls : List[A], n: Int) : A = {
  val lastN = ls.length - (n)
  ls(lastN)
}

lastNthElememnt(List(1, 1, 2, 3, 5, 8), 2)

//Builtin
def lastNthBuiltin[A](n: Int, ls: List[A]): A = {
  if (n <= 0) throw new IllegalArgumentException
  if (ls.length < n) throw new NoSuchElementException
  ls.takeRight(n).head
}

lastNthBuiltin(2, List(1, 1, 2, 3, 5, 8))

//My another approach
def lastNthRec[A](n: Int, ls: List[A]): A = {
  def internalRec[A](n: Int, ls: List[A]): A = {
    if (n < 0) throw new IllegalArgumentException
    if (ls.length < n) throw new NoSuchElementException
    n match {
      case 0 => ls.last
      case _ => internalRec(n - 1, ls.init)
    }
  }
  internalRec(n - 1, ls)
}

lastNthRec(2, List(1, 1, 2, 3, 5, 8))
lastNthRec(1, List(1, 1, 2, 3, 5, 8))

// Suggested non-builtin
def lastNthRecursive[A](n: Int, ls: List[A]): A = {
  def lastNthR(count: Int, resultList: List[A], curList: List[A]): A = curList match {
    case Nil if count > 0 => throw new NoSuchElementException
    case Nil => resultList.head
    case _ :: tail => lastNthR(count - 1, if (count > 0) resultList else resultList.tail, tail)
  }

  if (n <= 0) throw new IllegalArgumentException else lastNthR(n, ls, ls)
}

lastNthRecursive(2, List(1, 1, 2, 3, 5, 8))
lastNthRecursive(1, List(1, 1, 2, 3, 5, 8))



//// P03 (*) Find the Kth element of a list.
//Builtin
List(1, 1, 2, 3, 5, 8)(2)

// My Approach
