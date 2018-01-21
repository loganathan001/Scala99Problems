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
