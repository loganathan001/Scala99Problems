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



//// P03 (*) Find the Kth element of a list. By convent
// on, the first element in the list is element 0.
//Builtin
List(1, 1, 2, 3, 5, 8)(2)

// My Approach
@tailrec
def nthElem[A](ls: List[A], n: Int) : A =  n match {
    case n if n < 0 || n >= ls.size => throw new IllegalArgumentException
    case 0 => ls.head
    case _ => nthElem(ls.tail, n-1)
}

nthElem(List(1, 1, 2, 3, 5, 8), 2)
nthElem(List(1, 1, 2, 3, 5, 8), 3)
nthElem(List(1, 1, 2, 3, 5, 8), 0)
nthElem(List(1, 1, 2, 3, 5, 8), 5)


//Suggested non-builtin
def nthRecursive[A](n: Int, ls: List[A]): A = (n, ls) match {
  case (0, h :: _) => h
  case (n, _ :: tail) => nthRecursive(n - 1, tail)
  case (_, Nil) => throw new NoSuchElementException
}

//// P04 (*) Find the number of elements of a list.
//Builtin
List(1, 1, 2, 3, 5, 8).length

// My Approach
def listSize[A](ls: List[A]) : Int = {
  @tailrec
  def internalListSize[A](n: Int, ls: List[A]) : Int = ls match {
    case Nil => n
    case _::tail => internalListSize(n + 1, tail)
  }
  internalListSize(0, ls)
}

listSize(List(1, 1, 2, 3, 5, 8))
listSize(List(1))
listSize(List())

// Suggested
// Simple recursive solution.
def lengthRecursive[A](ls: List[A]): Int = ls match {
  case Nil => 0
  case _ :: tail => 1 + lengthRecursive(tail)
}

// Tail recursive solution. Theoretically more efficient; with tail-call
// elimination in the compiler, this would run in constant space.
// Unfortunately, the JVM doesn't do tail-call elimination in the general
// case. Scala *will* do it if the method is either final or is a local
// function. In this case, `lengthR` is a local function, so it should
// be properly optimized.
// For more information, see
// http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html
def lengthTailRecursive[A](ls: List[A]): Int = {
  def lengthR(result: Int, curList: List[A]): Int = curList match {
    case Nil => result
    case _ :: tail => lengthR(result + 1, tail)
  }

  lengthR(0, ls)
}
// More pure functional solution, with folds.
def lengthFunctional[A](ls: List[A]): Int = ls.foldLeft(0) { (c, _) => c + 1 }

lengthFunctional(List(1, 1, 2, 3, 5, 8))

//// P05 (*) Reverse a list.
//Builtin
List(1, 1, 2, 3, 5, 8).reverse

// My Approach
def reverseList[A](ls: List[A]): List[A] = {
  @tailrec
  def reverseListR[A](result: List[A], ls: List[A]): List[A] = ls match {
    case Nil => result
    case _ =>  reverseListR(result:+ls.last, ls.init)
  }

  reverseListR(Nil, ls)
}

reverseList(List(1, 1, 2, 3, 5, 8))

// Suggested
// Simple recursive. O(n^2)
def reverseRecursive[A](ls: List[A]): List[A] = ls match {
  case Nil => Nil
  case h :: tail => reverseRecursive(tail) ::: List(h)
}

// Tail recursive.
def reverseTailRecursive[A](ls: List[A]): List[A] = {
  def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
    case Nil => result
    case h :: tail =>
      //println(s"h: $h, tail: $tail, result: $result, currentlist: $curList h::result: ${h::result}")
      reverseR(h :: result, tail)

  }

  reverseR(Nil, ls)
}
reverseTailRecursive(List(1, 1, 2, 3, 5, 8))


// Pure functional
def reverseFunctional[A](ls: List[A]): List[A] =
  ls.foldLeft(List[A]()) { (r, h) => h :: r }

reverseFunctional(List(1, 1, 2, 3, 5, 8))


//// P06(*) Find out whether a list is a palindrome.
//My approach
def isPalindrome[A](ls: List[A]) : Boolean = {
  ls.equals(ls.reverse)
}

isPalindrome(List(1, 1, 2, 3, 5, 8))
isPalindrome(List(1, 6, 2, 3, 2, 6, 1))

//Suggested
// In theory, we could be slightly more efficient than this. This approach
// traverses the list twice: once to reverse it, and once to check equality.
// Technically, we only need to check the first half of the list for equality
// with the first half of the reversed list. The code to do that more
// efficiently than this implementation is much more complicated, so we'll
// leave things with this clear and concise implementation.
def isPalindrome1[A](ls: List[A]): Boolean = ls == ls.reverse

//My approach 2
def isPalindrome2[A] (ls: List[A]) : Boolean = {
  if (ls.isEmpty || ls.length == 1) {
    true
  }

  val halfIndex = ls.length / 2 - 1
  val secondHalfStartIndex = if (ls.length % 2 == 0) halfIndex + 1 else halfIndex + 2;

  @tailrec
  def check(forwardIndex: Int, backwardIndex: Int): Boolean = {
    if (forwardIndex <= halfIndex && backwardIndex >= secondHalfStartIndex) {
      if (ls(forwardIndex) == ls(backwardIndex)) {
        check(forwardIndex + 1, backwardIndex - 1)
      } else {
        false
      }
    } else {
      true
    }
  }
  check(0, ls.length - 1)
}

isPalindrome2(List(1, 1, 2, 3, 5, 8))
isPalindrome2(List(1, 6, 2, 3, 2, 6, 1))
isPalindrome2(List(1, 6, 2, 2, 6, 1))
isPalindrome2(List(1, 2, 2, 1))
isPalindrome2(List(1, 1))
isPalindrome2(List(1))
isPalindrome2(List())




