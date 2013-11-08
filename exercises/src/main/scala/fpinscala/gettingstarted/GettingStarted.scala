package fpinscala.gettingstarted

// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

  // A definition of factorial, using a local, tail recursive function
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  // Another implementation of `factorial`, this time with a `while` loop
  def factorial2(n: Int): Int = {
    var acc = 1
    var i = n
    while (i > 0) { acc *= i; i -= 1 }
    acc
  }

  // Exercise 1: Write a function to compute the nth fibonacci number

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(m1: Int, m2: Int, count: Int): Int =
      if (count == 1) m1 + m2
      else loop(m2, m1 + m2, count - 1)

    if (n == 1 || n == 2) n - 1
    else loop(0, 1, n - 2)
  }

  def testFib(): Unit = {
    assert(0 == fib(1), s"expected 0 got ${fib(1)}")
    assert(1 == fib(2), s"expected 1 got ${fib(2)}")
    assert(1 == fib(3), s"expected 1 got ${fib(3)}")
    assert(2 == fib(4), s"expected 2 got ${fib(4)}")
    assert(3 == fib(5), s"expected 3 got ${fib(5)}")
    assert(5 == fib(6), s"expected 5 got ${fib(6)}")
    assert(8 == fib(7), s"expected 8 got ${fib(7)}")
  }

  // This definition and `formatAbs` are very similar..
  private def formatFactorial(n: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(n, factorial(n))
  }

  // We can generalize `formatAbs` and `formatFactorial` to
  // accept a _function_ as a parameter
  def formatResult(name: String, n: Int, f: Int ⇒ Int) = {
    val msg = "The %s of %d is %d."
    msg.format(n, f(n))
  }
}

object FormatAbsAndFactorial {

  import MyModule._

  // Now we can use our general `formatResult` function
  // with both `abs` and `factorial`
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
  }
}

// Functions get passed around so often in FP that it's
// convenient to have syntax for constructing a function
// *without* having to give it a name
object AnonymousFunctions {
  import MyModule._

  // Some examples of anonymous functions:
  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment", 7, (x: Int) ⇒ x + 1))
    println(formatResult("increment2", 7, (x) ⇒ x + 1))
    println(formatResult("increment3", 7, x ⇒ x + 1))
    println(formatResult("increment4", 7, _ + 1))
    println(formatResult("increment5", 7, x ⇒ { val r = x + 1; r }))
  }
}

object MonomorphicBinarySearch {

  // First, a binary search implementation, specialized to `Double`,
  // another primitive type in Scala, representing 64-bit floating
  // point numbers
  // Ideally, we could generalize this to work for any `Array` type,
  // so long as we have some way of comparing elements of the `Array`
  def binarySearch(ds: Array[Double], key: Double): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val d = ds(mid2) // We index into an array using the same
        // syntax as function application
        if (d == key) mid2
        else if (d > key) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, ds.length - 1)
  }

}

object PolymorphicFunctions {

  // Here's a polymorphic version of `binarySearch`, parameterized on
  // a function for testing whether an `A` is greater than another `A`.
  def binarySearch[A](as: Array[A], key: A, gt: (A, A) ⇒ Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A, A) ⇒ Boolean): Boolean = {
    @annotation.tailrec
    def loop(idx: Int): Boolean =
      if (idx == as.size - 1) true
      else if (!gt(as(idx), as(idx + 1))) false
      else loop(idx + 1)

    if (as.isEmpty || as.size == 1) true
    else loop(0)
  }

  def testIsSorted(): Unit = {
    def lt(a: Int, b: Int) = a < b
    assert(true == isSorted(Array(), lt), s"should be sorted")
    assert(true == isSorted(Array(1), lt), s"should be sorted")
    assert(true == isSorted(Array(1, 3), lt), s"should be sorted")
    assert(true == isSorted(Array(1, 2, 3), lt), s"should be sorted")
    assert(true == isSorted(Array(1, 2, 3, 4), lt), s"should be sorted")
    assert(false == isSorted(Array(3, 2, 1), lt), s"should not be sorted: Array(3, 2, 1).")
    assert(false == isSorted(Array(1, 2, 1), lt), s"should not be sorted: Array(1, 2, 1).")
  }

  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:

  // Exercise 3: Implement `partial1`.

  def partial1[A, B, C](a: A, f: (A, B) ⇒ C): B ⇒ C = f(a, _)

  def add1 = partial1(1, (a: Int, b: Int) ⇒ a + b)

  // Exercise 4: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A, B, C](f: (A, B) ⇒ C): A ⇒ (B ⇒ C) =
    ???

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 5: Implement `uncurry`
  def uncurry[A, B, C](f: A ⇒ B ⇒ C): (A, B) ⇒ C =
    ???

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.

  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

  // Exercise 6: Implement `compose`

  def compose[A, B, C](f: B ⇒ C, g: A ⇒ B): A ⇒ C =
    ???
}
