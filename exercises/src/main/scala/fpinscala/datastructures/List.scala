package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil         ⇒ 0 // The sum of the empty list is 0.
    case Cons(x, xs) ⇒ x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          ⇒ 1.0
    case Cons(0.0, _) ⇒ 0.0
    case Cons(x, xs)  ⇒ x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          ⇒ x
    case Nil                                   ⇒ 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) ⇒ x + y
    case Cons(h, t)                            ⇒ h + sum(t)
    case _                                     ⇒ 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        ⇒ a2
      case Cons(h, t) ⇒ Cons(h, append(t, a2))
    }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) ⇒ B): B = // Utility functions
    l match {
      case Nil         ⇒ z
      case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0)((x, y) ⇒ x + y)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`, see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Cons(h, t) ⇒ t
    case _          ⇒ throw new UnsupportedOperationException("tail of empty list")
  }

  def setHead[A](l: List[A])(h: A): List[A] = l match {
    case Cons(_, t) ⇒ Cons(h, t)
    case Nil        ⇒ Nil
  }

  def testSetHead(): Unit = {
    assert(List(3, 2, 3) == setHead(List(1, 2, 3))(3))
    assert(List() == setHead(List())(3))
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(remainder: List[A], count: Int): List[A] =
      if (count == 0) remainder
      else loop(tail(remainder), count - 1)

    loop(l, n)
  }

  def testDrop(): Unit = {
    assert(List(1, 2, 3) == drop(List(1, 2, 3), 0), s"drop(0) should not modify the list")
    assert(List(2, 3) == drop(List(1, 2, 3), 1), s"drop(1) should drop 1")
    assert(List(3) == drop(List(1, 2, 3), 2), s"drop(2) should drop 2")
    assert(List() == drop(List(1, 2, 3), 3), s"drop(3) should drop all elements")
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A ⇒ Boolean): List[A] =
    l match {
      case Nil                 ⇒ Nil
      case Cons(h, _) if !f(h) ⇒ l
      case Cons(_, t)          ⇒ dropWhile(t)(f)
    }

  def testDropWhile(): Unit = {
    assert(List(1, 2, 3) == dropWhile(List(1, 2, 3))(_ < 1), s"dropWhile should not drop if the head does not match the given predicate")
    assert(List(2, 3) == dropWhile(List(1, 2, 3))(_ < 2), s"dropWhile should drop while predicate is fulfilled 1")
    assert(List(3) == dropWhile(List(1, 2, 3))(_ < 3), s"dropWhile should drop while predicate is fulfilled 2")
    assert(List() == dropWhile(List(1, 2, 3))(_ < 4), s"dropWhile should drop while predicate is fulfilled 3")
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) ⇒ Nil
    case Cons(h, t)   ⇒ Cons(h, init(t))
    case Nil          ⇒ throw new UnsupportedOperationException("init of empty list")
  }

  def testInit(): Unit = {
    assert(List(1, 2) == init(List(1, 2, 3)))
    assert(List(1) == init(List(1, 2)))
    assert(List() == init(List(1)))
    assert(try { init(List()) == ??? } catch { case ex: UnsupportedOperationException ⇒ true })
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) ⇒ acc + 1)

  def testLength(): Unit = {
    assert(0 == length(List()))
    assert(1 == length(List(1)))
    assert(2 == length(List(1, 2)))
    assert(3 == length(List(1, 2, 3)))
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) ⇒ B): B = l match {
    case Nil        ⇒ z
    case Cons(h, t) ⇒ foldLeft(t, f(z, h))(f)
  }

  def testFoldLeft(): Unit = {
    assert(foldRight(List(): List[Int], 0)((_, acc) ⇒ acc + 1) == foldLeft(List(): List[Int], 0)((acc, _) ⇒ acc + 1))
    assert(foldRight(List(1), 0)((_, acc) ⇒ acc + 1) == foldLeft(List(1), 0)((acc, _) ⇒ acc + 1))
    assert(foldRight(List(1), 0)((_, acc) ⇒ acc + 1) == foldLeft(List(1), 0)((acc, _) ⇒ acc + 1))
    assert(foldRight(List(1, 2), 0)((_, acc) ⇒ acc + 1) == foldLeft(List(1, 2), 0)((acc, _) ⇒ acc + 1))
  }

  def fsum[A](l: List[Int]) = foldLeft(l, 0)(_ + _)
  def fproduct[A](l: List[Int]) = foldLeft(l, 1)(_ * _)
  def flength[A](l: List[A]) = foldLeft(l, 0)((acc, _) ⇒ acc + 1)

  def testSumProductLength(): Unit = {
    assert(sum(List(1, 2, 4)) == fsum(List(1, 2, 4)))
    assert(product(List(1, 2, 4)) == fproduct(List(1, 2, 4)))
    assert(length(List(1, 2, 4)) == flength(List(1, 2, 4)))
  }

  def reverse[A](l: List[A]) = foldLeft(l, Nil: List[A])((acc, a) ⇒ Cons(a, acc))

  def testReverse(): Unit = {
    assert(List(3, 2, 1) == reverse(List(1, 2, 3)))
    assert(List() == reverse(List()))
  }

  def foldLeftR[A, B](l: List[A], z: B)(f: (B, A) ⇒ B) =
    (foldRight(l, { id: B ⇒ id }) { (a: A, acc: B ⇒ B) ⇒ { b ⇒ f(acc(b), a) } })(z)

  def foldRightR[A, B](l: List[A], z: B)(f: (A, B) ⇒ B) =
    (foldLeft(l, { id: B ⇒ id }) { (acc: B ⇒ B, a: A) ⇒ { b ⇒ f(a, acc(b)) } })(z)

  def map[A, B](l: List[A])(f: A ⇒ B): List[B] = sys.error("todo")
}
