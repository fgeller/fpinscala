package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A, B](t: Tree[A])(l: A ⇒ B)(b: (B, B) ⇒ B): B =
    t match {
      case Leaf(a)        ⇒ l(a)
      case Branch(t1, t2) ⇒ b(fold(t1)(l)(b), fold(t2)(l)(b))
    }

  def size[A](t: Tree[A]): Int = fold(t)(a ⇒ 1)(_ + _)

  def testSize(): Unit = {
    assert(1 == size(Leaf(1)))
    assert(2 == size(Branch(Leaf(1), Leaf(1))))
    assert(3 == size(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))))
  }

  def maximum(t: Tree[Int]): Int = fold(t)(a ⇒ a)(_ max _)

  def testMaximum(): Unit = {
    assert(1 == maximum(Leaf(1)))
    assert(2 == maximum(Branch(Leaf(1), Leaf(2))))
    assert(2 == maximum(Branch(Leaf(2), Leaf(1))))
    assert(2 == maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(1)))))
    assert(3 == maximum(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))))
  }

  def depth[A](t: Tree[A]): Int = fold(t)(a ⇒ 0)((b1, b2) ⇒ 1 + (b1 max b2))

  def testDepth(): Unit = {
    assert(0 == depth(Leaf(1)))
    assert(1 == depth(Branch(Leaf(1), Leaf(2))))
    assert(2 == depth(Branch(Leaf(1), Branch(Leaf(2), Leaf(1)))))
  }

  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = fold(t)(a ⇒ Leaf(f(a)): Tree[B])(Branch(_, _))

  def testMap(): Unit = {
    assert(Leaf(2) == map(Leaf(1))(_ + 1))
    assert(Branch(Leaf(2), Leaf(3)) == map(Branch(Leaf(1), Leaf(2)))(_ + 1))
    assert(Branch(Leaf(2), Branch(Leaf(3), Leaf(2))) == map(Branch(Leaf(1), Branch(Leaf(2), Leaf(1))))(_ + 1))
  }

}
