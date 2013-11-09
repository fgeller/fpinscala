package fpinscala.errorhandling

import scala.{ Option ⇒ _, Either ⇒ _, _ } // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {

  def map[B](f: A ⇒ B): Either[E, B] = this match {
    case l @ Left(_) ⇒ l
    case Right(a)    ⇒ Right(f(a))
  }

  def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B] = this match {
    case l @ Left(_) ⇒ l
    case Right(r)    ⇒ f(r)
  }

  def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B] = this match {
    case Left(_)      ⇒ b
    case r @ Right(_) ⇒ r
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C] = flatMap { av ⇒
    b map { bv ⇒ f(av, bv) }
  }
}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception ⇒ Left(e) }

  def Try[A](a: ⇒ A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception ⇒ Left(e) }

  def traverse[E, A, B](l: List[A])(f: A ⇒ Either[E, B]): Either[E, List[B]] =
    l.foldRight(Right(Nil): Either[E, List[B]]) { (a, acc) ⇒
      acc flatMap { l ⇒ f(a) map { b ⇒ b :: l } }
    }

  def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] =
    traverse(l)(e ⇒ e)

}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

sealed trait Validation[+E, +A] extends Either[List[E], A] {

  def map2[EE >: E, B, C](b: Validation[EE, B])(f: (A, B) ⇒ C): Validation[EE, C] =
    (this, b) match {
      case (Failure(f1), Failure(f2)) ⇒ Failure(f1 ++ f2)
      case (_, f @ Failure(_))        ⇒ f
      case (f @ Failure(_), _)        ⇒ f
      case (Success(a), Success(b))   ⇒ Success(f(a, b))
    }

}
object Validation {

  def traverse[E, A, B](l: List[A])(f: A ⇒ Validation[E, B]): Validation[E, List[B]] =
    l.foldRight(Success(Nil): Validation[E, List[B]]) {
      (a, acc) ⇒
        acc match {
          case Failure(f1) ⇒
            f(a) match {
              case Failure(f2) ⇒ Failure(f1 ++ f2)
              case _           ⇒ Failure(f1)
            }
          case Success(ls) ⇒
            f(a) match {
              case Success(s)        ⇒ Success(s :: ls)
              case fail @ Failure(_) ⇒ fail
            }
        }
    }

}
case class Failure[+E](get: List[E]) extends Validation[E, Nothing]
case class Success[+A](get: A) extends Validation[Nothing, A]

object Makers {
  def mkName(name: String): Validation[String, Name] =
    if (name == "" || name == null) Failure(List("Name is empty."))
    else Success(new Name(name))
  def mkAge(age: Int): Validation[String, Age] =
    if (age < 0) Failure(List("Age is out of range."))
    else Success(new Age(age))
  def mkPerson(name: String, age: Int): Validation[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}
