package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We will later define other functions in terms of `nextInt`.
}

object RNG {
  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (randomInt, nextRNG) = rng.nextInt
    val positiveRandomInt =
      if (randomInt == Int.MinValue) Int.MaxValue
      else randomInt.abs

    (positiveRandomInt, nextRNG)
  }

  def double: RNG ⇒ (Double, RNG) =
    map(positiveInt) { positiveRandomInt ⇒
      if (positiveRandomInt == Int.MaxValue) 0D
      else positiveRandomInt / Int.MaxValue.toDouble
    }

  def intDouble: Rand[(Int,Double)] =
    map2(int, double)((_, _))

  def doubleInt: Rand[(Double,Int)] =
    map2(double, int)((_, _))

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (randomDouble1, nextRNG1) = double(rng)
    val (randomDouble2, nextRNG2) = double(nextRNG1)
    val (randomDouble3, nextRNG3) = double(nextRNG2)
    ((randomDouble1, randomDouble2, randomDouble3), nextRNG3)
  }

  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))


  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng ⇒ {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng ⇒ {
      fs.foldRight((List.empty[A], rng)) {
        case (ra, (as, lastRNG)) ⇒
          val (a, nextRNG) = ra(lastRNG)
          (a :: as, nextRNG)
      }
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng ⇒ {
      val (a, nextRNG) = f(rng)
      g(a)(nextRNG)
    }

  def positiveLessThan(upperLimit: Int): Rand[Int] =
    flatMap(positiveInt) { positiveRandomInt ⇒
      val mod = positiveRandomInt % upperLimit
      if (positiveRandomInt + (upperLimit - 1) - mod > 0)
        unit(mod)
      else
        positiveLessThan(upperLimit)
    }
}
case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
