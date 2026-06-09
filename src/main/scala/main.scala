import scala.annotation.tailrec
import MyList.*
import MyOption.*
import Tree.*

@main
def main(): Unit = {
  println(tail(MyList(1, 2, 3)))
  println(setHead(3, MyList(1, 2, 3)))
  println(drop(MyList(1, 2, 3), 4))
  println(dropWhile(MyList(1, 2, 3, 4), x => x % 2 == 0))
  println(init(MyList(1, 2, 3)))
  println(foldRight(MyList(1, 2, 3), Nil, Cons(_, _)))
  println(length(MyList(1, 2, 3, 4, 5, 6)))
  println(reverse(MyList(1, 2, 3, 4, 5)))
  println(append(MyList(1, 2, 3), MyList(4, 5, 6)))
  println(convertToString(MyList(1.0, 2.0)))
  println(map(MyList(1, 2, 3), _ + 1))
  println(flatMap(MyList(1, 2, 3), a => MyList(a, a)))
  println(addLists(MyList(1, 2, 3), MyList(4, 5, 6)))
  println(hasSubsequence(MyList(1, 2, 3), MyList(2, 3)))
  println(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d"))).size)
  println(foldSize(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
  println(maximum(Branch(Leaf(3), Branch(Leaf(6), Leaf(3)))))
  println(foldMaximum(Branch(Leaf(3), Branch(Leaf(6), Leaf(3)))))
  println(depth(Branch(Branch(Leaf(4), Leaf(6)), Leaf(3))))
  println(foldDepth(Branch(Branch(Leaf(4), Leaf(6)), Leaf(3))))
  println(Some(5).map(_ + 4))
  println((None: MyOption[Int]).map(_ + 3))
  println(mean(List(1.0, 2.0, 3.0, 4.0, 5.0)))
  println(variance(List(1.0, 2.0, 3.0, 4.0, 5.0)))
  println(sequence(MyList(Some(3), Some(4))))
  val incrementFailingOnThree: Int => MyOption[Int] = a => if a == 3 then None else Some(a + 1)
  println(_traverse(MyList(1, 2, 3, 4))(incrementFailingOnThree))
  println(sequence(MyList(MyEither.Right(1), MyEither.Right(2), MyEither.Left("Uh oh"), MyEither.Right(3), MyEither.Left("Stinky"))))
  println(MyEither.Left("Uh oh").map2(MyEither.Left("Stinky"))((_,_)))
  println(map2Both(MyEither.Left("Uh oh"), MyEither.Left("Stinky"), (_,_)))
}

def map2Both[E, A, B, C](
    a: MyEither[E, A],
    b: MyEither[E, B],
    f: (A, B) => C): MyEither[MyList[E], C] =
  (a, b) match {
    case (MyEither.Right(aa), MyEither.Right(bb)) => MyEither.Right(f(aa, bb))
    case (MyEither.Left(e), MyEither.Right(_)) => MyEither.Left(MyList(e))
    case (MyEither.Right(_), MyEither.Left(e)) => MyEither.Left(MyList(e))
    case (MyEither.Left(e1), MyEither.Left(e2)) => MyEither.Left(MyList(e1, e2))
  }

def sequence[E, A](as: MyList[MyEither[E, A]]): MyEither[E, MyList[A]] =
  foldRight(as, MyEither.Right(Nil): MyEither[E, MyList[A]], (a: MyEither[E, A], acc) => a.map2(acc)(Cons(_, _)))

def traverse[E, A, B](as: MyList[A])(f: A => MyEither[E, B]): MyEither[E, MyList[B]] =
  foldRight(as, MyEither.Right(Nil), (a: A, acc: MyEither[E, MyList[B]]) => f(a).map2(acc)(Cons(_, _)))

enum MyEither[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): MyEither[E, B] = this match {
    case Right(value) => Right(f(value))
    case Left(value) => Left(value)
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case Left(_) => b
    case Right(value) => Right(value)
  }

  def map2[EE >: E, B, C](that: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    this.flatMap(_this => that.map(_that => f(_this, _that)))

def traverse[A, B](as: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] =
  sequence(map(as, f))

def _traverse[A, B](as: MyList[A])(f: A => MyOption[B]): MyOption[MyList[B]] =
  foldRight(as, Some(Nil), (a: A, acc) => map2(f(a), acc)(Cons(_, _)))

def sequence[A](as: MyList[MyOption[A]]): MyOption[MyList[A]] =
  foldRight(as, Some(Nil), (a: MyOption[A], acc) => map2(a, acc)(Cons(_, _)))

// this could be done with pattern matching but we can map/flatmap for nicer implementation
def map2[A, B, C](a: MyOption[A], _b: MyOption[B])(f: (A, B) => C): MyOption[C] =
  a.flatMap(_a => _b.map(_b => f(_a, _b)))

def lift[A, B](f: A => B): MyOption[A] => MyOption[B] =
  a => a.map(f)

def mean(xs: Seq[Double]): MyOption[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): MyOption[Double] =
  val meanXs = mean(xs)
  meanXs.flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

enum MyOption[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): MyOption[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = map(f).getOrElse(None)

  // first map will make Some(Some(a)) | None; then getOrElse will extract some(a) or ob if None
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = map(Some(_)).getOrElse(ob)

  // this could also just be done with a match
  def filter(f: A => Boolean): MyOption[A] = flatMap(a => if f(a) then Some(a) else None)

def foldMap[A, B](t: Tree[A], f: A => B): Tree[B] =
  fold(t, a => Leaf(f(a)), Branch(_, _))

def foldDepth[A](t: Tree[A]): Int = fold(t, _ => 1, 1 + _.max(_))

def foldMaximum(t: Tree[Int]): Int = fold(t, a => a, _.max(_))

def foldSize[A](t: Tree[A]): Int = fold(t, _ => 1, 1 + _ + _)

def fold[A, B](t: Tree[A], f: A => B, g: (B, B) => B): B = t match {
  case Leaf(v) => f(v)
  case Branch(l, r) => g(fold(l, f, g), fold(r, f, g))
}

def map[A, B](f: A => B, t: Tree[A]): Tree[B] = t match {
  case Leaf(x) => Leaf(f(x))
  case Branch(l, r) => Branch(map(f, l), map(f, r))
}

def depth[A](t: Tree[A]): Int = t match {
  case Leaf(v) => 1
  case Branch(l, r) => 1 + depth(l).max(depth(r))
}

def maximum(t: Tree[Int]): Int = t match {
  case Leaf(v) => v
  case Branch(l, r) => maximum(l).max(maximum(r))
}

enum Tree[+A]:
  case Leaf(v: A)
  case Branch(l: Tree[A], r: Tree[A])

  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

@tailrec
def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean =
  @tailrec
  def startsWith(l: MyList[A], prefix: MyList[A]): Boolean = (l, prefix) match
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startsWith(t, t2)
    case _ => false

  sup match
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)

def addOne(xs: MyList[Int]): MyList[Int] =
  foldRight(xs, Nil: MyList[Int], (i, acc) => Cons(i + 1, acc))

def convertToString(ds: MyList[Double]): MyList[String] =
  foldRight(ds, Nil: MyList[String], (d, acc) => Cons(d.toString, acc))

def map[A, B](as: MyList[A], f: A => B): MyList[B] =
  foldRight(as, Nil, (a, acc) => Cons(f(a), acc))

def filter[A](as: MyList[A], f: A => Boolean): MyList[A] =
  foldRight(as, Nil: MyList[A], (a, acc) => if f(a) then Cons(a, acc) else acc)

def flatMap[A, B](as: MyList[A], f: A => MyList[B]): MyList[B] =
  foldRight(as, Nil, (a, acc) => append(f(a), acc))

def filterPrime[A](as: MyList[A], f: A => Boolean): MyList[A] =
  flatMap(as, a => if f(a) then MyList(a) else Nil)

def addLists(xs: MyList[Int], ys: MyList[Int]): MyList[Int] = (xs, ys) match {
  case (xs, Nil) => Nil
  case (Nil, ys) => Nil
  case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
}

def combine[A, B, C](as: MyList[A], bs: MyList[B], f: (a: A, b: B) => C): MyList[C] = (as, bs) match {
  case (as, Nil) => Nil
  case (Nil, bs) => Nil
  case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), combine(as, bs, f))
}

// we build an accumulator up in a tail recursive fashion
def zipWithTailRec[A, B, C](a: MyList[A], b: MyList[B], f: (A, B) => C): MyList[C] =
  @tailrec
  def loop(a: MyList[A], b: MyList[B], acc: MyList[C]): MyList[C] =
    (a, b) match
      case (Nil, _) => acc
      case (_, Nil) => acc
      case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))

  reverse(loop(a, b, Nil))


def fib(n: Int): Int =
  if n == 0 then return 0
  if n == 1 then return 1

  @tailrec
  def go(n: Int, prev: Int, curr: Int): Int =
    if n == 2 then prev + curr
    else go(n - 1, curr, prev + curr)

  go(n, 0, 1)

def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  @tailrec
  def go(i: Int): Boolean =
    if i >= as.length - 1 then true
    else if gt(as(i), as(i + 1)) then false
    else go(i + 1)

  if as.length <= 1 then true else go(0)
}

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

enum MyList[+A]:
  case Nil
  case Cons(head: A, tail: MyList[A])

  override def toString: String = this match {
    case Nil => "[]"
    case _ =>
      @tailrec
      def format(list: MyList[A], acc: String): String = list match {
        case Nil => acc + "]"
        case Cons(h, Nil) => format(Nil, acc + h.toString)
        case Cons(h, t) => format(t, acc + h.toString + ", ")
      }

      "[" + format(this, "")
  }

object MyList:
  def apply[A](as: A*): MyList[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail *))


def sum(ints: MyList[Int]): Int = ints match {
  case Nil => 0
  case Cons(x, xs) => x + sum(xs)
}

def tail[A](xs: MyList[A]): MyList[A] = xs match
  case Nil => sys.error("Nil list; no tail")
  case Cons(x: A, xs: MyList[A]) => xs

def setHead[A](x: A, xs: MyList[A]): MyList[A] = Cons(x, tail(xs))

@tailrec
def drop[A](as: MyList[A], n: Int): MyList[A] = (as, n) match {
  case (_, 0) => as
  case (Nil, _) => Nil
  case (Cons(a, as), n) => drop(as, n - 1)
}

@tailrec
def dropWhile[A](as: MyList[A], f: A => Boolean): MyList[A] = as match {
  case Nil => Nil
  case Cons(a, as) => if f(a) then dropWhile(as, f) else Cons(a, as)
}

def init[A](as: MyList[A]): MyList[A] = as match {
  case Nil => sys.error("init of empty list")
  case Cons(a, Nil) => Nil
  case Cons(a, rest) => Cons(a, init(rest))
}

def foldRight[A, B](as: MyList[A], acc: B, f: (A, B) => B): B = as match {
  case Nil => acc
  case Cons(x, xs) => f(x, foldRight(xs, acc, f))
}

def length[A](as: MyList[A]): Int =
  foldRight(as, 0, (_, acc) => acc + 1)

@tailrec
def foldLeft[A, B](as: MyList[A], acc: B, f: (B, A) => B): B = as match {
  case Nil => acc
  case Cons(a, as) => foldLeft(as, f(acc, a), f)
}

def reverse[A](xs: MyList[A]): MyList[A] = foldLeft(xs, Nil, (xs: MyList[A], x: A) => Cons(x, xs))

def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
  foldRight(a1, a2, Cons(_, _))