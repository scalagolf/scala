sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => f(x, foldRight(xs,z)(f))
}

@annotation.tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
}

// ====

// append를 foldLeft나 foldRight를 이용해서 구현
def append[A,B](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))
