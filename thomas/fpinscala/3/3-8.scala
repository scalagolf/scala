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

def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B =
    as match {
        case Nil => z
        case Cons(x,xs) => f(x, foldRight(xs,z)(f))
    }

def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

// ====

// Cons 자체를 foldRight에 전달하면 어떤 일이 발생할까?
foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

// : 다음과 같이 List를 재생성한다.
// foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
// Cons(1, foldRight(List(2,3), Nil)(Cons(_,_)))
// Cons(1, Cons(2, foldRight(List(3), Nil)(Cons(_,_))))
// Cons(1, Cons(2, Cons(3, foldRight(List(), Nil)(Cons(_,_)))))
// Cons(1, Cons(2, Cons(3, Nil)))