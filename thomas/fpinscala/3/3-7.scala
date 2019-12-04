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

// foldRight로 구현된 product가 0.0을 만나면 즉시 재귀를 멈추고 0.0을 돌려줄까?

// : 별도의 종료 조건이 없기 때문에 다음과 같이 끝까지 재귀 실행한다.
// foldRight(List(1,0, 2.0, 3.0), 1.0)(_ * _)
// 1.0 * foldRight(List(0.0, 3.0), 1.0)(_ * _)
// 1.0 * 0.0 * foldRight(List(3.0), 1.0)(_ * _)
// 1.0 * 0.0 * 3.0 * foldRight(List(), 1.0)(_ * _)
// 1.0 * 0.0 * 3.0 * 1.0
// 0.0