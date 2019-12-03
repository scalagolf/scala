sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// ====

// 트리의 각 요소를 주어진 함수로 수정하는 함수
def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Left(a) => f(a)
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
}