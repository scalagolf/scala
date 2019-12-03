sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// ====

// 트리의 노드 개수를 세는 함수
def size[A](t: Tree[A]): Int = t match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case Leaf(_) => 1
}