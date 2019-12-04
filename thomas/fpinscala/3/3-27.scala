sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// ====

// 뿌리에서 임의의 잎으로의 가장 긴 경로의 길이를 돌려주는 함수
def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
}