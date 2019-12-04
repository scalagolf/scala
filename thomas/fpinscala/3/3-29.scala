sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left) max maximum(right)
}

def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
}

def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
}

// ====

// fold
def fold[A,B](t: Tree[A])(f: A => B)(g: (Tree[A],Tree[A]) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g(left, right)
}

def maximum2(t: Tree[Int]): Int = fold(t)(v => v)(maximum2(_) max maximum2(_))

def depth2[A](t: Tree[A]): Int = fold(t)(v => 0)((l,r) => 1 + (depth2(l) max depth2(r)))

def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)):Tree[B])((l,r) => Branch(map2(l)(f), map2(r)(f):Tree[B]))