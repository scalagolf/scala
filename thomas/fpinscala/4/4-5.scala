import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
        case Some(a) => Some(f(a))
        case None => None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case Some(a) => f(a)
        case None => None
    }

    def getOrElse[B >: A](default: => B): B = this match {
        case Some(a) => a
        case None => default
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
        case Some(_) => this: Option[B]
        case None => ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
        case Some(a) if f(a) => this
        case default => None: Option[A]
    }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a,b) match {
    case (Some(a), Some(b)) => Some(f(a,b))
    case (_, _) => None
}

// ====

// 목록을 단 한 번만 훑는 효율적인 구현
def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
}