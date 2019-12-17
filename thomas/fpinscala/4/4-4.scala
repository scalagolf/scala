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

// ====

// Option들의 목록을 받고, 그 목록에 있는 모든 Some 값으로 구성된 목록을 담은 Option을 돌려주는 함수
// 원래의 목록에 None이 하나라도 있으면 함수의 결과도 None
def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t) map (hh :: _))
}