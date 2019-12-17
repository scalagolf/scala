// Option에 대한 함수 구현
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
