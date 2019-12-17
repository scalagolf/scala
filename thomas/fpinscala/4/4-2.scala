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

def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

// ====

// variance 함수를 flatMap을 이용해서 구현
// 순차열의 평균이 m이라 할 때, 분산은 순차열의 각 요소 x에 대한 math.pow(x - m, 2)들의 평균
def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))