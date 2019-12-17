import scala.util.{Either => _, Right => _, Left => _}

sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
    }
        
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Right(a) => Right(a)
        case Left(_) => b
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
        a1 <- this
        b1 <- b
    } yield f(a1, b1)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list!")
    else Right(xs.sum / xs.length)

def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }


// ====

// Either에 대한 sequence와 traverse
def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case h::t => h.map2(sequence(t))(_ :: _)
}

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =  as match {
    case Nil => Right(Nil)
    case h::t => f(h).map2(traverse(t)(f))(_ :: _)
}
