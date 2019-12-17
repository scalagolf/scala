// 두 함수를 합성하는 고차 함수
def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))