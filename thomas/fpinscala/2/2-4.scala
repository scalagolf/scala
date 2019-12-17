// curry 변환을 역으로 수행하는 고차 함수
def uncurry[A,B,C](f: A => B => C): (A,B) => C =
    (a: A, b: B) => f(a)(b)