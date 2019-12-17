// n번째 피보나치 수를 돌려주는 재귀함수. 0, 1, 1, 2, 3, 5, ...
def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, curr: Int, next: Int): Int =
        if (n <= 0) curr
        else go(n-1, next, curr+next)
    go(n-1, 0, 1)
}