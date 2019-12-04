// List가 다른 List를 부분 순차열로서 담고 있는지 점검하는 함수
@annotation.tailrec
def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    if (sup.take(sub.length) == sub) true
    else if (sup.tail == Nil) false
    else hasSubsequence(sup.tail, sub)
}