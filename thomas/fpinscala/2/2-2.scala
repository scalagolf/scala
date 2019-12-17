// 주어진 비교 함수에 의거해서 정렬되어 있는지 점검하는 함수
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(as: Array[A], ordered: (A,A) => Boolean): Boolean =
        if (as.length <= 1) true
        else if (ordered(as(0), as(1))) loop(as.tail, ordered)
        else false
    loop(as, ordered)
}