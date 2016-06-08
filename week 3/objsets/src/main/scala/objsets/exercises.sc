trait List[T] {
  def head: T

  def tail: List[T]

  def isEmpty: Boolean

}

class Nil[T] extends List[T] {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")

  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")

  def isEmpty = true
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}

def nth(num: Int, list: List[Int]): Int = {
  if (list.isEmpty) throw new IndexOutOfBoundsException
  else if (num == 0) list.head
  else nth(num - 1, list.tail)

}


