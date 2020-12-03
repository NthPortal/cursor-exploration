package scala.collection
package mutable
package cursor

/**
 * A [[Cursor]] that can remove elements from the collection being
 * traversed.
 *
 * @tparam A the type of elements this cursor traverses
 */
trait RemoveCursor[+A] extends Cursor[A] {

  /**
   * Removes the current element under this cursor from the collection
   * being traversed.
   *
   * @throws scala.NoSuchElementException if this cursor is not over
   *                                      an element
   */
  @throws[NoSuchElementException]
  def remove(): Unit
}

object RemoveCursor {
  private[this] val _empty: RemoveCursor[Nothing] = EmptyCursor.instance

  /** @return an empty remove cursor over zero elements */
  @inline final def empty[A]: RemoveCursor[A] = _empty
}
