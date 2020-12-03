package scala.collection

/**
 * A [[Cursor]] that can also traverse a collection in reverse,
 * and can change the direction of traversal at any time.
 *
 * When a reverse cursor is over the first element of a collection, a
 * subsequent call to `retreat()` places the cursor over nothing, before
 * the first element of the collection.
 *
 * @tparam A the type of elements this cursor traverses
 */
trait ReverseCursor[+A] extends Cursor[A] {

  /**
   * Retreats this cursor and places it over the previous element,
   * or over nothing if there are no previous elements to traverse
   * in reverse.
   *
   * @return `true` if the cursor was placed over the previous
   *         element; `false` otherwise
   */
  def retreat(): Boolean
}

object ReverseCursor {
  private[collection] trait Empty extends Cursor.Empty with ReverseCursor[Nothing] {
    def retreat(): Boolean = false
  }

  private[this] val _empty: ReverseCursor[Nothing] = new AbstractCursor[Nothing] with Empty

  /** @return an empty reverse cursor over no elements */
  @inline final def empty[A]: ReverseCursor[A] = _empty
}
