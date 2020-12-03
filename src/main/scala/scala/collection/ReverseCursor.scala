package scala.collection

import scala.collection.mutable.cursor.EmptyCursor

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
   * Retreating a cursor that is already over nothing and has no
   * previous elements to traverse has no effect.
   *
   * @return `true` if the cursor was placed over the previous
   *         element; `false` otherwise
   */
  def retreat(): Boolean

  /**
   * Attempts to retreat this cursor a given number of times.
   *
   * @param times the number of times to retreat the cursor
   * @return `true` if the cursor was able to be retreated the
   *         given number of times and was placed over an element
   *         the final time it was retreated; `false` otherwise
   */
  final def retreat(times: Int): Boolean = {
    require(times >= 0, "cannot retreat a negative number of times")
    var remaining = times
    while (remaining > 0 && retreat()) {
      remaining -= 1
    }
    remaining == 0
  }
}

object ReverseCursor {
  private[this] val _empty: ReverseCursor[Nothing] = EmptyCursor.instance

  /** @return an empty reverse cursor over zero elements */
  @inline final def empty[A]: ReverseCursor[A] = _empty
}
