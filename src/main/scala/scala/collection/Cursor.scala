package scala.collection

/**
 * A utility for traversing a collection with a cursor that
 * points to or "hovers over" each element one at a time.
 *
 * At any given time, a cursor is said to be either "over an element"
 * or "over nothing"; likewise, when a cursor is over an element, that
 * element is said to be "under the cursor". When using a cursor to
 * traverse a collection, it starts over nothing, and the first call
 * to `advance()` places the cursor over the first element of the collection
 * (assuming it is non-empty). Subsequent calls to `advance()` put the
 * cursor over successive elements in the collection. A method call (on a
 * `Cursor` supporting such functionality) that removes the `current`
 * element under the cursor from the underlying collection places the
 * cursor over nothing in a "gap" between the prior and subsequent elements
 * of the collection, where the removed element used to be. When a cursor is
 * over the last element of a collection, a subsequent call to `advance()`
 * places the cursor over nothing, past the last element of the collection.
 *
 * @tparam A the type of elements this cursor traverses
 */
trait Cursor[+A] extends IterableOnce[A] {

  /**
   * Advances this cursor and places it over the next element,
   * or over nothing if there are no subsequent elements to traverse.
   *
   * Advancing a cursor that is already over nothing and has no
   * subsequent elements to traverse has no effect.
   *
   * @return `true` if the cursor was placed over the next
   *         element; `false` otherwise
   */
  def advance(): Boolean

  /**
   * Attempts to advance this cursor a given number of times.
   *
   * @param times the number of times to advance the cursor
   * @return `true` if the cursor was able to be advanced the
   *         given number of times and was placed over an element
   *         the final time it was advanced; `false` otherwise
   */
  final def advance(times: Int): Boolean = {
    require(times >= 0, "cannot advance a negative number of times")
    var remaining = times
    while (remaining > 0 && advance()) {
      remaining -= 1
    }
    remaining == 0
  }

  /**
   * @throws scala.NoSuchElementException if this cursor is not over
   *                                      an element
   * @return the current element under this cursor
   */
  @throws[NoSuchElementException]
  def current: A

  /**
   * @return an [[Iterator]] over the remaining elements for this
   *         cursor to traverse, excluding the element currently under
   *         the cursor (if any)
   */
  override def iterator: Iterator[A] = new Cursor.WrappedCursor[A](this)

  /**
   * @return the number of elements remaining for this cursor to traverse,
   *         excluding the element currently under the cursor (if any)
   */
  def size: Int = {
    var res = 0
    while (advance()) res += 1
    res
  }
}

object Cursor {
  // sentinel indicating that a cursor is not over an element
  private[collection] final val overNothing = new AnyRef

  // TODO: create a single empty cursor implementation with all the traits/mixins
  private[collection] trait Empty extends Cursor[Nothing] {
    def advance() = false
    @throws[NoSuchElementException]
    def current = throw new NoSuchElementException("current when cursor over nothing")
    override def knownSize: Int = 0
  }

  private[this] val _empty: Cursor[Nothing] = new AbstractCursor[Nothing] with Empty

  /** @return an empty cursor over no elements */
  @inline final def empty[A]: Cursor[A] = _empty

  /**
   * @return a `Cursor` that iterates over the remaining elements
   *         of the given [[Iterator]]
   */
  def fromIterator[A](iterator: Iterator[A]): Cursor[A] =
    iterator match {
      case iterator: WrappedCursor[A] => iterator.cursor
      case iterator                   => new WrappedIterator[A](iterator)
    }

  private final class WrappedCursor[+A](val cursor: Cursor[A]) extends AbstractIterator[A] {
    // 1: need to check `advance()`
    // 2: `hasNext` based on last `advance()`
    // 3: `!hasNext` based on last `advance()`
    private[this] var state: Int = 1

    // always sets `state` to `2` or `3`
    private def tryAdvance(): Unit = {
      val _hasNext = cursor.advance()
      state = if (_hasNext) 2 else 3
    }

    def hasNext: Boolean = {
      if (state == 1) tryAdvance()
      state == 2
    }

    def next(): A = {
      if (state == 1) tryAdvance()
      if (state == 3) Iterator.empty.next()
      else /* state == 2 */ {
        val res = cursor.current
        state = 1
        res
      }
    }
  }

  private final class WrappedIterator[+A](it: Iterator[A]) extends AbstractCursor[A] {
    private[this] var cur: AnyRef = overNothing

    def advance(): Boolean = {
      val hasNext = it.hasNext
      cur = if (hasNext) it.next().asInstanceOf[AnyRef] else overNothing
      hasNext
    }

    def current: A =
      if (cur eq overNothing) Cursor.empty.current
      else cur.asInstanceOf[A]

    override def iterator: Iterator[A] = it
  }
}

abstract class AbstractCursor[+A] extends Cursor[A]
