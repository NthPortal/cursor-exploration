package scala.collection

trait Cursor[+A] extends IterableOnce[A] {
  def advance(): Boolean

  @throws[NoSuchElementException]
  def current: A

  override def iterator: Iterator[A] = new Cursor.WrappedCursor[A](this)

  def size: Int = {
    var res = 0
    while (advance()) res += 1
    res
  }
}

object Cursor {
  // sentinel indicating that a cursor is not over an element
  private[collection] final val overNothing = new AnyRef

  // may be needed by more advanced mutable Cursors
  private[collection] trait Empty extends Cursor[Nothing] {
    def advance() = false
    @throws[NoSuchElementException]
    def current = throw new NoSuchElementException("current when cursor over nothing")
    override def knownSize: Int = 0
  }

  private[this] val _empty: Cursor[Nothing] = new AbstractCursor[Nothing] with Empty

  @inline final def empty[A]: Cursor[A] = _empty

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
      if (hasNext) cur = it.next().asInstanceOf[AnyRef]
      hasNext
    }

    def current: A =
      if (cur eq overNothing) Cursor.empty.current
      else cur.asInstanceOf[A]

    override def iterator: Iterator[A] = it
  }
}

abstract class AbstractCursor[+A] extends Cursor[A]
