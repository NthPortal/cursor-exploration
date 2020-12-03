package scala.collection
package mutable
package cursor

/** An empty cursor implementing all cursor interfaces. */
private[collection] final class EmptyCursor
    extends AbstractCursor[Nothing]
    with ReverseCursor[Nothing]
    with RemoveCursor[Nothing] {
  def advance(): Boolean = false
  def retreat(): Boolean = false
  @throws[NoSuchElementException]
  def current: Nothing = throw new NoSuchElementException("current when cursor over nothing")
  @throws[NoSuchElementException]
  def remove(): Unit = throw new NoSuchElementException("remove when cursor over nothing")
}

private[collection] object EmptyCursor {
  /** '''The''' empty cursor. */
  final val instance = new EmptyCursor
}
