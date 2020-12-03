package scala.collection

trait ReverseCursor[+A] extends Cursor[A] {
  def retreat(): Boolean
}

object ReverseCursor {
  private[collection] trait Empty extends Cursor.Empty with ReverseCursor[Nothing] {
    def retreat(): Boolean = false
  }

  private[this] val _empty: ReverseCursor[Nothing] = new AbstractCursor[Nothing] with Empty

  @inline final def empty[A]: ReverseCursor[A] = _empty
}
