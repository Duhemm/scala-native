package java.lang

import scala.scalanative.native._
import scala.scalanative.runtime

class _Object {
  def _equals(that: _Object): scala.Boolean =
    runtime.rawPointer(this) == runtime.rawPointer(that)

  def _hashCode(): scala.Int =
    runtime.rawPointer(this).hashCode

  def _toString(): String =
    getClass.getName + "@" + Integer.toHexString(hashCode)

  def _getClass(): _Class[_] =
    new _Class(runtime.getType(this))

  def _notify(): Unit =
    runtime.getMonitor(this)._notify

  def _notifyAll(): Unit =
    runtime.getMonitor(this)._notifyAll

  def _wait(): Unit =
    runtime.getMonitor(this)._wait

  def _wait(timeout: scala.Long): Unit =
    runtime.getMonitor(this)._wait(timeout)

  def _wait(timeout: scala.Long, nanos: Int): Unit =
    runtime.getMonitor(this)._wait(timeout, nanos)

  protected def _clone(): _Object = ???

  protected def _finalize(): Unit = ()
}
