package scala.scalanative
package runtime

import native._

/**
 * The Boehm GC conservative garbage collector
 *
 * @see [[http://hboehm.info/gc/gcinterface.html C Interface]]
 */
@link("gc")
@extern
object GC {
  @name("scalanative_alloc_raw")
  def malloc(size: CSize): Ptr[_] = extern
  @name("scalanative_alloc_raw_atomic")
  def malloc_atomic(size: CSize): Ptr[_] = extern
  def gc_save(): Unit                    = extern
  def gc_restore(): Unit                 = extern
}
