package scala.scalanative

import scala.reflect.ClassTag
import native._
import runtime.Intrinsics._

package object runtime {

  /** Used as a stub right hand of intrinsified methods. */
  def undefined: Nothing = throw new UndefinedBehaviorError

  /** Returns info pointer for given type. */
  def typeof[T](implicit ct: ClassTag[T]): Ptr[Type] = undefined

  /** Intrinsified unsigned devision on ints. */
  def divUInt(l: Int, r: Int): Int = undefined

  /** Intrinsified unsigned devision on longs. */
  def divULong(l: Long, r: Long): Long = undefined

  /** Intrinsified unsigned remainder on ints. */
  def remUInt(l: Int, r: Int): Int = undefined

  /** Intrinsified unsigned remainder on longs. */
  def remULong(l: Long, r: Long): Long = undefined

  /** Intrinsified byte to unsigned int converstion. */
  def byteToUInt(b: Byte): Int = undefined

  /** Intrinsified byte to unsigned long conversion. */
  def byteToULong(b: Byte): Long = undefined

  /** Intrinsified short to unsigned int conversion. */
  def shortToUInt(v: Short): Int = undefined

  /** Intrinsified short to unsigned long conversion. */
  def shortToULong(v: Short): Long = undefined

  /** Intrinsified int to unsigned long conversion. */
  def intToULong(v: Int): Long = undefined

  /** Select value without branching. */
  def select[T](cond: Boolean, thenp: T, elsep: T)(
      implicit ct: ClassTag[T]): T = undefined

  /** Cast ref to a pointer */
  def rawPointer(ref: AnyRef): Ptr[Byte] =
    ref.cast[Ptr[Byte]]

  /** Packs `id` in the first 16 bits of `ptr`. */
  def packed(id: Short, ptr: Ptr[Byte]): AnyRef = {
    val asLong      = ptr.cast[Long]
    val packedValue = id.toLong << 48
    (asLong | packedValue).cast[AnyRef]
  }

  /** Removes packed information from the first 16 bits of `ptr` */
  def unpackedPtr(ref: AnyRef): Ptr[Byte] = {
    val asLong = ref.cast[Long]
    (asLong & 0x0000FFFFFFFFFFFFL).cast[Ptr[Byte]]
  }

  /** Extract packed information from `ptr`. */
  def unpackedId(ref: AnyRef): Short = {
    val asLong = ref.cast[Long]
    ((asLong & 0xFFFF000000000000L) >> 48).toShort
  }

  /** Allocate memory in gc heap using given info pointer. */
  def alloc(ty: Ptr[Type], size: CSize): AnyRef = {
    val ptr = GC.malloc(size).cast[Ptr[Ptr[Type]]]
    !ptr = ty
    val id = (!ty).id.toShort
    packed(id, ptr.cast[Ptr[Byte]])
  }

  /** Allocate memory in gc heap using given info pointer.
   *
   * The allocated memory cannot be used to store pointers.
   */
  def allocAtomic(ty: Ptr[Type], size: CSize): AnyRef = {
    val ptr = GC.malloc_atomic(size).cast[Ptr[Ptr[Type]]]
    // initialize to 0
    `llvm.memset.p0i8.i64`(ptr.cast[Ptr[Byte]], 0, size, 1, false)
    !ptr = ty
    packed((!ty).id.toShort, ptr.cast[Ptr[Byte]])
  }

  /** Read type information of given object. */
  def getType(obj: Object): Ptr[Type] = !unpackedPtr(obj).cast[Ptr[Ptr[Type]]]

  /** Get monitor for given object. */
  def getMonitor(obj: Object): Monitor = Monitor.dummy

  /** Initialize runtime with given arguments and return the
   * rest as Java-style array.
   */
  def init(argc: Int, argv: Ptr[Ptr[Byte]]): ObjectArray = {
    val args = new scala.Array[String](argc - 1)

    // skip the executable name in argv(0)
    var c = 0
    while (c < argc - 1) {
      // use the default Charset (UTF_8 atm)
      args(c) = fromCString(argv(c + 1))
      c += 1
    }

    args.asInstanceOf[ObjectArray]
  }
}
