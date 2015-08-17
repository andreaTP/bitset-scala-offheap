package eu.unicredit.offheap

import scala.offheap._

class BitSet(nbits: Int)(implicit region: Region) {

  def this()(implicit region: Region) =
    this(64)

  private var _size = {
    var tsize = 64
    while (tsize < nbits)
      tsize += 64

    tsize
  }

  private var _lenght = 0

  private lazy val byteSize = ((_size / 64) + 1)

  private def byteOfBitN(n: Int) =
    n / 64

  private def byteMask(n: Int) =
    1L << (n % 64)

  private var inner: Array[Long] = Array.fill(byteSize)(0L)

  def clear() = {
    var i = 0
    while (i < byteSize) {
      inner.update(i, 0L)
      i += 1
    }
  }

  def set(index: Int): Unit =
    set(index, true)

  def set(index: Int, value: Boolean): Unit =
    _set(index, _ => value)

  private def _set(index: Int, _value: (Boolean) => Boolean): Unit = {
    val bytePos = byteOfBitN(index)
    val bitMask = byteMask(index)
    val last = inner(bytePos)

    val oldValue = ((last & bitMask) != 0)
    val value = _value(oldValue)
    if (oldValue != value) {
      if (value)
        inner.update(bytePos, (last | bitMask))
      else
        inner.update(bytePos, (last & (0xFFFFFFFFFFFFFFFFL - bitMask)))
    }
  }


  def flip(bitIndex: Int): Unit =
    _set(bitIndex, oldValue => !oldValue)

  //NOT optimized...
  def flip(fromIndex: Int, toIndex: Int): Unit = {
    var i = fromIndex
    while (i < toIndex) {
      _set(i, oldValue => !oldValue)
      i += 1
    }
  }

  def set(fromIndex: Int, toIndex: Int): Unit =
    set(fromIndex, toIndex, true)

    private val rightMask: Map[Int, Long] = Map(
      0 -> 0L,
      1 -> 1L,
      2 -> 3L,
      3 -> 7L,
      4 -> 15L,
      5 -> 31L,
      6 -> 63L,
      7 -> 127L,
      8 -> 255L,
      9 -> 511L,
      10 -> 1023L,
      11 -> 2047L,
      12 -> 4095L,
      13 -> 8191L,
      14 -> 16383L,
      15 -> 32767L,
      16 -> 65535L,
      17 -> 131071L,
      18 -> 262143L,
      19 -> 524287L,
      20 -> 1048575L,
      21 -> 2097151L,
      22 -> 4194303L,
      23 -> 8388607L,
      24 -> 16777215L,
      25 -> 33554431L,
      26 -> 67108863L,
      27 -> 134217727L,
      28 -> 268435455L,
      29 -> 536870911L,
      30 -> 1073741823L,
      31 -> 2147483647L,
      32 -> 4294967295L,
      33 -> 8589934591L,
      34 -> 17179869183L,
      35 -> 34359738367L,
      36 -> 68719476735L,
      37 -> 137438953471L,
      38 -> 274877906943L,
      39 -> 549755813887L,
      40 -> 1099511627775L,
      41 -> 2199023255551L,
      42 -> 4398046511103L,
      43 -> 8796093022207L,
      44 -> 17592186044415L,
      45 -> 35184372088831L,
      46 -> 70368744177663L,
      47 -> 140737488355327L,
      48 -> 281474976710655L,
      49 -> 562949953421311L,
      50 -> 1125899906842623L,
      51 -> 2251799813685247L,
      52 -> 4503599627370495L,
      53 -> 9007199254740991L,
      54 -> 18014398509481983L,
      55 -> 36028797018963967L,
      56 -> 72057594037927935L,
      57 -> 144115188075855871L,
      58 -> 288230376151711743L,
      59 -> 576460752303423487L,
      60 -> 1152921504606846975L,
      61 -> 2305843009213693951L,
      62 -> 4611686018427387903L,
      63 -> 9223372036854775807L
    )

    private val leftMask: Map[Int, Long] = Map(
      0 -> -1L,
      1 -> -2L,
      2 -> -4L,
      3 -> -8L,
      4 -> -16L,
      5 -> -32L,
      6 -> -64L,
      7 -> -128L,
      8 -> -256L,
      9 -> -512L,
      10 -> -1024L,
      11 -> -2048L,
      12 -> -4096L,
      13 -> -8192L,
      14 -> -16384L,
      15 -> -32768L,
      16 -> -65536L,
      17 -> -131072L,
      18 -> -262144L,
      19 -> -524288L,
      20 -> -1048576L,
      21 -> -2097152L,
      22 -> -4194304L,
      23 -> -8388608L,
      24 -> -16777216L,
      25 -> -33554432L,
      26 -> -67108864L,
      27 -> -134217728L,
      28 -> -268435456L,
      29 -> -536870912L,
      30 -> -1073741824L,
      31 -> -2147483648L,
      32 -> -4294967296L,
      33 -> -8589934592L,
      34 -> -17179869184L,
      35 -> -34359738368L,
      36 -> -68719476736L,
      37 -> -137438953472L,
      38 -> -274877906944L,
      39 -> -549755813888L,
      40 -> -1099511627776L,
      41 -> -2199023255552L,
      42 -> -4398046511104L,
      43 -> -8796093022208L,
      44 -> -17592186044416L,
      45 -> -35184372088832L,
      46 -> -70368744177664L,
      47 -> -140737488355328L,
      48 -> -281474976710656L,
      49 -> -562949953421312L,
      50 -> -1125899906842624L,
      51 -> -2251799813685248L,
      52 -> -4503599627370496L,
      53 -> -9007199254740992L,
      54 -> -18014398509481984L,
      55 -> -36028797018963968L,
      56 -> -72057594037927936L,
      57 -> -144115188075855872L,
      58 -> -288230376151711744L,
      59 -> -576460752303423488L,
      60 -> -1152921504606846976L,
      61 -> -2305843009213693952L,
      62 -> -4611686018427387904L,
      63 -> -9223372036854775808L,
      64 -> 0L
    )

  def set(fromIndex: Int, toIndex: Int, value: Boolean): Unit = {

    val firstByte = byteOfBitN(fromIndex)
    val lastByte = byteOfBitN(toIndex)

    val bytesPos =
      firstByte to lastByte

    val firstByteNewValue = {
      val bound = fromIndex - firstByte*64
      var i = 63
      var mask = 0L

      if (value) {
        mask = leftMask(bound)
        /*while (i >= bound) {
          mask += (1L << i)
          i -= 1
        }*/
        inner(firstByte) | mask
      } else {
        mask = rightMask(bound)
        /*i = 0
        while (i < bound) {
          mask += (1L << i)
          i += 1
        }*/
        inner(firstByte) & mask
      }
    }

    val lastByteNewValue = {
      val bound = toIndex - lastByte*64
      var i = 63
      var mask = 0L

      if (value) {
        mask = rightMask(bound)
        /*i = 0
        while (i < bound) {
          mask += (1L << i)
          i += 1
        }*/
        inner(firstByte) | mask
      } else {
        mask = leftMask(bound)
        /*while (i >= bound) {
          mask += (1L << i)
          i -= 1
        }*/
        inner(firstByte) & mask
      }
    }

    val default: Long =
       (if (value) 0xFFFFFFFFFFFFFFFFL
       else 0x0000000000000000L)

    if (firstByte == lastByte) {
      val b: Long = {
        if (value) {
          firstByteNewValue & lastByteNewValue
        } else {
          firstByteNewValue | lastByteNewValue
        }
      }
      inner.update(firstByte, b)
    } else {
      inner.update(firstByte, firstByteNewValue)
      var i = firstByte + 1
      while (i < lastByte) {
        inner.update(i, default)
        i += 1
      }
      inner.update(lastByte, lastByteNewValue)
    }
  }

  def clear(bitIndex: Int): Unit =
    _set(bitIndex, _ => false)

  def clear(fromIndex: Int, toIndex: Int): Unit =
    set(fromIndex, toIndex, false)

  def get(index: Int): Boolean = {
    val last = inner(byteOfBitN(index))

    (last & byteMask(index)) > 0
  }

  def nextSetBit(fromIndex: Int): Int =
    _nextBit(fromIndex, x => x)

  def nextClearBit(fromIndex: Int): Int =
    _nextBit(fromIndex, x => !x)

  def _nextBit(fromIndex: Int, f: Boolean => Boolean): Int = {
    var res = -1
    val firstByte = byteOfBitN(fromIndex)
    var i = firstByte

    while (i < inner.size && res == -1) {
      val byte = inner(i)
      val init =
        if (i == firstByte)
          fromIndex % 64
        else
          64
      var k = init
      while (k < 8 && res == -1) {
        if (f((byte & (1L << k)) > 0))
          res = ((i*64)+k)
        k+=1
      }
      i+=1
    }

    if (res == -1) throw new IndexOutOfBoundsException()
    else res
  }

  def previousSetBit(fromIndex: Int): Int =
    _previousBit(fromIndex, x => x)

  def previousClearBit(fromIndex: Int): Int =
    _previousBit(fromIndex, x => !x)

  def _previousBit(fromIndex: Int, f: Boolean => Boolean): Int = {
    var res = -1
    val firstByte = byteOfBitN(fromIndex)
    var i = firstByte
    while (i >= 0 && res == -1) {
      val byte = inner(i)
      val end =
        if (i == firstByte)
          /*64 - */(fromIndex % 64)
        else
          64
      var k = end - 1
      while (k >= 0 && res == -1) {
        if (f((byte & (1L << k)) > 0))
          res = ((i*64)+(k))
        k-=1
      }
      i-=1
    }

    if (res == -1) throw new IndexOutOfBoundsException()
    else res
  }

  def and(set: BitSet): Unit = {
    var i = 0
    while (i < byteSize) {
      inner.update(i, (inner(i) & set.inner(i)).toByte)
      i+=1
    }
  }

  def or(set: BitSet): Unit = {
    var i = 0
    while (i < byteSize) {
      inner.update(i, (inner(i) | set.inner(i)).toByte)
      i+=1
    }
  }

  def size(): Int =
    _size

  private val bitNumber: Map[Long, Int] = Map(
      0x00L -> 0,
      0x01L -> 1,
      0x02L -> 1,
      0x03L -> 2,
      0x04L -> 1,
      0x05L -> 2,
      0x06L -> 2,
      0x07L -> 3,
      0x08L -> 1,
      0x09L -> 2,
      0x0AL -> 2,
      0x0BL -> 3,
      0x0CL -> 2,
      0x0DL -> 3,
      0x0EL -> 3,
      0x0FL -> 4
      )

  def length(): Int = {
    var res = 0
    var i = 0
    while (i < inner.size) {
      val byte = inner(i)
      var k = 0
      while (k < 8) {
        if ((byte & (1 << k)) > 0)
          res = ((i*8)+k+1)
        k+=1
      }
      i+=1
    }
    res
  }

  def cardinality(): Int = {
    var res = 0L
    var i = 0
    while (i < byteSize) {
      val byte = inner(i)
      var k = 0
      while (k < 16) {
        res += bitNumber(byte >> 4*k) & 0x0000000FL
      }
      i+=1
    }
    res.toInt
  }

  override def clone(): BitSet = {
    val ret = new BitSet(_size)
    var i = 0
    while (i < byteSize) {
      ret.inner.update(i, inner(i))
      i += 1
    }

    ret
  }

  override def toString(): String = {
    val sb = new StringBuilder()
    sb.append("{")
    var first = false
    for (i <- 0 until inner.size) {
      val byte = inner(i)
      for (k <- 0 until 64) {
        if ((byte & (1L << k)) > 0L) {
          if (!first)
            first = true
          else
            sb.append(", ")

          sb.append(i*8+k)
        }
      }
    }
    sb.append("}")
    sb.toString
  }

}
