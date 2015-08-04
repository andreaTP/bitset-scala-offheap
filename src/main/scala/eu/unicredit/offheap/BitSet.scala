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
        inner.update(bytePos, (last & (0xFFFFFFFFL - bitMask)))
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
    
  //map lookup is faster than while loop with bit shift...
  private val leftMask: Map[Int, Int] = Map(
      0 -> 255,
      1 -> 254,
      2 -> 252,
      3 -> 248,
      4 -> 240,
      5 -> 224,
      6 -> 192,
      7 -> 128,
      8 -> 0)
      
  private val rightMask: Map[Int, Int] = Map(
      0 -> 0,
      1 -> 1,
      2 -> 3,
      3 -> 7,
      4 -> 15,
      5 -> 31,
      6 -> 63,
      7 -> 127,
      8 -> 255)
      
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
        //mask = leftMask(bound)
        while (i >= bound) {
          mask += (1L << i)
          i -= 1
        }
        inner(firstByte) | mask 
      } else {
        //mask = rightMask(bound)
        i = 0
        while (i < bound) {
          mask += (1L << i)
          i += 1
        }
        inner(firstByte) & mask
      }
    }

    
    val lastByteNewValue = {
      val bound = toIndex - lastByte*64
      var i = 63
      var mask = 0L
      
      if (value) {
        //mask = rightMask(bound)
        i = 0
        while (i < bound) {
          mask += (1L << i)
          i += 1
        }
        inner(firstByte) | mask 
      } else {
        //mask = leftMask(bound)
        while (i >= bound) {
          mask += (1L << i)
          i -= 1
        }
        inner(firstByte) & mask
      }
    }
    
    val default: Long =
       (if (value) 0xFFFFFFFFL
       else 0x00000000L)

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
          firstByte % 64
        else
          64
      var k = init
      while (k < 8 && res == -1) {
        if (f((byte & (1L << k)) > 0))
          res = ((i*8)+k)
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
    var i = byteOfBitN(fromIndex) - 1
    while (i >= 0 && res == -1) {
      val byte = inner(i)
      val end =
        if (i == firstByte)
          64 - (fromIndex % 64)
        else
          64
      var k = end - 1
      while (k >= 0 && res == -1) {
        if (f((byte & (1L << k)) > 0))
          res = ((i*8)+k)
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