package eu.unicredit.offheap

import scala.offheap._

class BitSet(nbits: Int)(implicit a: Allocator) {
  
  def this()(implicit a: Allocator) =
    this(64)
  
  private var _size = {
    var tsize = 64
    while (tsize < nbits)
      tsize += 64
    
    tsize
  }

  private var _lenght = 0
  
  private def byteSize() = ((_size / 8) + 1)
  
  private def byteOfBitN(n: Int) =
    n / 8    

  private def byteMask(n: Int) =
    1 << (n % 8)
    
  private var inner: Array[Byte] = Array.fill(byteSize)(0)

  override def finalize() =
    a.free(inner.addr)
    
  def clear() = {
    var i = 0
    while (i < byteSize) {
      inner.update(i, 0.toByte)
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
        inner.update(bytePos, (last | bitMask).toByte)
      else
        inner.update(bytePos, (last & (0xFF - bitMask)).toByte)
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
    val bytesPos =
      byteOfBitN(fromIndex) to byteOfBitN(toIndex)
         
    val firstByte = byteOfBitN(fromIndex)
    val lastByte = byteOfBitN(toIndex)
    
    val firstByteNewValue = {
      val bound = fromIndex - firstByte*8
      var i = 7
      var mask = 0
      
      if (value) {
        mask = leftMask(bound)
        /*while (i >= bound) {
          mask += (1 << i)
          i -= 1
        }*/
        inner(firstByte) | mask 
      } else {
        mask = rightMask(bound)
        /*
        i = 0
        while (i < bound) {
          mask += (1 << i)
          i += 1
        }
        */
        inner(firstByte) & mask
      }
    }.toByte

    
    val lastByteNewValue = {
      val bound = toIndex - lastByte*8
      var i = 7
      var mask = 0
      
      if (value) {
        mask = rightMask(bound)
        /*i = 0
        while (i < bound) {
          mask += (1 << i)
          i += 1
        }*/
        inner(firstByte) | mask 
      } else {
        mask = leftMask(bound)
        /*while (i >= bound) {
          mask += (1 << i)
          i -= 1
        }*/
        inner(firstByte) & mask
      }
    }.toByte
    
    val default =
       (if (value) 0xFF
       else 0x00).toByte

    if (firstByte == lastByte) {
      val b = { 
        if (value) {
          firstByteNewValue & lastByteNewValue
        } else {
          firstByteNewValue | lastByteNewValue
        }
      }
      inner.update(firstByte, b.toByte)
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
          firstByte % 8
        else
          0
      var k = init
      while (k < 8 && res == -1) {
        if (f((byte & (1 << k)) > 0))
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
          8 - (fromIndex % 8)
        else
          8
      var k = end - 1
      while (k >= 0 && res == -1) {
        if (f((byte & (1 << k)) > 0))
          res = ((i*8)+k)
        k-=1
      }
      i-=1
    }
    
    if (res == -1) throw new IndexOutOfBoundsException()
    else res
  }

  def size(): Int =
    _size

  private val bitNumber: Map[Int, Int] = Map(
      0x00 -> 0,
      0x01 -> 1,
      0x02 -> 1,
      0x03 -> 2,
      0x04 -> 1,
      0x05 -> 2,
      0x06 -> 2,
      0x07 -> 3,
      0x08 -> 1,
      0x09 -> 2,
      0x0A -> 2,
      0x0B -> 3,
      0x0C -> 2,
      0x0D -> 3,
      0x0E -> 3,
      0x0F -> 4
      )
  
  def length(): Int = {
    var res = 0
    var i = 0
    while (i < inner.size) {
      val byte = inner(i)
      res += bitNumber((byte >> 4) & 0x0F) + bitNumber(byte & 0x0F)
      /*var k = 0
      while (k < 8) {
        if ((byte & (1 << k)) > 0)
          res = ((i*8)+k+1)
        k+=1
      }*/
      i+=1
    }
    res
  }

  override def toString(): String = {
    val sb = new StringBuilder()
    sb.append("{")
    var first = false
    for (i <- 0 until inner.size) {
      val byte = inner(i)
      for (k <- 0 until 8) {
        if ((byte & (1 << k)) > 0) {
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