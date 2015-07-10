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
  
  def byteSize() = ((_size / 8) + 1)
  
  def byteOfBitN(n: Int) =
    n / 8    

  def byteMask(n: Int) =
    1 << (n % 8)
  
  def rightMask(n: Int) =
    (for (i <- 0 until (n%8))
    yield 1 << i).sum
    
  def leftMask(n: Int) =
    (for (i <- 0 until (n%8))
    yield 1 << (7-i)).sum

  private var inner: Array[Byte] = Array.fill(byteSize)(0)

  def clear() =
    for (i <- 0 until byteSize)
      inner.update(i, 0.toByte)
    
  def set(index: Int): Unit =
    set(index, true)

  def set(index: Int, value: Boolean): Unit = {  
    val bytePos = byteOfBitN(index)
    val bitMask = byteMask(index)
    val last = inner(bytePos)

    val oldValue = ((last & bitMask) != 0)
    if (oldValue != value) {
      if (value)
        inner.update(bytePos, (last | bitMask).toByte)
      else
        inner.update(bytePos, (last & (0xFF - bitMask)).toByte)
    }
  }
  
  def set(fromIndex: Int, toIndex: Int, value: Boolean): Unit = {
    val bytesPos =
      (for (i <- fromIndex until toIndex)
      yield byteOfBitN(i)).distinct
         
    val firstByte = byteOfBitN(fromIndex)
    val lastByte = byteOfBitN(toIndex)
    val default =
       if (value) 0xFF
       else 0x00
       
    val values =
      for (i <- bytesPos) yield {
        {if (i == firstByte)
          if (value)
            leftMask(fromIndex) | inner(i)
          else
            rightMask(fromIndex) & inner(i)
        else 
          0x00} |
        {if (i == lastByte)
          if (value)
            rightMask(toIndex) | inner(i)
          else
            leftMask(toIndex) & inner(i)
        else 
          0x00} |
        {if (i != lastByte && i != firstByte)
          default
        else
          0x00
        }
      }
     
    for (i <- bytesPos.zip(values))
      inner.update(i._1, i._2.toByte)
  }

  def get(index: Int): Boolean = {
    val last = inner(byteOfBitN(index))

    (last & byteMask(index)) > 0
  }
  
  def size(): Int =
    _size
    
  def length(): Int = {
    var res = 0
    for (i <- 0 until inner.size) {
      val byte = inner(i)
      for (k <- 0 until 8) {
        if ((byte & (1 << k)) > 0)
          res = ((i*8)+k+1)
      }
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