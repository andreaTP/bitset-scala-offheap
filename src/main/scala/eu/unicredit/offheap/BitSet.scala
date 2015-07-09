package eu.unicredit.offheap

import scala.offheap._

class BitSet(nbits: Int)(implicit a: Allocator) {
  
  def this()(implicit a: Allocator) =
    this(64)
  
  private var size = {
    var _size = 64
    while (_size < nbits)
      _size += 64
    
    _size
  }
  
  def byteSize() = ((size / 8) + 1)
  
  def byteOfBitN(n: Int) =
    n / 8    

  def byteMask(n: Int) =
    1 << (n % 8)
    
  private var inner: Array[Byte] = Array.fill(byteSize)(0)
    
  
  def clear() =
    inner = Array.uninit[Byte](byteSize())
    
  def set(index: Int): Unit = {
    val bytePos = byteOfBitN(index)
    val bitMask = byteMask(index)
    val last = inner(bytePos)
    
    if ((last & bitMask) == 0)
      inner.update(bytePos, (last | bitMask).toByte)
  }
  
  def get(index: Int): Boolean = {
    val last = inner(byteOfBitN(index))
    
    (last & byteMask(index)) > 0
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