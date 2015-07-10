package eu.unicredit.test

import org.scalatest._

import java.util.{BitSet => juBitSet}
import eu.unicredit.offheap.BitSet

class BitSetTest extends FlatSpec {

  implicit val alloc = scala.offheap.malloc
  
  "A BitSet" should "be created offheap with a size" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    assert(bs.size() == jbs.size())
    
    val bs2 = new BitSet(120)
    val jbs2 = new juBitSet(120)
    
    assert(bs2.size() == jbs2.size())
  }
  
  it should "store bits" in {
    val bs = new BitSet()
    
    assert(bs.get(0) == false)
    assert(bs.get(1) == false)
    assert(bs.get(2) == false)
    
    bs.set(0)
    bs.set(1)
    bs.set(2)
    
    assert(bs.get(0) == true)
    assert(bs.get(1) == true)
    assert(bs.get(2) == true)
  }
  
  it should "toString true bits" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    assert(bs.toString == "{}")
    assert(bs.toString == jbs.toString)
    
    bs.set(0)
    bs.set(1)
    bs.set(2)
    
    jbs.set(0)
    jbs.set(1)
    jbs.set(2)
    
    assert(bs.toString == "{0, 1, 2}")
    assert(bs.toString == jbs.toString)
  }

  it should "be cleared in one operation" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    bs.set(0)
    bs.set(1)
    bs.set(2)
    
    jbs.set(0)
    jbs.set(1)
    jbs.set(2)
    
    assert(bs.toString != "{}")
    assert(bs.toString == jbs.toString)
    
    bs.clear()
    jbs.clear()
    
    assert(bs.toString == "{}")
    assert(bs.toString == jbs.toString)
  }
  
  it should "retrieve logical length" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    bs.set(0)
    bs.set(1)
    bs.set(2)
    
    jbs.set(0)
    jbs.set(1)
    jbs.set(2)
    
    assert(bs.length() == 3)
    assert(bs.length() == jbs.length())
  }

  it should "set range of bits" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    bs.set(0,2,true)
    
    jbs.set(0,2,true)
    
    assert(bs.toString == "{0, 1}")
    assert(bs.toString == jbs.toString)
    
    bs.set(4, 20, true)
    jbs.set(4, 20, true)
    assert(bs.toString == jbs.toString)
  }

}