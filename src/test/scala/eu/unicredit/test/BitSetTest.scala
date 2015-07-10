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
  
  it should "flip bit at specified index" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    bs.set(0,2,true)
    
    jbs.set(0,2,true)
    
    bs.flip(1)
    jbs.flip(1)
    
    assert{bs.get(1) == false}
    assert{bs.get(1) == jbs.get(1)}
    
    bs.flip(5)
    jbs.flip(5)
    
    assert{bs.get(5) == true}
    assert{bs.get(5) == jbs.get(5)}
  }
  
  it should "flip bit in a range" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    bs.set(0,2,true)
    
    jbs.set(0,2,true)
    
    bs.flip(1,5)
    jbs.flip(1,5)
    
    assert{bs.get(1) == false}
    assert{bs.get(1) == jbs.get(1)}
    assert{bs.get(2) == true}
    assert{bs.get(2) == jbs.get(2)}
    assert{bs.get(3) == true}
    assert{bs.get(3) == jbs.get(3)}
  }

  it should "get next set bit" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    bs.set(5)
    
    jbs.set(5)
    
    assert{bs.nextSetBit(2) == 5}
    assert{bs.nextSetBit(2) == jbs.nextSetBit(2)}
  }
  
  it should "get next clear bit" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    bs.set(0,5)
    
    jbs.set(0,5)
    
    assert{bs.nextClearBit(2) == 5}
    assert{bs.nextClearBit(2) == jbs.nextClearBit(2)}
  }
  
  it should "get previous set bit" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    bs.set(5)
    
    jbs.set(5)
    
    assert{bs.previousSetBit(8) == 5}
    assert{bs.previousSetBit(8) == jbs.previousSetBit(8)}
  }
  
  /* there is a bug in set to be fixed
  it should "get previous clear bit" in {
    val bs = new BitSet()
    val jbs = new juBitSet()
    
    bs.set(6, 10)
    
    jbs.set(6, 10)
    
    //assert{bs.previousClearBit(8) == 5}
    assert{bs.previousClearBit(8) == jbs.previousClearBit(8)}
  }
  */
}