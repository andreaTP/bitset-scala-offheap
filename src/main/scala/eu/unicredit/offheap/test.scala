package eu.unicredit.offheap

object test extends App {

  println("testing bitsets...")
  
  def now() = System.currentTimeMillis()
  
  val samples = 10000000
  
  val bsSize = 10000
  
  implicit val alloc = scala.offheap.malloc
  
  val bs = new BitSet(bsSize)
  val beforeOffheap = now()
  
  for (_ <- 0 until samples) {
    bs.set(1)
    bs.set(3)
    bs.set(5)
    bs.set(7)
    bs.set(9)
    bs.set(11)
    bs.set(13)
    bs.set(15)
    
    bs.set(bsSize-1)
    bs.set(bsSize-3)
    bs.set(bsSize-5)
    bs.set(bsSize-7)
    bs.set(bsSize-9)
    bs.set(bsSize-11)
    bs.set(bsSize-13)
    bs.set(bsSize-15)
    
    assert(bs.get(1) == true)
    assert(bs.get(2) == false)
    assert(bs.get(3) == true)
    assert(bs.get(4) == false)
    assert(bs.get(5) == true)
    assert(bs.get(6) == false)
    assert(bs.get(7) == true)
    assert(bs.get(8) == false)
    //bs.clear()
  }
  
  val afterOffheap = now()
  
  val bs2 = new java.util.BitSet(bsSize)
  val beforeJU = now()
  
  for (_ <- 0 until samples) {
    bs2.set(1)
    bs2.set(3)
    bs2.set(5)
    bs2.set(7)
    bs2.set(9)
    bs2.set(11)
    bs2.set(13)
    bs2.set(15)

    bs2.set(bsSize-1)
    bs2.set(bsSize-3)
    bs2.set(bsSize-5)
    bs2.set(bsSize-7)
    bs2.set(bsSize-9)
    bs2.set(bsSize-11)
    bs2.set(bsSize-13)
    bs2.set(bsSize-15)
    
    
    assert(bs2.get(1) == true)
    assert(bs2.get(2) == false)
    assert(bs2.get(3) == true)
    assert(bs2.get(4) == false)
    assert(bs2.get(5) == true)
    assert(bs2.get(6) == false)
    assert(bs2.get(7) == true)
    assert(bs2.get(8) == false)
    //bs.clear()
  }

  val afterJU = now()
  
  println("Performances are")
  println("Mine -> "+(afterOffheap-beforeOffheap))
  println("JU   -> "+(afterJU-beforeJU))
  
}