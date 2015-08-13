package eu.unicredit.offheap

object test extends App {

  println("testing bitsets...")
  
    //implicit val alloc = scala.offheap.malloc
    implicit val props = scala.offheap.Region.Props()
    
    implicit val region = scala.offheap.Region.open
  
    //Example
    
    import scala.util.Random
    val mappa =  /*Map(
        "uno" -> List(1,2,3),
        "due" -> List(4,5,6)
    )*/
    (for (i <- 0 to (Random.nextInt(10)*100)) yield {
      Random.nextString(10) -> (for (j <- 0 to (Random.nextInt(50))) yield j).toList 
    }).toMap
    
    import scala.collection.mutable.HashMap
    def makeBitSets[A](lists: Map[String, List[A]]): Map[String, BitSet] = {
      val index = HashMap[A, Int]()
      var count = 0

      lists map {
        case (key, xs) =>
          val bitset = new BitSet()

          xs foreach { x =>
            index get x map { n =>
              bitset set n
            } getOrElse {
              index(x) = count
              bitset set count
              count += 1
            }
        }
        (key -> bitset)
      }
    }
    
    def relatedness[A](aIn: BitSet, bIn: BitSet): Double = {
      val scale = 40000
      val aSize = aIn.size
      val bSize = bIn.size
      val min = aSize min bSize
      val max = aSize max bSize
      val temp = aIn.clone() //loosing time...
      temp and bIn
      val iSize = temp.cardinality()

      //(log(1 + max) - log(1 + iSize)) / (log(scale) - log(min))
      iSize
    }
    
        
  //original
    def originalMakeBitSets[A](lists: Map[String, List[A]]) = {
    val index = HashMap[A, Int]()
    var count = 0

    lists map {
      case (key, xs) =>
        val bitset = scala.collection.mutable.BitSet()

        xs foreach { x =>
          index get x map { n =>
            bitset += n
          } getOrElse {
            index(x) = count
            bitset += count
            count += 1
          }
        }
        (key -> bitset)
    }
  }

  def originalRelatedness[A](aIn: scala.collection.mutable.BitSet, bIn: scala.collection.mutable.BitSet): Double = {
    val scale = 40000
    val aSize = aIn.size
    val bSize = bIn.size
    val min = aSize min bSize
    val max = aSize max bSize
    val iSize = (aIn intersect bIn).size

    //(log(1 + max) - log(1 + iSize)) / (log(scale) - log(min))
    iSize
  }

    println("OFFHEAP")
    val now11 = System.currentTimeMillis()
    val bss1 = makeBitSets(mappa)
    
    for (one <- bss1.values) {
      for (two <- bss1.values) {
        /*println*/(relatedness(one, two))
      }
    }
    val now12 = System.currentTimeMillis()
    println("time -> "+(now12-now11))
    
    println()
    println("STD")
    val now21 = System.currentTimeMillis()
    val bss2 = originalMakeBitSets(mappa)
    
    for (one <- bss2.values) {
      for (two <- bss2.values) {
        /*println*/(originalRelatedness(one, two))
      }
    }
    val now22 = System.currentTimeMillis()
    println("time -> "+(now22-now21))
    /*
    val b = new BitSet(10)
  
    b.set(6,9)
    
    println(b)
    
    b.set(8,9, false)
    
    println(b)
    */
  /*
    b.set(0,2)
    
    println(b)
    
    b.set(0,7)
    
    println(b)
    
    b.set(1,2,false)
    
    println(b)
    */
    System.exit(0)
  
  /*
   * 
@densh notes

ok -> Never use Scala’s for loops in performance critical code. While loops only.
ok -> If method is not part of public api it has to be private.
don't care -> If you don’t explicitly design for inheritance, you should always make class final.
TBD -> Never use your own hand-rolled measurements for performance testing. Use jmh. There is an excellent sbt-jmh plugin. You can have a look at how to use in scala-offheap benchmark suite. I also highly recommend reading all of the examples: http://hg.openjdk.java.net/code-tools/jmh/file/tip/jmh-samples/src/main/java/org/openjdk/jmh/samples/. They can teach a lot on how to benchmark code on JVM properly.
let see -> If you allocate offheap and support all allocators you should always clean up by calling free in the finalizer. This is not necessary if you take implicit region rather than allocator, regions clean themselves up automatically.
don't care -> It’s a good idea to provide a helper that lets you construct bitset as if it was a default collection through something like: BitSet(a, b, c)
equivalent to java one -> It’s a good idea to include class name in the toString printout. It’s even better if toString is effectively the code to construct the copy of the object.
TBD -> Tests, tests are super important. The more the merrier
   * 
   */
  
  /*
   * Example to use

    def makeBitSets[A](lists: Map[String, List[A]]) = {
    val index = HashMap[A, Int]()
    var count = 0

    lists map {
      case (key, xs) =>
        val bitset = BitSet()

        xs foreach { x =>
          index get x map { n =>
            bitset += n
          } getOrElse {
            index(x) = count
            bitset += count
            count += 1
          }
        }
        (key -> bitset)
    }
  }

  def relatedness[A](aIn: BitSet, bIn: BitSet): Double = {
    val scale = 40000
    val aSize = aIn.size
    val bSize = bIn.size
    val min = aSize min bSize
    val max = aSize max bSize
    val iSize = (aIn intersect bIn).size
    da implementare con AND e lenght

    (log(1 + max) - log(1 + iSize)) / (log(scale) - log(min))
  }

   * 
   */
  
  /*
  def now() = System.currentTimeMillis()
  
  val samples = 10000000
  
  val bsSize = Integer.MAX_VALUE / 1000
  
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
    
    bs.set(1, false)
    assert(bs.get(1) == false)
    
    bs.set(3, false)
    assert(bs.get(3) == false)
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
    
    bs2.set(1, false)
    assert(bs2.get(1) == false)
    
    bs2.set(3, false)
    assert(bs2.get(3) == false)
    //bs.clear()
  }

  val afterJU = now()
  
  println("Performances are")
  println("Mine -> "+(afterOffheap-beforeOffheap))
  println("JU   -> "+(afterJU-beforeJU))
  */
  region.close
}