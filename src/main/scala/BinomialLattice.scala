import RandomVariable._
import scala.collection.mutable.ListBuffer//may need to 'grow' lattice on-demand, else just use Array
import scala.reflect.ClassTag

/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  */
package BinomialLattice {

import java.text.DecimalFormat

trait BinomialLattice[A]{
    def apply(i:Int):RandomVariable[A]
    //def size():Option[Int] = None//either bounded size or None
    def probability():Double = 0.5//probability is assumed to be uniform in this implementation (in other words, ignored)
    def startVal():A = apply(0)(0)
    def lift[B](func:(A)=>B):BinomialLattice[B] = {//map is probably a better name for this
      new BinomialLattice[B] {
        override def apply(i: Int): RandomVariable[B] = (j:Int)=>func(BinomialLattice.this(i)(j))
        //override def size() = BinomialLattice.this.size()
      }
    }
    def lift[B,C](func:(A,B)=>C,lattice:BinomialLattice[B]):BinomialLattice[C] = {
      new BinomialLattice[C] {
        override def apply(i: Int): RandomVariable[C] = {
          val lat1 = BinomialLattice.this(i)
          val lat2 = lattice(i)
          (j:Int)=>func(lat1(j),lat2(j))
        }
        //override def size() = BinomialLattice.this.size()
      }
    }
    def fold[B](func:(A)=>B):BinomialLattice[B] = {//this is not a real fold function, used to transform lattice when more than one index required
      new BinomialLattice[B] {
        override def apply(i: Int): RandomVariable[B] = (j:Int)=>func(BinomialLattice.this(i)(j))
        //override def size() = BinomialLattice.this.size()
      }
    }
    override def toString() =  {
      val formatter = new DecimalFormat("#.####")
        val str = new StringBuilder
        for(i <- 0 to 3){
          for(j <- 0 to i){
            val formatted = formatter.format(apply(i)(j))
            str.append(formatted).append('\t')
          }
          str.append('\n')
        }
        str.append("...")
        str.toString()
    }

  }
  trait BinomialLatticeBounded[A] extends BinomialLattice[A]{
    val formatter = new DecimalFormat("#.####")
    def size():Int
    override def toString() = {
        val str = new StringBuilder
        for(i <- 0 to size-1){
          for(j <- 0 to i){
            val formatted = formatter.format(apply(i)(j))
            str.append(formatted).append('\t')
          }
          str.append('\n')
        }
        str.toString()
    }
  }
  class ConstantBL[A](k:A) extends BinomialLattice[A]{
    override def apply(i:Int) = (j:Int)=>k
  }
  class IdentityBL() extends BinomialLattice[Int]{
    override def apply(i:Int) = (j:Int)=>i
  }
  class GenerateBL(_size:Int, startVal:Double, upFactor:Double, upProbability:Double=0.5) extends BinomialLatticeBounded[Double]{
    val cache = new ListBuffer[Array[Double]]
    val downFactor:Double = 1.0/upFactor

    cache.insert(0,Array(startVal))
    for(i <- 1 to _size-1){
      val arr = new Array[Double](i+1)
      arr(0) = downFactor * cache(i-1)(0)
      for(j <- 1 to i){
        arr(j) = upFactor * cache(i-1)(j-1)
      }
      cache.insert(i,arr)
    }
    override def apply(i:Int) = cache(i)
    override def size() = _size
  }
  class TransformBL[A:ClassTag](source:BinomialLatticeBounded[A], func:((A,A)=>A)) extends BinomialLatticeBounded[A]{
    val cache = new ListBuffer[Array[A]]

    if (size() == 0) source
    else{
      for(i <- 0 to size-1) cache.insert(i, new Array[A](i+1))

      for(i <- (0 to size-1).reverse){
        for(j <- (0 to i)){
          val x = source(i+1)(j)
          val y = source(i+1)(j+1)
          cache(i)(j) = func(x,y)
        }
      }
      cache
    }
    override def apply(i:Int) = cache(i)
    override def size() = source.size()-1
  }

}
