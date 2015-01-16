/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  */
package RandomProcess {

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import RandomVariable.RandomVariable

import scala.reflect.ClassTag

//Processes where they are unbounded, but grow only on demand (and can reduce in size again?)
//Processes where only right most RV is required so whole tree is not instantiated?
trait RandomProcess[A]{
  def apply(i:Int):RandomVariable[A]
  def apply(date:LocalDate):RandomVariable[A] = apply(ChronoUnit.DAYS.between(LocalDate.now(),date).toInt)

  // is there a general trait which defines this function?
  def map[B](mapf: (A)=>B):RandomProcess[B] = {
    return new RandomProcess[B]{
      override def apply(i:Int) = {
        val rv: RandomVariable[A] = RandomProcess.this(i)
        (idx:Int) => mapf(rv(idx))
      }
      override def size() = RandomProcess.this.size()
      override def probability() = RandomProcess.this.probability()
    }
  }
  def size():Option[Int] = None//either bounded size or None
  def probability():Double = 0.5//?
}
trait RandomProcessMutable[A] extends RandomProcess[A]{
  def set(date:LocalDate, j:Int, value:A):Unit = set(ChronoUnit.DAYS.between(LocalDate.now(),date).toInt,j,value) //keep LocalDate for semantic clarity or switch to Int?
  def set(i:Int, j:Int, value:A):Unit
}
class RandomProcessMemoryLess[A](func:(LocalDate)=>RandomVariable[A]) extends RandomProcess[A]{
  override def apply(date:LocalDate) = func(date)
  override def apply(i:Int) = apply(LocalDate.now().plusDays(i))
}
class RandomProcessBounded[A:ClassTag](_size:Int) extends RandomProcessMutable[A]{
  val cached = new Array[Array[A]](_size)
  for(i <- 0 to cached.length-1) cached(i) = new Array[A](i+1) //initialize array

  override def apply(i:Int) = (idx:Int)=>cached(i)(idx)
  override def size() = Some(_size)
  override def set(i:Int, j:Int, value:A) = cached(i)(j) = value
  override def toString() = {
    val str = new StringBuilder
    for(i <- 0 to _size-1){
      for(j <- 0 to i) str.append(cached(i)(j)).append(',')
      str.append('\n')
    }
    str.toString()
  }
}
/*class RandomProcessPassThrough[A](func:(LocalDate)=>A) extends RandomProcess[A]{
  override def apply(date:LocalDate) = func(date)
  override def apply(i:Int) = apply(LocalDate.now().plusDays(i))
}*/

/*trait RV[A]{
  def apply(i:Int):A
  def expectedValue:A
}*/
}
