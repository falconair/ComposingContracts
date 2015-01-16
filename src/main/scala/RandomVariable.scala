/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  */

package object RandomVariable{
  type RandomVariable[A] = (Int)=>A
}
/*trait RV[A]{
  def apply(i:Int):A
  def expectedValue:A
}*/
