import ComposingContractsLatticeImplementation.ComposingContractsLatticeImplementation
import org.scalatest._
/** *
  *
  * @author shahbaz
  *
  */
object Main extends App{

  def randomProcessFinalValTest() = {
    val days = 9
    val startingVal = 100
    val volatility = 0.606966
    val rp = ComposingContractsLatticeImplementation. binomialPriceTree(days,startingVal,volatility)
    val rv = rp(days)
    println(rp)
    //stack.push(2)
    //stack.pop() should be (2)
    //stack.pop() should be (1)
  }

  randomProcessFinalValTest()

}
