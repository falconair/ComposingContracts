import java.time.{Period, LocalDate, Duration}
import java.time.temporal.ChronoUnit
import scala.reflect.ClassTag
import java.util._
import scala.ref.WeakReference
import scala.collection.JavaConverters._

/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  *         Thanks to #scala members on freenode:
  *         tpolecat
  *         Kristien
  *
  *         Contracts presentation:
  *         http://www.cs.uu.nl/docs/vakken/afp/Presentations/Contracts.pdf
  *
  *         Pricing composable contracts on the GP-GPU:
  *         http://hiperfit.dk/pdf/WerkAhnfelt_2011-10ab.pdf
  *
  *         AlexanderAA's haskell implementation
  *         https://github.com/AlexanderAA/haskell-contract-valuation
  *
  *         Anton Van Straaten
  *         http://contracts.scheming.org
  *
  */
class ComposingContracts {

  //Contract primitives
  abstract class Contract
  case class Zero     ()                                                             extends Contract
  case class One      (currency: String)                                             extends Contract
  case class Give     (contract: Contract)                                           extends Contract
  case class And      (contract1: Contract, contract2: Contract)                     extends Contract
  case class Or       (contract1: Contract, contract2: Contract)                     extends Contract
  case class Cond     (cond: Obs[Boolean], contract1: Contract, contract2: Contract) extends Contract
  case class Scale    (scale: Obs[Double], contract: Contract)                       extends Contract
  //case class When     (cond: Obs[Boolean], contract: Contract)                     extends Contract
  case class When     (date: LocalDate, contract: Contract)                          extends Contract
  case class Anytime  (cond: Obs[Boolean], contract: Contract)                       extends Contract
  case class Until    (cond: Obs[Boolean], contract: Contract)                       extends Contract

  //Observable primitives
  abstract class Obs[A] {

    def +(that:Obs[A])(implicit n: Numeric[A]):Obs[A] = Lift2Obs((a:A,b:A)=> n.plus(a,b), this,that)
    def -(that:Obs[A])(implicit n: Numeric[A]):Obs[A] = Lift2Obs((a:A,b:A)=> n.minus(a,b), this,that)
    def *(that:Obs[A])(implicit n: Numeric[A]):Obs[A] = Lift2Obs((a:A,b:A)=> n.times(a,b), this,that)
    //def /(that:Obs[A])(implicit n: Numeric[A]):Obs[A] = Lift2Obs((a:A,b:A)=> n.times(a,1.0/b), this,that)

    def ==(that:Obs[A])(implicit n: Ordering[A]):Obs[Boolean] = Lift2Obs((a:A,b:A)=> n.==(a,b), this,that)
    def !=(that:Obs[A])(implicit n: Ordering[A]):Obs[Boolean] = Lift2Obs((a:A,b:A)=> n.!=(a,b), this,that)
    def >(that:Obs[A])(implicit n: Ordering[A]):Obs[Boolean] = Lift2Obs((a:A,b:A)=> n.gt(a,b), this,that)
    def <(that:Obs[A])(implicit n: Ordering[A]):Obs[Boolean] = Lift2Obs((a:A,b:A)=> n.lt(a,b), this,that)
    def >=(that:Obs[A])(implicit n: Ordering[A]):Obs[Boolean] = Lift2Obs((a:A,b:A)=> n.gteq(a,b), this,that)
    def <=(that:Obs[A])(implicit n: Ordering[A]):Obs[Boolean] = Lift2Obs((a:A,b:A)=> n.lteq(a,b), this,that)

  }
  case class Konst[A]          (k: A)                                        extends Obs[A]
  case class LiftObs[B, A]     (lifted: (B) => A, o: Obs[B])                 extends Obs[A]
  case class Lift2Obs[C, B, A] (lifted: (C, B) => A, o1: Obs[C], o2: Obs[B]) extends Obs[A]
  case class ReturnObs[A]      (transform: (A) => A, startCond:A,o: Obs[A])  extends Obs[A]
  case class DateObs           ()                                            extends Obs[LocalDate]

  //Process primitives
  abstract class PR[A] {

    def +(that:PR[A])(implicit n: Numeric[A]):PR[A] = Lift2PR(n.plus, this,that)
    def -(that:PR[A])(implicit n: Numeric[A]):PR[A] = Lift2PR(n.minus, this,that)
    def *(that:PR[A])(implicit n: Numeric[A]):PR[A] = Lift2PR(n.times, this,that)
    //def /(that:PR[A])(implicit n: Numeric[A]):PR[A] = Lift2PR((a:A,b:A)=> n.times(a,1.0/b), this,that)

    def ==(that:PR[A])(implicit n: Ordering[A]):PR[Boolean] = Lift2PR((a:A,b:A)=> n.==(a,b), this,that)
    def !=(that:PR[A])(implicit n: Ordering[A]):PR[Boolean] = Lift2PR((a:A,b:A)=> n.!=(a,b), this,that)
    def >(that:PR[A])(implicit n: Ordering[A]):PR[Boolean] = Lift2PR((a:A,b:A)=> n.gt(a,b), this,that)
    def <(that:PR[A])(implicit n: Ordering[A]):PR[Boolean] = Lift2PR((a:A,b:A)=> n.lt(a,b), this,that)
    def >=(that:PR[A])(implicit n: Ordering[A]):PR[Boolean] = Lift2PR((a:A,b:A)=> n.gteq(a,b), this,that)
    def <=(that:PR[A])(implicit n: Ordering[A]):PR[Boolean] = Lift2PR((a:A,b:A)=> n.lteq(a,b), this,that)

  }
  case class K[A]             (k: A)                                      extends PR[A]
  case class DatePR           ()                                          extends PR[LocalDate]
  case class CondPR[A]        (cond: PR[Boolean], a: PR[A], b: PR[A])     extends PR[A]
  case class LiftPR[B, A]     (lifted: (B) => A, o: PR[B])                extends PR[A]
  case class Lift2PR[C, B, A] (lifted: (C, B) => A, o1: PR[C], o2: PR[B]) extends PR[A]
  case class ReturnPR[A]      (transform: (A) => A, o: PR[A])             extends PR[A]
  case class Snell[A]         (o: PR[Boolean], c: PR[Double])             extends PR[Double]
  //case class Disc[A]          (o: PR[Boolean], c: PR[Double])             extends PR[Double]
  case class Disc[A]          (date:LocalDate, c: PR[Double])             extends PR[Double]
  case class Absorb[A]        (o: PR[Boolean], c: PR[Double])             extends PR[Double]
  case class Exch[A]          (curr: String)                              extends PR[Double]

  type RV[A] = (LocalDate, Int)=>A //TODO: WRONG!


  def contractValuation(contract: Contract): PR[Double] = contract match {
    case Zero() => K(0)
    case One(currency) => Exch(currency)
    //case Give(c: Contract) => LiftPR((a: Double) => -1 * a, contractValuation(c))
    case Give(c: Contract)                     => contractValuation(Scale(Konst(-1),c))
    case Scale(o: Obs[Double], c: Contract)    => Lift2PR((a: Double, b: Double) => a * b, observableValuation(o), contractValuation(c))
    case And(c1: Contract, c2: Contract)       => Lift2PR((a: Double, b: Double) => a + b, contractValuation(c1), contractValuation(c2))
    case Or(c1: Contract, c2: Contract)        => Lift2PR((a: Double, b: Double) => Math.max(a, b), contractValuation(c1), contractValuation(c2))
    case Cond(o: Obs[Boolean], c1: Contract, c2: Contract) => CondPR(observableValuation(o), contractValuation(c1), contractValuation(c2))
    //case When(o: Obs[Boolean], c: Contract)    => Disc(observableValuation(o), contractValuation(c))
    case When(date: LocalDate, c: Contract)    => Disc(date, contractValuation(c))
    case Anytime(o: Obs[Boolean], c: Contract) => Snell(observableValuation(o), contractValuation(c))
    case Until(o: Obs[Boolean], c: Contract)   => Absorb(observableValuation(o), contractValuation(c))
  }

  def observableValuation[A](observable: Obs[A]): PR[A] = observable match {
    case Konst(k)                 => K(k)
    case LiftObs(func, o: Obs[A]) => LiftPR(func, observableValuation(o))
    case Lift2Obs(func, o1, o2)   => Lift2PR(func, observableValuation(o1), observableValuation(o2))
    case DateObs()                => DatePR()
    case ReturnObs                => ReturnPR()
  }

  def prValuation[A](pr: PR[A], marketData:MarketData): RV[A] = pr match {
    case K(k) => (date: LocalDate, latticeIdx: Int) => k
    case DatePR() => (date: LocalDate, latticeIdx: Int) => date
    case CondPR(cond: PR[Boolean], a: PR[A], b: PR[A]) => {
      val o = prValuation(cond,marketData)
      val ca = prValuation(a,marketData)
      val cb = prValuation(b,marketData)
      
      (date: LocalDate, latticeIdx: Int) => {
        if (o(date, latticeIdx)) ca(date, latticeIdx)
        else cb(date, latticeIdx)
      }
    }
    case LiftPR(lifted, o) => {
      val obs = prValuation(o,marketData)
      (date: LocalDate, latticeIdx: Int) => lifted(obs(date, latticeIdx))
    }
    case Lift2PR(lifted, o1, o2) => {
      val obs1 = prValuation(o1,marketData)
      val obs2 = prValuation(o2,marketData)
      (date: LocalDate, latticeIdx: Int) => lifted(obs1(date, latticeIdx), obs2(date, latticeIdx))
    }
    case Exch(curr: String ) => {
      val exchangeRate = marketData.exchangeRates(curr)
      (date: LocalDate, latticeIdx: Int) => exchangeRate.get(ChronoUnit.DAYS.between (LocalDate.now (), date).toInt,latticeIdx)
    }
    case Disc(date:LocalDate, c: PR[Double]) => {
      /*
       * Give contract and it's lattice, say foreign currency, lattice is full of values
       * for wrong dates, set contract lattice values to zero, only values remain at date
       * starting with date-1, work backwards: average of child nodes, take present value using interest rate lattice at corresponding node
       */

      val daysUntilMaturity = ChronoUnit.DAYS.between(LocalDate.now(), date).toInt
      val con = prValuation(c,marketData)
      
      //val irLattice = BinomialLattice.binomialPriceTree(date,daysUntilMaturity,interestRate,interestRateVol)
      val irLattice = marketData.interestRate
      val valueLattice = BinomialLattice.populate(date,daysUntilMaturity,(i:Int, j:Int)=>con(ChronoUnit.DAYS.addTo(LocalDate.now(),i),j))
      val discountedLattice = BinomialLattice.discount(valueLattice,irLattice)

      (date: LocalDate, latticeIdx: Int) => discountedLattice.get(ChronoUnit.DAYS.between (LocalDate.now (), date).toInt,latticeIdx)
    }
    //case ReturnPR(transform, startCondition) => K(1.0) //TODO:
    //case Snell(o: PR[Boolean], c: PR[Double]) => K(1.0) //TODO: 
    //case Absorb(o: PR[Boolean], c: PR[Double]) => K(1.0) //TODO: 
  }

  case class MarketData(interestRate:BinomialLattice[Double], exchangeRates:collection.mutable.Map[String,BinomialLattice[Double]])
}

object Main extends App {

  val cc = new ComposingContracts

  //val lattice = BinomialLattice.binomialPriceTree(LocalDate.now.plus(1,ChronoUnit.YEARS),3,30, .25)
  //val lattice = BinomialLattice.binomialPriceTree(LocalDate.now.plus(1,ChronoUnit.YEARS),12,100, .05)
  //val lattice = BinomialLattice.binomialPriceTree(LocalDate.now.plus(46,ChronoUnit.DAYS),8,142.410,.182)
  //lattice.printLattice()
  //printLattice(cc.discountLattice(lattice),3)

  //Custom combinators
  //def at(date: LocalDate): cc.Obs[Boolean] = cc.Lift2Obs((a: LocalDate, b: LocalDate) => a.compareTo(b) == 0, cc.DateObs(), cc.Konst(date))

  //Tests
  val exchangeRates = collection.mutable.Map(
    "USD" -> BinomialLattice.binomialPriceTree(LocalDate.now().plus(1,ChronoUnit.YEARS),365,1,0),
    "GBS" -> BinomialLattice.binomialPriceTree(LocalDate.now().plus(1,ChronoUnit.YEARS),365,1.55,.0467),
    "EUR" -> BinomialLattice.binomialPriceTree(LocalDate.now().plus(1,ChronoUnit.YEARS),365,1.21,.0515)
  )
  val marketData = cc.MarketData(
    BinomialLattice.binomialPriceTree(LocalDate.now().plus(1,ChronoUnit.YEARS),365,.05,.05), //interest rate (use a universal rate for now)
    exchangeRates //exchange rates
  )

  //custom contract
  def zcb(maturity:LocalDate, notional:Double, currency:String) = cc.When(maturity, cc.Scale(cc.Konst(notional),cc.One(currency)))
  def europeanOption(at:LocalDate, c1:cc.Contract) = cc.When(at,cc.Or(c1,cc.Zero()))
  //def american(c:cc.Contract, date1:LocalDate, date2:LocalDate) = cc.Anytime(between(date1,date2),c)
  //custom observable
  //def between(date1:LocalDate, date2:LocalDate):cc.Obs[Boolean] = cc.Lift2Obs((a,b)=> a >= date1 && a <= date2, cc.Konst(date1), cc.Konst(date2))
  //val msft:cc.Obs[Double] = BinomialLattice.binomialPriceTree(LocalDate.now().plus(1,ChronoUnit.YEARS),365,46.45,.2)

  //portfolio test
  val portfolio = Array(
    cc.One("USD"),
    cc.One("EUR"),
    cc.Scale(cc.Konst(-1),cc.One("USD")),
    cc.Give(cc.One("USD")),
    cc.Give(cc.Give(cc.One("USD"))),
    cc.And(cc.One("USD"), cc.One("USD")),
    cc.And(cc.One("USD"), cc.Give(cc.One("USD"))),
    //cc.When(at(LocalDate.now().plus(2, ChronoUnit.DAYS)), cc.One("USD"))
    cc.When(LocalDate.now().plus(5, ChronoUnit.DAYS), cc.One("USD"))
    //cc.When(LocalDate.now().plus(5, ChronoUnit.DAYS), cc.Scale(msft, cc.One("USD")))
  )
  
  for(contract <- portfolio){
    println("===========")
    val vp = cc.contractValuation(contract)
    val rv = cc.prValuation(vp, marketData)
    println("Contract: "+contract)
    println("Value Process: "+vp)
    println("Random Variable: "+rv)
    println("Random Variable contents: ")
    rv.printLattice()
  }

}

class BinomialLattice[A:ClassTag](val terminalDate:LocalDate, val periods:Int, upProbability:Double=0.5) {

  //def this(fromNow:Int, chronoUnit: ChronoUnit, upProbability:Double=0.5) = this(LocalDate.now().plus(fromNow,chronoUnit),fromNow,upProbability)

  val lattice = new Array[Array[A]](periods)
  var _size = periods
  
  for(i<-0 to (periods-1)){
    lattice(i) = new Array[A](i+1)
  }

  //TODO: just create a view over an existing bl, don't waste memory
  def map[B:ClassTag](func:(A)=>B):BinomialLattice[B] = {
    val bl = new BinomialLattice[B](terminalDate,periods,upProbability)
    for(i <- 0 to _size-1){
      for(j <- 0 to i){
        bl.set(i,j, func(lattice(i)(j)))
      }
    }
    bl
  }
  def size():Int = _size
  def probability():Double = upProbability
  def get(i:Int,j:Int):A = lattice(i)(j)
  def set(i:Int,j:Int,value:A) = lattice(i)(j) = value //TODO: private?
  def printLattice():Unit={
    for(days <- 0 to _size-1){
      for(idx <- 0 to days){
        print(lattice(days)(idx))
        print('\t')
      }
      println("")
    }
  }
}

object BinomialLattice {
  def populate[A:ClassTag](terminalDate:LocalDate, periods:Int, func:(Int,Int)=>A):BinomialLattice[A] = {
    val bl = new BinomialLattice[A](terminalDate,periods)
    for(i <- 0 to bl.size()-1){
      for(j <- 0 to i){
        bl.set(i,j,func(i,j))
      }
    }
    bl
  }
  def discount(toDiscount:BinomialLattice[Double], interestRates:BinomialLattice[Double] /*,terminalIndex:Int*/):BinomialLattice[Double] = {
    val bl = new BinomialLattice[Double](toDiscount.terminalDate,toDiscount.periods)
    val terminalIndex = bl.size()

    //populate values for terminal date
    for(j <- 0 to terminalIndex-1){
      bl.set(terminalIndex-1,j, toDiscount.get(terminalIndex-1,j))
    }

    //populate discounted values for all non-terminal dates
    for( i <- (0 to (terminalIndex -2)).reverse){
      for(j <- 0 to i){
        val upVal = bl.get(i+1,j+1)
        val downVal = bl.get(i+1,j)
        val expectedValue = bl.probability() * upVal + (1.0-bl.probability()) * downVal //probabilities are simplified to 1/2
        bl.set(i,j, expectedValue/(1.0+interestRates.get(i,j)))
      }
    }

    bl
  }
  def binomialPriceTree(terminalDate:LocalDate, periods:Int, startVal:Double, annualizedVolatility:Double, upProb:Double=0.5):BinomialLattice[Double] = {
    val bl = new BinomialLattice[Double](terminalDate, periods)

    val businessDaysInYear = 365.0
    val daysUntilTermination = ChronoUnit.DAYS.between(LocalDate.now(),terminalDate)*1.0
    val daysInPeriod = daysUntilTermination/(periods*1.0)
    val fractionOfYear = daysInPeriod/(businessDaysInYear*1.0)
    val changeFactorUp = Math.exp(annualizedVolatility* 1.0 * Math.sqrt(fractionOfYear))
    val changeFactorDown = 1/changeFactorUp

    for(i <- 0 to (bl.size-1)){
      for(j <- 0 to i){
        if(i == 0) bl.set(i,j, startVal)
        else{
          val prevNode = if((j-1)<0) 0 else j-1
          val prevValue = bl.get(i-1,prevNode)
          val isUp = j > prevNode
          val factor = if(isUp) changeFactorUp else changeFactorDown
          bl.set(i,j,prevValue * factor)
        }
      }
    }
    bl
  }
}