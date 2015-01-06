import java.time.{Period, LocalDate, Duration}
import java.time.temporal.ChronoUnit
import ComposingContracts._

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
package object ComposingContracts{
  type RV[A] = (Int)=>A
}
package ComposingContracts{

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
  case class ReturnObs[A]      (transform: (A) => A, startCond:A)            extends Obs[A]
  case class DateObs           ()                                            extends Obs[LocalDate]

  //Process optimization (declarative) layer
  abstract class PROpt[A] {

    def +(that:PROpt[A])(implicit n: Numeric[A]):PROpt[A] = Lift2PR(n.plus, this,that)
    def -(that:PROpt[A])(implicit n: Numeric[A]):PROpt[A] = Lift2PR(n.minus, this,that)
    def *(that:PROpt[A])(implicit n: Numeric[A]):PROpt[A] = Lift2PR(n.times, this,that)
    //def /(that:PR[A])(implicit n: Numeric[A]):PR[A] = Lift2PR((a:A,b:A)=> n.times(a,1.0/b), this,that)

    def ==(that:PROpt[A])(implicit n: Ordering[A]):PROpt[Boolean] = Lift2PR((a:A,b:A)=> n.==(a,b), this,that)
    def !=(that:PROpt[A])(implicit n: Ordering[A]):PROpt[Boolean] = Lift2PR((a:A,b:A)=> n.!=(a,b), this,that)
    def >(that:PROpt[A])(implicit n: Ordering[A]):PROpt[Boolean] = Lift2PR((a:A,b:A)=> n.gt(a,b), this,that)
    def <(that:PROpt[A])(implicit n: Ordering[A]):PROpt[Boolean] = Lift2PR((a:A,b:A)=> n.lt(a,b), this,that)
    def >=(that:PROpt[A])(implicit n: Ordering[A]):PROpt[Boolean] = Lift2PR((a:A,b:A)=> n.gteq(a,b), this,that)
    def <=(that:PROpt[A])(implicit n: Ordering[A]):PROpt[Boolean] = Lift2PR((a:A,b:A)=> n.lteq(a,b), this,that)

  }
  case class K[A]             (k: A)                                            extends PROpt[A]
  case class DatePR           ()                                                extends PROpt[LocalDate]
  case class CondPR[A]        (cond: PROpt[Boolean], a: PROpt[A], b: PROpt[A])  extends PROpt[A]
  case class LiftPR[B, A]     (lifted: (B) => A, o: PROpt[B])                   extends PROpt[A]
  case class Lift2PR[C, B, A] (lifted: (C, B) => A, o1: PROpt[C], o2: PROpt[B]) extends PROpt[A]
  case class ReturnPR[A]      (transform: (A) => A, start: A)                   extends PROpt[A]
  case class Snell[A]         (o: PROpt[Boolean], c: PROpt[Double])             extends PROpt[Double]
  //case class Disc[A]          (o: PR[Boolean], c: PR[Double])                   extends PR[Double]
  case class Disc[A]          (date:LocalDate, c: PROpt[Double])                extends PROpt[Double]
  case class Absorb[A]        (o: PROpt[Boolean], c: PROpt[Double])             extends PROpt[Double]
  case class Exch[A]          (curr: String)                                    extends PROpt[Double]

  //Why does type have ti be dounded??
  case class MarketData(interestRate:RandomProcessBounded[Double], exchangeRates:collection.mutable.Map[String,RandomProcessBounded[Double]])

  //TODO: move random process to different package
  //Processes where they are unbounded, but grow only on demand (and can reduce in size again?)
  //Processes where only right most RV is required so whole tree is not instantiated?
  trait RandomProcess[A]{
    def apply(i:Int):RV[A]
    def apply(date:LocalDate):RV[A] = apply(ChronoUnit.DAYS.between(LocalDate.now(),date).toInt)

    // is there a general trait which defines this function?
    def map[B](mapf: (A)=>B):RandomProcess[B] = {
      return new RandomProcess[B]{
        override def apply(i:Int) = {
          val rv: RV[A] = RandomProcess.this(i)
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
  class RandomProcessMemoryLess[A](func:(LocalDate)=>RV[A]) extends RandomProcess[A]{
    override def apply(date:LocalDate) = func(date)
    override def apply(i:Int) = apply(LocalDate.now().plusDays(i))
  }
  class RandomProcessBounded[A:ClassTag](_size:Int) extends RandomProcessMutable[A]{
    val cached = new Array[Array[A]](_size)
    for(i <- 0 to cached.length-1) cached(i) = new Array[A](i+1) //initialize array

    override def apply(i:Int) = (idx:Int)=>cached(i)(idx)
    override def size() = Some(_size)
    override def set(i:Int, j:Int, value:A) = cached(i)(j) = value
  }


  object ComposingContracts {
    def contractToPROpt(contract: Contract): PROpt[Double] = contract match {
      case Zero() => K(0)
      case One(currency) => Exch(currency)
      //case Give(c: Contract) => LiftPR((a: Double) => -1 * a, contractValuation(c))
      case Give(c: Contract) => contractToPROpt(Scale(Konst(-1), c))
      case Scale(o: Obs[Double], c: Contract) => Lift2PR((a: Double, b: Double) => a * b, obsToPROpt(o), contractToPROpt(c))
      case And(c1: Contract, c2: Contract) => Lift2PR((a: Double, b: Double) => a + b, contractToPROpt(c1), contractToPROpt(c2))
      case Or(c1: Contract, c2: Contract) => Lift2PR((a: Double, b: Double) => Math.max(a, b), contractToPROpt(c1), contractToPROpt(c2))
      case Cond(o: Obs[Boolean], c1: Contract, c2: Contract) => CondPR(obsToPROpt(o), contractToPROpt(c1), contractToPROpt(c2))
      //case When(o: Obs[Boolean], c: Contract)    => Disc(observableValuation(o), contractValuation(c))
      case When(date: LocalDate, c: Contract) => Disc(date, contractToPROpt(c))
      case Anytime(o: Obs[Boolean], c: Contract) => Snell(obsToPROpt(o), contractToPROpt(c))
      case Until(o: Obs[Boolean], c: Contract) => Absorb(obsToPROpt(o), contractToPROpt(c))
    }

    def obsToPROpt[A](observable: Obs[A]): PROpt[A] = observable match {
      case Konst(k) => K(k)
      case LiftObs(func, o: Obs[A]) => LiftPR(func, obsToPROpt(o))
      case Lift2Obs(func, o1, o2) => Lift2PR(func, obsToPROpt(o1), obsToPROpt(o2))
      case DateObs() => DatePR()
      case ReturnObs(transform, start) => ReturnPR(transform, start)
    }

    //TODO: implementation function which produces Date=>RV, for monte carlo, different function needed
    //each 'case' should be its own function
    //binomial lattice just serves as an implemeentation of date=>rv, could be cachable, lazy, etc.
    def binomialValuation[A](pr: PROpt[A], marketData: MarketData): RandomProcess[A] = pr match {
      case K(k) => new RandomProcessMemoryLess[A]((_) => (_) => k)
      case DatePR() => new RandomProcessMemoryLess[A]((date: LocalDate) => (_) => date)
      case CondPR(cond: PROpt[Boolean], a: PROpt[A], b: PROpt[A]) => {
        val o = binomialValuation(cond, marketData)
        val ca = binomialValuation(a, marketData)
        val cb = binomialValuation(b, marketData)

        new RandomProcessMemoryLess[A](
          (date: LocalDate) => (idx: Int) => if (o(date)(idx)) ca(date)(idx) else cb(date)(idx)
        )
      }
      case LiftPR(lifted, o) => {
        val obs = binomialValuation(o, marketData)
        new RandomProcessMemoryLess[A](
          (date: LocalDate) => (idx: Int) => lifted(obs(date)(idx))
        )
      }
      case Lift2PR(lifted, o1, o2) => {
        val obs1 = binomialValuation(o1, marketData)
        val obs2 = binomialValuation(o2, marketData)
        new RandomProcessMemoryLess[A](
          (date: LocalDate) => (idx: Int) => lifted(obs1(date)(idx), obs2(date)(idx))
        )
      }
      case Exch(curr: String) => {
        val exchangeRate = marketData.exchangeRates(curr)
        new RandomProcessMemoryLess[A](
          (date: LocalDate) => (idx: Int) => exchangeRate(date)(idx)
        )
      }
      case Disc(date: LocalDate, c: PROpt[Double]) => {
        /*
       * Give contract and it's lattice, say foreign currency, lattice is full of values
       * for wrong dates, set contract lattice values to zero, only values remain at date
       * starting with date-1, work backwards: average of child nodes, take present value using interest rate lattice at corresponding node
       */

        val daysUntilMaturity = ChronoUnit.DAYS.between(LocalDate.now(), date).toInt
        val con = binomialValuation(c, marketData)
        val interestRates = marketData.interestRate

        //TODO: extract out this logic
        val _process = new RandomProcessBounded[Double](daysUntilMaturity)
        for (j <- 0 to daysUntilMaturity - 1) _process.set(daysUntilMaturity - 1, j, con(date)(j)) //set terminal to contract value on maturity
        discount(_process,interestRates)
      }
      //case ReturnPR(transform, startCondition) => (date: LocalDate) => (idx: Int) => 1.0 //TODO:
      //case Snell(o: PR[Boolean], c: PR[Double]) => K(1.0) //TODO:
      //case Absorb(o: PR[Boolean], c: PR[Double]) => K(1.0) //TODO:
    }

    //move random process functions to different package
    //generalize to some sort of left generator?
    def binomialPriceTree(days:Int, startVal:Double, annualizedVolatility:Double, probability:Double=0.5):RandomProcessBounded[Double] = {
      val process = new RandomProcessBounded[Double](days)

      val businessDaysInYear = 365.0
      val fractionOfYear = days/(businessDaysInYear*1.0)
      val changeFactorUp = Math.exp(annualizedVolatility* 1.0 * Math.sqrt(fractionOfYear))
      val changeFactorDown = 1/changeFactorUp

      for(i <- 0 to (process.size().get-1)){
        for(j <- 0 to i){
          if(i == 0) process.set(i,j, startVal)
          else{
            val prevNode = if((j-1)<0) 0 else j-1
            val prevValue = process(i-1)(prevNode)
            val isUp = j > prevNode
            val factor = if(isUp) changeFactorUp else changeFactorDown
            process.set(i,j,prevValue * factor)
          }
        }
      }
      process
    }

    //generalize to some sort of right generator?
    def discount(toDiscount:RandomProcessBounded[Double], interestRates:RandomProcess[Double]):RandomProcess[Double] = {
      val size = toDiscount.size().get
      val _process = new RandomProcessBounded[Double](size)
      for (j <- 0 to size - 1) _process.set(size - 1, j, toDiscount(size-1)(j)) //set terminal to contract value on maturity
      for (i <- (0 to (size - 2)).reverse) {
        for (j <- 0 to i) {
          val upVal = _process(i + 1)(j + 1)
          val downVal = _process(i + 1)(j)
          val expectedValue =.5 * upVal +.5 * downVal //probabilities are simplified to 1/2
          _process.set(i, j, expectedValue / (1.0 + interestRates(LocalDate.now().plusDays(i))(j)))
        }
      }
      _process
    }
  }

}

object Main extends App {


  //val lattice = BinomialLattice.binomialPriceTree(LocalDate.now.plus(1,ChronoUnit.YEARS),3,30, .25)
  //val lattice = BinomialLattice.binomialPriceTree(LocalDate.now.plus(1,ChronoUnit.YEARS),12,100, .05)
  //val lattice = BinomialLattice.binomialPriceTree(LocalDate.now.plus(46,ChronoUnit.DAYS),8,142.410,.182)
  //lattice.printLattice()
  //printLattice(cc.discountLattice(lattice),3)

  //Custom combinators
  //def at(date: LocalDate): cc.Obs[Boolean] = cc.Lift2Obs((a: LocalDate, b: LocalDate) => a.compareTo(b) == 0, cc.DateObs(), cc.Konst(date))

  //Tests
  val exchangeRates = collection.mutable.Map(
    "USD" -> ComposingContracts. binomialPriceTree(365,1,0),
    "GBS" -> ComposingContracts.binomialPriceTree(365,1.55,.0467),
    "EUR" -> ComposingContracts.binomialPriceTree(365,1.21,.0515)
  )
  val marketData = MarketData(
    ComposingContracts.binomialPriceTree(365,.05,.05), //interest rate (use a universal rate for now)
    exchangeRates //exchange rates
  )

  //custom contract
  def zcb(maturity:LocalDate, notional:Double, currency:String) = When(maturity, Scale(Konst(notional),One(currency)))
  def europeanOption(at:LocalDate, c1:Contract) = When(at,Or(c1,Zero()))
  //def american(c:cc.Contract, date1:LocalDate, date2:LocalDate) = cc.Anytime(between(date1,date2),c)
  //custom observable
  //def between(date1:LocalDate, date2:LocalDate):cc.Obs[Boolean] = cc.Lift2Obs((a,b)=> a >= date1 && a <= date2, cc.Konst(date1), cc.Konst(date2))
  //val msft:cc.Obs[Double] = BinomialLattice.binomialPriceTree(LocalDate.now().plus(1,ChronoUnit.YEARS),365,46.45,.2)

  //portfolio test
  val portfolio = Array(
    One("USD"),
    One("EUR"),
    Scale(Konst(-1),One("USD")),
    Give(One("USD")),
    Give(Give(One("USD"))),
    And(One("USD"), One("USD")),
    And(One("USD"), Give(One("USD"))),
    //cc.When(at(LocalDate.now().plus(2, ChronoUnit.DAYS)), cc.One("USD"))
    When(LocalDate.now().plus(5, ChronoUnit.DAYS), One("USD"))
    //cc.When(LocalDate.now().plus(5, ChronoUnit.DAYS), cc.Scale(msft, cc.One("USD")))
  )
  
  for(contract <- portfolio){
    println("===========")
    val vp = ComposingContracts.contractToPROpt(contract)
    val rv = ComposingContracts.binomialValuation(vp, marketData)
    println("Contract: "+contract)
    println("Value Process: "+vp)
    println("Random Variable: "+rv)
    //println("Random Variable contents: ")
    //rv.printLattice()
  }

}
