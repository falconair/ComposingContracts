import java.time.LocalDate
import java.time.temporal.ChronoUnit

import ComposingContracts._
import RandomProcess._
/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  */
package ComposingContractsLatticeImplementation{


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
case class ConstPR[A]             (k: A)                                      extends PROpt[A]
case class DatePR           ()                                                extends PROpt[LocalDate]
case class CondPR[A]        (cond: PROpt[Boolean], a: PROpt[A], b: PROpt[A])  extends PROpt[A]
case class LiftPR[B, A]     (lifted: (B) => A, o: PROpt[B])                   extends PROpt[A]
case class Lift2PR[C, B, A] (lifted: (C, B) => A, o1: PROpt[C], o2: PROpt[B]) extends PROpt[A]
case class LookupPR[A]      (lookup:String)                                   extends PROpt[Double]

case class Snell[A]         (date:LocalDate, c: PROpt[Double])                extends PROpt[Double]
case class Disc[A]          (date:LocalDate, c: PROpt[Double])                extends PROpt[Double]
case class Absorb[A]        (date:LocalDate, c: PROpt[Double])                extends PROpt[Double]
case class Exch[A]          (curr: String)                                    extends PROpt[Double]
//case class Project[A] //if discount calculates front to back, project goes from back to front...for arbitrary RP like stocks

//Why does type have to be bounded??
case class Environment(interestRate:RandomProcessBounded[Double],
                       exchangeRates:collection.mutable.Map[String,RandomProcessBounded[Double]],
                       lookup:collection.mutable.Map[String,RandomProcessBounded[Double]])

//TODO: move random process to different package


object ComposingContractsLatticeImplementation {
  def contractToPROpt(contract: Contract): PROpt[Double] = contract match {
    case Zero()                                => ConstPR(0)
    case One(currency)                         => Exch(currency)
    case Give(c: Contract)                     => contractToPROpt(Scale(Const(-1), c))
    case Scale(o: Obs[Double], c: Contract)    => Lift2PR((a: Double, b: Double) => a * b, obsToPROpt(o), contractToPROpt(c))
    case And(c1: Contract, c2: Contract)       => Lift2PR((a: Double, b: Double) => a + b, contractToPROpt(c1), contractToPROpt(c2))
    case Or(c1: Contract, c2: Contract)        => Lift2PR((a: Double, b: Double) => Math.max(a, b), contractToPROpt(c1), contractToPROpt(c2))
    case Cond(o: Obs[Boolean], c1: Contract, c2: Contract) => CondPR(obsToPROpt(o), contractToPROpt(c1), contractToPROpt(c2))
    //case When(o: Obs[Boolean], c: Contract)    => Disc(observableValuation(o), contractValuation(c))
    case When(date: LocalDate, c: Contract)    => Disc(date, contractToPROpt(c))
    case Anytime(date: LocalDate, c: Contract) => Snell(date, contractToPROpt(c))
    case Until(date: LocalDate, c: Contract)   => Absorb(date, contractToPROpt(c))
  }

  def obsToPROpt[A](observable: Obs[A]): PROpt[A] = observable match {
    case Const(k)              => ConstPR(k)
    case Lift(func, o: Obs[A]) => LiftPR(func, obsToPROpt(o))
    case Lift2(func, o1, o2)   => Lift2PR(func, obsToPROpt(o1), obsToPROpt(o2))
    case DateObs()             => DatePR()
    case Lookup(lookup)        => LookupPR(lookup)
  }

  //TODO: implementation function which produces Date=>RV, for monte carlo, different function needed
  //each 'case' should be its own function
  //binomial lattice just serves as an implemeentation of date=>rv, could be cachable, lazy, etc.
  def binomialValuation[A](pr: PROpt[A], marketData: Environment): RandomProcess[A] = pr match {
    case ConstPR(k) => new RandomProcessMemoryLess[A]((_) => (_) => k)
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
    case LookupPR(lookup) => {
      marketData.lookup(lookup)
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
      val _process = new RandomProcessBounded[Double](daysUntilMaturity+1)//daysUntilMaturity+1 because if contract matures today, it still needs today's valuation
      for (j <- 0 to daysUntilMaturity ) _process.set(daysUntilMaturity , j, con(date)(j) ) //set terminal to contract value on maturity
      discount(_process,interestRates)
    }
    case Snell(date: LocalDate, c: PROpt[Double]) => {
      /*
     * Give contract and it's lattice, say foreign currency, lattice is full of values
     * for wrong dates, set contract lattice values to zero, only values remain at date
     * starting with date-1, work backwards: average of child nodes, take present value using interest rate lattice at corresponding node
     */

      val daysUntilMaturity = ChronoUnit.DAYS.between(LocalDate.now(), date).toInt
      val con = binomialValuation(c, marketData)
      val interestRates = marketData.interestRate

      //TODO: extract out this logic
      val _process = new RandomProcessBounded[Double](daysUntilMaturity+1)//daysUntilMaturity+1 because if contract matures today, it still needs today's valuation
      for (j <- 0 to daysUntilMaturity ) _process.set(daysUntilMaturity , j, con(date)(j) ) //set terminal to contract value on maturity
      val discounted = discount(_process,interestRates)
      val result = new RandomProcessBounded[Double](daysUntilMaturity+1)
      for (i <- 0 to daysUntilMaturity ) for(j <- 0 to i) result.set(i , j, Math.max(discounted(i)(j), con(i)(j)))
      result
    }
    //case ReturnPR(transform) => (date: LocalDate) => new RandomProcessPassThrough(transform)
    //case Absorb(o: PR[Boolean], cond: PROpt[Boolean], c: PR[Double]) => K(1.0) //TODO:
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