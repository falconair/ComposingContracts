import java.time.LocalDate
import java.time.temporal.ChronoUnit

import ComposingContracts._
import BinomialLattice._
/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  */
package LatticeImplementation{


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
case class ConstPR[A]       (k: A)                                            extends PROpt[A]
case class DatePR           ()                                                extends PROpt[LocalDate]
case class CondPR[A]        (cond: PROpt[Boolean], a: PROpt[A], b: PROpt[A])  extends PROpt[A]
case class LiftPR[B, A]     (lifted: (B) => A, o: PROpt[B])                   extends PROpt[A]
case class Lift2PR[C, B, A] (lifted: (C, B) => A, o1: PROpt[C], o2: PROpt[B]) extends PROpt[A]
case class LookupPR[A]      (lookup:String)                                   extends PROpt[Double]

case class Snell[A]         (date:LocalDate, c: PROpt[Double])                extends PROpt[Double]
case class Disc[A]          (date:LocalDate, c: PROpt[Double])                extends PROpt[Double]
case class Absorb[A]        (date:LocalDate, c: PROpt[Double])                extends PROpt[Double]
case class Exch[A]          (curr: String)                                    extends PROpt[Double]

//Why does type have to be bounded??
case class Environment(interestRate:BinomialLatticeBounded[Double],
                       exchangeRates:collection.mutable.Map[String,BinomialLatticeBounded[Double]],
                       lookup:collection.mutable.Map[String,BinomialLatticeBounded[Double]])

//TODO: move random process to different package


object LatticeImplementation {
  def contractToPROpt(contract: Contract): PROpt[Double] = contract match {
    case Zero()                                => ConstPR(0)
    case One(currency)                         => Exch(currency)
    case Give(c: Contract)                     => contractToPROpt(Scale(Const(-1), c))
    case Scale(o: Obs[Double], c: Contract)    => Lift2PR((a: Double, b: Double) => a * b, obsToPROpt(o), contractToPROpt(c))
    case And(c1: Contract, c2: Contract)       => Lift2PR((a: Double, b: Double) => a + b, contractToPROpt(c1), contractToPROpt(c2))
    case Or(c1: Contract, c2: Contract)        => Lift2PR((a: Double, b: Double) => Math.max(a, b), contractToPROpt(c1), contractToPROpt(c2))
    case Cond(o: Obs[Boolean], c1: Contract, c2: Contract) => CondPR(obsToPROpt(o), contractToPROpt(c1), contractToPROpt(c2))
    case When(date: LocalDate, c: Contract)    => Disc(date, contractToPROpt(c))
    case Anytime(date: LocalDate, c: Contract) => Snell(date, contractToPROpt(c))
    //case Until(date: LocalDate, c: Contract)   => Absorb(date, contractToPROpt(c))
  }

  def obsToPROpt[A](observable: Obs[A]): PROpt[A] = observable match {
    case Const(k)              => ConstPR(k)
    case Lift(func, o: Obs[A]) => LiftPR(func, obsToPROpt(o))
    case Lift2(func, o1, o2)   => Lift2PR(func, obsToPROpt(o1), obsToPROpt(o2))
    case DateObs()             => DatePR()
    case Lookup(lookup)        => LookupPR(lookup)
  }


  def binomialValuation[A](pr: PROpt[A], marketData: Environment): BinomialLattice[A] = pr match {
    case ConstPR(k) => new ConstantBL(k)
    case DatePR() => new PassThroughBL((date:LocalDate)=>(idx:Int)=>date)
    case CondPR(cond: PROpt[Boolean], a: PROpt[A], b: PROpt[A]) => {
      val o = binomialValuation(cond, marketData)
      val ca = binomialValuation(a, marketData)
      val cb = binomialValuation(b, marketData)

      new PassThroughBL[A](
        (date: LocalDate) => (idx: Int) => if (o(date)(idx)) ca(date)(idx) else cb(date)(idx)
      )
    }
    case LiftPR(lifted, o) => {
      val obs = binomialValuation(o, marketData)
      new PassThroughBL[A](
        (date: LocalDate) => (idx: Int) => lifted(obs(date)(idx))
      )
    }
    case Lift2PR(lifted, o1, o2) => {
      val obs1 = binomialValuation(o1, marketData)
      val obs2 = binomialValuation(o2, marketData)
      new PassThroughBL[A](
        (date: LocalDate) => (idx: Int) => lifted(obs1(date)(idx), obs2(date)(idx))
      )
    }
    case LookupPR(lookup) => {
      marketData.lookup(lookup)
    }
    case Exch(curr: String) => {
      val exchangeRate = marketData.exchangeRates(curr)
      new PassThroughBL[A](
        (date: LocalDate) => (idx: Int) => exchangeRate(date)(idx)
      )
    }
    case Disc(date: LocalDate, c: PROpt[Double]) => {
      val daysUntilMaturity = ChronoUnit.DAYS.between(LocalDate.now(), date).toInt
      val con = binomialValuation(c, marketData)
      val interestRates = marketData.interestRate

      val _process = new PassThroughBoundedBL[Double]((date:LocalDate)=>(idx:Int)=>con(date)(idx),daysUntilMaturity+1)//daysUntilMaturity+1 because if contract matures today, it still needs today's valuation
      discount(_process,interestRates)
    }
    case Snell(date: LocalDate, c: PROpt[Double]) => {
      /*
      Take final column of the tree, discount it back one step. Take max of the discounted column and the original column, repeat.
       */
      val daysUntilMaturity = ChronoUnit.DAYS.between(LocalDate.now(), date).toInt
      val con = binomialValuation(c, marketData)
      val interestRates = marketData.interestRate

      val _process:BinomialLatticeBounded[Double] = new PassThroughBoundedBL[Double]((date:LocalDate)=>(idx:Int)=>con(date)(idx),daysUntilMaturity+1)//daysUntilMaturity+1 because if contract matures today, it still needs today's valuation
      var result = _process
      for(i <- 0 to _process.size()){
        val discounted:BinomialLatticeBounded[Double] = discount(result,interestRates)
        val contractAndDiscounted:BinomialLatticeBounded[(Double,Double)] = discounted.zip(_process)
        result = contractAndDiscounted.map[Double]((cNd)=>{
          val c = cNd._1
          val d = cNd._2
          Math.max(c,d)
        })
      }
      result
    }
    //case Absorb(o: PR[Boolean], cond: PROpt[Boolean], c: PR[Double]) => K(1.0) //TODO:
  }

  //move random process functions to different package
  //generalize to some sort of left generator?
  def binomialPriceTree(days:Int, startVal:Double, annualizedVolatility:Double, probability:Double=0.5):BinomialLatticeBounded[Double] = {

    val businessDaysInYear = 365.0
    val fractionOfYear = (days* 1.0)/businessDaysInYear
    val changeFactorUp = vol2chfactor(annualizedVolatility,fractionOfYear)
    val process = new GenerateBL(days+1,startVal,changeFactorUp)
    process
  }

  def vol2chfactor(vol:Double, fractionOfYear:Double) = {
    Math.exp(vol * Math.sqrt(fractionOfYear))
  }

  def discount(toDiscount:BinomialLatticeBounded[Double], interestRates:BinomialLattice[Double]):BinomialLatticeBounded[Double] = {
    val averaged = new PropagateLeftBL[Double](toDiscount, (x,y)=>(x+y)/2.0)//assume .5 probability
    val zipped = averaged.zip(interestRates)
    zipped.map[Double](avg_ir=>{
      val avg = avg_ir._1
      val ir = avg_ir._2
      avg/(1.0+ir)
    })
  }
}

}