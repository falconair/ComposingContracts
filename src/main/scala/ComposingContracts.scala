import java.time.LocalDate
import java.time.temporal.ChronoUnit
import scala.reflect.ClassTag

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
  */
class ComposingContracts {

  //Contract primitives
  abstract class Contract
  case class Zero() extends Contract
  case class One(currency: String) extends Contract
  case class Give(contract: Contract) extends Contract
  case class And(contract1: Contract, contract2: Contract) extends Contract
  case class Or(contract1: Contract, contract2: Contract) extends Contract
  case class Cond(cond: Obs[Boolean], contract1: Contract, contract2: Contract) extends Contract
  case class Scale(scale: Obs[Double], contract: Contract) extends Contract
  case class When(cond: Obs[Boolean], contract: Contract) extends Contract
  case class Anytime(cond: Obs[Boolean], contract: Contract) extends Contract
  case class Until(cond: Obs[Boolean], contract: Contract) extends Contract

  def contractValuation(contract: Contract): PR[Double] = contract match {
    case Zero() => K(0)
    case One(currency) => Exch(currency)
    case Give(c: Contract) => LiftPR((a: Double) => -1 * a, contractValuation(c))
    case Scale(o: Obs[Double], c: Contract) => Lift2PR((a: Double, b: Double) => a * b, observableValuation(o), contractValuation(c))
    case And(c1: Contract, c2: Contract) => Lift2PR((a: Double, b: Double) => a + b, contractValuation(c1), contractValuation(c2))
    case Or(c1: Contract, c2: Contract) => Lift2PR((a: Double, b: Double) => Math.max(a, b), contractValuation(c1), contractValuation(c2))
    case Cond(o: Obs[Boolean], c1: Contract, c2: Contract) => CondPR(observableValuation(o), contractValuation(c1), contractValuation(c2))
    case When(o: Obs[Boolean], c: Contract) => Disc(observableValuation(o), contractValuation(c))
    case Anytime(o: Obs[Boolean], c: Contract) => Snell(observableValuation(o), contractValuation(c))
    case Until(o: Obs[Boolean], c: Contract) => Absorb(observableValuation(o), contractValuation(c))
  }
  
  //Observable primitives
  abstract class Obs[A] {
    /*
     def +[B](that:Obs[B]):Obs[A] = Lift2Obs((a:A,b:B)=> a+b, this,that)
     def -[B](that:Obs[B]):Obs[A] = Lift2Obs((a:A,b:B)=> a-b, this,that)
     def *[B](that:Obs[B]):Obs[A] = Lift2Obs((a:A,b:B)=> a*b, this,that)
     def /[B](that:Obs[B]):Obs[A] = Lift2Obs((a:A,b:B)=> a/b, this,that)

     def ==[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a==b, this,that)
     def !=[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a!=b, this,that)
     def >[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a>b, this,that)
     def <[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a<b, this,that)
     def >=[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a>=b, this,that)
     def <=[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a<=b, this,that)
     */
  }
  case class Konst[A](k: A) extends Obs[A]
  case class LiftObs[B, A](lifted: (B) => A, o: Obs[B]) extends Obs[A]
  case class Lift2Obs[C, B, A](lifted: (C, B) => A, o1: Obs[C], o2: Obs[B]) extends Obs[A]
  case class DateObs() extends Obs[LocalDate]
  //+,-,*,/


  def observableValuation[A](observable: Obs[A]): PR[A] = observable match {
    case Konst(k: A) => K(k)
    case LiftObs(func, o: Obs[A]) => LiftPR(func, observableValuation(o))
    case Lift2Obs(func, o1, o2) => Lift2PR(func, observableValuation(o1), observableValuation(o2))
    case DateObs() => DatePR()
    //+,-,*,/
  }
  
  
  //Process primitives
  //type PR[A] = (LocalDate)=>A
  abstract class PR[A]
  case class K[A](k: A) extends PR[A]
  case class DatePR() extends PR[LocalDate]
  case class CondPR[A](cond: PR[Boolean], a: PR[A], b: PR[A]) extends PR[A]
  case class LiftPR[B, A](lifted: (B) => A, o: PR[B]) extends PR[A]
  case class Lift2PR[C, B, A](lifted: (C, B) => A, o1: PR[C], o2: PR[B]) extends PR[A]
  case class Snell[A](o: PR[Boolean], c: PR[Double]) extends PR[Double]
  case class Disc[A](o: PR[Boolean], c: PR[Double]) extends PR[Double]
  case class Absorb[A](o: PR[Boolean], c: PR[Double]) extends PR[Double]
  case class Exch[A](curr: String) extends PR[Double]
  //+,-,*,/


  def prValuation[A](pr: PR[A]): (LocalDate, Int) => A = pr match {
    case K(k: A) => (date: LocalDate, latticeIdx: Int) => k
    case DatePR() => (date: LocalDate, latticeIdx: Int) => date
    case CondPR(cond: PR[Boolean], a: PR[A], b: PR[A]) => (date: LocalDate, latticeIdx: Int) => {
      val o = prValuation(cond)
      if (o(date, latticeIdx)) {
        val c = prValuation(a)
        c(date, latticeIdx)
      }
      else {
        val c = prValuation(b)
        c(date, latticeIdx)
      }
    }
    case LiftPR(lifted, o) => (date: LocalDate, latticeIdx: Int) => {
      val obs = prValuation(o)
      lifted(obs(date, latticeIdx))
      
    }
    case Lift2PR(lifted, o1, o2) => (date: LocalDate, latticeIdx: Int) => {
      val obs1 = prValuation(o1)
      val obs2 = prValuation(o2)
      lifted(obs1(date, latticeIdx), obs1(date, latticeIdx))
    }
    case Exch(curr: String ) => (date: LocalDate, latticeIdx: Int) => { 1 }//TODO: all exchange rates are zero until rest of the program works
    //case Disc(o: PR[Boolean], c: PR[Double]) => K(1.0) //TODO: all exchange rates are zero until rest of the program works
    //case Snell(o: PR[Boolean], c: PR[Double]) => K(1.0) //TODO: all exchange rates are zero until rest of the program works
    //case Absorb(o: PR[Boolean], c: PR[Double]) => K(1.0) //TODO: all exchange rates are zero until rest of the program works
  }


  def presentValue(amt: Double, interestRate: Double, years: Double): PR[Double] = {
    K(amt / Math.pow(1 + interestRate, years))
  }

}

object Main extends App {

  val cc = new ComposingContracts

  //Custom combinators
  //case class Lift2Obs[C,B,A] (lifted:(C,B)=>A, o1:Obs[C], o2:Obs[B]) extends Obs[A]
  def at(date: LocalDate): cc.Obs[Boolean] = cc.Lift2Obs((a: LocalDate, b: LocalDate) => a.compareTo(b) == 0, cc.DateObs(), cc.Konst(date))

  //Tests
  println(cc.contractValuation(cc.One("USD")))
  println(cc.contractValuation(cc.And(cc.One("USD"), cc.One("USD"))))
  println(cc.contractValuation(cc.And(cc.One("USD"), cc.Give(cc.One("USD")))))
  println(cc.contractValuation(cc.When(at(LocalDate.now().plus(1, ChronoUnit.YEARS)), cc.One("USD"))))

}
