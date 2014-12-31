import java.time.LocalDate
import java.time.Duration
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
    //case Give(c: Contract) => LiftPR((a: Double) => -1 * a, contractValuation(c))
    case Give(c: Contract) => contractValuation(Scale(Konst(-1),c))
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
     def +(that:Obs[A])(implicit n: Numeric[A]):Obs[A] = Lift2Obs((a:A,b:A)=> n.plus(a,b), this,that)
     def -(that:Obs[A])(implicit n: Numeric[A]):Obs[A] = Lift2Obs((a:A,b:A)=> n.minus(a,b), this,that)
     def *(that:Obs[A])(implicit n: Numeric[A]):Obs[A] = Lift2Obs((a:A,b:A)=> n.times(a,b), this,that)
     //def /(that:Obs[A])(implicit n: Numeric[A]):Obs[A] = Lift2Obs((a:A,b:A)=> n.times(a,1/b), this,that)
     
     //def -[B](that:Obs[B]):Obs[A] = Lift2Obs((a:A,b:B)=> a-b, this,that)
     //def *[B](that:Obs[B]):Obs[A] = Lift2Obs((a:A,b:B)=> a*b, this,that)
     //def /[B](that:Obs[B]):Obs[A] = Lift2Obs((a:A,b:B)=> a/b, this,that)

     //def ==[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a==b, this,that)
     //def !=[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a!=b, this,that)
     //def >[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a>b, this,that)
     //def <[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a<b, this,that)
     //def >=[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a>=b, this,that)
     //def <=[B](that:Obs[B]):Obs[Boolean] = Lift2Obs((a:A,b:B)=> a<=b, this,that)
     */
  }
  case class Konst[A](k: A) extends Obs[A]
  case class LiftObs[B, A](lifted: (B) => A, o: Obs[B]) extends Obs[A]
  case class Lift2Obs[C, B, A](lifted: (C, B) => A, o1: Obs[C], o2: Obs[B]) extends Obs[A]
  case class DateObs() extends Obs[LocalDate]
  //+,-,*,/


  def observableValuation[A](observable: Obs[A]): PR[A] = observable match {
    case Konst(k) => K(k)
    case LiftObs(func, o: Obs[A]) => LiftPR(func, observableValuation(o))
    case Lift2Obs(func, o1, o2) => Lift2PR(func, observableValuation(o1), observableValuation(o2))
    case DateObs() => DatePR()
    //+,-,*,/
  }
  
  
  //Process primitives
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

  type RV[A] = (LocalDate, Int)=>A

  def prValuation[A](pr: PR[A]): RV[A] = pr match {
    case K(k) => (date: LocalDate, latticeIdx: Int) => k
    case DatePR() => (date: LocalDate, latticeIdx: Int) => date
    case CondPR(cond: PR[Boolean], a: PR[A], b: PR[A]) => {
      val o = prValuation(cond)
      val ca = prValuation(a)
      val cb = prValuation(b)
      
      (date: LocalDate, latticeIdx: Int) => {
        if (o(date, latticeIdx)) ca(date, latticeIdx)
        else cb(date, latticeIdx)
      }
    }
    case LiftPR(lifted, o) => {
      val obs = prValuation(o)
      (date: LocalDate, latticeIdx: Int) => lifted(obs(date, latticeIdx))
    }
    case Lift2PR(lifted, o1, o2) => {
      val obs1 = prValuation(o1)
      val obs2 = prValuation(o2)
      (date: LocalDate, latticeIdx: Int) => lifted(obs1(date, latticeIdx), obs2(date, latticeIdx))
    }
    case Exch(curr: String ) => (date: LocalDate, latticeIdx: Int) => { 1 }//TODO: all exchange rates are 1 until rest of the program works
    case Disc(o: PR[Boolean], c: PR[Double]) => {
      val obs = prValuation(o)
      val con = prValuation(c)
      
      (date: LocalDate, latticeIdx: Int) => if(obs(date,latticeIdx))con(date,latticeIdx) else -1
    }
    //case Snell(o: PR[Boolean], c: PR[Double]) => K(1.0) //TODO: all exchange rates are zero until rest of the program works
    //case Absorb(o: PR[Boolean], c: PR[Double]) => K(1.0) //TODO: all exchange rates are zero until rest of the program works
  }

  def presentValue(amt: Double, interestRate: Double, years: Double): PR[Double] = {
    K(amt / Math.pow(1 + interestRate, years))
  }
  
  //Extract lattice methods into their own class
  def makeLattice(start:Double, changeFactor:Double, size:Int):RV[Double] = {
    val lattice = new Array[Array[Double]](size)
    for ( i <- 0 to (lattice.length - 1)) { 
      lattice(i) = new Array[Double](i+1); 
      for( j <- 0 to i){
        if(i == 0) lattice(i)(j) = start
        else{
          val prevNode = if((j-1)<0) 0 else j-1
          val prevValue = lattice(i-1)(prevNode)
          val isUp = j > prevNode
          val factor = if(isUp) (1+changeFactor) else 1.0 / (1+changeFactor)
          lattice(i)(j) = prevValue * factor
        }
      }
     }
    (date:LocalDate, idx:Int)=>lattice(ChronoUnit.DAYS.between(LocalDate.now(),date).toInt)(idx)
  }
  

}

object Main extends App {
  
  val cc = new ComposingContracts
  
  //val lattice = cc.makeLattice(100,0.0029,10)
  //printLatice(lattice, 9)

  //Custom combinators
  //case class Lift2Obs[C,B,A] (lifted:(C,B)=>A, o1:Obs[C], o2:Obs[B]) extends Obs[A]
  def at(date: LocalDate): cc.Obs[Boolean] = cc.Lift2Obs((a: LocalDate, b: LocalDate) => a.compareTo(b) == 0, cc.DateObs(), cc.Konst(date))

  //Tests
  val portfolio = Array(
    cc.One("USD"),
    cc.Scale(cc.Konst(-1),cc.One("USD")),
    cc.Give(cc.One("USD")),
    cc.Give(cc.Give(cc.One("USD"))),
    cc.And(cc.One("USD"), cc.One("USD")),
    cc.And(cc.One("USD"), cc.Give(cc.One("USD"))),
    cc.When(at(LocalDate.now().plus(1, ChronoUnit.YEARS)), cc.One("USD"))
  )
  
  for(contract <- portfolio){
    println("===========")
    val vp = cc.contractValuation(contract)
    val rv = cc.prValuation(vp)
    println("Contract: "+contract)
    println("Value Process: "+vp)
    println("Random Variable: "+rv)
    println("Random Variable contents: ")
    printLatice(rv,3)
  }

  def printLatice[A](lattice:cc.RV[A], size:Int):Unit={
    for(days <- 0 to size){
      for(idx <- 0 to days){
        print(lattice(LocalDate.now().plus(days, ChronoUnit.DAYS),idx))
        print('\t')
      }
      println("")
    }
  }
}

