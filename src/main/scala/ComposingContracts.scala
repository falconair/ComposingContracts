import java.time.LocalDate
import scala.reflect.ClassTag

/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  * Thanks to #scala members on freenode:
  * tpolecat
  * Kristien
  *
  * Contracts presentation:
  * http://www.cs.uu.nl/docs/vakken/afp/Presentations/Contracts.pdf
  *
  * Pricing composable contracts on the GP-GPU:
  * http://hiperfit.dk/pdf/WerkAhnfelt_2011-10ab.pdf
  *
  * AlexanderAA's haskell implementation
  * https://github.com/AlexanderAA/haskell-contract-valuation
  *
  */

  //Contract primitives
  abstract class Contract
  case class Zero() extends Contract
  case class One(currency:String) extends Contract
  case class Give(contract:Contract) extends Contract
  case class And(contract1:Contract, contract2:Contract) extends Contract
  case class Or(contract1:Contract, contract2:Contract) extends Contract
  case class Cond(cond:Obs[Boolean], contract1:Contract, contract2:Contract) extends Contract
  case class Scale(scale:Obs[Double], contract:Contract) extends Contract
  case class When(cond:Obs[Boolean], contract1:Contract) extends Contract
  case class Anytime(cond:Obs[Boolean], contract1:Contract) extends Contract
  case class Until(cond:Obs[Boolean], contract1:Contract) extends Contract

  //Observable primitives
  abstract class Obs[A]{
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
}
  case class Konst[A] (k:A) extends Obs[A]
  case class LiftObs[B,A] (lifted:(B)=>A, o:Obs[B]) extends Obs[A]
  case class Lift2Obs[C,B,A] (lifted:(C,B)=>A, o1:Obs[C], o2:Obs[B]) extends Obs[A]
  case class DateObs () extends Obs[LocalDate]
  //+,-,*,/

  //Process primitives
  //type PR[A] = (LocalDate)=>A
  abstract class PR[A]
  case class K[A](k:A) extends PR[A]
  case class DatePR() extends PR[LocalDate]
  case class CondPR[A](cond:PR[Boolean],a:PR[A],b:PR[A]) extends PR[A]
  case class LiftPR[B,A] (lifted:(B)=>A, o:PR[B]) extends PR[A]
  case class Lift2PR[C,B,A] (lifted:(C,B)=>A, o1:PR[C], o2:PR[B]) extends PR[A]

  case class RV[A: ClassTag] (rv:(LocalDate)=>Array[A]) // taken from Anton Van Straaten's http://contracts.scheming.org/

  //implicit Numeric def
  //Thanks to tpolecat on #scala
  /*implicit def obsNumeric[A](implicit ev: Numeric[A]) = new Numeric[Obs[A]] {
    def plus(x: Obs[A], y: Obs[A]): Obs[A] = Lift2Obs(ev.plus, x, y)
    def toDouble(x: Obs[A]): Double = ???
    def toFloat(x: Obs[A]): Float = ???
    def toInt(x: Obs[A]): Int = ???
    def negate(x: Obs[A]): Obs[A] = LiftObs(ev.negate,x)
    def fromInt(x: Int): Obs[A] = ???
    def toLong(x: Obs[A]): Long = ???
    def times(x: Obs[A], y: Obs[A]): Obs[A] = Lift2Obs(ev.times, x, y)
    def minus(x: Obs[A], y: Obs[A]): Obs[A] = Lift2Obs(ev.minus, x, y)
    def compare(x: Obs[A], y: Obs[A]): Int = Lift2Obs(ev.compare, x, y)
    }*/

  /*
    implicit def prNumeric(implicit ev: Numeric[Double]) = new Numeric[PR[Double]] {
    override def plus(x: PR[Double], y: PR[Double]): PR[Double] = Lift2PR(ev.plus, x, y)
    override def toDouble(x: PR[Double]): Double = ???
    override def toFloat(x: PR[Double]): Float = ???
    override def toInt(x: PR[Double]): Int = ???
    override def negate(x: PR[Double]): PR[Double] = ???
    override def fromInt(x: Int): PR[Double] = ???
    override def toLong(x: PR[Double]): Long = ???
    override def times(x: PR[Double], y: PR[Double]): PR[Double] = Lift2PR(ev.times, x, y)
    override def minus(x: PR[Double], y: PR[Double]): PR[Double] = Lift2PR(ev.minus, x, y)
    override def compare(x: PR[Double], y: PR[Double]): Int = ???
  }*/

object Main extends App{
  def contractValuation(contract:Contract):PR[Double] = contract match{
    case Zero() => K(0)
    case One(currency) => exch(currency)
    case Give(c:Contract) => LiftPR((a:Double)=> -1*a,contractValuation(c))
    case Scale(o:Obs[Double],c:Contract) => Lift2PR((a:Double,b:Double)=> a*b, observableValuation(o),contractValuation(c))
    case And (c1:Contract,c2:Contract) => Lift2PR((a:Double,b:Double)=>a+b,contractValuation(c1), contractValuation(c2))
    case Or (c1:Contract,c2:Contract) => Lift2PR((a:Double,b:Double)=>Math.max(a,b),contractValuation(c1), contractValuation(c2))
    case Cond (o:Obs[Boolean], c1:Contract,c2:Contract) => CondPR(observableValuation(o),contractValuation(c1),contractValuation(c2))
    case When (o:Obs[Boolean], c:Contract) => disc(observableValuation(o), contractValuation(c))
    case Anytime (o:Obs[Boolean], c:Contract) => snell(observableValuation(o), contractValuation(c))
    case Until (o:Obs[Boolean], c:Contract) => absorb(observableValuation(o), contractValuation(c))
  }

  def observableValuation[A](observable:Obs[A]):PR[A] = observable match{
    case Konst(k) => K(k)
    case LiftObs(func, o) => LiftPR(func,observableValuation(o))
    case Lift2Obs(func,o1,o2) => Lift2PR(func,observableValuation(o1),observableValuation(o2))
    case DateObs() => DatePR()
    //+,-,*,/
  }
  
  def prValuation[A: ClassTag](pr:PR[A]):RV[A] = pr match{
    case K(k:A) => RV((date:LocalDate)=>Array(k))
    case DatePR() => RV((dt:LocalDate)=>Array(dt))
    //case CondPR(cond:PR[Boolean],a:PR[A],b:PR[A]) => 
    //case LiftPR (lifted, o) =>
    //case Lift2PR (lifted, o1, o2) =>
  }

  //Model specific functions
  def exch(currency:String):PR[Double]={
    K(1.0)//TODO: all exchange rates are zero until rest of the program works
  }
  
  def disc(observable:PR[Boolean], contract:PR[Double]):PR[Double]={
    
  }
  
  def snell(observable:PR[Boolean], contract:PR[Double]):PR[Double]={
    
  }
  
  def absorb(observable:PR[Boolean], contract:PR[Double]):PR[Double]={
    
  }
  
  def presentValue(amt:Double, interestRate:Double, years:Double):PR[Double] = {
    K(amt/Math.pow(1+interestRate,years))
  }
  
  //Custom combinators
  //case class Lift2Obs[C,B,A] (lifted:(C,B)=>A, o1:Obs[C], o2:Obs[B]) extends Obs[A]
  def at(date:LocalDate):Obs[Boolean] = Lift2Obs((a:LocalDate,b:LocalDate)=> a.compareTo(b)==0,DateObs(),Konst(date))

  //Tests
  println(contractValuation(One("USD")))
  println(contractValuation(And(One("USD"),One("USD"))))
  println(contractValuation(And(One("USD"),Give(One("USD")))))
  println(contractValuation(When(at(LocalDate.now()),One("USD"))))
  
}
