import java.time.LocalDate

/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  */
package ComposingContracts {

  //Contract primitives
  sealed trait Contract
  case class Zero() extends Contract
  case class One(currency: String) extends Contract
  case class Give(contract: Contract) extends Contract
  case class And(contract1: Contract, contract2: Contract) extends Contract
  case class Or(contract1: Contract, contract2: Contract) extends Contract
  case class Cond(cond: Obs[Boolean], contract1: Contract, contract2: Contract) extends Contract
  case class Scale(scale: Obs[Double], contract: Contract) extends Contract
  case class When(date: LocalDate, contract: Contract) extends Contract
  case class Anytime(date: LocalDate, contract: Contract) extends Contract
  //case class Until(date: LocalDate, contract: Contract) extends Contract

  //Observable primitives
  abstract class Obs[A] {

    def +(that: Obs[A])(implicit n: Numeric[A]): Obs[A] = Lift2(n.plus, this, that)
    def -(that: Obs[A])(implicit n: Numeric[A]): Obs[A] = Lift2(n.minus, this, that)
    def *(that: Obs[A])(implicit n: Numeric[A]): Obs[A] = Lift2(n.times, this, that)
    //http://stackoverflow.com/questions/6188990/writing-a-generic-mean-function-in-scala
    //def /(that:Obs[A])(implicit n: Numeric[A]):Obs[A] = Lift2Obs((a:A,b:A)=> n.times(a,1.0/b), this,that)

    def ==(that: Obs[A])(implicit n: Ordering[A]): Obs[Boolean] = Lift2((a: A, b: A) => n.==(a, b), this, that)
    def !=(that: Obs[A])(implicit n: Ordering[A]): Obs[Boolean] = Lift2((a: A, b: A) => n.!=(a, b), this, that)
    def >(that: Obs[A])(implicit n: Ordering[A]): Obs[Boolean] = Lift2((a: A, b: A) => n.gt(a, b), this, that)
    def <(that: Obs[A])(implicit n: Ordering[A]): Obs[Boolean] = Lift2((a: A, b: A) => n.lt(a, b), this, that)
    def >=(that: Obs[A])(implicit n: Ordering[A]): Obs[Boolean] = Lift2((a: A, b: A) => n.gteq(a, b), this, that)
    def <=(that: Obs[A])(implicit n: Ordering[A]): Obs[Boolean] = Lift2((a: A, b: A) => n.lteq(a, b), this, that)
    //def &&(that:Obs[Boolean])(implicit n: Boolean[A]):Obs[Boolean] = Lift2Obs((a:Boolean,b:Boolean)=> a.&&(b), this,that)

  }
  case class Const[A](k: A) extends Obs[A]
  case class Lift[B, A](lifted: (B) => A, o: Obs[B]) extends Obs[A]
  case class Lift2[C, B, A](lifted: (C, B) => A, o1: Obs[C], o2: Obs[B]) extends Obs[A]
  case class DateObs() extends Obs[LocalDate]
  case class Lookup[A](lookup: String) extends Obs[Double]
}




