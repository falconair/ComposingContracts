import java.time.LocalDate
import scala.math.Ordering.Implicits._


import ComposingContracts._
import LatticeImplementation._

/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  *         Thanks to Simon Peyton Jones and Jean-Marc Eber
  *
  *         Thanks to #scala members on freenode:
  *         tpolecat
  *         Kristien
  *         OlegYch
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
  */
/*
TODO:
clean up Environment, 'lookup' is too generic
actually price a stock options and compare to the real thing
add optimizer: Give(Give(c)) == c, etc.
narrow random process to BinomialLattice?
separate binoialTree generation from lattice generation (one takes vol, other takes up factor directly)
 */
object Main extends App {
  //Required for doing LocalDate comparisons
  implicit val LocalDateOrdering = scala.math.Ordering.fromLessThan[java.time.LocalDate]{case (a,b) => (a compareTo b) < 0}

  //Custom combinators
  //def at(date: LocalDate): cc.Obs[Boolean] = cc.Lift2Obs((a: LocalDate, b: LocalDate) => a.compareTo(b) == 0, cc.DateObs(), cc.Konst(date))

  //custom contract
  def usd(amount:Double) = Scale(Const(amount),One("USD"))
  def buy(contract:Contract, amount:Double) = And(contract,Give(usd(amount)))
  def sell(contract:Contract, amount:Double) = And(Give(contract),usd(amount))
  def zcb(maturity:LocalDate, notional:Double, currency:String) = When(maturity, Scale(Const(notional),One(currency)))
  def option(contract:Contract) = Or(contract,Zero())
  def europeanCallOption(at:LocalDate, c1:Contract, strike:Double) = When(at, option(buy(c1,strike)))
  def europeanPutOption(at:LocalDate, c1:Contract, strike:Double) = When(at, option(sell(c1,strike)))
  def americanCallOption(at:LocalDate, c1:Contract, strike:Double) = Anytime(at, option(buy(c1,strike)))
  def americanPutOption(at:LocalDate, c1:Contract, strike:Double) = Anytime(at, option(sell(c1,strike)))

  //custom observable
  //def and(a:Obs[Boolean], b:Obs[Boolean]):Obs[Boolean] = Lift2[Boolean,Boolean,Boolean]((a,b)=>a && b, a, b)
  //def between(date1:LocalDate, date2:LocalDate):Obs[Boolean] = and(Const(date1) >= DateObs(), Const(date2) <= DateObs())
  def stock(symbol:String) = Scale(Lookup(symbol),One("USD"))
  //def cash(amt:Double) = Scale(Const(amt),One("USD"))
  val msft = stock("MSFT")


  //Tests
  val exchangeRates = collection.mutable.Map(
    "USD" -> LatticeImplementation.binomialPriceTree(365,1,0),
    "GBP" -> LatticeImplementation.binomialPriceTree(365,1.55,.0467),
    "EUR" -> LatticeImplementation.binomialPriceTree(365,1.21,.0515)
  )
  val lookup = collection.mutable.Map(
    "MSFT" -> LatticeImplementation.binomialPriceTree(365,45.48,.220),
    "ORCL" -> LatticeImplementation.binomialPriceTree(365,42.63,.1048),
    "EBAY" -> LatticeImplementation.binomialPriceTree(365,53.01,.205)
  )
  val marketData = Environment(
    LatticeImplementation.binomialPriceTree(365,.15,.05), //interest rate (use a universal rate for now)
    exchangeRates, //exchange rates
    lookup
  )

  //portfolio test
  val portfolio = Array(
    One("USD")
    ,stock("MSFT")
    ,buy(stock("MSFT"),45)
    //,sell(stock("MSFT"),45)
    ,option(buy(stock("MSFT"),45))
    //,option(sell(stock("MSFT"),45))
    /*,One("EUR")
    ,Scale(Const(-1),One("USD"))
    ,Give(One("USD"))
    ,Give(Give(One("USD")))
    ,And(One("USD"), One("USD"))
    ,And(One("USD"), Give(One("USD")))
    ,When(LocalDate.now(),One("USD"))
    ,When(LocalDate.now().plusDays(5),One("USD"))
    ,When(LocalDate.now().plusDays(5),cash(49))
    */,europeanCallOption(LocalDate.now().plusDays(5),stock("MSFT"),45)
    //,europeanPutOption(LocalDate.now().plusDays(5),stock("MSFT"),45)
    ,americanCallOption(LocalDate.now().plusDays(5),stock("MSFT"),45)
    //,americanPutOption(LocalDate.now().plusDays(5),stock("MSFT"),45)
  )

  for(contract <- portfolio){
    println("===========")
    val propt = LatticeImplementation.contractToPROpt(contract)
    val rp = LatticeImplementation.binomialValuation(propt, marketData)
    println("Contract: "+contract)
    println("Random Process(for optimization): "+propt)
    println("Present val: "+rp.startVal())
    println("Random Process: \n"+rp)
  }

}
