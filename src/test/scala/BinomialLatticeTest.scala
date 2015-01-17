import java.time.LocalDate

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import BinomialLattice._
/** *
  *
  * @author Shahbaz Chaudhary (shahbazc gmail com)
  *
  */
class BinomialLatticeTest extends FunSuite with BeforeAndAfter{
  test("ConstantBL always returns the same thing") {
    val lattice = new ConstantBL(5)
    assert(lattice(0)(0) === 5)
    assert(lattice(1)(0) === 5)
    assert(lattice(1)(1) === 5)
    assert(lattice(10)(0) === 5)
    assert(lattice(10)(9) === 5)
  }

  test("PassThroughBL always returns the identity") {
    val lattice = new PassThroughBL((date:LocalDate)=>(idx:Int)=>date)
    assert(lattice(LocalDate.now())(0) === LocalDate.now())
    assert(lattice(LocalDate.now())(1) === LocalDate.now())
    assert(lattice(LocalDate.now())(2) === LocalDate.now())
  }

  test("GenerateBL generates the correct lattice") {
    val lattice = new GenerateBL(5,100,1.1)
    //println(lattice)
    assert(lattice(0)(0) === 100)
    assert(Math.abs(lattice(4)(0) - 68.3) < 0.01)
    assert(Math.abs(lattice(4)(2) - 100) < 0.01)
    assert(Math.abs(lattice(4)(4) - 146.41) < 0.01)
    assert(lattice.size() == 5)

  }

  test("GenerateBL generates the correct lattice for 0 days") {
    val lattice = new GenerateBL(0,100,1.1)
    //println(lattice)
    assert(lattice(0)(0) === 100)
    assert(lattice.size() == 0)

  }

  test("BinomialLattice.map() returns correct lattice") {
    val lattice = new GenerateBL(5,100,1.1).map((x:Double)=>x*2)
    //println(lattice)
    assert(lattice(0)(0) === 100*2)
    assert(Math.abs(lattice(4)(0) - 68.3*2) < 0.01)
    assert(Math.abs(lattice(4)(2) - 100*2) < 0.01)
    assert(Math.abs(lattice(4)(4) - 146.41*2) < 0.01)

  }

  test("PropagateLeftBL returns correct lattice") {
    val source = new GenerateBL(5,100,1.1)
    val lattice = new PropagateLeftBL(source, (x:Double, y:Double)=>(x+y)/2.0)
    //println(lattice)
    //assert(lattice(0)(0) === 100)
    assert(Math.abs(lattice(3)(0) - 75.47) < 0.01)
    assert(Math.abs(lattice(3)(2) - 110.5) < 0.01)
    assert(Math.abs(lattice(3)(3) - 133.70) < 0.01)
    assert(lattice.size() == 4)
    assert(source.size() == lattice.size()+1)

  }
}
