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

  test("IdentityBL always returns the identity") {
    val lattice = new IdentityBL
    assert(lattice(0)(0) === 0)
    assert(lattice(1)(0) === 1)
    assert(lattice(1)(1) === 1)
    assert(lattice(10)(0) === 10)
    assert(lattice(10)(9) === 10)
  }

  test("GenerateBL generates the correct lattice") {
    val lattice = new GenerateBL(5,100,1.1)
    //println(lattice)
    assert(lattice(0)(0) === 100)
    assert(Math.abs(lattice(4)(0) - 68.3) < 0.01)
    assert(Math.abs(lattice(4)(2) - 100) < 0.01)
    assert(Math.abs(lattice(4)(4) - 146.41) < 0.01)

  }

  test("BinomialLattice.map() returns correct lattice") {
    val lattice = new GenerateBL(5,100,1.1).lift((x:Double)=>x*2)
    //println(lattice)
    assert(lattice(0)(0) === 100*2)
    assert(Math.abs(lattice(4)(0) - 68.3*2) < 0.01)
    assert(Math.abs(lattice(4)(2) - 100*2) < 0.01)
    assert(Math.abs(lattice(4)(4) - 146.41*2) < 0.01)

  }

  test("TransformBL returns correct lattice") {
    val source = new GenerateBL(5,100,1.1)
    val lattice = new TransformBL(source, (x:Double, y:Double)=>(x+y)/2.0)
    //println(lattice)
    //assert(lattice(0)(0) === 100)
    assert(Math.abs(lattice(3)(0) - 75.47) < 0.01)
    assert(Math.abs(lattice(3)(2) - 110.5) < 0.01)
    assert(Math.abs(lattice(3)(3) - 133.70) < 0.01)
    assert(source.size() == lattice.size()+1)

  }
}
