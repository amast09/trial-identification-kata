package trial.identification.kata

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import munit.FunSuite
import org.scalacheck.Gen

class AgeRequirementTest extends FunSuite with ScalaCheckSuite {

  property("Age.apply returns `Right(Age(_))` for valid positive ages") {
    forAll(Gen.posNum[Int]) { (positiveInt: Int) =>
      assertEquals(Age(positiveInt), Right(Age(positiveInt).toOption.get))
    }
  }

  property(
    "Age.apply returns `Left(NegativeAge(_))` for invalid negative ages"
  ) {
    forAll(Gen.negNum[Int]) { (negativeInt: Int) =>
      assertEquals(Age(negativeInt), Left(NegativeAge(negativeInt)))
    }
  }

  property(
    "Age.fromString returns `Right(Age(_))` for valid ages"
  ) {
    forAll(Gen.posNum[Int]) { (positiveInt: Int) =>
      assertEquals(
        Age.fromString(positiveInt.toString),
        Right(Age(positiveInt).toOption.get)
      )
    }
  }

  property(
    "Age.fromString returns `Left(InvalidIntegerValue(_))` for invalid ages that are not integers"
  ) {
    forAll(Gen.double) { (doubleValue: Double) =>
      assertEquals(
        Age.fromString(doubleValue.toString),
        Left(InvalidIntegerValue(doubleValue.toString))
      )
    }
  }

  property(
    "Age.fromString returns `Left(NegativeAge(_))` for invalid negative ages"
  ) {
    forAll(Gen.negNum[Int]) { (negativeInt: Int) =>
      assertEquals(
        Age.fromString(negativeInt.toString),
        Left(NegativeAge(negativeInt))
      )
    }
  }

  test("Operator.toString returns the correct string") {
    assertEquals(Operator.>.toString(), ">")
    assertEquals(Operator.>=.toString(), ">=")
    assertEquals(Operator.<.toString(), "<")
    assertEquals(Operator.<=.toString(), "<=")
    assertEquals(Operator.==.toString(), "=")
  }

  property("Operator.fromString returns `Right(operator)` for valid operators") {
    forAll(Generators.operatorGen) { (operator: Operator) =>
      assertEquals(Operator.fromString(operator.toString()), Right(operator))
    }
  }

  property("Operator.fromString returns `Left(InvalidOperator(_))` for invalid operators") {
    forAll(Gen.alphaNumStr) { (invalidString: String) =>
      assertEquals(
        Operator.fromString(invalidString),
        Left(InvalidOperator(invalidString))
      )
    }
  }

  property(
    "AgeRequirement.fromString returns `Right(AgeReq)` for valid requirements"
  ) {
    forAll(Generators.ageRequirementGen) { (ageRequirement: AgeRequirement) =>
      assertEquals(
        AgeRequirement.fromString(
          s"${ageRequirement.operator} ${ageRequirement.age.value}"
        ),
        Right(ageRequirement)
      )
    }
  }

  property(
    "AgeRequirement.fromString returns `Left(UnableToParse(_))` for invalid age requirements"
  ) {
    forAll(Gen.alphaNumStr) { (invalidString: String) =>
      assertEquals(
        AgeRequirement.fromString(invalidString),
        Left(UnableToParse(invalidString))
      )
    }
  }

  property(
    "AgeRequirement.verify returns the correct comparison values"
  ) {
    forAll(Gen.posNum[Int]) { (genInt: Int) =>
      val ageValue = Math.min(Int.MaxValue - 1, Math.max(1, genInt))
      val age = Age(ageValue).toOption.get
      val agePlus1 = Age(ageValue + 1).toOption.get
      val ageMinus1 = Age(ageValue - 1).toOption.get

    assertEquals(
      AgeRequirement(Operator.>, age).verify(agePlus1),
      true
    )
    assertEquals(
      AgeRequirement(Operator.>=, age).verify(agePlus1),
      true
    )
    assertEquals(
      AgeRequirement(Operator.>=, age).verify(age),
      true
    )
    assertEquals(
      AgeRequirement(Operator.<, age).verify(ageMinus1),
      true
    )
    assertEquals(
      AgeRequirement(Operator.<=, age).verify(ageMinus1),
      true
    )
    assertEquals(
      AgeRequirement(Operator.<=, age).verify(age),
      true
    )
    assertEquals(
      AgeRequirement(Operator.==, age).verify(age),
      true
    )

    assertEquals(
      AgeRequirement(Operator.>, age).verify(age),
      false
    )
    assertEquals(
      AgeRequirement(Operator.>=, age).verify(ageMinus1),
      false
    )
    assertEquals(
      AgeRequirement(Operator.<, age).verify(age),
      false
    )
    assertEquals(
      AgeRequirement(Operator.<=, age).verify(agePlus1),
      false
    )
    assertEquals(
      AgeRequirement(Operator.==, age).verify(agePlus1),
      false
    )
    }
  }
}
