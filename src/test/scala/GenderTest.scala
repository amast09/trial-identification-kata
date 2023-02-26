package trial.identification.kata

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import munit.FunSuite
import org.scalacheck.Gen

class GenderTest extends FunSuite with ScalaCheckSuite {
  property(
    "Gender.fromString returns `Right(gender)` for valid genders case insensitive"
  ) {
    forAll(Generators.genderGen) { (gender: Gender) =>
      val genderString = gender.toString()

      assertEquals(Gender.fromString(genderString), Right(gender))
      assertEquals(
        Gender.fromString(genderString.toLowerCase()),
        Right(gender)
      )
      assertEquals(
        Gender.fromString(genderString.toUpperCase()),
        Right(gender)
      )
    }
  }

  test("Gender.fromString returns `Left(GenderParseFailure(_))` for invalid genders") {
    assertEquals(Gender.fromString("m"), Left(GenderParseFailure("m")))
    assertEquals(Gender.fromString("f"), Left(GenderParseFailure("f")))
    assertEquals(Gender.fromString("Lung"), Left(GenderParseFailure("Lung")))
  }
}
