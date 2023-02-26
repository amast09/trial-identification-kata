package trial.identification.kata

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import munit.FunSuite
import org.scalacheck.Gen

class AnatomicSiteTest extends FunSuite with ScalaCheckSuite {
  property(
    "AnatomicSite.fromString returns `Right(operator)` for valid sites case insensitive"
  ) {
    forAll(Generators.anatomicSiteGen) { (anatomicSite: AnatomicSite) =>
      val siteString = anatomicSite.toString()

      assertEquals(AnatomicSite.fromString(siteString), Right(anatomicSite))
      assertEquals(
        AnatomicSite.fromString(siteString.toLowerCase()),
        Right(anatomicSite)
      )
      assertEquals(
        AnatomicSite.fromString(siteString.toUpperCase()),
        Right(anatomicSite)
      )
    }
  }

  test("Operator.fromString returns the first site match in the string") {
    forAll(
      Generators.anatomicSiteGen,
      Gen.listOf(
        Gen.oneOf(
          Generators.anatomicSiteGen.map(_.toString),
          Generators.alphaStringGen
        )
      )
    ) { (expectedSite: AnatomicSite, anatomicSites: List[String]) =>
      assertEquals(
        AnatomicSite.fromString(
          (expectedSite.toString() +: anatomicSites).mkString(" ")
        ),
        Right(expectedSite)
      )
    }
  }

  test("Operator.fromString returns `Left(AnatomicSiteParseFailure(_))` for invalid operators") {
    val invalidSite = "fingernail"
    assertEquals(
      AnatomicSite.fromString(invalidSite),
      Left(AnatomicSiteParseFailure(invalidSite))
    )
  }
}
