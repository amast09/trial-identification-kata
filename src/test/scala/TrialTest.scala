package trial.identification.kata

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import munit.FunSuite
import org.scalacheck.Gen

class TrialTest extends FunSuite with ScalaCheckSuite {
  property(
    "Trial.csvRowDecoder returns `Right(Trial(_))` for a valid trial CSV row"
  ) {
    forAll(Generators.trialGen) { (trial: Trial) =>
      val csvRow = List(
        trial.id,
        trial.title,
        trial.description,
        trial.phase.toString(),
        trial.condition,
        trial.anatomicSite.toString(),
        trial.diagnoses.toList.mkString("|"),
        s"${trial.ageRequirement.operator.toString} ${trial.ageRequirement.age.value.toString}"
      )

      assertEquals(Trial.csvRowDecoder.decode(csvRow), Right(trial))
    }
  }

  property(
    "Trial.csvRowDecoder returns `Left(_)` for an invalid trial CSV row"
  ) {
    forAll(Generators.trialGen) { (trial: Trial) =>
      val csvRow = List(
        trial.id,
        trial.title,
        trial.condition
      )

      assert(Trial.csvRowDecoder.decode(csvRow).isLeft)
    }
  }

  property(
    "Trial.kataEvaluator returns true for patients that meet the diagnosis requirement, anatomic site requirement and age requirement if age data is provided"
  ) {
    forAll(Generators.validPatientForTrial) {
      (patientAndTrial: (Patient, Trial)) =>
        assert(
          Trial
            .kataEvaluator(patientAndTrial._2)
            .isPatientEligible(patientAndTrial._1)
        )
    }
  }
}
