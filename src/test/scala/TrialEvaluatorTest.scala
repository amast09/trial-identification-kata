package trial.identification.kata

import munit.ScalaCheckSuite
import org.scalacheck.Prop.{forAll}
import scala.util.Random
import munit.FunSuite
import org.scalacheck.Gen
import cats.data.NonEmptyList

class TrialEvaluatorTest extends FunSuite with ScalaCheckSuite {
  property(
    "AgeCriterion.evaluate returns `Pass` when the patient age fulfills the age criteria"
  ) {
    forAll(Generators.ageGen, Generators.trialGen, Generators.patientGen) {
      (age: Age, trial: Trial, patient: Patient) =>
        assertEquals(
          AgeCriterion.evaluate(
            trial.copy(ageRequirement = AgeRequirement(Operator.==, age)),
            patient.copy(maybeAge = Some(age))
          ),
          TrialCriterionEvaluation.Pass
        )
    }
  }

  property(
    "AgeCriterion.evaluate returns `InsufficientData` when the Patient is missing age data"
  ) {
    forAll(Generators.ageGen, Generators.trialGen, Generators.patientGen) {
      (age: Age, trial: Trial, patient: Patient) =>
        assertEquals(
          AgeCriterion.evaluate(trial, patient.copy(maybeAge = None)),
          TrialCriterionEvaluation.InsufficientData
        )
    }
  }

  property(
    "AgeCriterion.evaluate returns `Fail` when the patient age does not fulfill the age criteria"
  ) {
    forAll(Generators.ageGen, Generators.trialGen, Generators.patientGen) {
      (age: Age, trial: Trial, patient: Patient) =>
        assertEquals(
          AgeCriterion.evaluate(
            trial.copy(ageRequirement = AgeRequirement(Operator.>, age)),
            patient.copy(maybeAge = Some(age))
          ),
          TrialCriterionEvaluation.Fail
        )
    }
  }

  property(
    "DiagnosisCriterion.evaluate returns `Pass` when the patient contains one of the trial's diagnosis's"
  ) {
    forAll(
      Generators.alphaStringGen,
      Generators.trialGen,
      Generators.patientGen
    ) { (matchingDiagnosis: String, trial: Trial, patient: Patient) =>
      val shuffledDiagnoses =
        Random.shuffle(matchingDiagnosis +: trial.diagnoses.toList)
      val diagnoses = NonEmptyList.fromList(shuffledDiagnoses).get

      assertEquals(
        DiagnosisCriterion.evaluate(
          trial.copy(diagnoses = diagnoses),
          patient.copy(diagnosis = matchingDiagnosis)
        ),
        TrialCriterionEvaluation.Pass
      )
    }
  }

  property(
    "DiagnosisCriterion.evaluate is case-insensitive"
  ) {
    forAll(
      Generators.alphaStringGen,
      Generators.trialGen,
      Generators.patientGen
    ) { (matchingDiagnosis: String, trial: Trial, patient: Patient) =>
      val shuffledDiagnoses =
        Random.shuffle(
          matchingDiagnosis.toUpperCase() +: trial.diagnoses.toList
        )
      val diagnoses = NonEmptyList.fromList(shuffledDiagnoses).get

      assertEquals(
        DiagnosisCriterion.evaluate(
          trial.copy(diagnoses = diagnoses),
          patient.copy(diagnosis = matchingDiagnosis.toLowerCase())
        ),
        TrialCriterionEvaluation.Pass
      )
    }
  }

  // Note: I think this is the behavior required, it could be adjusted based on requirements changing though
  property(
    "DiagnosisCriterion.evaluate returns `Pass` when the patient contains a more specific diagnosis that is listed in the trial's diagnosis's"
  ) {
    forAll(
      Generators.alphaStringGen,
      Generators.alphaStringGen,
      Generators.trialGen,
      Generators.patientGen
    ) {
      (
          diagnosisPrefix: String,
          matchingDiagnosis: String,
          trial: Trial,
          patient: Patient
      ) =>
        val shuffledDiagnoses =
          Random.shuffle(matchingDiagnosis +: trial.diagnoses.toList)
        val diagnoses = NonEmptyList.fromList(shuffledDiagnoses).get

      assertEquals(
        DiagnosisCriterion.evaluate(
          trial.copy(diagnoses = diagnoses),
          candidatePatient =
            patient.copy(diagnosis = s"${diagnosisPrefix} ${matchingDiagnosis}")
        ),
        TrialCriterionEvaluation.Pass
      )
    }
  }

  property(
    "DiagnosisCriterion.evaluate returns `Fail` when the patient does not contain one of the trial's diagnosis's"
  ) {
    forAll(
      Generators.alphaStringGen,
      Generators.trialGen,
      Generators.patientGen
    ) { (matchingDiagnosis: String, trial: Trial, patient: Patient) =>
      assertEquals(
        DiagnosisCriterion.evaluate(
          trial.copy(diagnoses = NonEmptyList.one("Flu")),
          patient.copy(diagnosis = "Dementia")
        ),
        TrialCriterionEvaluation.Fail
      )
    }
  }

  property(
    "AnatomicSiteCriterion.evaluate returns `Pass` when the patient's anatomic site matches the trial's anatomic site"
  ) {
    forAll(
      Generators.anatomicSiteGen,
      Generators.trialGen,
      Generators.patientGen
    ) { (anatomicSite: AnatomicSite, trial: Trial, patient: Patient) =>
      assertEquals(
        AnatomicSiteCriterion.evaluate(
          trial.copy(anatomicSite = anatomicSite),
          patient.copy(maybeAnatomicSite = Some(anatomicSite))
        ),
        TrialCriterionEvaluation.Pass
      )
    }
  }

  property(
    "AnatomicSiteCriterion.evaluate returns `Fail` when the patient's anatomic site does not match the trial's anatomic site"
  ) {
    forAll(
      Generators.trialGen,
      Generators.patientGen
    ) { (trial: Trial, patient: Patient) =>
      assertEquals(
        AnatomicSiteCriterion.evaluate(
          trial.copy(anatomicSite = AnatomicSite.Bladder),
          patient.copy(maybeAnatomicSite = Some(AnatomicSite.Kidney))
        ),
        TrialCriterionEvaluation.Fail
      )
    }
  }

  property(
    "AnatomicSiteCriterion.evaluate returns `InsufficientData` when the patient's anatomic site is missing"
  ) {
    forAll(
      Generators.anatomicSiteGen,
      Generators.trialGen,
      Generators.patientGen
    ) { (anatomicSite: AnatomicSite, trial: Trial, patient: Patient) =>
      assertEquals(
        AnatomicSiteCriterion.evaluate(
          trial,
          patient.copy(maybeAnatomicSite = None)
        ),
        TrialCriterionEvaluation.InsufficientData
      )
    }
  }

  property(
    "TrialEvaluator.isPatientEligible should return `True` when all criterion return `Pass`"
  ) {
    forAll(
      Gen.nonEmptyListOf(Generators.PassingCriterion),
      Gen.nonEmptyListOf(Generators.PassingCriterion),
      Generators.trialGen,
      Generators.patientGen
    ) {
      (
          requiredCriteria: List[TrialCriterion],
          optionalCriteria: List[TrialCriterion],
          trial: Trial,
          patient: Patient
      ) =>
        val trialEvaluator =
          TrialEvaluator(requiredCriteria, optionalCriteria)(trial)
        assert(trialEvaluator.isPatientEligible(patient))
    }
  }

  property(
    "TrialEvaluator.isPatientEligible should return `False` when a required criterion returns `Fail`"
  ) {
    forAll(
      Gen.nonEmptyListOf(Generators.trialCriterionGen),
      Gen.nonEmptyListOf(Generators.trialCriterionGen),
      Generators.trialGen,
      Generators.patientGen
    ) {
      (
          requiredCriteria: List[TrialCriterion],
          optionalCriteria: List[TrialCriterion],
          trial: Trial,
          patient: Patient
      ) =>
        val trialEvaluator = TrialEvaluator(
          Random.shuffle(Generators.FailingCriterion +: requiredCriteria),
          optionalCriteria
        )(trial)
        assertEquals(trialEvaluator.isPatientEligible(patient), false)
    }
  }

  property(
    "TrialEvaluator.isPatientEligible should return `False` when an optional criterion returns `Fail`"
  ) {
    forAll(
      Gen.nonEmptyListOf(Generators.trialCriterionGen),
      Gen.nonEmptyListOf(Generators.trialCriterionGen),
      Generators.trialGen,
      Generators.patientGen
    ) {
      (
          requiredCriteria: List[TrialCriterion],
          optionalCriteria: List[TrialCriterion],
          trial: Trial,
          patient: Patient
      ) =>
        val trialEvaluator = TrialEvaluator(
          requiredCriteria,
          Random.shuffle(Generators.FailingCriterion +: optionalCriteria)
        )(trial)
        assertEquals(trialEvaluator.isPatientEligible(patient), false)
    }
  }

  property(
    "TrialEvaluator.isPatientEligible should return `False` when a required criterion returns `InsufficientData`"
  ) {
    forAll(
      Gen.nonEmptyListOf(Generators.trialCriterionGen),
      Gen.nonEmptyListOf(Generators.trialCriterionGen),
      Generators.trialGen,
      Generators.patientGen
    ) {
      (
          requiredCriteria: List[TrialCriterion],
          optionalCriteria: List[TrialCriterion],
          trial: Trial,
          patient: Patient
      ) =>
        val trialEvaluator = TrialEvaluator(
          Random.shuffle(Generators.InsufficientCriterion +: requiredCriteria),
          optionalCriteria
        )(trial)
        assertEquals(trialEvaluator.isPatientEligible(patient), false)
    }
  }

  property(
    "TrialEvaluator.isPatientEligible should return `True` when all required criterion return `Pass` and all optional criterion return `InsufficientData` or `Pass`"
  ) {
    forAll(
      Gen.nonEmptyListOf(Generators.PassingCriterion),
      Gen.nonEmptyListOf(
        Gen.oneOf(Generators.InsufficientCriterion, Generators.PassingCriterion)
      ),
      Generators.trialGen,
      Generators.patientGen
    ) {
      (
          requiredCriteria: List[TrialCriterion],
          optionalCriteria: List[TrialCriterion],
          trial: Trial,
          patient: Patient
      ) =>
        val trialEvaluator =
          TrialEvaluator(requiredCriteria, optionalCriteria)(trial)
        assert(trialEvaluator.isPatientEligible(patient))
    }
  }
}
