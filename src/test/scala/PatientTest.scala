package trial.identification.kata

import munit.ScalaCheckSuite
import org.scalacheck.Prop._
import munit.FunSuite
import org.scalacheck.Gen

class PatientTest extends FunSuite with ScalaCheckSuite {
  property("Patient.csvRowDecoder returns `Right(Patient(_))` for a valid patient CSV row") {
    forAll(Generators.patientGen) { (patient: Patient) =>
      val csvRow = List(
        patient.id,
        patient.maybeAge.map(_.value.toString).getOrElse(""),
        patient.maybeGender.map(_.toString).getOrElse(""),
        patient.diagnosis,
        patient.maybeAnatomicSite.map(_.toString).getOrElse("")
      )

      assertEquals(Patient.csvRowDecoder.decode(csvRow), Right(patient))
    }
  }

  property("Patient.csvRowDecoder returns `Left(_)` for an invalid patient CSV row") {
    forAll(Generators.patientGen) { (patient: Patient) =>
      val csvRow = List(
        patient.maybeAge.map(_.toString).getOrElse(""),
        patient.diagnosis,
        patient.maybeAnatomicSite.map(_.toString).getOrElse("")
      )

      assert(Patient.csvRowDecoder.decode(csvRow).isLeft)
    }
  }
}
