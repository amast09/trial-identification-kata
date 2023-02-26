package trial.identification.kata

import kantan.csv._
import kantan.csv.ops._

case class Patient(
    id: String,
    maybeAge: Option[Age],
    maybeGender: Option[Gender],
    diagnosis: String,
    maybeAnatomicSite: Option[AnatomicSite]
)

object Patient {
  def apply(
      id: String,
      maybeAge: Option[Age],
      maybeGender: Option[Gender],
      diagnosis: String,
      maybeAnatomicSite: Option[AnatomicSite]
  ): Patient = {
    new Patient(
      id,
      maybeAge,
      maybeGender,
      diagnosis.toLowerCase().trim(),
      maybeAnatomicSite
    )
  }

  // Note: Currently the program blows up when there is invalid data. This could
  //  be improved with further requirements around how error handling should function.
  //  Currently this is a naive implementation expecting all data to be valid that will
  //  blow up immediately if it encounters any invalid data
  implicit val csvRowDecoder: RowDecoder[Patient] = {
    RowDecoder.decoder[
      String,
      Option[String],
      Option[String],
      String,
      Option[String],
      Patient
    ](0, 1, 2, 3, 4)(
      (
          idColumnValue: String,
          maybeAgeColumnValue: Option[String],
          maybeGenderColumnValue: Option[String],
          diagnosisColumnValue: String,
          maybeAnatomicSiteColumnValue: Option[String]
      ) => {
        val maybeAge = maybeAgeColumnValue.map(ageColumnValue =>
          Age.fromString(ageColumnValue).toOption.get
        )
        val maybeGender = maybeGenderColumnValue.map(genderColumnValue =>
          Gender.fromString(genderColumnValue).toOption.get
        )

        val maybeAnatomicSite =
          maybeAnatomicSiteColumnValue.map(anatomicSiteColumnValue =>
            AnatomicSite.fromString(anatomicSiteColumnValue).toOption.get
          )

        Patient(
          id = idColumnValue,
          maybeAge = maybeAge,
          maybeGender = maybeGender,
          diagnosis = diagnosisColumnValue,
          maybeAnatomicSite = maybeAnatomicSite
        )
      }
    )
  }

  def patientsFromCSVFile(fileName: String): List[Patient] = getClass
    .getResource(fileName)
    .asCsvReader[Patient](rfc.withHeader)
    .toList
    .flatMap(_.toOption)
}
