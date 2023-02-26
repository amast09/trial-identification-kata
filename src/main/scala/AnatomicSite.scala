package trial.identification.kata

case class AnatomicSiteParseFailure(invalidValue: String)

sealed trait AnatomicSite
object AnatomicSite {
  case object Lung extends AnatomicSite
  case object Kidney extends AnatomicSite
  case object Brain extends AnatomicSite
  case object Bladder extends AnatomicSite

  // Note: The example in the instructions only match on a single site,
  //  so initial implementation will be naive and pick the first match
  //
  // Note: I only included sites that are represented in the data,
  //  obviously there is more than these 4 sites in the real world :)
  def fromString(
      anatomicSiteString: String
  ): Either[AnatomicSiteParseFailure, AnatomicSite] =
    anatomicSiteString
      .toLowerCase()
      .split(" ")
      .foldLeft[Either[AnatomicSiteParseFailure, AnatomicSite]](
        Left(AnatomicSiteParseFailure(anatomicSiteString))
      )((maybeSite, nextWord) => {
        maybeSite.orElse(nextWord match {
          case "lung"    => Right(Lung)
          case "kidney"  => Right(Kidney)
          case "brain"   => Right(Brain)
          case "bladder" => Right(Bladder)
          case _         => maybeSite
        })
      })
}
