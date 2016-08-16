getCitedReferenceFromSCI <- function(path) {
  df <- getInformationFromSCI(path, "CR")
  names(df) <- c("articleid", "CitedReference")
  return(df)
}
