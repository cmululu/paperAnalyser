getAuthorFromSCI <- function(path, fullname = T) {
  ##如果fullname等于TRUE，则在AF字段提取作者信息
  ##如果fullname等于FLASE，则在AU字段提取作者信息
  if (fullname)
    df <- getInformationFromSCI(path, "AF")
  else
    df <- getInformationFromSCI(path, "AU")
  names(df) <- c("articleid", "authorname")
  return(df)
}
