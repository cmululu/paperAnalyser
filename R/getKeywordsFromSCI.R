getKeywordsFromSCI <- function(path) {
  require(stringi)
  unsplit.df <- getInformationFromSCI(path, "DE")
  names(unsplit.df) <- c("articleid", "keyword")
  df<-NA
  for (i in 1:nrow(unsplit.df)) {
    ##按分号拆分keyword，排除空置，添加到data.frame中
    keywords <- unlist(stri_split_fixed(unsplit.df$keyword[i],";",omit_empty=T))
    articleid <- unsplit.df$articleid[i]
    df<-rbind(df,data.frame(articleid,keywords))
  }
  ##删除第一行的NA
  return(df[-1,])
}



