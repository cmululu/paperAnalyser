getAuthorFromCnkiNotefirst<- function(path,firstauthor = FALSE){

  require(XML)
  require(data.table)
  require(stringi)

  xml <- xmlParse(path,encoding = "UTF-8")

  ns <- getNodeSet(xml, "//Bibliography")
  ##将list转换为数据框
  df <- rbindlist(lapply(ns, function(x){
    ##提取pmid
    title <- xpathSApply(x,".//Title",xmlValue)
    year <- xpathSApply(x,".//Year",xmlValue)
    author <- xpathSApply(x,".//Author//FullName",xmlValue)
    organization <- xpathSApply(x,".//Author//Organization",xmlValue)
    if(length(organization) == 0) organization <-NA
    author <-stri_split_regex(author,";|,")
    author <- unlist(author)
    if(firstauthor) author <- author[1]
    return(data.frame(title,year,author,organization,stringsAsFactors = F))
  }))
  class(df) <- "data.frame"
  return(df)
}
