getAuthorFromWanfangNotefirst<- function(path){

  require(XML)
  require(data.table)
  require(stringi)

  xml <- xmlParse(path,encoding = "UTF-8")

  ns <- getNodeSet(xml, "//Bibliography")
  ##将list转换为数据框
  df <- rbindlist(lapply(ns, function(x){
  title <- xpathSApply(x,".//Title[@Lang='chi']|.//Title[not(@Lang)]",xmlValue)
  year <- xpathSApply(x,".//Year",xmlValue)
  auns <- getNodeSet(x, ".//Author/Info[@Lang='chi']|.//Author/Info[not(@Lang)]")
  audf <-rbindlist(lapply(auns, function(y){
    author <- xpathSApply(y,"./FullName",xmlValue)
    orgnization <- xpathSApply(y,"./Organization",xmlValue)
    if(length(orgnization) == 0) return(data.frame(author,NA))
    else return(data.frame(author,orgnization))
  }))


  if(length(title) == 0 ) title <- NA
  if(length(year) == 0) year <- NA
  if(nrow(audf) == 0) audf <- NA
  return(data.frame(title,year,audf,stringsAsFactors = F))
  }),fill = T)
  class(df) <- "data.frame"
  return(df)
}
