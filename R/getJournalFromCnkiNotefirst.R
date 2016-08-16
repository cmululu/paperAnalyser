getJournalFromCnkiNotefirst<- function(path){

  require(XML)
  require(data.table)

  xml <- xmlParse(path,encoding = "UTF8")

  ns <- getNodeSet(xml, "//Bibliography")
  ##将list转换为数据框
  df <- rbindlist(lapply(ns, function(x){
    ##提取pmid
    title <- xpathSApply(x,".//Title",xmlValue)
    year <- xpathSApply(x,".//Year",xmlValue)
    publisher <- xpathSApply(x,".//Publisher",xmlValue)
    if(length(title) == 0) title <- NA
    if(length(year) == 0) year <- NA
    if(length(publisher) == 0) publisher <- NA
    return(data.frame(title,year,publisher,stringsAsFactors = F))
  }))
  class(df) <- "data.frame"
  return(df)
}
