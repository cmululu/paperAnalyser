getJournalFromWanfangNotefirst<- function(path){

  require(XML)
  require(data.table)

  xml <- xmlParse(path,encoding = "UTF-8")

  ns <- getNodeSet(xml, "//Bibliography")
  ##将list转换为数据框
  df <- rbindlist(lapply(ns, function(x){
    title <- xpathSApply(x,".//Title[@Lang = 'chi']|.//Title[not(@Lang)]",xmlValue)
    year <- xpathSApply(x,".//Year",xmlValue)
    journal <-xpathSApply(x,".//Media[@Lang ='chi']|.//Media[not(@Lang)]",xmlValue)
    if(length(year) == 0) year <- NA
    if(length(title) == 0) title <- NA
    if(length(journal) == 0) journal <- NA
    return(data.frame(title,year,journal,stringsAsFactors = F))
  }))
  class(df) <- "data.frame"
  return(df)
}
