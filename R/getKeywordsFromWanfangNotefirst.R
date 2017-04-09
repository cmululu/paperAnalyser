getKeywordsFromWanfangNotefirst<- function(path,mode="chinese"){

  require(XML)
  require(data.table)
  require(stringi)

  xml <- xmlParse(path,encoding = "UTF8")

  ns <- getNodeSet(xml, "//Bibliography")
  ##将list转换为数据框
  df <- rbindlist(lapply(ns, function(x){
    ##提取pmid
    title <- xpathSApply(x,".//Title[@Lang='chi']|.//Title[not(@Lang)]",xmlValue)
    year <- xpathSApply(x,".//Year",xmlValue)
    if(mode == "english" | mode == "2")
      keywords <- xpathSApply(x,".//Keyword[@Lang='eng']",xmlValue)
    else if(mode == "all" | mode == "3")
      keywords <- xpathSApply(x,".//Keyword",xmlValue)
    else
      keywords <- xpathSApply(x,".//Keyword[@Lang='chi']|.//Keyword[not(@Lang)]",xmlValue)
    if(length(title) == 0 ) title <- NA
    if(length(year) == 0) year <- NA
    if(length(keywords) ==0) keywords <- NA
    return(data.frame(title,year,keywords,stringsAsFactors = F))
  }))
  class(df) <- "data.frame"
  return(df)
}
