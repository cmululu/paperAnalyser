getKeywordsFromCnkiNotefirst<- function(path,mode="chinese"){

  require(XML)
  require(data.table)
  require(stringi)

  xml <- xmlParse(path,encoding = "UTF8")

  ns <- getNodeSet(xml, "//Bibliography")
  ##将list转换为数据框
  df <- rbindlist(lapply(ns, function(x){
    ##提取pmid
    title <- xpathSApply(x,".//Title",xmlValue)
    year <- xpathSApply(x,".//Year",xmlValue)
    if(mode == "english" | mode == "2")
      keywords <- xpathSApply(x,".//Keyword[@Lang='en']",xmlValue)
    else if(mode == "all" | mode == "3")
      keywords <- xpathSApply(x,".//Keyword",xmlValue)
    else
      keywords <- xpathSApply(x,".//Keyword[@Lang='zh-CHS']",xmlValue)
    keywords <-stri_split_regex(keywords,";|,")
    keywords <- unlist(keywords)
    return(data.frame(title,year,keywords,stringsAsFactors = F))
  }))
  class(df) <- "data.frame"
  return(df)
}
