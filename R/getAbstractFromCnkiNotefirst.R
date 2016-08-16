getAbstractFromCnkiNotefirst<- function(path){

  require(XML)
  require(data.table)

  xml <- xmlParse(path,encoding = "UTF-8")

  ns <- getNodeSet(xml, "//Bibliography")
  ##将list转换为数据框
  df <- rbindlist(lapply(ns, function(x){
    ##提取pmid
    title <- xpathSApply(x,".//Title",xmlValue)
    year <- xpathSApply(x,".//Year",xmlValue)
    abstract <- xpathSApply(x,".//Abstract[@Lang='zh-CHS']|.//Abstract[not(@Lang)]",xmlValue)
    if(length(title) == 0) title <- NA
    if(length(year) == 0) year <- NA
    if(length(abstract) == 0) abstract <- NA
    return(data.frame(title,year,abstract,stringsAsFactors = F))
  }))
  class(df) <- "data.frame"
  return(df)
}
