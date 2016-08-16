getKeywordsFromPubmedXML<- function(path){
  ##author: yueyy
  require(XML)
  require(data.table)
  xml <- xmlParse(path)
  ns <- getNodeSet(xml, "//MedlineCitation")
  ##将list转换为数据框
  df <- rbindlist(lapply(ns, function(x){
    ##提取pmid
    pmid <- xpathSApply(x,"./PMID",xmlValue)
    ##提取出版年
    year <- xpathSApply(x,".//PubDate/Year",xmlValue)
    ##如果pubDate下有没有Year则提取MedlineDate，并截取前4位
    if(length(year) == 0 ) {
      medlinedate <- xpathSApply(x,".//PubDate/MedlineDate",xmlValue)
      year <- substr(medlinedate, start = 1L, stop = 4L)
    }
    ##获取kewords
    keywords <- xpathSApply(x,".//Keyword",xmlValue)
    if(length(title) == 0 ) title <- NA
    if(length(year) == 0) year <- NA
    if(length(keywords) ==0) keywords <- NA
    return(data.frame(pmid, year, keywords, stringsAsFactors = F))
  }))
  class(df) <- "data.frame"
  return(df)
}
