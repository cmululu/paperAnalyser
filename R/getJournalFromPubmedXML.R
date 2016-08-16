
getJournalFromPubmedXML <- function(path){

  ##获取pubyear，pmid,杂志名,杂志缩写，国家组成的数据框
  ##注：只有每篇文章所提取字段均为唯一时，才能多个字段组合为多列data.##frame,否则只能一个唯一值加一个非唯一值构成，两列的data.frame

  require(XML)
  require(data.table)

  xml <- xmlParse(path)
  ns <- getNodeSet(xml, "//MedlineCitation")
  ##将list转换为数据框
  df <- rbindlist(lapply(ns, function(x) {
    ##提取年份
    ##如果pubDate下有Year则提取Year
    year <- xpathSApply(x,".//PubDate/Year",xmlValue)
    ##如果pubDate下有没有Year则提取MedlineDate，并截取前4位
    if(length(year) == 0 ) {
      medlinedate <- xpathSApply(x,".//PubDate/MedlineDate",xmlValue)
      year <- substr(medlinedate, start = 1L, stop = 4L)
    }
    ##提取pmid
    pmid <- xpathSApply(x,"./PMID",xmlValue)
    ##提取杂志名
    journal.name <- xpathSApply(x,".//Journal/Title",xmlValue)
    ##提取杂志缩写
    journal.abbr.name <- xpathSApply(x,".//Journal/ISOAbbreviation",xmlValue)
    ##提取杂志所在国家
    journal.country <- xpathSApply(x,"./MedlineJournalInfo/Country",xmlValue)
    ##提取杂志语种
    journal.language <- xpathSApply(x,".//Language",xmlValue)
    ##如果任何一项为空，就将该项设置为NA
    if(length(pmid) == 0) pmid <- NA
    if(length(journal.name) == 0) journal.name <- NA
    if(length(journal.abbr.name) == 0) journal.abbr.name <- NA
    if(length(journal.country) == 0) journal.country <- NA
    if(length(journal.language) == 0) journal.language <- NA
    return(data.frame(year,pmid, journal.name, journal.abbr.name, journal.country,journal.language,stringsAsFactors = F))
  }))
  ##去掉journal.df的data.table类属性，只保留data.frame类属性.
  ##否则journal.df[,1]格式访问列无法得到想要的结果
  class(df) <- c("data.frame")
  return(df)
}
