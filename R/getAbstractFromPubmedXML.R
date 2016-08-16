getAbstractFromPubmedXML <- function(path){
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
    ##提取zhaoyao
    abstract <- xpathSApply(x,".//AbstractText",xmlValue)
    if(length(abstract) > 1){
      attr <-xpathApply(x,".//AbstractText",xmlAttrs)
      #if(is.null(nrow(attr))) label <- attr
      #else label <- attr["Label",]
      abstract<-paste(attr,abstract,sep = ":")
    }
    ##如果任何一项为空，就将该项设置为NA
    if(length(pmid) == 0) pmid <- NA
    if(length(abstract) == 0) abstract <- NA
    return(data.frame(year,pmid,abstract ,stringsAsFactors = F))
  }))
  class(df) <- c("data.frame")
  return(df)
}
