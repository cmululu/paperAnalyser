getAuthorFromWanfangNotefirst<- function(path,firstauthor=FALSE){
  #firstauthor=FALSEfirstauthor=TRUEֻ??ȡ??һ????
  require(XML)
  require(data.table)
  require(stringi)

  xml <- xmlParse(path,encoding = "UTF-8")

  ns <- getNodeSet(xml, "//Bibliography")
  ##??listת??Ϊ???ݿ?
  df <- rbindlist(lapply(ns, function(x){
    title <- xpathSApply(x,".//Title[@Lang='chi']|.//Title[not(@Lang)]",xmlValue)
    year <- xpathSApply(x,".//Year",xmlValue)
    auns <- getNodeSet(x, ".//Author/Info[@Lang='chi']|.//Author/Info[not(@Lang)]")
    if(firstauthor) auns <- auns[1]
    audf <-rbindlist(lapply(auns, function(y){
      author <- xpathSApply(y,"./FullName",xmlValue)
      orgnization <- xpathSApply(y,"./Organization",xmlValue)
      if(length(orgnization) == 0){
          return(data.frame(author,orgnization=NA))
        }
      else
        {
          return(data.frame(author,orgnization))
        }
    }),fill=TRUE,use.names = TRUE)
    if(length(title) == 0 | nrow(audf) == 0) return()
    if(length(year) == 0) year <- NA
    return(data.frame(title,year,audf,stringsAsFactors = F))
  }),fill=TRUE,use.names = TRUE)
  class(df) <- "data.frame"
  return(df)
}
