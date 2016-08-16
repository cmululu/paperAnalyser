
getAuthorFromPubmedXML <- function(path){
  ## 获取pmid,author组成的数据框,每篇文章author不唯一
  ## author:yueyy

  require(data.table)
  require(XML)

  ##读取xml文件
  xml <- xmlParse(path, encoding = "UTF-8")
  ##提取MedlineCitation节点集合
  ns <- getNodeSet(xml, "//MedlineCitation")
  ##生成数据框
  df <- rbindlist(lapply(ns, function(x) {
    ##提取文章的pmid，pmid唯一
    pmid <- xpathSApply(x,"./PMID",xmlValue)
    ##提取出版年
    year <- xpathSApply(x,".//PubDate/Year",xmlValue)
    ##如果pubDate下有没有Year则提取MedlineDate，并截取前4位
    if(length(year) == 0 ) {
      medlinedate <- xpathSApply(x,".//PubDate/MedlineDate",xmlValue)
      year <- substr(medlinedate, start = 1L, stop = 4L)
    }
    ##提取作者节点集
    author.ns <- getNodeSet(x, ".//Author")
    ##组合作者名称和所在单位
    author.df <- rbindlist(lapply(author.ns, function(y){
      ##提取last name
      lastname <- xpathSApply(y,"./LastName",xmlValue)
      ##提取fore name
      forename <- xpathSApply(y,"./ForeName",xmlValue)
      ##组合lastname和forename，
      name <- paste(forename,lastname)
      ##提取作者所在单位，可能不唯一
      affiliation <- xpathSApply(y,"./AffiliationInfo/Affiliation",xmlValue)
      ##如果没有作者单位，以NA表示。如果作者有所在单位（可能多于一个单位），
      ##则与名字组合为数据框
      if(length(name) > 0){
        if(length(affiliation) == 0){
          data.frame(authorname = name, affiliation = NA,stringsAsFactors = F)
        }else{
          data.frame(authorname = name, affiliation = affiliation,stringsAsFactors = F)
        }
      }
    }
    ))
    ##如果文章有作者信息，则生成数据框，否则返回空数据框
    if(length(author.df) == 0) author.df <- NA
    return(data.frame(pmid,year,author.df,stringsAsFactors = F))
  }))
  class(df) <- "data.frame"
  return(df)
}
