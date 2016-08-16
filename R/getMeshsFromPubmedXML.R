getMeshsFromPubmedXML<- function(path, mode = "all_desriptor_with_qualifier"){
  ##author: yueyy
  ##mode=  "all_desriptor_with_qualifier" 全部主题词加副主题词/或者简写mode=“1”
  ##mode == "all_desriptor_without_qualifier" 全部主题词无副主题词/或者简写mode=“2”
  ##mode == "ymajortopic_desriptor_with_qualifier" 主要主题词加副主题词/或者简写mode=“3”
  ##mode == "ymajortopic_desriptor_without_qualifier" 主要主题词无副主题词/或者简写mode=“4”
  ##mode =="nmajortopic_desriptor_with_qualifier" 次要主题词加副主题词或者/简写mode=“5”
  ##mode =="nmajortopic_desriptor_without_qualifier"  次要主题词无副主题词/或者简写mode=“6”

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
    ##获取meshheading节点
    meshns <- getNodeSet(x,".//MeshHeading")
    ##提取主题词
    if(mode == "all_desriptor_with_qualifier" || mode =="1"){
      meshheading <- unlist(lapply(meshns, function(y){
        mesh <- xpathSApply(y,"./DescriptorName",xmlValue)
        qual <- xpathSApply(y,"./QualifierName",xmlValue)
        if(length(mesh) > 0){
          if(length(qual) > 0){
            return(paste(mesh,"<",qual,">",sep = ""))
          }  else{
            return(mesh)
          }
        }
      }))
    } else if(mode == "all_desriptor_without_qualifier" || mode =="2"){
      meshheading <- xpathSApply(x,".//DescriptorName",xmlValue)
    } else if(mode == "ymajortopic_desriptor_with_qualifier" || mode =="3"){
      meshheading <- unlist(lapply(meshns, function(y){
        mesh <- xpathSApply(y,"./DescriptorName[@MajorTopicYN = 'Y']",xmlValue)
        qual <- xpathSApply(y,"./QualifierName",xmlValue)
        if(length(mesh) > 0){
          if(length(qual) > 0){
            return(paste(mesh,"<",qual,">",sep = ""))
          }  else{
            return(mesh)
          }
        }
      }))
    } else if(mode == "ymajortopic_desriptor_without_qualifier"  || mode =="4"){
      meshheading <- xpathSApply(x,".//DescriptorName[@MajorTopicYN = 'Y']",xmlValue)
    } else if(mode =="nmajortopic_desriptor_with_qualifier"  || mode =="5"){
      meshheading <- unlist(lapply(meshns, function(y){
        mesh <- xpathSApply(y,"./DescriptorName[@MajorTopicYN = 'N']",xmlValue)
        qual <- xpathSApply(y,"./QualifierName",xmlValue)
        if(length(mesh) > 0){
          if(length(qual) > 0){
            return(paste(mesh,"<",qual,">",sep = ""))
          }  else{
            return(mesh)
          }
        }
      }))
    } else if(mode =="nmajortopic_desriptor_without_qualifier"  || mode =="6"){
      meshheading <- xpathSApply(x,".//DescriptorName[@MajorTopicYN = 'N']",xmlValue)
    }

    ##如果有主题词，生成dataframe
    if(length(pmid) == 0) pmid <- NA
    if(length(year) == 0) year <- NA
    if(length(meshheading) == 0) meshheading <- NA
    return(data.frame(pmid, year, meshheading, stringsAsFactors = F))
  }))
  class(df) <- "data.frame"
  return(df)
}
