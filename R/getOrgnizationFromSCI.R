getOrgnizationFromSCI <- function(path){
  df <- getInformationFromSCI(path,"C1")
  result_df <-data.frame("","",stringsAsFactors = F)
  names(result_df) <- c("articleid","orgnization")

  for(i in 1:nrow(df)){
    c1 <- df$C1[i]
    ##如果组织的构成中含有"]",即组织的构成中包括了作者信息，只提取组织
    if(!is.na(stri_extract_first_fixed(c1,"]")))
      orgnization <- unlist(stri_split_fixed(c1,"]"))[2]
    else
      orgnization <- c1
    result_df <- rbind(result_df,list(df$articleid[i],orgnization))
  }
  return(result_df[-1,])
}
