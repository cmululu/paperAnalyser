getAbstractFromSCI <- function(path) {
    require(iterators)
    require(stringr)

    ##字段缩写abbname
    abbname <- "AB"
    ##逐行读取文件
    it <- ireadLines(file.path(path), warn = FALSE)
    ##文章ID
    articleid <- 1
   # m <- matrix(NA, ncol = 2)
    df <- data.frame("","",stringsAsFactors = F)
    names(df) <- c("articleid", abbname)
    ab <-""
    token <- 0
    while (TRUE) {
      str <- nextElem(it)
      nch <- nchar(str)
      first_2_chars <- str_sub(str, start = 1L, end = 2L)

      ##如果是开头文件名和版本号则跳过
      if (first_2_chars == "FN" || first_2_chars == "VR")
        next()
      ##如果是如果是结尾标识则结束循环
      if (first_2_chars == "EF")
        break
      ##如果是记录结束标识则文章号加1
      if(first_2_chars == "ER")
        articleid <- articleid + 1
      ##如果不是要查找的字段，并且也不是字段下方的内容则跳过
      if (first_2_chars != abbname && token != abbname)
        next()
      other_chars <- str_sub(str, start = 4L, end = -1L)
      if (first_2_chars == abbname) {
        token <- abbname
        ab <- other_chars
      } else if (first_2_chars != "C1" && token == abbname) {
        ab <- paste(ab,str)
      } else {
        df <- rbind(df,list(articleid,ab))
        token <- 0
        next()
      }
    }
    return(df[-1, ])
  }
