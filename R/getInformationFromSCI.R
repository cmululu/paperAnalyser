getInformationFromSCI <- function(path, abbname, paste = FALSE) {
  require(iterators)
  require(stringr)

  ##字段缩写abbname
  abbname <- abbname
  ##逐行读取文件
  it <- ireadLines(file.path(path), warn = FALSE)
  ##文章ID
  articleid <- 1
  df <- data.frame("", "", stringsAsFactors = F)
  names(df) <- c("articleid", abbname)
  #如果paste等于TRUE,用来存储组合字段
  paste_result <- ""
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
    ##如果是如果是记录结束标识则文章号加1
    if (first_2_chars == "ER")
      articleid <- articleid + 1
    ##如果不是要查找的字段，并且也不是字段下方的内容则跳过
    if (first_2_chars != abbname && token != abbname)
      next()
    ##存储去除头两个字符的剩余字符
    other_chars <- str_sub(str, start = 4L, end = -1L)
    ##如果头两个字符是abbname制定的内容，则赋值token为abbname
    if (first_2_chars == abbname) {
      token <- abbname
      ##如果paste为TURE,就用paste连接多行内容为一行，否则就生成文章号和abb那么构成的多行数据框
      if (paste)
        paste_result <- other_chars
      else
        df <- rbind(df, list(articleid, other_chars))
    } else if (first_2_chars == "  " && token == abbname) {
      if (paste)
        paste_result <- paste(paste_result, str)
      else
        df <- rbind(df, list(articleid, other_chars))
    } else {
      if (paste)
        df <- rbind(df, list(articleid, paste_result))
      token <- 0
      next()
    }
  }
  return(df[-1,])
}
