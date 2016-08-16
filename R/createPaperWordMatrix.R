createPaperWordMatrix <- function(df,paperid_colname,word_colname){
  ##author: yueyy
  ##paperid,df中表示文章编号的列名
  ##word，df中表示词所在列的列名
  paperid <- do.call("$",list(df,paperid_colname))
  word <- do.call("$",list(df,word_colname))
  c <- length(unique (paperid))
  r <- length(unique(word))
  ##生成矩阵
  mymatrix <- matrix(data = 0, nrow = r, ncol = c)
  ##矩阵转换为数据框
  df <- as.data.frame(mymatrix)
  ##为数据框行列命名，以便后面用行列名称访问数据框，为数据框赋值以便生成词篇矩阵的数据框格式数据
  colnames(df) <- unique (paperid)
  rownames(df) <- unique(word)
  ##数据框赋值
  for(i in 1:nrow(df)) {
    c <- paperid[i]
    r <- word[i]
    df[r,c] <- df[r,c]+1
  }
  return(df)
}
