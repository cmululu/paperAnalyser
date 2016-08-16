createCowordMatrix <- function(df, id_colname,word_colname){
  ##df必须由getMeshheading，getAuthor或者mergeYearToMesh方法生成的数据框
  ##id_colname：df中标记文章唯一性的编号，如pmid或者ariticleid等
  ##word_colname: df中用于生成共现矩阵的列名称，如meshheading或者author等


  id <- do.call('$',list(df,id_colname))
  word <- do.call('$',list(df,word_colname))

  id <- as.vector(id)
  word <- as.vector(word)

  word_count <- length(unique(word))

  ##输入由getword_colname生成的数据框
  ##初始化共词矩阵
  coword_matrix <- matrix(data = 0, nrow = word_count, ncol = word_count)

  ##为矩阵添加行列名称
  rownames(coword_matrix) <- unique(word)
  colnames(coword_matrix) <- unique(word)

  ##矩阵赋值
  ##定义用于对比的起始列
  key <-1

  for (x in 2:nrow(df)) {
    #由于字段提取后df的相同的id_colname都连在一起，判断id_colname改变就是文章改变，跳过文章的一个词，然后从第二个词开始与该篇文章之前的词进行对比
    if(id[x] != id[x-1]){
      #超级赋值，否则else的for中无法获得key
      assign("key",x)
      next()
    }
    if(id[x] == id[x-1]){
      for(y in key:(x-1)) {
        mh1 <- word[x]
        mh2 <- word[y]
        #由于是对称矩阵，所以在对称位置赋值
        coword_matrix[mh1,mh2] <- coword_matrix[mh1,mh2]+1
        coword_matrix[mh2,mh1] <- coword_matrix[mh2,mh1]+1
      }
    }
  }
  ##对角线赋值0
  diag(coword_matrix) <- 0

  return(coword_matrix)
}
