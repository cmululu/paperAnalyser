countIncreasedNumberOfPaperPerWord <- function(df,paperid_colname,word_colname){
  #拆分数据框返回每个meshheading作为name，pmid作为列表的列表
  paperid <- do.call("$",list(df,paperid_colname))
  word <- do.call("$",list(df,word_colname))

  ls.split <- split(paperid, word)
  #统计上步列表中每个meshheading对应的pmid个数
  ls.split.length <- lapply(ls.split, length)
  #将上步的列表转换为数据框
  ls.df <-data.frame(names(ls.split.length),  unlist(ls.split.length), stringsAsFactors = FALSE)

  colnames(ls.df) <- c("word","papernumber")
  #按pmid个数进行降序排列
  ls.df <- ls.df[order(ls.df$papernumber, decreasing = T),]

  #初始化一个用于储存唯一pmid号的向量
  unique_article_vector <- NULL
  #初始化一个列名为meshheading和uniqu.article.number的空数据框，并禁止将字符转为因子，因为后面储存的是字符而不是因子
  mesh_article_df <- data.frame(word = NA,papernumber = NA,stringsAsFactors = FALSE)
  colnames(mesh_article_df) <- c("word","papernumber")
  #计算
  for(x in 1:nrow(ls.df)) {
    word <-as.vector(ls.df$word[x])
    new_vector <-ls.split[[word]]
    unique_article_vector <- unique(c(unique_article_vector,new_vector))
    unique_article_number <- length(unique_article_vector)
    mesh_article_df <-rbind(mesh_article_df, list(word, unique_article_number))
  }
  return(mesh_article_df[-1,])
}
