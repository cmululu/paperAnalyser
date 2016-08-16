ig.ec <- lapply(ig.ls,function(x){
  +     V(x)$ec <- evcent(x)$vector
  +     return(x)
  + })
