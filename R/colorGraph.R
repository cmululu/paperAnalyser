colorGraph <- function(g){
  require(RColorBrewer)
  require(igraph)
  plot.igraph(g,
              vertex.size = V(g)$ec*50,
              vertex.color = brewer.pal(10,"Spectral")[cut(V(g)$ec,breaks = seq(0,1,0.1),labels = F)],
              edge.width = E(g)$weight,
              layout = layout_with_kk)
}
