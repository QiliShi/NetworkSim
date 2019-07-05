#' @title Get the paired nodes which shared neighbors from two networks
#'
#' @description Get the paired nodes which shared neighbors from two networks
#'
#' @details nodesCom(net1,net2)
#'
#' @param net1 The input network, which should be a graph object
#'
#' @param  net2 The input network, which should be a graph object
#'
#' @return A matrix of paired nodes
#'
#' @examples
#'
#'  paired <- nodesCom(TCGA-AF-2687.net,TCGA-A6-2686.net)
#'
#' @export nodesCom

nodesCom <-function(net1,net2){
  neighbors1 <- sapply(names(igraph::V(net1)),function(x) names(igraph::neighbors(net1,v=x)))
  neighbors2 <- sapply(names(igraph::V(net2)),function(x) names(igraph::neighbors(net2,v=x)))
  paired.list <- list()
  k=1
  for (i in 1:length(neighbors1)){
    for (j in 1:length(neighbors2)){
      if (any(neighbors1[[i]]%in%neighbors2[[j]])){
        paired.list[[k]] <- c(names(neighbors1)[i],c(names(neighbors2)[j]))
        k <- k+1
      }
    }
  }
  paired <- do.call(rbind,paired.list)
  
  if (length(paired)>0){
  
    colnames(paired) <- c('net1','net2')
  }
  
  return(paired)
}
