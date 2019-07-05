#' @title Calculate the structural equivalence between two networks
#'
#' @description Calculate the structural equivalence between two networks
#'
#' @details netSE(net1,net2,node1,node2,method='cosine')
#'
#' @param net1 The input network, which should be a graph object
#'
#' @param  net2 The input network, which should be a graph object
#'
#' @param  node1 The paired node of network 1
#' 
#' @param  node2 The paired node of network 2
#'
#' @param  method The similarity measure to be used. This must be "cosine" or "euclidean".
#'
#' @return Similarity value between 0 and 1
#'
#' @examples
#'
#'  netSE(TCGA-AF-2687.net, TCGA-A6-2686.net,'NMT1', 'MED10')
#'
#' @export netSE

netSE <- function(net1,net2,node1,node2,method='cosine'){
  method <- match.arg(method,c("cosine","euclidean"))

  length12 <- length(names(igraph::neighbors(net1,node1))[names(igraph::neighbors(net1,node1))
                     %in%names(igraph::neighbors(net2,node2))])
  length1 <-length(names(igraph::neighbors(net1,node1)))

  length2 <-length(names(igraph::neighbors(net2,node2)))

  if (method=='cosine'){
    sim <- length12/(length1*length2)^0.5
  }
  if (method=='euclidean'){
    sim <- 2*length12/(length1+length2)
  }
  if (is.na(sim)) sim <-0
  
  return(sim)
}

