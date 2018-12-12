#' @title Calculate the structural equivalence between two networks
#'
#' @description Calculate the structural equivalence between two networks
#'
#' @details netSE(net1,net2,node,method='cosine')
#'
#' @param net1 The input network, which should be a graph object
#'
#' @param  net2 The input network, which should be a graph object
#'
#' @param  node Gene symbol, which is shared between two networks
#'
#' @param  method The similarity measure to be used. This must be "cosine" or "euclidean".
#'
#' @return Similarity value between 0 and 1
#'
#' @examples
#'
#'  netSE(NOR2ADE.net,ADE2CAR.net,'EREG')
#'
#' @export netSE

netSE <- function(net1,net2,node,method='cosine'){
  method <- match.arg(method,c("cosine","euclidean"))

  length12 <- length(names(igraph::neighbors(net1,node))[names(igraph::neighbors(net1,node))
                     %in%names(igraph::neighbors(net2,node))])
  length1 <-length(names(igraph::neighbors(net1,node)))

  length2 <-length(names(igraph::neighbors(net2,node)))

  if (method=='cosine'){
    sim <- length12/(length1*length2)^0.5
  }
  if (method=='euclidean'){
    sim <- 2*length12/(length1+length2)
  }
  return(sim)
}

