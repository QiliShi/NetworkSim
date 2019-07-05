#' @title Calculate the structural equivalence of paired nodes
#'
#' @description Calculate the structural equivalence of paired nodes
#'
#' @details netSEs(net1,net2,nodes,method='cosine')
#'
#' @param net1 The input network, which should be a graph object
#'
#' @param  net2 The input network, which should be a graph object
#'
#' @param  nodes The paired nodes between two networks
#'
#' @param  method The similarity measure to be used. This must be "cosine" or "euclidean".
#'
#' @return A vector of similarity value 
#'
#' @examples
#' 
#'  paired<-nodesCom(TCGA-AF-2687.net, TCGA-A6-2686.net)
#'
#'  netSEs(TCGA-AF-2687.net, TCGA-A6-2686.net, paired)
#'
#' @export netSEs

netSEs <- function(net1,net2,nodes,method='cosine'){
  
  Paired.SE<- apply(nodes,1,function(x) netSE(net1,net2,x[1],x[2],method=method))
  
  return(Paired.SE)
}


