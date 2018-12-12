#' @title Get the common nodes from two networks
#'
#' @description Get the common nodes from two networks
#'
#' @details subnet(net1,net2)
#'
#' @param net1 The input network, which should be a graph object
#'
#' @param  net2 The input network, which should be a graph object
#'
#' @return A vector of gene symbols
#'
#' @examples
#'
#'  Aligned <- nodesCom(NOR2ADE.net,ADE2CAR.net)
#'
#' @export nodesCom

nodesCom <-function(net1,net2){
  common<- names(igraph::V(net1))[names(igraph::V(net1))%in%names(igraph::V(net2))]
  return(common)
}
