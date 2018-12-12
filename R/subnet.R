#' @title Subnetwork from a network
#'
#' @description subnet generates a subnetwork from a network,which consists of specified nodes
#'
#' @details subnet(net,nodes,impl="auto",neighbors=F,delete=T)
#'
#' @param net The original net, which should be a graph object
#' 
#' @param  nodes Gene symbol, which should match the nodes in original net
#' 
#' @param  impl Character scalar, to choose between two implementation of the subgraph calculation. Details are implemented in subraph function of igraph package
#' 
#' @param  neighbors Logical scalar, whether to get the neighbors of input nodes
#' 
#' @param  delete Logical scalar, whether to remove the nodes which do not have any neighbors in the output network
#'
#' @return A new igraph graph object
#'
#' @examples  
#' 
#' NOR2ADE.net <-subnet(net=databases.net,nodes = NOR2ADE.DEGs)
#'
#' @export subnet

subnet <- function(net,nodes,impl="auto",neighbors=F,delete=T)
{
  impl <- match.arg(impl,c("auto", "copy_and_delete","create_from_scratch"))
  
  nodes <- nodes[nodes%in%names(igraph::V(net))]
  
  if (!neighbors){
    
    subgraph <- igraph::induced.subgraph(net,vids =nodes,impl =impl)
  }
  else{
    Neighbors <-do.call(c,sapply(nodes, function(x) names(igraph::neighbors(net,v=x))))
    nodes <- unique(c(Neighbors,nodes))
    subgraph <- igraph::induced.subgraph(net,vids = nodes)
  }
  
  if (delete){
    subgraph <- igraph::induced.subgraph(subgraph,vids = names(igraph::degree(subgraph))[igraph::degree(subgraph)>0])
  }
  return(subgraph)
}