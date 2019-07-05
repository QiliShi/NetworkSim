#' @title Calculate the paired structural equivalences among networks
#'
#' @description Calculate the paired structural equivalences among networks
#'
#' @details mnetSE(netlist,node,method='cosine')
#'
#' @param netlist The input networks, which should a list of graph objects
#'
#' @param  node node for calculation of paired structural equivalences
#'
#' @param  method The similarity measure to be used. This must be "cosine" or "euclidean".
#'
#' @return A matrix of similarity value between 0 and 1
#'
#' @examples
#'
#'  mnetSE(COAD.net,'APC')
#'
#' @export mnetSE

mnetSE <- function(netlist,node,method='cosine'){
  
  method <- match.arg(method,c("cosine","euclidean"))
  
  net.sim <- matrix(data=1,nrow=length(netlist),ncol=length(netlist))
  
  rownames(net.sim) <- names(netlist);colnames(net.sim) <- names(netlist)
  
  
  for ( i in 1:(length(netlist)-1)){
    
    nodes1 <- names(igraph::V(netlist[[i]]))
    
    for (j in (i+1):length(netlist)){
      
      nodes2 <- names(igraph::V(netlist[[j]]))
      
      if (node%in%nodes2&node%in%nodes1){
        
        net.sim [i,j] <- netSE(netlist[[i]],netlist[[j]],node1=node,node2=node,method = method)
      }
      else{
        net.sim[i,j] <- 0
      }
      net.sim[j,i] <- net.sim[i,j]
    }
  }
  return(net.sim)
}

