#' @title Calculate the pair-wise ODAs among networks
#'
#' @description Calculate the pair-wise ODAs among networks
#'
#' @details mnetODA(netlist,,mean='arithmetic',net2edge=T)
#'
#' @param netlist The input networks, which should a list of graph objects
#'
#' @param  mean "arithmetic" or "geometric" mean to be used
#'
#' @param  net2edge Whether the input graph object should to transformed into a edge matrix
#'
#' @return A matrix of similarity value between 0 and 1
#'
#' @examples
#'
#'  mnetODA(COAD.net)
#'
#' @export mnetSE

mnetODA <- function(netlist,mean='arithmetic',net2edge=T){
  
  mean <- match.arg(mean,c("arithmetic", "geometric"))
  
  net.sim <- matrix(data=1,nrow=length(netlist),ncol=length(netlist))
  
  rownames(net.sim) <- names(netlist);colnames(net.sim) <- names(netlist)
  
  for ( i in 1:(length(netlist)-1)){
    
    n1 <- length(V(netlist[[i]]))
    
    for (j in (i+1):length(netlist)){
    
      n2 <- length(V(netlist[[j]]))
      
      if (n1*n2>0){
    
        net.sim[i,j] <- netGDD(netlist[[i]],netlist[[j]],mean=mean,net2edge=net2edge)
      }
      else{
        
        net.sim[i,j]<-0
        
      }
      net.sim[j,i] <- net.sim[i,j]
    }
  } 
  return(net.sim)
}

