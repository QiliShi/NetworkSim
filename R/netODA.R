#' @title Calculate the orbit distribution agreement (ODA) between two networks
#'
#' @description Calculate the orbit distribution agreement (ODA) between two networks
#'
#' @details netODA(net1,net2,mean='arithmetic',net2edge=T)
#'
#' @param net1 The input network, which should be one of incgraph and igraph objects
#'
#' @param  net2 The input network, which should be one of incgraph and igraph objects
#'
#' @param  mean "arithmetic" or "geometric" mean to be used
#'
#' @param  net2edge Whether the input graph object should to transformed into a edge matrix
#'
#' @return Agreement value between 0 and 1
#'
#' @examples
#'
#'  netODA(TCGA-AF-2687.net, TCGA-A6-2686.net)
#'
#' @export netODA

netODA <- function(net1,net2,mean='arithmetic',net2edge=T){

  mean <- match.arg(mean,c("arithmetic", "geometric"))

  if (net2edge) {
    net1 <- t(apply(igraph::get.edgelist(net1,names = F),1,as.integer))

    net2 <- t(apply(igraph::get.edgelist(net2,names = F),1,as.integer))
  }

  net1.orb.counts <- orca::count5(net1)

  net2.orb.counts <- orca::count5(net2)

  net1.orbS <- apply(net1.orb.counts,2,function(x){
                     tmp<-as.data.frame(table(x));tmp[,1]<-as.character(tmp[,1]);return(tmp)})
  net2.orbS <- apply(net2.orb.counts,2,function(x){
                     tmp<-as.data.frame(table(x));tmp[,1]<-as.character(tmp[,1]);return(tmp)})

  orbN <- function(o1,o2){
    u<-union(o1$x,o2$x);supple1<-u[!u%in%o1$x];
    if (length(supple1)>0){
      o1<-rbind(o1,data.frame(x=supple1,Freq=0));}

    o1$x<-as.numeric(o1$x);o1<-o1[order(o1$x),];o1<-o1[o1$x!=0,];

    if (sum(o1$Freq)!=0){
      o1$norm<-o1$Freq/o1$x;o1$frac<-o1$norm/sum(o1$norm);
      return(o1$frac)}
    if (sum(o1$Freq)==0){
      return(o1$Freq)}}
  net1.orbN <- mapply(orbN,net1.orbS,net2.orbS,SIMPLIFY = T)
  net2.orbN <- mapply(orbN,net2.orbS,net1.orbS,SIMPLIFY = T)

  if (mean=="arithmetic"){
    sim.GDD <- mean(1-mapply(function(o1,o2) sum((o1-o2)^2)^0.5,net1.orbN,net2.orbN))
  }
  if (mean=="geometric"){
    sim.GDD <- prod(1-mapply(function(o1,o2) sum((o1-o2)^2)^0.5,net1.orbN,net2.orbN))^(1/73)
  }

  return(sim.GDD)
}
