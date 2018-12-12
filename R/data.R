#' integrated PPI network
#'
#' A dataset containing the integrated PPI network from Biogrid, HPRD and mentha databases
#'
#'
#' @format A igraph object
#'
"databases.net"

#' DEGs between colonic normals and adenomas
#'
#' A dataset containing 4567 differentially expressed genes between normals and adenomas
#'
#'
#' @format A  character vector
#'
"NOR2ADE.DEGs"

#' DEGs of the adenoma-carcinoma transition
#'
#' A dataset containing 1290 DEGs of the adenoma-carcinoma transition
#'
#'
#' @format A  character vector
#'
"ADE2CAR.DEGs"

#' PPI network of NOR2ADE.DEGs
#'
#' A dataset containing the PPI network consisted of DEGs between colonic normals and adenomas
#'
#'
#' @format A igraph object
#'
"NOR2ADE.net"

#' PPI network of ADE2CAR.DEGs
#'
#' A dataset containing the PPI network consisted of DEGs between colonic adenomas and carcinomas
#'
#'
#' @format A igraph object
#'
"ADE2CAR.net"
