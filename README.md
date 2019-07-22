NetworkSim
==========

***Current version***:1.1.1, ***Depends***: R (&gt;= 3.4.0)

Description
-----------

NetworkSim, an R package that provides two different ways for undirected network comparison, which contains the structural equivalence based on paired nodes and the graphlet-based systematic measurement.

Installation
------------

NetworkSim currently needs to be installed using devtools. The igraph and orca packages also needs to be installed.

``` r
devtools::install_github("QiliShi/NetworkSim")
```

Running times
-------------

Create different sizes of PPI networks

``` r
library(NetworkSim)
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
small_net1 <-subnet(databases.net,nodes = c(names(neighbors(databases.net,'TP53')),'TP53')[1:75],neighbors = F)
small_net2 <-subnet(databases.net,nodes = c(names(neighbors(databases.net,'TP53')),'TP53')[40:120],neighbors = F)
middle_net1 <-subnet(databases.net,nodes = c(names(neighbors(databases.net,'TP53')),'TP53')[1:300],neighbors = F)

middle_net2 <-subnet(databases.net,nodes = c(names(neighbors(databases.net,'TP53')),'TP53')[150:450],neighbors = F)

nodes <- c(names(neighbors(databases.net,'TP53')),'TP53',names(neighbors(databases.net,'MYC')),'MYC',
           names(neighbors(databases.net,'APP')))
large_net1 <-subnet(databases.net,nodes = nodes[1:2000],neighbors = F)

large_net2 <-subnet(databases.net,nodes = nodes[500:3300],neighbors = F)
nodes <- names(V(databases.net))

Vlarge_net1 <-subnet(databases.net,nodes = sample(nodes,15000),neighbors = F)

Vlarge_net2 <-subnet(databases.net,nodes = sample(nodes,15000),neighbors = F)
```

Measure the running times on small (tens of nodes, hundreds of edges), medium (hundreds of nodes, thousands of edges), large ( thousands of nodes, tens of thousands of edges), and very large ( tens of thousands of nodes, hundreds of thousands of edges) sizes of PPI networks from Human Protein Reference Database (HPRD), BioGRID and mentha databases.

``` r
system.time(paired <- nodesCom(middle_net1,middle_net2))
```

    ##    user  system elapsed 
    ##    0.92    0.01    1.08

``` r
system.time(netSE(middle_net1,middle_net2,paired[1,1],paired[1,2]))
```

    ##    user  system elapsed 
    ##    0.02    0.00    0.01

``` r
system.time(netODA(middle_net1,middle_net2))
```

    ##    user  system elapsed 
    ##    2.00    0.02    2.08

Examples
--------

Create the input networks with igraph types

1.Create networks from our intergrated human PPI network.

``` r
TCGA_AF_2687.net<-subnet(net=databases.net, nodes=TCGA_AF_2687.Muts)
```

2.Create networks from user's own data, which should be a data frame containing a symbolic edge list in the two columns.

``` r
library(sand)
```

    ## Loading required package: igraphdata

    ## 
    ## Statistical Analysis of Network Data with R
    ## Type in C2 (+ENTER) to start with Chapter 2.

``` r
g <- graph.data.frame(elist.lazega,directed=F)
```

Get the paired nodes between two networks

``` r
paired<-nodesCom(TCGA_AF_2687.net, TCGA_A6_2686.net)
```

Calculate a similarity of a pair of nodes

``` r
netSE(TCGA_AF_2687.net, TCGA_A6_2686.net, 'NMT1', 'MED10')
```

    ## [1] 0.1825742

Calculate all structural equivalence of the paired nodes

``` r
paired.SE<-netSEs(TCGA_AF_2687.net,TCGA_A6_2686.net, paired)
```

The overall agreement based on graphlets of two networks can be gained by

``` r
netODA(TCGA_AF_2687.net, TCGA_A6_2686.net)
```

    ## [1] 0.4195262
