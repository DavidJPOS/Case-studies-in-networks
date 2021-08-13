###############################################################################################
## Project: case studies in networks
## Script purpose: explore igraphs functions
## Date: 13-08-2021
## Author: David JP O'Sullivan
############################################################################################### 

library(tidyverse)
library(igraphdata)
library(igraph)

# using the ukfaculty data to explore the following
data(package = 'igraphdata')
data(UKfaculty) # load  dataset into memory

# what type of network is this? Directed? Undirected? Does it contain multi-edges? 
UKfaculty # this network is a little to large to plot

mention_df <- read_csv(file = './data/sim_mention_df.csv')
follower_df <- read_csv(file = './data/sim_follower_df.csv')

# EX ----------------------------------------------------------------------

# Q run other community detection algorithms and compare the modularity
# on the mentions networks

# cluster_fast_greedy()
# cluster_spinglass()
# cluster_leading_eigen()

cl_le <- cluster_leading_eigen(g_mention %>% as.undirected())
cl_fg <- cluster_fast_greedy(g_mention %>% as.undirected())
# cl_sg <- cluster_spinglass(g_mention)
cl_wt <- cluster_walktrap(g_mention %>% as.undirected())
cl_lv <- cluster_louvain(g_mention %>% as.undirected())



res <- tibble(
  name = c('leading eignvalue', 'fast greedy', 'walkter', 'louvain'),
  modularity = c(modularity(cl_le), modularity(cl_fg), 
                 modularity(cl_wt), modularity(cl_lv))
  
)

res

# Ex ----------------------------------------------------------------------

# Q: load in the UKfaculty data

# Q: run community detection, do these communties seem meaning full? 
# (does this network have any attribute that might be of use in checking this?)

uk_com <- cluster_louvain(UKfaculty %>% as.undirected())


compair_df <- tibble(louvain = uk_com$membership, defined_groups = V(UKfaculty)$Group) 

compair_df %>% count(louvain, defined_groups) %>% arrange(desc(n))