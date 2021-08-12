###############################################################################################
## Project: case studies in networks
## Script purpose: explore igraphs functions
## Date: 13-08-2021
## Author: David JP O'Sullivan
############################################################################################### 

# packages ----------------------------------------------------------------

rm(list = ls())

library(igraph)
library(igraphdata)
library(tidyverse)

library(cowplot)
theme_set(theme_cowplot())
library(knitr)
library(viridis)


# very briefly a community detection example ------------------------------

pm = cbind( c(.1, .01), c(.01, .1) )
g_sbm = sample_sbm(200, pref.matrix=pm, block.sizes=c(100,100))

lay = layout.auto(g_sbm)
plot(g_sbm, vertex.label = NA, vertex.color = "steelblue", vertex.size = 5,
     layout = lay)

wc = cluster_louvain(g_sbm)
modularity(wc)
communities(wc)

# membership(wc)
plot(wc, g_sbm, vertex.label = NA, vertex.size = 5, layout = lay)

# cluster_fast_greedy()
# cluster_spinglass()
# cluster_leading_eigen()
# cluster_edge_betweenness()

# create the required network objects -------------------------------------

mention_df <- read_csv(file = './data/sim_mention_df.csv')
follower_df <- read_csv(file = './data/sim_follower_df.csv')

g_mention <- graph.data.frame(mention_df)
g_follower <- graph.data.frame(follower_df)

# make the networks g_mentionaller -----------------------------------------------

# this implentation of community detection only work on 
# undirected graph so create a undirected version
ud_sf = as.undirected(g_follower)
ud_sm = as.undirected(g_mention)

# user louvain to find the community in the graph 
?cluster_louvain
?communities

com_sf = cluster_louvain(ud_sf)
com_sm = cluster_louvain(ud_sm)

# plot(com_sf, ud_sf, vertex.label = NA)
# plot(com_sm, ud_sm, vertex.label = NA)

modularity(com_sf)
modularity(com_sm)

com_sm %>% names

# V(g_mention)$name
# V(g_follower)$name
# save the communities for each node to the nodes o
V(g_mention)$com_sm = com_sm$membership
V(g_mention)$com_sf = com_sf$membership

# now this is important for later, we want to intersection of both graphs
# so create a unique identifter for the new 'community clusters'
V(g_mention)$com_ins = paste0(V(g_mention)$com_sm, V(g_mention)$com_sf)

com_unique = unique(V(g_mention)$com_ins)
com_unique # 

cluster_sent <- tibble(CC = com_unique, sent_in = NA, sent_out = NA)

# we will have to use the pull functon with the tibble in the following for loop
# this is because subsetting a tibble ALWAYS returns tibble but we need a vector 
# to do the logical operations properly

for(i in 1:nrow(cluster_sent)){ # for each CC
  # i = 2
  # calculate the average sentiment in and out of the nodes inside
  cluster_sent[i, 2] = mean(V(g_mention)[com_ins == pull(cluster_sent[i, 1])]$avg_sen_in, na.rm = TRUE)
  cluster_sent[i, 3] = mean(V(g_mention)[com_ins == pull(cluster_sent[i, 1])]$avg_sen_out, na.rm = TRUE)
}

# plots the results
ggplot(cluster_sent, aes(x = sent_in, y = sent_out, color = CC)) +
  geom_point()
# .... again this is fake data, if you actually got this i would be very worried....

# now use k means clustering to find the number of CC that we have in the data
# first cycle over each possible number of clusters calculating the
# total 'within-sum-of-squares'. the lower the better
K = 10
wss_df <- tibble(k = 1:K, wss = NA)
for(i in 1:nrow(wss_df)){
  km = kmeans(cluster_sent[, -1], centers = i)
  wss_df$wss[i] = km$tot.withinss
}

# we are looking for the 'elbow' when we plot the data.
# this marks the optimal number of clusters
ggplot(wss_df, aes(x = k, y = wss)) + 
  geom_point() + geom_line()
#..... 2 seems like a good bet.

# find the clusters for K = 2
set.seed(1)
K = 2
km = kmeans(cluster_sent[, -1], centers = K)
km

# create a df mapping the clusters centers
cl_df <- km$centers %>% as_tibble()
cl_df$group <- c('no', 'yes')
# add a col to `cluster_sent` that corresponds if the communty is
# yes or no
cluster_sent$group <- if_else(km$cluster == 2, 'yes', 'no')


# plot cluster boundaries
ggplot(cl_df, aes(x = sent_in, y = sent_out)) + 
  # following two function from `ggfroce` teleslate the space
  geom_voronoi_tile(aes(fill = group, group = -1L, alpha = 0.2)) +
  geom_voronoi_segment() +
  geom_point(shape = 3, size = 5) +
  # add a layer of points for each community
  geom_point(data = cluster_sent, aes(x = sent_in, sent_out)) + 
  xlim(-2,2) + # increase the size of the page
  ylim(-2,2)

# map the node communities to a class (yes or no)
V(g_mention)$class =
  plyr::mapvalues(
    x = V(g_mention)$com_ins,
    from = cluster_sent$CC,
    to = cluster_sent$group
  )

# load the true labels for each node
true_labels <- read_csv(file = './data/sim_mention_labels.csv')
# the name of the g_mention and true_label are no in the same order
# need to create an intermediate df to map the values
name_to <- V(g_mention)$name %>% enframe(name = NULL) %>% 
  rename(name = value)
V(g_mention)$real <- left_join(name_to, true_labels, by = 'name')$real

table(V(g_mention)$real)
table(V(g_mention)$real)

# break down the values by real and the predicted class
conf_mat <- table(V(g_mention)$real, V(g_mention)$class)
conf_mat

acc <- (conf_mat[1,1] + conf_mat[2,2])/sum(conf_mat)
acc



# EX ----------------------------------------------------------------------

# Q run other community detection algorithms and compare the modularity

# cluster_fast_greedy()
# cluster_spinglass()
# cluster_leading_eigen()

# Ex ----------------------------------------------------------------------

# Q: load in the UKfaculty data

# Q: run community detection, do these communties seem meaning full? 
# (does this network have any attribute that might be of use in checking this?)

