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
plot(wc, g_sbm, vertex.label = NA, vertex.size = 5,
     layout = lay)

# cluster_fast_greedy()
# cluster_spinglass()
# cluster_leading_eigen()
# cluster_edge_betweenness()

# create the required network objects -------------------------------------

mention_df <- read_csv(file = './data/sim_mention_df.csv')
follower_df <- read_csv(file = './data/sim_follower_df.csv')

mention_df <- mention_df %>% 
  mutate(unix_time = created_at %>% as.POSIXct,
         bined_time15min = cut(unix_time, breaks="15 min", labels=FALSE),
         bined_time = cut(unix_time, breaks="1 day", labels=FALSE))


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






