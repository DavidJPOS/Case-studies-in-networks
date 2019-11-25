# packages ----------------------------------------------------------------

library(igraph)
library(igraphdata)
library(tidyverse)

library(cowplot)
theme_set(theme_cowplot())
library(knitr)

library(viridis)

mention_df <- read_csv(file = './data/sim_mention_df.csv')
follower_df <- read_csv(file = './data/sim_follower_df.csv')

mention_df <- mention_df %>% 
  mutate(unix_time = created_at %>% as.POSIXct,
         bined_time15min = cut(unix_time, breaks="15 min", labels=FALSE),
         bined_time = cut(unix_time, breaks="1 day", labels=FALSE))


g_mention <- graph.data.frame(mention_df)
g_follower <- graph.data.frame(follower_df)

V(g_mention)$total_sen_in <- 
  graph.strength(g_mention, mode = "in", weights = E(g_mention)$sent_diff)

V(g_mention)$total_sen_out <- 
  graph.strength(g_mention, mode = "out", weights = E(g_mention)$sent_diff)

V(g_mention)$degree_in <- degree(g_mention, mode = 'in')
V(g_mention)$degree_out <- degree(g_mention, mode = 'out')


V(g_mention)$avg_sen_in <- V(g_mention)$total_sen_in/V(g_mention)$degree_in
V(g_mention)$avg_sen_out <- V(g_mention)$total_sen_out/V(g_mention)$degree_out


# g_mention <- graph.data.frame(mention_df)
# g_follower <- graph.data.frame(follower_df)

# make the networks g_mentionaller -----------------------------------------------

ud_sf = as.undirected(g_follower)
ud_sm = as.undirected(g_mention)

com_sf = cluster_louvain(ud_sf)
com_sm = cluster_louvain(ud_sm)

# plot(com_sf, ud_sf, vertex.label = NA)
# plot(com_sm, ud_sm, vertex.label = NA)

modularity(com_sf)
modularity(com_sm)

com_sm %>% names

# V(g_mention)$name
# V(g_follower)$name

V(g_mention)$com_sm = com_sm$membership
V(g_mention)$com_sf = com_sf$membership

V(g_mention)$com_ins = paste0(V(g_mention)$com_sm, V(g_mention)$com_sf)

com_unique = unique(V(g_mention)$com_ins)



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

# modularity() 
# cluster_fast_greedy()
# cluster_spinglass()
# cluster_leading_eigen()
# cluster_edge_betweenness()




