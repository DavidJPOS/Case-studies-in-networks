

# load libraries that we are going to use ---------------------------------

# # if you haven't install the libraries that we are going to use
# install.packages("dplyr")
# install.packages("igraph")
# install.packages('igraphdata')

library(igraph)
library(igraphdata)
library(tidyverse)

# creating graphs in r ----------------------------------------------------

adj_mat <- matrix(data = c(0,1,1,0,
                           1,0,1,0,
                           1,1,0,0,
                           0,1,0,0), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected')


plot(g)

lay <- layout.auto(g)


plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

?is.simple
is.simple(g)


g %>% vcount
g %>% ecount

vertex_attr(g)
edge_attr(g)
summary(g)

V(g)$color = 'red'
vertex_attr(g)

plot(g, layout = lay, vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')


# b

adj_mat <- matrix(data = c(1,1,1,0,
                           1,0,1,0,
                           1,1,0,0,
                           0,1,0,1), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected')

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')


is.simple(g)
is.loop(g)
E(g)[is.loop(g)]$color = 'red'


plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

E(g)$color

E(g)$color = 'grey'
E(g)[is.loop(g)]$color = 'red'


plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

# c

adj_mat <- matrix(data = c(0,2,1,0,
                           2,0,1,3,
                           1,1,0,0,
                           0,3,0,0), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected')

E(g)$color = 'grey'
E(g)[is.multiple(g)]$color = 'red'

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

# d

adj_mat <- matrix(data = c(0,1,0,0,
                           0,0,1,1,
                           1,0,0,0,
                           0,0,0,0), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'directed')

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

E(g)[from(2)]
E(g)[to(2)]


E(g)$color = 'grey'
E(g)[from(2)]$color = 'red'
E(g)[to(2)]$color = 'blue'

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

# weighted

adj_mat <- matrix(data = c(0,2,0.5,0,
                           2,0,1,4,
                           0.5,0,0,0,
                           0,4,0,0), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected', weighted = TRUE)

g

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = E(g)$weight*3, vertex.label.color = 'black')


# complete graph

adj_mat <- matrix(data = c(0,1,1,1,
                           1,0,1,1,
                           1,1,0,1,
                           1,1,1,0), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected', weighted = TRUE)

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')



# connected ---------------------------------------------------------------

set.seed(1)
adj_mat <- matrix(data = c(0,1,1,0,0,0,0,
                           1,0,1,0,0,0,0,
                           1,1,0,0,0,0,0,
                           0,0,0,0,0,0,1,
                           0,0,0,0,0,1,1,
                           0,0,0,0,1,0,1,
                           0,0,0,1,1,1,0), 
                  nrow = 7, ncol = 7, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected')
V(g)$color <- c(rep('cyan', 3), rep('red', 4))
lay = layout.auto(g)
plot(g, layout = lay, vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')


?components
cl <- components(g)
V(g)$memb <- cl$membership


g <- add.edges(g, edges = c(2,4))
plot(g, layout = lay, vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')



# recovering graphs -------------------------------------------------------

get.adjacency(g)

get.adjacency(g, sparse = FALSE)

get.adjedgelist(g)

get.data.frame(g)
graph_list <- get.data.frame(g, what = 'both')
graph_list$vertices
graph_list$edges


# degree of a network -----------------------------------------------------

?igraphdata
data(package = 'igraphdata')
data(karate)

plot(karate, vertex.label = NA, 
     vertex.color = 'red', vertex.size = 17, edge.color = 'black')


?degree
deg_df <- degree(karate) %>% enframe() %>% 
  rename(degree = value)

deg_dist <- deg_df %>% 
  count(degree) %>%
  mutate(prob = n/(sum(n)))

ggplot(data = deg_dist, aes(x = degree, y = prob)) +
  geom_line() +
  geom_point(size = 6, color = 'steelblue') +
  xlab('Degree') + ylab('Prob of degree')



# # mutual links ----------------------------------------------------------

adj_mat <- matrix(data = c(0,1,1,1,
                           1,0,1,0,
                           1,1,0,0,
                           0,1,0,0), 
                  nrow = 4, ncol = 4, byrow = TRUE)


g <- graph.adjacency(adjmatrix = adj_mat, mode = 'directed')
lay <- layout.auto(g)

E(g)$color = 'grey'
is.mutual(g)
E(g)[is.mutual(g)]$color = 'red'

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

g_sub <- subgraph.edges(g, E(g)[is.mutual(g)], delete.vertices = FALSE)
plot(g_sub, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

g_sub <- subgraph.edges(g, E(g)[is.mutual(g)], delete.vertices = TRUE)
plot(g_sub, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')


g_sub <- subgraph.edges(g, E(g)[is.mutual(g)], delete.vertices = TRUE)
plot(g_sub, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')


png('./mutual_links.png', bg = 'transparent')
plot(g_sub, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')
dev.off()


# clustering coefficent ---------------------------------------------------

g <- karate

plot(g)

transitivity(g)

V(g)$trans <- transitivity(graph = g, type = 'local')

g_df <- tibble(names = V(g)$name, trans = V(g)$trans)

g_df <- g_df %>% 
  arrange(desc(trans))

ggplot(g_df, aes(x = trans)) +
  geom_density(fill = 'steelblue')

ggplot(g_df, aes(x = trans)) +
  geom_histogram(fill = 'steelblue')


# graph itterators --------------------------------------------------------



# the mode argument of the nei() function
g <- graph( c(1,2, 2,3, 2,4, 4,2) )
plot(g)
V(g)[ nei( c(1,3) ) ]
V(g)[ nei( c(1,3), "in") ]
V(g)[ nei( c(1,3), "out") ]

# operators for edge sequences
g <- barabasi.game(100, power=0.3)
E(g)[ 0:2 %--% 1:5 ]
E(g)[ 0:2 %->% 1:5 ]
E(g)[ 0:2 %<-% 1:5 ]


# now for a little more praticle example ----------------------------------

g_er = erdos.renyi.game(n = 250, p.or.m = .002)

l = layout.auto(g_er)
# plot(g_er, vertex.label = "", vertex.color = "red", layout = l)
cl = clusters(g_er)
names(cl)

cl$membership
cl$csize
cl$csize %>% table
cl$no

plot(cluster.distribution(g_er), col = "steelblue", type = "b", pch = 20)


M = 100
no_nodes = 500
p = seq(from = 0.00001, to = 0.001, by = 0.00005)
MC_clustersize = matrix(data = NA, nrow = M, ncol = length(p))

MC_comp_size <- tibble(
  p = seq(from = 0.00001, to = 0.001, by = 0.00005) %>% 
    rep(each = M), 
  size = NA
    )

for(i in 1:nrow(MC_comp_size)){
  g_temp = erdos.renyi.game(n = no_nodes, p.or.m = MC_comp_size$p[i])
  MC_comp_size$size[i] = max(components(g_temp)$csize)
}
  
comp_summary <- 
  MC_comp_size %>% group_by(p) %>% 
  summarise(
    mean_size = mean(size),
    sd_size = sd(size), 
    sd_size_p = mean_size + sd_size,
    sd_size_m = mean_size - sd_size,
    size_025 = quantile(size, 0.025),
    size_975 = quantile(size, 0.975))


ggplot(comp_summary, aes(x = p, y = mean_size)) + 
  geom_ribbon(aes(ymin = size_025, ymax = size_975), fill = "red", alpha=0.5) + 
  geom_point() +
  geom_line()
  

ggplot(comp_summary, aes(x = p, y = mean_size)) + 
  geom_point() +
  geom_line() + 
  geom_ribbon(aes(ymin = size_025, ymax = size_975), fill = "red", alpha=0.5)




# rm(list = ls())
# gc()


