###############################################################################################
## Project: Case studies in networks
## Script purpose: introduction to igraph
## Date: 13-08-2021
## Author: David JP O'Sullivan
###############################################################################################

# load libraries that we are going to use ---------------------------------
rm(list = ls()) # ls() lists all the objects in name space
# rm removes object. (Just to make sure the workspace is clean)


# # if you haven't install the libraries that we are going to use
# install.packages("dplyr")
# install.packages("igraph")
# install.packages('igraphdata')

library(igraph) # graph analysis tool
library(igraphdata) # igraph data sets
library(tidyverse) # family of packages that makes analysis and plotting 
# data easier

# creating graphs in r ----------------------------------------------------

# here we are going to recreate some simple plots from the slides

# create an adj matrix
adj_mat <- matrix(data = c(0,1,1,0,
                           1,0,1,1,
                           1,1,0,0,
                           0,1,0,0), 
                  # no of rows and cols
                  nrow = 4, ncol = 4, byrow = TRUE)
# by defauls matrix fills by cols, i perfer by row

# create an unidired matrix from the adj_mat
g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected')


plot(g) # plots the graph

# create a layout for the points
?layout # to see other layout options for networks
lay <- layout.auto(g)

# add some arugment to make the graph nice looking
plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

?is.simple
is.simple(g)


# some function to examine graphs
vcount(g)
ecount(g)

g %>% vcount
g %>% ecount

vertex_attr(g)
edge_attr(g)
summary(g)

# to set the color of nodes
V(g)$color = 'red'
vertex_attr(g)

# plot.graph looks for argument 'color' attribute so no need to specify it
plot(g, layout = lay, vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')


# b self loop 

adj_mat <- matrix(data = c(1,1,1,0,
                           1,0,1,0,
                           1,1,0,0,
                           0,1,0,1), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected')

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')


is.simple(g) # is the graph simply
is.loop(g) # which links are loops?
# change their color
E(g)[is.loop(g)]$color = 'red'

# plot them
plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

# ugh something went wrong

E(g)$color # check the attr
# ok we need a default value

E(g)$color = 'grey'
E(g)[is.loop(g)]$color = 'red'


plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')
# much better

# c multigraph
adj_mat <- matrix(data = c(0,2,1,0,
                           2,0,1,3,
                           1,1,0,0,
                           0,3,0,0), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected')

# higlight multi edges
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

# use the from and to agrument with a vector sequence to find edges of 
# interest (you also have the nei() for any type of neighbour)
E(g)[from(2)]
E(g)[to(2)]


E(g)$color = 'grey'
E(g)[from(2)]$color = 'red'
E(g)[to(2)]$color = 'blue'

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

# e weighted

adj_mat <- matrix(data = c(0,2,0.5,0,
                           2,0,1,4,
                           0.5,0,0,0,
                           0,4,0,0), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected', weighted = TRUE)

g

# E(g)$weight stores the link weights
plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = E(g)$weight*3, vertex.label.color = 'black')


# f complete graph

adj_mat <- matrix(data = c(0,1,1,1,
                           1,0,1,1,
                           1,1,0,1,
                           1,1,1,0), 
                  nrow = 4, ncol = 4, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected', weighted = TRUE)

plot(g, layout = lay, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')



# connected ---------------------------------------------------------------

set.seed(1) # see the random number so get the same plots
adj_mat <- matrix(data = c(0,1,1,0,0,0,0,
                           1,0,1,0,0,0,0,
                           1,1,0,0,0,0,0,
                           0,0,0,0,0,0,1,
                           0,0,0,0,0,1,1,
                           0,0,0,0,1,0,1,
                           0,0,0,1,1,1,0), 
                  nrow = 7, ncol = 7, byrow = TRUE)

g <- graph.adjacency(adjmatrix = adj_mat, mode = 'undirected')

# different component get different groups
V(g)$color <- c(rep('cyan', 3), rep('red', 4))
lay = layout.auto(g)
plot(g, layout = lay, vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

# you can find the size of compents and which nodes are part of which group 
# using the components function
?components
cl <- components(g) # get and save components object
V(g)$memb <- cl$membership # memembership to  the graph

# add an edge and plot again
g2 <- add.edges(g, edges = c(2,4))
plot(g2, layout = lay, vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

# one large happy component now 
components(g2)

# recovering graphs -------------------------------------------------------

get.adjacency(g) # return the sparse adj matrix
get.adjacency(g, sparse = FALSE)
get.data.frame(g) # edge list as a data frame

# returns a list of nodal and edge attributes
graph_list <- get.data.frame(g, what = 'both')
graph_list$vertices
graph_list$edges

get.data.frame(g, what = 'vertices')
get.data.frame(g, what = 'edges')

# readr package (included in the tidyverse) contain alot of useful function
?write_csv


# degree of a network -----------------------------------------------------

# some data to play with
?igraphdata
data(package = 'igraphdata')
data(karate) # load karate dataset into memory

plot(karate, vertex.label = NA, 
     vertex.color = 'red', vertex.size = 17, edge.color = 'black')


# how to find the degree of a node
?degree

degree(karate)
V(karate)$degree <- degree(karate)
deg_df <- karate %>% get.data.frame(what = 'vertices') %>% as_tibble

deg_dist <- deg_df %>% # take the deg_df
  count(degree) %>% # count the number of node with degree x
  mutate(prob = n/(sum(n))) # find the probablity 

# plot the degree dist
ggplot(data = deg_dist, aes(x = degree, y = prob)) +
  geom_point(size = 6, color = 'steelblue') +
  xlab('Degree') + ylab('Prob of degree')

# add a line from the point to the z-axis
ggplot(data = deg_dist, aes(x = degree, y = prob)) +
  geom_segment(mapping = aes(xend = degree, yend = rep(0,nrow(deg_dist)))) + 
  geom_point(size = 6, color = 'steelblue') +
  xlab('Degree') + ylab('Prob of degree')


# EX ----------------------------------------------------------------------

data(UKfaculty) # load  dataset into memory

# what type of network is this? Directed? Undirected? Does it contain multi-edges? 
UKfaculty # this network is a little to large to plot

# Q: first plot the network
# plot() function might be useful here...

# Q: does it look fully connected? Tightly packed? Later on how can we measure this?

## comment: its looks fully connected and tightly packaged. But the layout also
## could fool us. Later on we will measure the average shortest path length
## to get a feel for the network


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

g_sub <- subgraph.edges(g, E(g)[is.mutual(g)], delete.vertices = T)

plot(g_sub, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')

png('.plots/sec_1_mutual_links.png', bg = 'transparent')
plot(g_sub, vertex.color = 'cyan', vertex.size = 24,
     edge.width = 2, vertex.label.color = 'black')
dev.off()



# EX  ---------------------------------------------------------------------

# using UKfaculty
# Q: from the network calculate the number of edges
# Q: number and fraction of multiple, mutual and self links.

# comment: 


# clustering coefficient ---------------------------------------------------

g <- karate
plot(g) 
transitivity(g) # this is the gobal clustering coefficient

# calculate local trans and save to tibble, arrange
V(g)$trans <- transitivity(graph = g, type = 'local')
g_df <- tibble(names = V(g)$name, trans = V(g)$trans)
g_df <- g_df %>% arrange(desc(trans))

# plots the density
ggplot(g_df, aes(x = trans)) +
  geom_density(fill = 'steelblue')

# ugh ugly...

# change the smoothing
ggplot(g_df, aes(x = trans)) +
  geom_density(adjust = 0.1, fill = 'steelblue')

# try a histogram
ggplot(g_df, aes(x = trans)) +
  geom_histogram(fill = 'steelblue', color = 'black')


# EX ----------------------------------------------------------------------


# transitivity ------------------------------------------------------------
# Q: what is the transitivity on the network, both gobal and local (plot the local 
# transitivity)

# things that will be useful
# ?transitivity()
# maybe get.data.frame
# ggplot
# + xlab('local transitivity') can be added, to a plot to give a label


# one way to get transitivity in a plottable form in ggplot is by: 



# Q: what node have a clustering coefficient of 1? 


# graph itterators --------------------------------------------------------

# the mode argument of the nei() function
g <- graph( c(1,2, 2,3, 2,4, 4,2) )
plot(g)
V(g)[ nei( c(1,3) ) ]
V(g)[ nei( c(1,3), "in") ]
V(g)[ nei( c(1,3), "out") ]

# operators for edge sequences
g <- erdos.renyi.game(100, p=0.3,directed = TRUE)
E(g)[ 0:2 %--% 1:5 ] # all links between nodes 0:2 and 1:5
E(g)[ 0:2 %->% 1:5 ] # all links from 0:2 to 1:5
E(g)[ 0:2 %<-% 1:5 ] # all links to 0:2 from 1:5


# EX ----------------------------------------------------------------------


# Q: plot the in degree distribution

# Q: add the out distriubtion to hte plots (you can just add a layer with the new data)


# a more compact way (and advanced) of doing this is putting everything in one data frame
# to do this we want to create a longer version of degree_df first (by using pivot_longer), then by working on 
# degree's that are in or out (via group_by) computing the distribution


# Q: use the function cor() to find the correlation between in and out degree
# Q: what does this say about out network? 


# Q: what is the average degree of these nodes?




