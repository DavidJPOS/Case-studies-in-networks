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



# plotting ----------------------------------------------------------------
# Q: first plot the network

plot(UKfaculty)

# Q: does it look fully connected? Tightly packed? Later on how can we measure this?

## comment: its looks fully connected and tightly packaged. But the layout also
## could fool us. Later on we will measure the average shortest path length
## to get a feel for the network

# basics of the network ---------------------------------------------------

# Q: from the network calculate the number of edges
# Q: number and fraction of multiple, mutual and self links.

number_edges <- ecount(UKfaculty)

is.multiple(UKfaculty)
is.mutual(UKfaculty)
is.loop(UKfaculty)

sum(is.multiple(UKfaculty))
sum(is.mutual(UKfaculty))
sum(is.loop(UKfaculty))


sum(is.multiple(UKfaculty))/number_edges
sum(is.mutual(UKfaculty))/number_edges
sum(is.loop(UKfaculty))/number_edges

# comment: 
# This is a directed network with a high number of mutual edges. 


# transitivity ------------------------------------------------------------
# Q: what is the transitivity on the network, both gobal and local (plot the local 
# transitivity)
transitivity(UKfaculty)
V(UKfaculty)$trans = transitivity(UKfaculty, type = 'local')

# one way to get transitivity in a plottable form in ggplot is by: 
trans_df <- 
  UKfaculty %>% # taking the networks
  get.data.frame(what = 'vertices') %>% # extracting all vertex attr as a data frame 
  as_tibble # and converting to a tibble for nice printing

trans_df

ggplot(data = trans_df, aes(x = trans)) + 
  geom_histogram(color = 'black', fill = 'steelblue') + 
  xlab('local transitivity')

# Q: what node have a clustering coefficient of 1? 

trans_df %>% filter(trans == 1)# can extract this from the data frame 
V(UKfaculty)[trans == 1]  # or by using the graph

# degree distribution -----------------------------------------------------

# Q: plot the in degree distribution

V(UKfaculty)$degree_in <- degree(UKfaculty, mode ='in')
V(UKfaculty)$degree_out <- degree(UKfaculty, mode ='out')

degree_df <- 
  UKfaculty %>% #using the network
  get.data.frame(what = 'vertices') %>% # extract the vertex attr 
  as_tibble # convert to the table

degree_dist_in <- degree_df %>% count(degree_in) %>% mutate(p = n/sum(n))
ggplot(degree_dist_in, aes(x = degree_in, y = p)) + 
  geom_point(size = 2)

# Q: add the out distriubtion to hte plots (you can just add a layer with the new data)
degree_dist_out <- degree_df %>% count(degree_out) %>% mutate(p = n/sum(n)) 

ggplot(data = degree_dist_in, aes(x = degree_in, y = p)) + 
  geom_point(size = 2) + 
  geom_point(data = degree_dist_out, mapping = aes(x = degree_out, y = p), color = 'red')

# a more compact way (and advanced) of doing this is putting everything in one data frame
# to do this we want to create a longer version of degree_df first (by using pivot_longer), then by working on 
# degree's that are in or out (via group_by) computing the distribution

degree_df <- degree_df %>% rowid_to_column() # this will be a little bit easier to follow if 
# add a row id as a col
degree_df

degree_df_long <- # to create a longer version of the degree_df
  degree_df %>% # take the data frame
  select(rowid, degree_out, degree_in) %>% # take the cols we want to 
  # appear in the final data frame by
  pivot_longer(
    cols = c('degree_out', 'degree_in'), # taking these cols
    names_to = 'type', # putting the col name in the new col type
    values_to = 'degree' # and the value in the data frame in the new degree col
    )

degree_df_long # this has basically created new rows for each row id

# now we can calculate the degree dist in one long df
degree_dist <- degree_df_long %>% group_by(type) %>% 
  count(degree) %>% 
  mutate(p = n/sum(n), cdf = cumsum(p), ccdf = 1-cdf)

# which makes plotting easier by group in ggplot
degree_dist %>% ggplot(aes(x = degree, y = p, color = type)) + 
  geom_point()

degree_dist %>% ggplot(aes(x = degree, y = ccdf, color = type)) + 
  geom_point() + 
  geom_line()

# Q: use the function cor to find the correlation between in and out degree
# Q: what does this say about out network? 
cor(degree_df$degree_out,degree_df$degree_in)
# nodes that have a hight out degree are likely to have high in degree

# Q: what is the average degree of these nodes?

degree_df %>% 
  summarise(
    mean_in = mean(degree_in), 
    mean_out =  mean(degree_out) 
  )


# EX methods --------------------------------------------------------------

# Q: use the following code, as a basis, to test the transitivity of the network as
# the p changes
# Q: comment on the different relationship between the shortest path length and
# transitivity with the parameter p.
#
# we will come back to this relationship later. 
#

# network parameters
M = 500
no_nodes = 500
# where are we going to search over?
p = seq(from = 0.000, to = 0.01, by = 0.001)

# tibble to store data
smallworld_df <- tibble(
  p = p %>% rep(each = M), # how many time are we going to try each parameters? 
  mean_distance = NA, # same the average distance,
  trans = NA 
)

for(i in 1:nrow(smallworld_df)){
  # generate the graph and store the results
  g_temp = sample_smallworld(1, no_nodes, 4, smallworld_df$p[i])
  smallworld_df$mean_distance[i] <- mean_distance(g_temp)
  smallworld_df$trans[i] <- transitivity(g_temp)
  if(i %% 100 == 0){ # print progress every 100 steps
    print(i/nrow(smallworld_df))
  }
}

# calculate summary statistics
comp_summary <- 
  smallworld_df %>% group_by(p) %>% 
  summarise(
    mean_d = mean(mean_distance),
    size_025 = quantile(mean_distance, 0.025),
    size_975 = quantile(mean_distance, 0.975), 
    
    mean_tran = mean(trans),
    size_025_t = quantile(trans, 0.025),
    size_975_t = quantile(trans, 0.975), 
    )

# graph the results
ggplot(comp_summary, aes(x = p, y = mean_d)) + 
  geom_ribbon(aes(ymin = size_025, ymax = size_975), fill = "grey", alpha=0.5) + 
  geom_point() +
  geom_line() 

ggplot(comp_summary, aes(x = p, y = mean_tran)) + 
  geom_ribbon(aes(ymin = size_025_t, ymax = size_975_t), fill = "grey", alpha=0.5) + 
  geom_point() +
  geom_line() 




