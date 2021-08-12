###############################################################################################
## Project: Case studies in networks
## Script purpose: descriptive of networks
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


# create tables and plots -------------------------------------------------


# set you working dir
setwd('./')
# load the data the we are going to be playing with
mention_df <- read_csv(file = './data/sim_mention_df.csv')

# create the network from the data frames
g_mention <- graph.data.frame(mention_df)


g_mention

# plots the degree dist
degree_vec <- degree(g_mention) 
degree_df <- tibble(name = names(degree_vec), degree = degree_vec)
# try to plot the in and out degree dist

p2 <- degree_df %>% count(degree) %>% 
  mutate(p = n/sum(n), cum_p = 1 - cumsum(p)) %>% 
  ggplot(aes(x = degree, y = p)) + 
  geom_point()
p2

p3 <- degree_df %>% count(degree) %>% 
  mutate(p = n/sum(n), cum_p = 1- cumsum(p)) %>% 
  ggplot(aes(x = degree, y = cum_p)) + 
  geom_line(size = 2)
p3

# use the cowplot package to combine plots
pg1 <- cowplot::plot_grid(p2,p3, labels = c('A','B'))

p1
pg1

# compute the table of statistics and descriptive graphs                     

p3 <- ggplot(mention_df, aes(x = sent_diff)) + 
  geom_bar(fill = 'steelblue')


# saving plots
ggsave(filename = './plots/fig1.png', plot = pg1)


# EX ----------------------------------------------------------------------

# Q: read in the file sim_follower_df.csv, create an igraph network object
# and plot the degree distribution

follower_df <- read_csv(file = './data/sim_follower_df.csv')
g_follower <- graph.data.frame(follower_df)


# generating tables -------------------------------------------------------

# the statistics that are normally calculate as part of any explortary data 
# analysis


vcount(g_mention)
ecount(g_mention)
sum(is.mutual(g_mention))
mean(degree(g_mention, mode = 'out'))
transitivity(g_mention)


# create a table to store all the stats in
stats <- tibble(
  names = c('nodes', 'links', 'reciprocal links', 'avg. out-degree', 'transitivity'),
  mention = c(vcount(g_mention) %>% round(2),
              ecount(g_mention) %>% round(2),
              sum(is.mutual(g_mention)) %>% round(2),
              mean(degree(g_mention, mode = 'out')) %>% round(2),
              transitivity(g_mention) %>% round(2)
  )
)


knitr::kable(stats) # html output
knitr::kable(stats,format = 'latex') # generate an latex table

# you can use the following link to correct formatting issues
# https://www.tablesgenerator.com/latex_tables


# EX ----------------------------------------------------------------------

# Q read in the follower graph (sim_fillower_df.csv) and add the summary statistics
# to the above table and print in html format

# useful functions
# vcount()
# ecount()
# sum(is.mutual())
# mean(degree(, mode = 'out'))
# transitivity()


stats <- tibble(
  names = c('nodes', 'links', 'reciprocal links', 'avg. out-degree', 'transitivity'),
  mention = c(vcount(g_mention) %>% round(2),
              ecount(g_mention) %>% round(2),
              sum(is.mutual(g_mention)) %>% round(2),
              mean(degree(g_mention, mode = 'out')) %>% round(2),
              transitivity(g_mention) %>% round(2)
  ),
  #
  #
  #### something needs to go here....
  #
  #
)

# local properties of the networks ----------------------------------------

# calculate and save the local trans
V(g_mention)$trans <- transitivity(g_mention, type = 'local')

# shortest path calculation
sp_mat <- shortest.paths(g_mention,v = V(g_mention)$name)
dim(sp_mat) # return a matrix
sp_mat[1:4,1:4] # want the average over all the rows


?apply # the apply function will be useful here
apply(sp_mat[1:4,1:4], 1, mean)


V(g_mention)$average_path <- shortest.paths(g_mention,v = V(g_mention)$name) %>%
  apply(.,1,mean)

# note sometime when combine graph the user names will be not aligned
plotting_df <- tibble(
  name = V(g_mention)$name, 
  trans_m = V(g_mention)$trans, 
  avg_sp_m = V(g_mention)$average_path)

p5 <- ggplot(plotting_df, aes(x = trans_m)) + 
  geom_histogram(fill = 'steelblue', color = 'black')
p5

p6 <- ggplot(plotting_df, aes(x = avg_sp_m)) + 
  geom_histogram(fill = 'steelblue', color = 'black')
p6


pg3 <- plot_grid(p5,p6, labels = c('A','B'))
pg3
ggsave(filename = './local_dists.png', plot = pg3)


# EX  ---------------------------------------------------------------------

# Q add the same information that we did for the g_mention network
# to the g_follower network (transitivity, average path length, )
V(g_follower)$trans <- transitivity(g_follower, type = 'local')

V(g_follower)$average_path <- shortest.paths(g_follower,v = V(g_follower)$name) %>%
  apply(.,1,mean)


# Q create a data frame with this information

follow_summ_df = tibble(
  name = V(g_follower)$name, 
  trans_f = V(g_follower)$trans, 
  avg_sp_f = V(g_follower)$average_path)

# Q use the left_join function to combine both data frames
# ?left_join

plotting_df <- left_join(plotting_df, follow_summ_df, by = 'name')

# Q create plots for the follower graph of the transitivity and shortest path
# length. 

p7 <- ggplot(plotting_df, aes(x = trans_f)) + 
  geom_histogram(fill = 'steelblue', color = 'black')

p8 <- ggplot(plotting_df, aes(x = avg_sp_f)) + 
  geom_histogram(fill = 'steelblue', color = 'black')

# Q create a combined plot with all four plots.

pg3 <- plot_grid(p5,p6,p7,p8, labels = c('A','B','C','D'))
pg3

# network generation ------------------------------------------------------

g_er <- erdos.renyi.game(n = vcount(g_mention), p.or.m = ecount(g_mention), type = 'gnm')
g_conf <- sample_degseq(degree(g_mention))

d_m <- tibble(node_id = 1:vcount(g_mention), degree = degree(g_mention), 
              cluster_coef = transitivity(g_mention, type = 'local'), 
              type = "Emperical")
d_er <- tibble(node_id = 1:vcount(g_mention), degree = degree(g_er), 
               cluster_coef = transitivity(g_er, type = 'local'), type = "ER")
d_conf <- tibble(node_id = 1:vcount(g_mention), degree = degree(g_conf), 
                 cluster_coef = transitivity(g_conf, type = 'local'), type = "Conf")


d_all <- bind_rows(d_m, d_er, d_conf)

d_dist_all <- d_all %>% group_by(type) %>% 
  count(degree) %>% 
  mutate(p = n/sum(n)) %>% 
  ungroup() 

p9 <- ggplot(d_dist_all, aes(x = degree, y = p, color = type, shape = type)) + 
  geom_point(size = 3) + 
  xlab('Degree') 
  # scale_color_viridis_d() 

ggplot(d_dist_all, aes(x = degree, y = p, color = type, shape = type)) + 
  geom_jitter(size = 3) + 
  geom_line() + 
  theme(legend.position = "none") +
  xlab('Degree') 

p10 <- d_all %>% 
  ggplot(aes(x = cluster_coef, color = type, fill = type)) + 
  geom_density(alpha = 0.7, color = 'black') + 
  # scale_x_log10() +
  xlab('Clustering Coefficient') + 
  scale_fill_viridis_d()
  

pg3 <- plot_grid(p9, p10, labels = c('A','B'))
pg3

ggsave(filename = './network_generation_comp.png', plot = pg3)


# small worlds model ------------------------------------------------------

?sample_smallworld
g <- sample_smallworld(1, 25, 3, 0)
plot(g)
mean_distance(g)

g <- sample_smallworld(1, 25, 3, 0.025)
plot(g)
mean_distance(g)

g <- sample_smallworld(1, 25, 3, 0.001)
plot(g)
mean_distance(g)



# network parameters
M = 50
no_nodes = 500
# where are we going to search over?
p = seq(from = 0.000, to = 0.01, by = 0.0005)

# tibble to store data
smallworld_df <- tibble(
  p = p %>% rep(each = M), # how many time are we going to try each parameters? 
  mean_distance = NA # same the average distance
)

for(i in 1:nrow(smallworld_df)){
  # generate the graph and store the results
  g_temp = sample_smallworld(1, no_nodes, 4, smallworld_df$p[i])
  smallworld_df$mean_distance[i] <- mean_distance(g_temp)
  
  if(i %% 100 == 0){ # print progress every 100 steps
    print(i/nrow(smallworld_df))
  }
}

# calculate summary statistics
comp_summary <- 
  smallworld_df %>% group_by(p) %>% 
  summarise(
    mean_d = mean(mean_distance),
    sd_distance = sd(mean_distance), 
    sd_distance_p = mean_d + sd_distance,
    sd_distance_m = mean_d - sd_distance,
    size_025 = quantile(mean_distance, 0.025),
    size_975 = quantile(mean_distance, 0.975))

# graph the results
ggplot(comp_summary, aes(x = p, y = mean_d)) + 
  geom_ribbon(aes(ymin = size_025, ymax = size_975), fill = "grey", alpha=0.5) + 
  geom_point() +
  geom_line() 




# EX methods --------------------------------------------------------------

# Q: use the following code, as a basis, to test the transitivity of the network as
# the p changes
# Q: comment on the different relationship between the shortest path length and
# transitivity with the parameter p.
#
# we will come back to this relationship later. 
#

# network parameters
M = 50
no_nodes = 500
# where are we going to search over?
p_seq = seq(from = 0.000, to = 0.01, by = 0.001)

# tibble to store data
smallworld_df <- tibble(
  p = p_seq %>% rep(each = M), # how many time are we going to try each parameters? 
  mean_distance = NA, # same the average distance,
  trans = NA 
)

for(i in 1:nrow(smallworld_df)){
  # generate the graph and store the results
  # samllworld_df$ something <- something else
  
  if(i %% 100 == 0){ # print progress every 100 steps
    print(i/nrow(smallworld_df))
  }
}

# calculate summary statistics


# graph the results







