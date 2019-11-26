
# packages ----------------------------------------------------------------

rm(list = ls())

library(igraph)
library(igraphdata)
library(tidyverse)

library(cowplot)
theme_set(theme_cowplot())
library(knitr)

library(viridis)

# create tables and plots -------------------------------------------------


# set you working dir
setwd('./')
# load the data the we are going to be playing with
mention_df <- read_csv(file = './data/sim_mention_df.csv')
follower_df <- read_csv(file = './data/sim_follower_df.csv')

# use graph.data.frame to generate two networks

mention_df <- mention_df %>% 
  mutate(
    # create binned time (?cut)
    bined_time15min = cut(created_at, breaks="15 min", labels=FALSE),
    bined_time1day = cut(created_at, breaks="1 day", labels=FALSE)
    )

# create the network from the data frames
g_mention <- graph.data.frame(mention_df)
g_follower <- graph.data.frame(follower_df)

g_mention
g_follower

# how often do people tweet? 
p1 <- mention_df %>% 
  count(bined_time15min) %>% 
  ggplot(aes(x = bined_time15min, y= n)) +
  geom_line()
p1

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
ggsave(filename = 'fig1.png', plot = pg1)



# generating tables -------------------------------------------------------

# the statistics that are normally calculate as part of any explortary data 
# analysis
vcount(g_mention)
ecount(g_mention)
sum(is.mutual(g_mention))
mean(degree(g_mention, mode = 'out'))
transitivity(g_mention)

vcount(g_follower)
ecount(g_follower)
sum(is.mutual(g_follower))
mean(degree(g_follower, mode = 'out'))
transitivity(g_follower)

# create a table to store all the stats in
stats <- tibble(
  names = c('nodes', 'links', 'reciprocal links', 'avg. out-degree', 'transitivity'),
  mention = c(vcount(g_mention) %>% round(2),
              ecount(g_mention) %>% round(2),
              sum(is.mutual(g_mention)) %>% round(2),
              mean(degree(g_mention, mode = 'out')) %>% round(2),
              transitivity(g_mention) %>% round(2)
  ),
  follower = c(vcount(g_follower) %>% round(2),
               ecount(g_follower) %>% round(2),
               sum(is.mutual(g_follower)) %>% round(2),
               mean(degree(g_follower, mode = 'out')) %>% round(2),
               transitivity(g_follower) %>% round(2)
  )
)


knitr::kable(stats) # html output
knitr::kable(stats,format = 'latex') # generate an latex table

# you can use the following link to correct formatting issues
# https://www.tablesgenerator.com/latex_tables


# local properiest of the networks ----------------------------------------

# calculate and save the local trans
V(g_mention)$trans <- transitivity(g_mention, type = 'local')
V(g_follower)$trans <- transitivity(g_follower, type = 'local')


# shortest path calculation
sp_mat <- shortest.paths(g_mention,v = V(g_mention)$name)
dim(sp_mat) # return a matrix
sp_mat[1:4,1:4] # want the average over all the rows


?apply # the apply function will be useful here
apply(sp_mat[1:4,1:4], 1, mean)


V(g_mention)$average_path <- shortest.paths(g_mention,v = V(g_mention)$name) %>%
  apply(.,1,mean)

V(g_follower)$average_path <- shortest.paths(g_follower,v = V(g_follower)$name) %>%
  apply(.,1,mean)

# note sometime when combine two graphs the user names will be not aligned
plotting_df <- tibble(
  name = V(g_mention)$name, 
  trans_m = V(g_mention)$trans, 
  avg_sp_m = V(g_mention)$average_path)

temp_df = tibble(
  name = V(g_follower)$name, 
  trans_f = V(g_follower)$trans, 
  avg_sp_f = V(g_follower)$average_path)

# going both df by the varaible 'name'
plotting_df <- left_join(plotting_df, temp_df, by = 'name')

p5 <- ggplot(plotting_df, aes(x = trans_m)) + 
  geom_histogram(fill = 'steelblue', color = 'black')
p5

p6 <- ggplot(plotting_df, aes(x = avg_sp_m)) + 
  geom_histogram(fill = 'steelblue', color = 'black')
p6

p7 <- ggplot(plotting_df, aes(x = trans_f)) + 
  geom_histogram(fill = 'steelblue', color = 'black')

p8 <- ggplot(plotting_df, aes(x = avg_sp_f)) + 
  geom_histogram(fill = 'steelblue', color = 'black')



pg3 <- plot_grid(p5,p6,p7,p8, labels = c('A','B','C','D'))
pg3
ggsave(filename = './local_dists.png', plot = pg3)


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
  geom_point(size = 2) + 
  scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "none") + 
  xlab('Degree')


p10 <- d_all %>% 
  ggplot(aes(x = cluster_coef, color = type, fill = type)) + 
  geom_density() + 
  scale_x_log10() +
  # scale_y_log10() +
  scale_color_viridis(discrete = TRUE) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) + 
  xlab('Clustering Coefficient')

pg3 <- plot_grid(p9, p10, labels = c('A','B'))
pg3

ggsave(filename = './network_generation_comp.png', plot = pg3)


###########
# How much does the distribution of shortest paths vary between the different clusters?








