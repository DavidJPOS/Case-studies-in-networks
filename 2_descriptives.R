
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

# mention_df <- read_csv(file = './data/cleaned_mention_edges.csv')
# follower_df <- read_csv(file = './data/cleaned_follower_edges.csv')

mention_df <- read_csv(file = './data/sim_mention_df.csv')
follower_df <- read_csv(file = './data/sim_follower_df.csv')

# use graph.data.frame to generate two networks

mention_df <- mention_df %>% 
  mutate(unix_time = created_at %>% as.POSIXct,
         bined_time15min = cut(unix_time, breaks="15 min", labels=FALSE),
         bined_time = cut(unix_time, breaks="1 day", labels=FALSE))


g_mention <- graph.data.frame(mention_df)
g_follower <- graph.data.frame(follower_df)

p1 <- mention_df %>% 
  count(bined_time15min) %>% 
  ggplot(aes(x = bined_time15min, y= n)) +
  geom_line()

degree_vec <- degree(g_mention) 

degree_df <- tibble(name = names(degree_vec), degree = degree_vec)

p2 <- degree_df %>% count(degree) %>% 
  mutate(p = n/sum(n), cum_p = 1 - cumsum(p)) %>% 
  ggplot(aes(x = degree, y = p)) + 
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10()

p3 <- degree_df %>% count(degree) %>% 
  mutate(p = n/sum(n), cum_p = 1- cumsum(p)) %>% 
  ggplot(aes(x = degree, y = cum_p)) + 
  geom_line(size = 2) + 
  scale_x_log10() + 
  scale_y_log10()


pg1 <- cowplot::plot_grid(p2,p3, labels = c('A','B'))

p1
pg1

# compute the table of statistics and descriptive graphs                     

p3 <- mention_df %>% 
  count(pos, neg) %>% 
  mutate(p = n/sum(n)) %>% 
  mutate(pos = factor(pos, levels = 1:5),
         neg = factor(neg, levels = -1:-5)) %>% 
  ggplot(aes(x=pos, y=neg, fill=log(n))) + 
  geom_tile() 

p4 <- ggplot(mention_df, aes(x = sent_diff)) + 
  geom_bar(fill = 'steelblue')

pg2 <- cowplot::plot_grid(p3,p4, labels = c('A','B'))

# saving plots
ggsave(filename = 'fig1.png', plot = pg1)
ggsave(filename = 'fig2.png', plot = pg2)



# generating tables -------------------------------------------------------


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
knitr::kable(stats,format = 'latex') # 


# https://www.tablesgenerator.com/latex_tables


# local properiest of the networks ----------------------------------------

V(g_mention)$trans <- transitivity(g_mention, type = 'local')
V(g_follower)$trans <- transitivity(g_follower, type = 'local')

V(g_mention)$average_path <- shortest.paths(g_mention,v = V(g_mention)$name) %>%
  apply(.,1,mean)

V(g_follower)$average_path <- shortest.paths(g_follower,v = V(g_follower)$name) %>%
  apply(.,1,mean)


plotting_df <- tibble(
  name = V(g_mention)$name, 
  trans_m = V(g_mention)$trans, 
  avg_sp_m = V(g_mention)$average_path)

temp_df = tibble(
  name = V(g_follower)$name, 
  trans_f = V(g_follower)$trans, 
  avg_sp_f = V(g_follower)$average_path)

plotting_df <- left_join(plotting_df, temp_df, by = 'name')

p5 <- ggplot(plotting_df, aes(x = trans_m)) + 
  geom_histogram(fill = 'steelblue', color = 'black')

p6 <- ggplot(plotting_df, aes(x = avg_sp_m)) + 
  geom_histogram(fill = 'steelblue', color = 'black')

p7 <- ggplot(plotting_df, aes(x = trans_f)) + 
  geom_histogram(fill = 'steelblue', color = 'black')

p8 <- ggplot(plotting_df, aes(x = avg_sp_f)) + 
  geom_histogram(fill = 'steelblue', color = 'black')

pg3 <- plot_grid(p5,p6,p7,p8, labels = c('A','B','C','D'))

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
  scale_x_log10() + 
  scale_y_log10() +
  theme(legend.position = "none") + 
  xlab('Degree')


p10 <- d_all %>% 
  # filter(cluster_coef > 0.01) %>% 
  ggplot(aes(x = cluster_coef, color = type, fill = type)) + 
  geom_density() + 
  scale_x_log10() +
  # scale_y_log10() +
  scale_color_viridis(discrete = TRUE) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.7) + 
  xlab('Clustering Coefficient')

pg3 <- plot_grid(p9, p10, labels = c('A','B'))


ggsave(filename = './network_generation_comp.png', plot = p_combi)










