###############################################################################################
## Project: case studies in networks
## Script purpose: explore igraphs functions
## Date: 13-08-2021
## Author: David JP O'Sullivan
############################################################################################### 

library(igraph)
library(igraphdata)
library(tidyverse)

library(cowplot)
theme_set(theme_cowplot())
library(knitr)

mention_df <- read_csv(file = './data/sim_mention_df.csv')
follower_df <- read_csv(file = './data/sim_follower_df.csv')

g_mention <- graph.data.frame(mention_df)
g_follower <- graph.data.frame(follower_df)

############## ex
# include the 'undefined' group in you analysis [those where V(g_sim)$type == 0]
#
# try the analysis for the directed case (just between the positive and negative sides)
#
###########


g_sim <- as.undirected(g_mention)
V(g_sim)$type = 0 # default values
V(g_sim)[avg_sen_out < 0]$type = -1 # negative nodes
V(g_sim)[avg_sen_out > 0]$type = 1 # postive nodes

# create vectors taht store the nodes names for pos negative nodes
names_p <- V(g_sim)[type == 1]$name
names_n <- V(g_sim)[type == -1]$name

# create a df to hold the observed values
emp_connectivity <- tibble(
  con_type = c('p2p', 'p2n', 'n2n'), # types connections
  frac_con = c(
    # use graph itterators to find the fraction of links 
    length(E(g_sim)[names_p %--% names_p])/ecount(g_sim),
    length(E(g_sim)[names_p %--% names_n])/ecount(g_sim),
    length(E(g_sim)[names_n %--% names_n])/ecount(g_sim)
  )
) 


M = 500
connectivity_df <- tibble(
  run_id = 1:M,
  p2p = NA,
  p2n = NA,
  n2n = NA, 
)

type_dist <- V(g_sim)$type
for(i in 1:M){
  V(g_sim)$type <- sample(x = type_dist, size = vcount(g_sim), replace = TRUE)
  
  names_p <- V(g_sim)[type == 1]$name
  names_n <- V(g_sim)[type == -1]$name
  
  connectivity_df$p2p[i] <- length(E(g_sim)[names_p %--% names_p])/ecount(g_sim)
  connectivity_df$p2n[i] <- length(E(g_sim)[names_p %--% names_n])/ecount(g_sim)
  connectivity_df$n2n[i] <- length(E(g_sim)[names_n %--% names_n])/ecount(g_sim)
  
  if(i %% 100 == 0){
    print(i/nrow(connectivity_df))
  }
}

connectivity_df

# its easier to plot in ggplot if the data is in a long format
?pivot_longer # this is a handy way to reformat your code

connectivity_df_long <- connectivity_df %>% 
  pivot_longer(-run_id, names_to = 'con_type', values_to = 'frac_con') %>% 
  arrange(con_type)

# from the sim data calculate percentiles
inter <- connectivity_df_long %>% group_by(con_type) %>% 
  summarise(frac_conn_025 = quantile(frac_con, 0.025),
            frac_conn_975 = quantile(frac_con, 0.975)
  ) %>% arrange(con_type)
inter
# does the obs values fall inside the interval?
emp_connectivity$inside <- 
  emp_connectivity$frac_con >= inter$frac_conn_025 & 
  emp_connectivity$frac_con <= inter$frac_conn_975

# add the color for the points
emp_connectivity <- emp_connectivity %>%
  mutate(color_point = if_else(inside == TRUE, 'red', 'green'))

p2 <- ggplot(data = connectivity_df_long, aes(x = con_type, y = frac_con)) + 
  geom_violin(fill = 'steelblue') + 
  geom_boxplot(width = 0.1) + 
  geom_point(data = emp_connectivity, 
             aes(x = con_type, y = frac_con, size = 2, color = color_point)) + 
  scale_color_manual(values = emp_connectivity$color_point)

pg_1 <- plot_grid(p1, p2, labels = c('A','B'))
pg_1

ggsave(filename = './randomisation_plots.png')



# Randomization test for network ------------------------------------------
# Q: repeat the analysis but for the follower network

men_df <- get.data.frame(g_mention, what = 'edges') %>% as_tibble()
user_summ <- 
  men_df %>% group_by(from) %>% 
  summarise(
    avg_sen_out = mean(sent_diff)
  ) 


g_sim <- as.undirected(g_follower)

# this is the only tricky bit in the analysis. 
V(g_sim)[user_summ$from]$avg_sen_out = user_summ$avg_sen_out                 


V(g_sim)$type = 0 # default values
V(g_sim)[avg_sen_out < 0]$type = -1 # negative nodes
V(g_sim)[avg_sen_out > 0]$type = 1 # postive nodes

# create vectors taht store the nodes names for pos negative nodes
names_p <- V(g_sim)[type == 1]$name
names_n <- V(g_sim)[type == -1]$name

# create a df to hold the observed values
emp_connectivity <- tibble(
  con_type = c('p2p', 'p2n', 'n2n'), # types connections
  frac_con = c(
    # use graph itterators to find the fraction of links 
    length(E(g_sim)[names_p %--% names_p])/ecount(g_sim),
    length(E(g_sim)[names_p %--% names_n])/ecount(g_sim),
    length(E(g_sim)[names_n %--% names_n])/ecount(g_sim)
  )
) 


M = 500
connectivity_df <- tibble(
  run_id = 1:M,
  p2p = NA,
  p2n = NA,
  n2n = NA, 
)

type_dist <- V(g_sim)$type
for(i in 1:M){
  V(g_sim)$type <- sample(x = type_dist, size = vcount(g_sim), replace = TRUE)
  
  names_p <- V(g_sim)[type == 1]$name
  names_n <- V(g_sim)[type == -1]$name
  
  connectivity_df$p2p[i] <- length(E(g_sim)[names_p %--% names_p])/ecount(g_sim)
  connectivity_df$p2n[i] <- length(E(g_sim)[names_p %--% names_n])/ecount(g_sim)
  connectivity_df$n2n[i] <- length(E(g_sim)[names_n %--% names_n])/ecount(g_sim)
  
  if(i %% 100 == 0){
    print(i/nrow(connectivity_df))
  }
}

connectivity_df

# its easier to plot in ggplot if the data is in a long format
?pivot_longer # this is a handy way to reformat your code

connectivity_df_long <- connectivity_df %>% 
  pivot_longer(-run_id, names_to = 'con_type', values_to = 'frac_con') %>% 
  arrange(con_type)

# from the sim data calculate percentiles
inter <- connectivity_df_long %>% group_by(con_type) %>% 
  summarise(frac_conn_025 = quantile(frac_con, 0.025),
            frac_conn_975 = quantile(frac_con, 0.975)
  ) %>% arrange(con_type)
inter
# does the obs values fall inside the interval?
emp_connectivity$inside <- 
  emp_connectivity$frac_con >= inter$frac_conn_025 & 
  emp_connectivity$frac_con <= inter$frac_conn_975

# add the color for the points
emp_connectivity <- emp_connectivity %>%
  mutate(color_point = if_else(inside == TRUE, 'red', 'green'))

p2 <- ggplot(data = connectivity_df_long, aes(x = con_type, y = frac_con)) + 
  geom_violin(fill = 'steelblue') + 
  geom_boxplot(width = 0.1) + 
  geom_point(data = emp_connectivity, 
             aes(x = con_type, y = frac_con, size = 2, color = color_point)) + 
  scale_color_manual(values = emp_connectivity$color_point)

pg_1 <- plot_grid(p1, p2, labels = c('A','B'))
pg_1

ggsave(filename = './plots/randomisation_plots.png')



