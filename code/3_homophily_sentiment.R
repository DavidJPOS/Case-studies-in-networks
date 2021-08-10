###############################################################################################
## Project: case studies in networks
## Script purpose: explore igraphs functions
## Date: 13-08-2021
## Author: David JP O'Sullivan
############################################################################################### 

# packages ----------------------------------------------------------------

rm(ls = list())

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


# calculate the network statistics ----------------------------------------

?graph.strength # allow you to sum up attr of a eges for each noes

# calculate the total sentiment for a node in and out
V(g_mention)$total_sen_in <- 
  graph.strength(g_mention, mode = "in", weights = E(g_mention)$sent_diff)

V(g_mention)$total_sen_out <- 
  graph.strength(g_mention, mode = "out", weights = E(g_mention)$sent_diff)

# calculate and save the degree
V(g_mention)$degree_in <- degree(g_mention, mode = 'in')
V(g_mention)$degree_out <- degree(g_mention, mode = 'out')

# now calculate the average in and out sentiment
V(g_mention)$avg_sen_in <- V(g_mention)$total_sen_in/V(g_mention)$degree_in
V(g_mention)$avg_sen_out <- V(g_mention)$total_sen_out/V(g_mention)$degree_out

# is there any NA value? 
table(is.na(V(g_mention)$avg_sen_in))
table(is.na(V(g_mention)$avg_sen_out))

# if so remove them
# ind_in <- which(is.na(V(g_mention)$avg_sen_in))
# ind_out <- which(is.na(V(g_mention)$avg_sen_out))
# V(g_mention)[ind_in]$avg_sen_in <- 0
# V(g_mention)[ind_out]$avg_sen_out <- 0

# what is the observed correlation between nodes
emp_corr <- cor(V(g_mention)$avg_sen_in, V(g_mention)$avg_sen_out)
emp_corr # ..... that very high....

# create a df and plots the senti in and out
cor_df <- tibble(avg_sent_in = V(g_mention)$avg_sen_in, avg_sent_out = V(g_mention)$avg_sen_out)
cor_df %>% ggplot(aes(x = avg_sent_in,y=avg_sent_out)) + 
  geom_point()
# looks nice and pretty (because the data is made up!)

# want to do a shuffle test, sample the edge setniment and reweight edges and 
# calculate the correlation

M = 500 # how many times will we do this?
shuffle_test_df = tibble(run_id = 1:M, sim_cor = NA) # store the results here
# create the graph and store the edge weights
g_sim <- g_mention 
sent_diff_dist <- E(g_mention)$sent_diff

for(i in 1:M){ # for each sim
  # reweight the graph
  E(g_sim)$sent_diff <- sample(sent_diff_dist, size = ecount(g_sim), replace = TRUE)
  
  # and calculate the correlation as before
  V(g_sim)$total_sen_in <- 
    graph.strength(g_sim, mode = "in", weights = E(g_sim)$sent_diff)
  
  V(g_sim)$total_sen_out <- 
    graph.strength(g_sim, mode = "out", weights = E(g_sim)$sent_diff)
  
  V(g_sim)$degree_in <- degree(g_sim, mode = 'in')
  V(g_sim)$degree_out <- degree(g_sim, mode = 'out')
  
  
  V(g_sim)$avg_sen_in <- V(g_sim)$total_sen_in/V(g_sim)$degree_in
  V(g_sim)$avg_sen_out <- V(g_sim)$total_sen_out/V(g_sim)$degree_out
  
  ind_in <- which(is.na(V(g_sim)$avg_sen_in))
  ind_out <- which(is.na(V(g_sim)$avg_sen_out))
  
  V(g_sim)[ind_in]$avg_sen_in <- 0
  V(g_sim)[ind_out]$avg_sen_out <- 0
  
  # save the correlation
  shuffle_test_df$sim_cor[i] <- cor(V(g_sim)$avg_sen_in, V(g_sim)$avg_sen_out)
  
  if(i %% 100 == 0){
    print(i/nrow(shuffle_test_df))
  }
}

# what are the results of the test?
p1 <- ggplot(shuffle_test_df, aes(x = sim_cor)) + 
  geom_histogram(fill = 'steelblue', color = 'black') + 
  geom_vline(xintercept = emp_corr, linetype = 'dashed', col = 'red', size = 2)
p1


# nodal connectivity properies --------------------------------------------

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








