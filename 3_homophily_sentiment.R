

# packages ----------------------------------------------------------------

library(igraph)
library(igraphdata)
library(tidyverse)

library(cowplot)
theme_set(theme_cowplot())
library(knitr)

library(viridis)

mention_df <- read_csv(file = './data/sim_mention_df.csv')
follower_df <- read_csv(file = './data/sim_follower_df')

mention_df <- mention_df %>% 
  mutate(unix_time = created_at %>% as.POSIXct,
         bined_time15min = cut(unix_time, breaks="15 min", labels=FALSE),
         bined_time = cut(unix_time, breaks="1 day", labels=FALSE))

g_mention <- graph.data.frame(mention_df)
g_follower <- graph.data.frame(follower_df)

# make the networks g_mentionaller -----------------------------------------------


V(g_mention)$total_sen_in <- 
  graph.strength(g_mention, mode = "in", weights = E(g_mention)$sent_diff)

V(g_mention)$total_sen_out <- 
  graph.strength(g_mention, mode = "out", weights = E(g_mention)$sent_diff)

V(g_mention)$degree_in <- degree(g_mention, mode = 'in')
V(g_mention)$degree_out <- degree(g_mention, mode = 'out')


V(g_mention)$avg_sen_in <- V(g_mention)$total_sen_in/V(g_mention)$degree_in
V(g_mention)$avg_sen_out <- V(g_mention)$total_sen_out/V(g_mention)$degree_out

table(is.na(V(g_mention)$avg_sen_in))
table(is.na(V(g_mention)$avg_sen_out))

ind_in <- which(is.na(V(g_mention)$avg_sen_in))
ind_out <- which(is.na(V(g_mention)$avg_sen_out))

V(g_mention)[ind_in]$avg_sen_in <- 0
V(g_mention)[ind_out]$avg_sen_out <- 0

# table(V(g_mention)$avg_sen_in)

emp_corr <- cor(V(g_mention)$avg_sen_in, V(g_mention)$avg_sen_out)


M = 500
shuffle_test_df = tibble(run_id = 1:M, sim_cor = NA)
g_sim <- g_mention
sent_diff_dist <- E(g_mention)$sent_diff
for(i in 1:M){
  E(g_sim)$sent_diff <- sample(sent_diff_dist, size = ecount(g_sim), replace = TRUE)
  
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
  
  
  shuffle_test_df$sim_cor[i] <- cor(V(g_sim)$avg_sen_in, V(g_sim)$avg_sen_out)
  
  if(i %% 100 == 0){
    print(i/nrow(shuffle_test_df))
  }
}

x_range <- range(c(shuffle_test_df$sim_cor,emp_corr))

p1 <- ggplot(shuffle_test_df, aes(x = sim_cor)) + 
  geom_histogram(fill = 'steelblue', color = 'black') + 
  xlim(x_range) + 
  geom_vline(xintercept = emp_corr, linetype = 'dashed', col = 'red', size = 2)
p1


# nodal connectivity properies --------------------------------------------


V(g_mention)$type = 0
V(g_mention)[avg_sen_out < 0]$type = -1
V(g_mention)[avg_sen_out > 0]$type = 1

names_p <- V(g_mention)[type == 1]$name
names_n <- V(g_mention)[type == -1]$name


emp_connectivity <- tibble(
  con_type = c('p2p', 'p2n', 'n2n'), 
  frac_con = c(
    length(E(g_mention)[names_p %--% names_p])/ecount(g_mention),
    length(E(g_mention)[names_p %--% names_n])/ecount(g_mention),
    length(E(g_mention)[names_n %--% names_n])/ecount(g_mention)
    )
) 
g_sim <- as.undirected(g_mention)
M = 500
connectivity_df <- tibble(
  run_id = 1:M,
  p2p = NA,
  p2n = NA,
  n2n = NA, 
)


for(i in 1:M){
  V(g_sim)$type <- sample(x = V(g_mention)$type, size = vcount(g_sim), replace = TRUE)

  names_p <- V(g_sim)[type == 1]$name
  names_n <- V(g_sim)[type == -1]$name
  
  connectivity_df$p2p[i] <- length(E(g_sim)[names_p %--% names_p])/ecount(g_sim)
  connectivity_df$p2n[i] <- length(E(g_sim)[names_p %--% names_n])/ecount(g_sim)
  connectivity_df$n2n[i] <- length(E(g_sim)[names_n %--% names_n])/ecount(g_sim)
  
  if(i %% 100 == 0){
    print(i/nrow(connectivity_df))
  }
}

connectivity_df_long <- connectivity_df %>% 
  pivot_longer(-run_id, names_to = 'con_type', values_to = 'frac_con') %>% 
  arrange(con_type)

ci <- connectivity_df_long %>% group_by(con_type) %>% 
  summarise(frac_conn_025 = quantile(frac_con, 0.025),
            frac_conn_975 = quantile(frac_con, 0.975)
            ) %>% arrange(con_type)

emp_connectivity$inside <- 
  emp_connectivity$frac_con >= ci$frac_conn_025 & 
  emp_connectivity$frac_con <= ci$frac_conn_975

emp_connectivity <- emp_connectivity %>%
  mutate(color = if_else(inside == TRUE, 'green', 'red'),
         shape = if_else(inside == TRUE, '24', '22'))

p2 <- ggplot(data = connectivity_df_long, aes(x = con_type, y = frac_con)) + 
  geom_violin(fill = 'steelblue') + 
  geom_boxplot(width = 0.1) + 
  geom_point(data = emp_connectivity, 
             aes(x = con_type, y = frac_con, size = 2, 
                 color = color, shape = shape))



pg_1 <- plot_grid(p1, p2, labels = c('A','B'))
pg_1

ggsave(filename = './randomisation_plots.png')


  