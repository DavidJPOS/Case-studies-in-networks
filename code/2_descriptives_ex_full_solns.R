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

# network parameters
M = 50
no_nodes = 1000
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








