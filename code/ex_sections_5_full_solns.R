###############################################################################################
## Project: case studies in networks
## Script purpose: explore igraphs functions
## Date: 13-08-2021
## Author: David JP O'Sullivan
############################################################################################### 

library(tidyverse)
library(igraphdata)
library(igraph)



# want to simulate the ICM on the SW network with various networks
# we are going to keep track of the 
# 1) cascade size
# 2) transitivity
# 3) average shortest path length 
# for a range of rewiring probability on a small world network

# use M = 500 (number of times you try each parameter)
# network of 500 nodes
# each with 4 neigbours
# and a prob of 0.15 that a node will be infected/adopt if exposed

# p_rewire should go from 0.0 to 0.05 in steps of 0.01 

M = 500 # number of time each parameters is tried
sim_results <- # create a data frame to hold the results
  tibble( # with the following cols
    p_rewire = seq(from = 0.0, to = 0.05, by = 0.01) %>% rep(each = M),
    cascade_size = NA, 
    trans = NA,
    average_sp = NA
  )

network_size <- 500
no_nei <- 4
p_inf <- 0.15

for(i in 1:nrow(sim_results)){ # for i running over each row do the following: 
  p_rewire <- sim_results$p_rewire[i] # grab the rewiring probability
  
  g_sw <- sample_smallworld(1, network_size, no_nei, p_rewire)
  
  # simulate from the ICM
  sim_real <- ICM_sim(g_sw, p_inf)
  
  # add the results to the data frame
  sim_results$trans[i] = transitivity(g_sw)
  sim_results$average_sp[i] = mean_distance(g_sw)
  sim_results$cascade_size[i] <- nrow(sim_real) + 1
  
  
  if(i %% 100 == 0){ print(i/nrow(sim_results)) } 
}

sw_summ <- # to summarize the simulations
  sim_results %>% 
  group_by(p_rewire) %>% 
  summarise(
    m_cascade_size = mean(cascade_size),
    m_trans = mean(trans),
    m_average_sp = mean(average_sp)
  )


ggplot(sw_summ, aes(x = p_rewire, y = m_cascade_size)) + 
  geom_line() + 
  geom_point()

ggplot(sw_summ, aes(x = p_rewire, y = m_average_sp)) + 
  geom_line() + 
  geom_point()

ggplot(sw_summ, aes(x = p_rewire, y = m_trans)) + 
  geom_line() + 
  geom_point()

# pro tip, if the data is in the right format it is very easly to produce 
# summary statistics quickly! 
sw_summ %>% pivot_longer(2:4) %>% 
  ggplot(aes(x = p_rewire, y = value, color = name)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~name, scales = 'free')





