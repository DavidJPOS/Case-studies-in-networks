
rm(list = ls())

library(igraph)
library(igraphdata)
library(tidyverse)

library(cowplot)
theme_set(theme_cowplot())
library(knitr)

library(viridis)

# lower limit of detection for a community --------------------------------
# here are are going to build sbm with community struture but we want to be
# able to see where we are unable to see the community structure


N = 1000 # size of the network
N_1 = 650 # size of block one
N_2 = N - N_1 # size of second block
p_out = 0.001 # prob of connection between blocks
# sequence that we are going to search over
p_in_seq = seq(from =  0.075, to = 0.001, length.out = 20) %>% 
  rep(each = 50)
res <- tibble(p_in = p_in_seq, acc = NA, bal_acc = NA) # where we will
# save the results

for(i in 1:nrow(res)){ # for each row in res
  # generate a sbm graph
  p_mat <- matrix(c(res$p_in[i], p_out, p_out, res$p_in[i]), 2, 2)
  g_sbm <- sample_sbm(n = N, pref.matrix = p_mat, block.sizes = c(N_1, N_2))
  
  # find the communtiy struture
  V(g_sbm)$membership <- cluster_louvain(g_sbm)$membership
  # we know the real communities: first N_1 node are member of comm 1 and so on
  real <- c(rep(1, N_1), rep(2, N_2))
  # calculate the confusion matrix
  conf_mat <- table(V(g_sbm)$membership, real)
  
  # find the metrics
  acc <- sum(diag(conf_mat))/sum(conf_mat)
  bal_acc <- (conf_mat[1,1]/N_1) * 0.5 + (conf_mat[2,2]/N_2) * 0.5
  
  res$acc[i] <-  acc
  res$bal_acc[i] <-  bal_acc
  
  if(i %% 100 == 0){
    print(i/nrow(res))
  }
  
}

# just plots all the points
ggplot(data = res, aes(x = p_in, y = acc)) +
  geom_jitter() # plots the plots with a little 
  # noise if they are lying onto of each other

res_summary <- res %>% group_by(p_in) %>% 
  summarise(
    mean_acc = mean(acc),
    acc_025 = quantile(acc, 0.025),
    acc_975 = quantile(acc, 0.975),
    
    mean_bal_acc = mean(bal_acc),
    bal_acc_025 = quantile(bal_acc, 0.025),
    bal_acc_975 = quantile(bal_acc, 0.975)
  )

ggplot(res_summary, aes(x = p_in, y = mean_acc)) + 
  geom_ribbon(aes(ymin = acc_025, ymax = acc_975), fill = "red", alpha=0.5) + 
  geom_point() +
  geom_line()
  

ggplot(res_summary, aes(x = p_in, y = mean_bal_acc)) + 
  geom_ribbon(aes(ymin = bal_acc_025, ymax = bal_acc_975), fill = "red", alpha=0.5) + 
  geom_point() +
  geom_line()
  

# what could you add to this analysis? 
# how do modularity vary?
# are their other clustering methods that work better?
