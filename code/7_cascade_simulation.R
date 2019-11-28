
# load in packages --------------------------------------------------------

library(igraph)
library(igraphdata)
library(tidyverse)

library(cowplot)
theme_set(theme_cowplot())
library(knitr)

library(viridis)
library(randomForest)


mention_df <- read_csv(file = './data/sim_mention_class_df.csv')

mention_df <- mention_df %>% 
  mutate(unix_time = created_at %>% as.POSIXct,
         bined_time15min = cut(unix_time, breaks="15 min", labels=FALSE),
         bined_time = cut(unix_time, breaks="1 day", labels=FALSE))


g_mention <- graph.data.frame(mention_df)
g_follower <- graph.data.frame(follower_df)

V(g_mention)$total_sen_in <- 
  graph.strength(g_mention, mode = "in", weights = E(g_mention)$sent_diff)

V(g_mention)$total_sen_out <- 
  graph.strength(g_mention, mode = "out", weights = E(g_mention)$sent_diff)

V(g_mention)$degree_in <- degree(g_mention, mode = 'in')
V(g_mention)$degree_out <- degree(g_mention, mode = 'out')


V(g_mention)$avg_sen_in <- V(g_mention)$total_sen_in/V(g_mention)$degree_in
V(g_mention)$avg_sen_out <- V(g_mention)$total_sen_out/V(g_mention)$degree_out



# simulation function -----------------------------------------------------


ICM_sim = function(mat, prob){
  nam = 1:ncol(mat) # index of all the nodes we have
  inactive = 1:ncol(mat)# the set of inactive user
  curr_active = inactive %>% sample(.,1) #select a seed node
  df = data.frame(from = numeric(), to = numeric(), time = numeric())
  time_counter = 0 # how manny iterations have we done?
  no_exp = numeric(length(nam)) # vector to keep tack of the number of exp
  # house keeping
  # beyond the ICM models prob may contain the 
  # vetors of probabilites for each nodes or
  # a matrix for a more complex quanity
  
  adj_prob = mat * prob
  
  # loop through untill no more infected nodes are generated
  repeat{
    # check if the user has been activicated before
    curr_active = inactive[which(inactive %in% curr_active)]
    inactive = inactive[!(inactive %in% curr_active)] # remove the node that has been activicated
    if(length(curr_active) > 1){
      # reorder curr_active so new infections are in an arbitrary manner
      curr_active = sample(x = curr_active, length(curr_active), replace = FALSE)
    }
    active_next_ts = c()
    # find nei of curr_active
    for( i in curr_active ){ # i = curr_active
      exposed_nodes = nam[which(mat[,i] == 1)] # find the neighbours of infeceted nodes
      exposed_nodes = inactive[(inactive %in% exposed_nodes)]# rm exposed_nodes that have been previsouly act
      ind = (adj_prob[exposed_nodes, i] > runif(length(exposed_nodes))) # do they become infected
      
      # inactive = inactive[!(inactive %in% unique(exposed_nodes[ind]))] # remove the node that will be active
      active_next_ts = unique(c(active_next_ts, exposed_nodes[ind])) # if yes create the vector of newly infected users
      # print(active_next_ts)
      if(length(active_next_ts) > 0){
        df = 
          df %>%  # previous time set and add
          add_row(
            from = active_next_ts,
            to = rep(i, length(active_next_ts)),  
            time = rep(time_counter, length(active_next_ts))
          )
      }
      
    }
    # if no new infeceted nodes stop the loop
    if(length(active_next_ts) == 0){break}
    
    curr_active = active_next_ts
    time_counter = time_counter + 1;
  }
  colnames(df) = c("from","to","time")
  return(df)
}






# simulate cascades -------------------------------------------------------


emp_cascades <- read_csv(file = './data/emp_cascades.csv')

emp_cascade_size <- emp_cascades %>% 
  count(cascade_id) %>% 
  summarise(avg_cas_size = mean(n)) %>% 
  pull(avg_cas_size)
  
  

mat <- g_mention %>% get.adjacency()
M = 500
sim_results <- tibble(p = seq(from = 0.015, to = 0.027, by = 0.001) %>% rep(each = M),
                nrow = NA)

for(i in 1:nrow(sim_results)){
  sim_real <- ICM_sim(mat, prob = sim_results$p[i])
  sim_results$nrow[i] <- nrow(sim_real)
  
  if(i %% 100 == 0){
    print(i/nrow(sim_results))
  }
}


sim_summary <- sim_results %>% filter(nrow > 0) %>% 
  group_by(p) %>% 
  summarise(no_links = mean(nrow)) 
sim_summary %>% print(n = Inf)

sim_summary %>% 
  ggplot(aes(x = p, y = no_links)) +
  geom_point() + 
  geom_hline(yintercept = emp_cascade_size, linetype = 'dashed', col = 'red')


