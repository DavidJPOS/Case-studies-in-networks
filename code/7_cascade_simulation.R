
# load in packages --------------------------------------------------------

library(igraph)
library(igraphdata)
library(tidyverse)

library(cowplot)
theme_set(theme_cowplot())
library(knitr)

# read in the data from the folder 
mention_df <- read_csv(file = './data/sim_mention_class_df.csv')
g_mention <- graph.data.frame(mention_df)

# simulation function -----------------------------------------------------

ICM_sim = function(follower.net = follower.net, p, total = 1){
  all_cascades.df <- tibble(parent = character(), child = character(), generation = numeric(), ID = numeric(), exposures = numeric())
  for (j in 1:total) {
    active <- numeric()
    inactive <- numeric()
    removed <- numeric()
    
    V(follower.net)$name <- 1:vcount(follower.net) %>% as.character()
    follower.adj <- follower.net %>% get.adjacency()
    
    vertex_names <- vertex_attr(follower.net)$name
    seed <- sample(vertex_names,1)
    active <- seed
    inactive <- vertex_names[!vertex_names %in% seed]
    
    exposures <- numeric(gorder(follower.net))
    names(exposures) <- vertex_names
    cascade.df <- tibble(parent = character(), child = character(), generation = numeric())
    generation <- 1
    while (length(active)>0) {
      new_active <- character()
      # shuffle active
      if(length(active)>1){
        active <- sample(active)
      }
      for (i in active) {
        followers <- vertex_names[which(follower.adj[,i]==1)]
        potential_adopters <- followers[followers %in% inactive]
        exposures[potential_adopters] <- exposures[potential_adopters] + 1
        if(length(potential_adopters)>0){
          adopters <- potential_adopters[runif(length(potential_adopters)) < p]
          if(length(adopters)>0){
            new_active <- c(new_active, adopters)
            inactive <- inactive[! inactive %in% new_active]
            cascade.df <- cascade.df %>% add_row(parent = rep(i, length(adopters)), child = adopters, generation = rep(generation, length(adopters)))
          }
        }
      }
      generation <- generation + 1
      removed <- c(removed, active)
      active <- new_active
    }
    if(nrow(cascade.df)>0){
      all_cascades.df <- all_cascades.df %>% add_row(cascade.df %>% mutate(ID = rep(j, nrow(cascade.df)), exposures = exposures[cascade.df$child]))
    }
  }
  return(all_cascades.df)
}


# spreading on clustered networks -----------------------------------------

set.seed(1) 
network_size <- 25 # how many nodes? 
no_nei <- 4 # how many nei 

g_sw <- sample_smallworld(1, network_size, no_nei, 0) # create a small world networks
plot(g_sw) # plot it
transitivity(g_sw) # its got very high transitivity

# we can simulate cascades from this network
sim_res <- ICM_sim(g_sw, p_inf)
sim_res # they look like this
g_cas <- sim_res %>% graph.data.frame() # as, as always, we can turn them back into igraph networks for plotting

plot(g_cas) # by default the layout isnt very appealing
# what is the layout if it was a tree?
lay <- layout_as_tree(graph = g_cas, root = V(g_cas)[sim_res$parent[1]], mode = 'out')

par(mar = c(0,0,0,0))
plot(g_cas, layout = lay, vertex.color = 'red', vertex.label = NA, edge.color = 'black')
# much nicers

# simulate cascades -------------------------------------------------------

# using some 'emperical' data, want to see how well we do
emp_cascades <- read_csv(file = './data/emp_cascades.csv')

emp_cascade_size <- emp_cascades %>% 
  count(cascade_id) %>% 
  summarise(avg_cas_size = mean(n)) %>% 
  pull(avg_cas_size)
  

# sweep through multiple cascades and find the average cascades size, them pick the value
# that best matches our data.

M = 500
sim_results <- tibble(p = seq(from = 0.025, to = 0.03, by = 0.001) %>% rep(each = M),
                nrow = NA)
set.seed(191916)
for(i in 1:nrow(sim_results)){
  sim_real <- ICM_sim(g_mention, p = sim_results$p[i], total = 1)
  sim_results$nrow[i] <- nrow(sim_real)
  
  if(i %% 100 == 0){
    print(i/nrow(sim_results))
  }
}


sim_summary <- 
  sim_results %>% filter(nrow > 0) %>% 
  group_by(p) %>% 
  summarise(no_links = mean(nrow)) 
sim_summary %>% print(n = Inf)

sim_summary %>% 
  ggplot(aes(x = p, y = no_links)) +
  geom_point() + geom_line() + 
  geom_hline(yintercept = emp_cascade_size, linetype = 'dashed', col = 'red')

# what do the two distributions look like? 
sim_cas <- ICM_sim(g_mention, p = 0.028, total = 1000)

emp_dist = emp_cascades %>% select(parent = to, child = from, generation = time,ID = cascade_id) %>% 
  group_by(ID) %>% 
  summarise(cascade_size = n()) %>%
  count(cascade_size) %>% 
  mutate(p = n/sum(n), ccdf = 1 - cumsum(p), type = 'data')

sim_dist <- sim_cas %>%
  group_by(ID) %>% 
  summarise(cascade_size = n()) %>%
  count(cascade_size) %>% 
  mutate(p = n/sum(n), ccdf = 1 - cumsum(p), type = 'sim')

all_data <- bind_rows(sim_dist, emp_dist)

ggplot(all_data, aes(x = cascade_size, y = p, color = type)) +
  geom_point() + 
  scale_x_log10() +
  scale_y_log10()

ggplot(all_data, aes(x = cascade_size, y = ccdf, color = type)) +
  geom_point() + 
  geom_line() +
  scale_x_log10() +
  scale_y_log10()

# to do select the largest cascade from both and plot them... nicely

# strength of weak ties hypothesis ----------------------------------------

# now that we can simulate cascades easily enough. we can select 

M = 500
sim_results <- tibble(
  p_rewire = seq(from = 0.0, to = 0.05, by = 0.01) %>% rep(each = M),
  cascade_size = NA, 
  trans = NA,
  average_sp = NA
)

network_size <- 500
no_nei <- 4
p_inf <- 0.1

for(i in 1:nrow(sim_results)){ # for i running over each row do the following: 
  p_rewire <- sim_results$p_rewire[i] # grab the rewiring probability
  
  g_sw <- sample_smallworld(1, network_size, no_nei, p_rewire)
  
  # simulate from the ICM
  sim_real <- ICM_sim(g_sw, p_inf, total = 1)
  
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

# pro tip, if the data is in the right format it is very easly to produce 
# summary statistics quickly! 
sw_summ %>% pivot_longer(2:4) %>% 
  ggplot(aes(x = p_rewire, y = value, color = name)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~name, scales = 'free')
