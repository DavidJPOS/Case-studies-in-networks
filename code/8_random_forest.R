

# load packages and sort data ---------------------------------------------

# install.packages('deldir') # if you have problem later this might be missing

library(ggforce)

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



# load the true labels for each node
true_labels <- read_csv(file = './data/sim_mention_class_labels.csv')
# the name of the g_mention and true_label are no in the same order
# need to create an intermediate df to map the values
name_to <- V(g_mention)$name %>% enframe(name = NULL) %>%
  rename(name = value)
V(g_mention)$real <- left_join(name_to, true_labels, by = 'name')$real

table(V(g_mention)$real)


# now try a random forest model on the data -------------------------------

mention_df <- get.data.frame(g_mention, what = 'vertices') %>% as_tibble
mention_df$real <- as_factor(mention_df$real)

# create a training and test data set
smp_size <- floor(0.75 * nrow(mention_df))
set.seed(1)
train_ind <- sample(seq_len(nrow(mention_df)), size = smp_size)
train <- mention_df[train_ind, ]
test <- mention_df[-train_ind, ]
# 
# # fit a logist regression model
# log_fit <- glm(formula = real ~ avg_sen_in + avg_sen_out, data = train[,-1], family = binomial)
# summary(log_fit)
# 
# exp(coef(log_fit))
# 
# pred_vec <- predict(log_fit, test, type = 'response')
# pred_vec <- if_else(pred_vec <=0.5, 'yes', 'no') %>% factor(., levels = c('yes', 'no'))
# 
# conf_mat <- table(test$real, pred_vec)
# conf_mat
# acc <- (conf_mat[1,1] + conf_mat[2,2])/sum(conf_mat)
# acc


# fit the training data; and remove the node's name!
rf <- randomForest(real ~ avg_sen_in + avg_sen_out, data = train[,-c(1)])
rf

pred_vec <- predict(rf, test)
conf_mat <- table(test$real, pred_vec)
conf_mat
acc <- (conf_mat[1,1] + conf_mat[2,2])/sum(conf_mat)
acc


# compare auc values for both models --------------------------------------

# calculate the auc for this model
# preb_vec <- predict(rf, test, type = 'prob') # this how you get the probabilties




# improving the model -----------------------------------------------------


# what other feature could you engineer to improve predictions? 

# compute network freatures like. the 
# 1. average neightbour sentiment
# 2. comunity membership
# 3. community sentiment in and out
# 4. etc...


# re run the classificion model and see what freatures are important





