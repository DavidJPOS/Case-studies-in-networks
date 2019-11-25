
# lower limit of detection for a community --------------------------------


N = 1000
N_1 = 650
N_2 = N - N_1
p_in = 0.075
p_out = 0.001


p_in_seq = seq(from = p_in, to = 0.001, length.out = 20) %>% 
  rep(each = 50)

res <- tibble(p_in = p_in_seq, acc = NA, bal_acc = NA)

for(i in 1:nrow(res)){
  p_mat <-  matrix(c(res$p_in[i], p_out, p_out, res$p_in[i]), 2, 2)
  g_sbm <- sample_sbm(n = N, pref.matrix = p_mat, block.sizes = c(N_1, N_2))
  
  
  V(g_sbm)$membership <- cluster_louvain(g_sbm)$membership
  real <- c(rep(1, N_1), rep(2, N_2))
  conf_mat <- table(V(g_sbm)$membership, real)
  
  acc <- sum(diag(conf_mat))/sum(conf_mat)
  bal_acc <- (conf_mat[1,1]/N_1) * 0.5 + (conf_mat[2,2]/N_2) * 0.5
  
  res$acc[i] <-  acc
  res$bal_acc[i] <-  bal_acc
  
  if(i %% 100 == 0){
    print(i/nrow(res))
  }
  
}


ggplot(data = res, aes(x = p_in, y = acc)) +
  geom_point()

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
  

