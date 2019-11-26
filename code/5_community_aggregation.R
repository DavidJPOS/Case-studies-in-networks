
# kmean -------------------------------------------------------------------

# so now we want ot find the average sentimetn of each community
cluster_sent = 
  matrix(NA, ncol = 3, nrow = length(com_unique)) %>%
  as.data.frame()
colnames(cluster_sent) = c("CC", "sent_in","sent_out")
cluster_sent[, 1] = com_unique

for(i in 1:nrow(cluster_sent)){ 
  # i = 1
  cluster_sent[i, 2] = mean(V(g_mention)[com_ins == cluster_sent[i, 1]]$avg_sen_in,na.rm = TRUE)
  cluster_sent[i, 3] = mean(V(g_mention)[com_ins == cluster_sent[i, 1]]$avg_sen_out,na.rm = TRUE)
}


plot(cluster_sent[, -1])

M = 5
wss = numeric(M)
for(i in 1:M){
  km = kmeans(cluster_sent[, -1], centers = i)
  wss[i] = km$tot.withinss
}

plot(wss, pch = 20, col = "black", type = "b", 
     xlab = "No Clusters", ylab = "WSS")

M = 2
km = kmeans(cluster_sent[, -1], centers = M)



library(deldir)
rx = range(cluster_sent[,2]); rx = c(rx[1]-0.05,rx[2]+0.05)
ry = range(cluster_sent[,3]); ry = c(ry[1]-0.05,ry[2]+0.05)

V <- deldir(km$centers[,1],km$centers[,2], rw = c(rx,ry))
P = tile.list(V)
cols = c(
  rgb(0,1,0,0.15), # green - #FF000026
  rgb(1,0,0,0.15) # red - #00FF0026
)


plot(cluster_sent[, 2:3], pch=19, 
     xlab="Average user sentiment in", 
     ylab="Average user sentiment out",
     col = c("green3","blue","red"), cex = 1, 
     xlim = rx, ylim = ry)
plot(P, fillcol= cols[c(2, 1)], add = TRUE, pch = 3, cex = 1, border = 1)


# map the values to matrix ------------------------------------------------
map_values = plyr::mapvalues(
  x = km$cluster, 
  from = c(1, 2), 
  to = c("no", "yes"))

V(g_mention)$class = 
  plyr::mapvalues(
    x = V(g_mention)$com_ins,
    from = cluster_sent[,1],
    to = map_values
  )

V(g_mention)$class %>% table %>% prop.table %>% barplot()


true_labels <- read_csv(file = './data/sim_mention_labels.csv')

name_to <- V(g_mention)$name %>% enframe(name = NULL) %>% 
  rename(name = value)


V(g_mention)$real <- left_join(name_to, true_labels, by = 'name')$real

conf_mat <- table(V(g_mention)$real,V(g_mention)$class)

acc <- (conf_mat[1,1] + conf_mat[2,2])/sum(conf_mat)

acc
