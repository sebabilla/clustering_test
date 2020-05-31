#hclust algorithm
#フリーソフトによるデータ解析・マイニング　第28回
#https://www.stat.berkeley.edu/~s133/Cluster2a.html
dist <- round(dist(data))
method = c("complete","single","average","centroid","median","ward","mcquitty")
hclusttable_groups3 <- tibble(observation = 1:27)
par(mfrow=c(2,2))
for (i in 1:7) {
  cluster <- hclust(dist, method = method[i]) 
  plot(cluster)
  groups3 <- cutree(cluster,3) 
  hclusttable_groups3 <- tibble(hclusttable_groups3, groups3 = groups3)
  names(hclusttable_groups3)[i+1] <- paste("hc",method[i])
}
par(mfrow=c(1,1))

#kmeans algorithm
#evaluate the number of clusters
method = c("wss", "silhouette", "gap_stat")
a <- fviz_nbclust(data, kmeans, method = method[1],k.max = 20)+
  labs(subtitle = method[1])
b <- fviz_nbclust(data, kmeans, method = method[2],k.max = 20)+
  labs(subtitle = method[2])
c <- fviz_nbclust(data, kmeans, method = method[3],k.max = 20)+
  labs(subtitle = method[3])
gridExtra::grid.arrange(a,b,c, ncol=2)
#1 or 2 clusters are recommanded, but lets do 3, kmeans use randomnes so didn't always give same answer
#examples
a <- fviz_cluster(kmeans(data, 3), data = data, frame.type = "convex")
b <- fviz_cluster(kmeans(data, 3), data = data, frame.type = "convex")
c <- fviz_cluster(kmeans(data, 3), data = data, frame.type = "convex")
d <- fviz_cluster(kmeans(data, 3), data = data, frame.type = "convex")
gridExtra::grid.arrange(a,b,c,d)
#lets do it several times
kmeans3 <- replicate(20, {
km <- kmeans(data, 3)
tibble(clu = km$cluster) %>% 
  group_by(clu) %>% mutate(n = n()) %>% ungroup() %>%
  mutate(new_group = ifelse(n == max(n), 1, ifelse(n == min(n), 3, 2))) %>%
  pull(new_group) %>% unlist
})
#a selection based on max frequency may be better, but it's longer to code
meankmeans <- round(rowMeans(kmeans3))

clusters <- tibble(hclusttable_groups3, kmeans = meankmeans)
knitr::kable(clusters)

