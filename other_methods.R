#kmeans
method = c("wss", "silhouette", "gap_stat")
fviz_nbclust(data, kmeans, method = method[1],k.max = 20)+
    labs(subtitle = method[1])
fviz_nbclust(data, kmeans, method = method[2],k.max = 20)+
  labs(subtitle = method[2])
fviz_nbclust(data, kmeans, method = method[3],k.max = 20)+
  labs(subtitle = method[3])
#1 or 2 clusters are recommanded, but lets do 3
kmeans3 <- kmeans(data, 3)
fviz_cluster(kmeans3, data = data, frame.type = "convex")
table_groups3 <- tibble(table_groups3, kmeans = kmeans3$cluster)
