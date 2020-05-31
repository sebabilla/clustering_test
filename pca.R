tdata <- as.data.frame(t(data))
par(mfrow=c(3,5))
for (i in 1:27) {
  x <- data[,i] %>% unlist
  title <- paste("A",i, sep = "")
  hist(x, main=title)
}
par(mfrow=c(1,1))
round(cor(data),2)
colSums(cor(data)>=0.9) 
colSums(cor(data)>=0.4)
KMO(cor(data))
fa.parallel(data, fa = 'pc')
pc <- principal(data, nfactors=3, rotate="oblimin")
fa.plot(pc, title = "position of factors along PCx, PCy axes")
fa.diagram(pc, cut = 0.32, digits = 2, simple = FALSE)
print.psych(pc, cut = 0.32, sort = TRUE)
pc$loadings[1:45, 1:3]
fa.sort(pc)
ic<-iclust(data)
summary(ic)
