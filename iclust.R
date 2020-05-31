cdata <- as.data.frame(t(data))
ic<-iclust(cdata)
summary(ic)
