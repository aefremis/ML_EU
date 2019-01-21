#Recency Frequency and Monetery customer clusters

##### libraries #####
library(cluster);library(ggplot2)
library(gridExtra);library(data.table);library(outliers)

# load binary file
system.time(RFMscores <- fread('CustomerRFM.csv'))

#variables to use
varsToUse <- names(RFMscores)[-1]
pmatrix <- RFMscores[,varsToUse,with=F]

#remove outliers
outliers <- data.table(scores(pmatrix,type = "chisq",prob = 0.99))
outliers_ids <- lapply(outliers,
                       function(x) which(x==TRUE))

outliersToRemove <- unique(as.numeric(unlist(outliers_ids)))

RFMscores <- RFMscores[!outliersToRemove,]
pmatrix <- pmatrix[!outliersToRemove,]


# number of similarity groups
numClust <- 8

# Kmeans model
#distance matrix
d <- dist(pmatrix,method="euclidean")

pclusters <- kmeans(d,numClust) 

Kgroups <- as.factor(pclusters$cluster)

RFMscores[,cluster:=Kgroups]

# cluster means and metainfo
toTable <- RFMscores[,2:ncol(RFMscores),with=F]

# feature means by cluster
mean_summ_clus <- toTable[,lapply(.SD, mean), by = cluster]
mean_summ_stats <- mean_summ_clus[,2:ncol(mean_summ_clus),with=F]

#cluster population
mean_summ_clus$NumCustomers <- toTable[,.N,by=cluster]$N
mean_summ_clus[,propCust:=round(as.numeric(NumCustomers)/nrow(toTable),3)]
toppt <- mean_summ_clus[,c("cluster","NumCustomers","propCust"),with=F]

#visuals 

#summary box plot
meltT <- melt(toTable,id.vars = ncol(toTable))

pbox <- ggplot(meltT, aes(factor(variable),value))
pbox1 <- pbox + geom_boxplot()+
  facet_wrap( ~cluster,ncol=5)+
  theme(axis.text.x=element_text(angle=-45))+
  ggtitle("Distribution per cluster and group")

pbox_c <- ggplot(meltT, aes(factor(cluster),value))
pbox2 <- pbox_c + geom_boxplot()+
  facet_wrap( ~variable,ncol=5)+
  theme(axis.text.x=element_text(angle=-45))+
  ggtitle("Distribution per cluster and group")

grid.arrange(pbox2,pbox1)