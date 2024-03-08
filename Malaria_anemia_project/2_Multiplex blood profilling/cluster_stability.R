library(cluster) 
library(fpc)
library(data.table)
library(ggplot2)
library(ggrepel)

# reading file (importar o arquivo)
multiplex <- read.csv("/Users/joaoluizsfilho/Dropbox/Work Files/Matthia's Lab/Projects/Severe Anemia/Luminex/luminex_falciparum2.csv")

# retirando as colunas indesejadas 
select_cols <- colnames(multiplex)
select_cols <- select_cols[!select_cols %in% c("Group", "Age","Hemoglobin","Temperature","CXCL12")]
# select_cols
multiplex <- multiplex[,select_cols]
# rename Individuo column
multiplex$Individuo <- paste0("p", multiplex$Individuo)
# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$Individuo

# Prepare data
mydata <- scale(df_to_cluster, scale= TRUE, center= TRUE)

## PCA PLOT
pca <- prcomp(mydata, center= FALSE, scale= FALSE)
xpca <- data.table(pca$x, keep.rownames= 'pid') 
xpca[, category := category]
gg <- ggplot(data= xpca, aes(PC1, PC2, label= pid)) +
  geom_point() +
  geom_text_repel(colour= 'grey60') +
  theme_light()

ggsave(snakemake@output[['pca']], width= 16, height= 14, units= 'cm')

gg
###

pdf('kmeansruns', width= 16/2.54, height= 20/2.54)
boots <- list()
for(nclst in 2:4) {
  nstart <- 1
  clst <- list()
  for(i in 1:1000){
    set.seed(i)
    fit <- kmeans(mydata, centers= nclst, iter.max= 100, nstart= nstart)
    clst[[length(clst) + 1]] <- data.table(
      seed= i,
      pid= names(fit$cluster),
      clst_id= fit$cluster
    )
  }
  clst <- rbindlist(clst)
  
  grps <- clst[, list(grp= paste0(paste(sort(pid), collapse= ','), ',')), by= list(seed, clst_id)]
  grps[, clst_id := NULL]
  
  clusters <- grps[, list(cluster= paste(sort(grp), collapse= ';')), by= seed]
  clusters <- clusters[, list(.N, exseed= min(seed)), by= cluster][order(-N)]
  
  bestwss <- sum(kmeans(mydata, centers= nclst, iter.max= 100, nstart= 1000)$withinss)
  
  par(mfrow= c(3, 2), mar= c(2, 2, 1, 1), mgp= c(1, 0.5, 0), tcl= 0.2, bty= 'l', oma= c(0, 0, 2, 0))
  for(i in 1:min(nrow(clusters), 6)) {
    set.seed(clusters$exseed[i])
    fit <- kmeans(mydata, centers= nclst, iter.max= 100, nstart= nstart)
    clusplot(mydata, fit$cluster, main= '', color= TRUE, shade= FALSE, labels= 0, lines= 0, xlab= '', ylab= '', cex= 1.5)
    wss <- sum(fit$withinss)
    mtext(side= 1, line= -1.5, text= sprintf('In %s/%s; wss= %.1f', clusters$N[i], sum(clusters$N), wss), col= ifelse(wss == bestwss, 'red', 'black'), adj= 0.05)
    
    km.boot <- clusterboot(mydata, B= 500, bootmethod= "boot", clustermethod= kmeansCBI, krange= nclst, seed= clusters$exseed[i], count= FALSE, iter.max= 100, nstart= 1)
    boots[[length(boots) + 1]] <- data.table(
      n_clusters= nclst,
      seed= clusters$exseed[i],
      withinss= sum(km.boot$result$result$withinss),
      jaccard_index= km.boot$bootmean,
      cluster_id= 1:length(km.boot$bootmean),
      cluster_size= km.boot$result$result$size
    )
  }
  mtext(side= 3, line= 0.2, outer= TRUE, text= sprintf('N clusters= %s; best withinss= %.1f', nclst, bestwss))
}
dev.off()
boots <- rbindlist(boots)
boots[, jaccard_index := sprintf('%.3f', jaccard_index)]
boots[, withinss := sprintf('%.1f', withinss)]
write.table(boots, snakemake@output[['boots']], sep= '\t', row.names= FALSE, quote= FALSE)

library(fpc)

cf1 <- clusterboot(mydata,B=1000,
                   bootmethod="boot",
                   bscompare=TRUE, 
                   multipleboot=FALSE,
                   jittertuning=0.05, noisetuning=c(0.05,4),
                   subtuning=floor(nrow(data)/2),
                   clustermethod=kmeansCBI, krange=2, noisemethod=FALSE,count=FALSE,
                   showplots=TRUE,dissolution=0.5,
                   recover=0.75,seed=NULL,datatomatrix=TRUE)

## S3 method for class 'clboot'
print(cf1,statistics=c("mean","dissolution","recovery"))

## S3 method for class 'clboot'
plot(cf1,xlim=c(0,1),breaks=seq(0,1,by=0.05))

print(cf1)
plot(cf1)

#   Create the distance matrix.
d <- dist(mydata, method="euclidean") 
#   Do the clustering. 
pfit <- hclust(d, method="ward.D")   
#   Plot the dendrogram.
plot(pfit)

print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
  }
}

rect.hclust(pfit, k=2)

# get the cluster labels
groups <- cutree(pfit, k=2)

# --- results -- 
print_clusters(groups, 2)

library(fpc)   

# set the desired number of clusters                               
kbest.p<-2   

#   Run clusterboot() with hclust 
#   ('clustermethod=hclustCBI') using Ward's method 
#   ('method="ward"') and kbest.p clusters 
#   ('k=kbest.p'). Return the results in an object 
#   called cboot.hclust.
cboot.hclust <- clusterboot(mydata, B=1000, bootmethod="boot", clustermethod=kmeansCBI, krange=2)

cluster.stats(d, fit1$cluster, fit2$cluster)
#   The results of the clustering are in 
#   cboot.hclust$result. The output of the hclust() 
#   function is in cboot.hclust$result$result. 
#   cboot.hclust$result$partition returns a 
#   vector of clusterlabels. 

output <- cboot.hclust$result$result
groups <- cboot.hclust$result$partition

print_clusters(groups, kbest.p)  

# The vector of cluster stabilities. 
# Values close to 1 indicate stable clusters
cboot.hclust$bootmean                                   


# The count of how many times each cluster was 
# dissolved. By default clusterboot() runs 100 
# bootstrap iterations. 
# Clusters that are dissolved often are unstable. 
cboot.hclust$bootbrd                                    


