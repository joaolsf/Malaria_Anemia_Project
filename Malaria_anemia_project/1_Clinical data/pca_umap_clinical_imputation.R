

setwd("/Users/joaoluizsfilho/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/Clinical data")

# reading file (importar o arquivo)
multiplex <- read.csv("cohort_anemia_clinical_data_removed_mild_anemia_pLDH_outliers_complete_PCA_kmeans_two_groups_v2.csv")

# length <- multiplex$Length
# cats <- ifelse(length <= 1, 'Early death', 'Late death')
# retirando as colunas indesejadas for correlation
# select_cols <- colnames(multiplex)
# select_cols <- select_cols[!select_cols %in% c("BlockID", "Length","Score")]
# multiplex <- multiplex[,select_cols]

# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$PatientID

#subsetting active variables for the PCA
df_to_cluster.active <- df_to_cluster[,1:25]

# Prepare data
mydata <- (df_to_cluster.active)

#1-Handling missing data in the dataframe:
library("missMDA")
library("FactoMineR")
library("factoextra")
library(RColorBrewer)
library(ggrepel)
library(viridis)

#select the number of dimensions that will be used in the algorithm using the function estim_ncpPCA
#By default, the function estim_ncpPCA uses the GCV method.
#It is possible to use another cross-validation strategy by specifying the argument as follows:
#ncomp$ncp <- estim_ncpPCA(geno, ncp.min = 0, ncp.max = 6, method.cv="Kfold", nbsim=100,pNA=0.05)
#this piece of the code takes forever and it has not been working...it gives an error with lots of NA values.
#ncomp <- estim_ncpPCA(mydata)
#ncomp$ncp

#Perform the (regularized) iterative PCA algorithm with the number of dimensions selected in the previous step, using the function imputePCA:
res.imp <- imputePCA(mydata, ncp = 6)

mydata_complete <- res.imp$completeObs
write.csv(mydata_complete, "cohort_anemia_clinical_data_removed_mild_anemia_pLDH_outliers_complete.csv")

# 2- Batch correction with Limma
library(limma)
mydata <- read.csv("cohort_anemia_clinical_data_removed_mild_anemia_pLDH_outliers_complete.csv")

# convert columns for correction to vectors
age <- as.vector(mydata$Age)
group <- as.vector(multiplex$Clinical_group)
gender <- as.vector(multiplex$Gender)

# transpose dataframe to run correction -  samples as columns and features as rows
mydata2 <- mydata[,5:51]
rownames(mydata2) <- multiplex$PatientID
mydata2 <- t(mydata2)

# convert columns for covariate to matrix - v1 = correct by weight and length
mydata3 <-mydata[,3:4]
mydata3 <- as.matrix(mydata3)

# check dimensions
dim(mydata2)
dim(mydata3)
dim(matrix(1,ncol(mydata2),1))

# run the remove batch effect function and save the data
mydata4 <- removeBatchEffect(mydata2, batch = age, batch2 = gender, design = matrix(1,ncol(mydata2),1),
                             covariates = mydata3, group = group)  

# transpose the output - samples as rows and features as columns
mydata4 <- (t(mydata4))
#design=model.matrix(~group), 
write.csv(mydata4, "cohort_anemia_clinical_data_removed_mild_anemia_pLDH_outliers_complete_corrected.csv") # this looks weird, use the complete uncorrected dataset below and later check for the influence of age, gender, weight and length

# scale data
#mydata10 <- scale(mydata9)
#write.csv(mydata10, "cohort_anemia_clinical_data_removed_mild_anemia_pLDH_outliers_complete.csv")


##################################################################################################################################################################################################################################################################################################

library("missMDA")
library("FactoMineR")
library("factoextra")
library(RColorBrewer)
library(ggrepel)
library(viridis)

# PCA analysis

#perform PCA on the imputed data set. To this end, we propose to use the PCA function of the FactoMineR package:
multiplex <- read.csv("cohort_anemia_clinical_data_removed_mild_anemia_pLDH_outliers_complete.csv")

# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$PatientID

#subsetting active variables for the PCA
df_to_cluster.active <- df_to_cluster[,8:51]

# Prepare data
mydata <- (df_to_cluster.active)

pca.mydata <- PCA(mydata)
print(pca.mydata)

#extract the eigenvalues/variances of PCs
eig.val <- get_eigenvalue(pca.mydata)
eig.val
#visualize the eigenvalues
fviz_eig(pca.mydata)
#extrcat the results for individuals and variables, respectively
get_pca_ind(pca.mydata)
get_pca_var(pca.mydata)
#Visualize the results individuals and variables, respectively.
fviz_pca_ind(pca.mydata,
             pointshape = 21,
             pointsize = 6,
             fill.ind = multiplex$Clinical_group, invisible="quali") +
  ggpubr::fill_palette(palette = c("#0073C2FF", "salmon")) + theme_bw(base_size = 24)

fviz_pca_var(pca.mydata)
fviz_eig(pca.mydata, addlabels = TRUE, ylim = c(0, 50))

#method to extract the results, for variables,
var <- get_pca_var(pca.mydata)
var
fviz_pca_var(pca.mydata, col.var = "black")

#The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates)
#visualize the cos2 of variables on all the dimensions using the corrplot package:
library("corrplot")
corrplot(var$cos2, method = "circle", is.corr=FALSE, tl.cex = 0.8, tl.col = "black", cl.ratio = 0.15, cl.align.text = "l")
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca.mydata, choice = "var", axes = 1:2)
# Color by cos2 values: quality on the factor map
fviz_pca_var(pca.mydata, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, alpha.var = "cos2") # Avoid text overlapping

#The contributions of variables in accounting for the variability in a given principal component are expressed in percentage.
#use the function corrplot() [corrplot package] to highlight the most contributing variables for each dimension:
corrplot(var$contrib, method = "circle", is.corr=FALSE, tl.cex = 0.8, tl.col = "black", cl.ratio = 0.15, cl.align.text = "l")
# Contributions of variables to PC1
fviz_contrib(pca.mydata, choice = "var", axes = 1, top = 50, xtickslab.rt = 90, fill = "#800080", color = "#800080", ggtheme = theme_light())
# Contributions of variables to PC2
fviz_contrib(pca.mydata, choice = "var", axes = 2, top = 50,  xtickslab.rt = 90, fill = "#800080", color = "#800080", ggtheme = theme_light())
#The total contribution to PC1 and PC2 is obtained with the following R code:
fviz_contrib(pca.mydata, choice = "var", axes = 1:5, top = 50,  xtickslab.rt = 90, fill = "#800080")
# Color by contrib values: quality on the factor map
fviz_pca_var(pca.mydata, col.var = "contrib", select.var = list(contrib = 30),
             repel = TRUE, alpha.var = "contrib") + scale_color_gradient2(low="white", mid="blue", 
            high="red", midpoint=0.9) + theme_light() # Avoid text overlapping

#All the outputs of the PCA (individuals/variables coordinates, contributions, etc) can be exported at once, into a TXT/CSV file, using the function write.infile() [in FactoMineR] package:
# Export into a TXT file
write.infile(pca.mydata, "pca.txt", sep = "\t")
# Export into a CSV file
write.infile(pca.mydata, "pca.csv", sep = ";")

#PCA Biplot
#to color both individuals by clusters or k-means clusters and variables by contribution.
options(ggrepel.max.overlaps = 50)
display.brewer.all(n=10, exact.n=FALSE)

p <- fviz_pca_biplot(pca.mydata,
                      # Fill individuals by groups
                      geom.ind = "point",
                      pointshape = 21,
                      pointsize = 6,
                      label = "var",
                      fill.ind = multiplex$Clinical_group,
                      col.ind = "white",
                      # Color variable by groups
                      col.var = "contrib", addEllipses = FALSE, invisible="quali",
                      repel = TRUE, alpha.var = "contrib", gradient.cols = magma(256, direction = -1, begin = 0, end = 0.85)) +
  ggpubr::fill_palette(palette = c("#0073C2FF", "salmon")) + theme_bw(base_size = 24) #color palette from kmeans function above
p

##################################################################################################################################################################################################################################################################################################

# UMAP analysis

library(umap)
set.seed(1234)
mydata.umap <- umap(mydata) 
mydata.umap
head(mydata.umap$layout, 3)
mydata.labels <- multiplex[, "Clinical_group"]


library(tidyverse)
umap_df <- mydata.umap$layout %>%
  as.data.frame() %>%
  rename(UMAP1="V1",
         UMAP2="V2")

umap_df %>% head()

#Plot UMAP by disease group
umap_df %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             color = multiplex$Clinical_group,
             shape = multiplex$Clinical_group,
             size=3)) +
  geom_point()+
  labs(x = "UMAP1",
       y = "UMAP2",
       subtitle = "UMAP plot") + scale_color_manual(values = c("grey", "#0073C2FF", "salmon")) + theme_light(base_size = 24)


theme(legend.justification=c(),
      legend.position='bottom', plot.title = element_text(size=20), 
      axis.text.y = element_text(face = "plain", size=22, angle=0, vjust=1, hjust=0.5),
      axis.text.x = element_text(face = "plain", size=22, angle=0, vjust=1, hjust=0.5), 
      strip.text.x = element_text(size = 22, colour = "white", face = "plain"),
      strip.background = element_rect(color=NULL, fill="#800080", size=1.5, linetype="blank"))


##################################################################################################################################################################################################################################################################################################

# Kmeans analysis

# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:8) wss [i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:8, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model

# K-Means Cluster Analysis
pdf("seeds_kmeans_2clusters.pdf")
for(i in 1:40){
  set.seed(i)
  fit <- kmeans(mydata, 2) # 2 cluster solution
  #plotting kmeans graph
  library(cluster) 
  library(fpc)
  #plotcluster(mydata, fit$cluster)
  clusplot(mydata, main = i, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0)
}
dev.off()
#usar set.seed(19) para 4 clusters
#usar set.seed(39) para 3 clusters


set.seed(2)
fit <- kmeans(mydata, 2) # 5 cluster solution
#plotting kmeans graph
library(cluster) 
library(fpc)
#plotcluster(mydata, fit$cluster)
clusplot(mydata, main = 1, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0)
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata2 <- data.frame(mydata, fit$cluster)

#merging mydata e mydata2 para fazer o plot de PCA com os clusters
pca2 <- data.frame(subject = rownames(pca), PC1 = pca$PC1, PC2 = pca$PC2)
mydata3 <- data.frame(subject = rownames(mydata2), kmeans_clustering = mydata2$fit.cluster)
merged <- merge(pca2, mydata3, by = "subject", all = TRUE)

#merging mydata e mydata2 para fazer o plot de PCA com os clusters
pca2 <- data.frame(subject = rownames(pca), PC1 = pca$PC1, PC2 = pca$PC2)
mydata3 <- data.frame(subject = rownames(mydata2), kmeans_clustering = mydata2$fit.cluster)
merged <- merge(pca2, mydata3, by = "subject", all = TRUE)

#Fazer o plot de PCA usando o ggplot
#Fazer o plot PCA de mydata labeling parasitemia
library(ggplot2)
library(ggthemes)
library(ggrepel)
ggplot(pca2, aes(x = PC1, y = PC2)) + geom_point()
p <- ggplot(pca2, aes(x = PC1, y = PC2, label = subject))
p + geom_point(aes(colour = factor(cats2)), size = 10) + ggtitle("PC Patients") + theme_classic(base_size = 14) 

#Plotting PCA de merged para labeling Kmeans clustering
ggplot(merged, aes(x = PC1, y = PC2)) + geom_point()
p <- ggplot(merged, aes(PC1, PC2, label = subject, col=as.factor(kmeans_clustering)))

"#00BA38", "#619CFF", "#F8766D"

myColors <- c("#00BA38", "#619CFF", "#F8766D") 
p + geom_point (size = 5) + scale_color_manual(values=myColors) + ggtitle("PC Patients") + theme_classic(base_size = 14) 


