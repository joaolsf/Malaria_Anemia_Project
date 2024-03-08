

setwd("/Users/joaoluizsfilho/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/2_Luminex")

#1-Handling missing data in the dataframe:
library("missMDA")
library("FactoMineR")
library("factoextra")
library(RColorBrewer)
library(ggrepel)
library(viridis)

# PCA analysis

#perform PCA on the imputed data set. To this end, we propose to use the PCA function of the FactoMineR package:
multiplex <- read.csv("cohort_anemia_luminex_data_noCXCL12_nooutlier.csv")
multiplex2 <- multiplex[1:30,]

# data frame para cluster
df_to_cluster <- multiplex2[,-1]
rownames(df_to_cluster) <- multiplex2$PatientID

#subsetting active variables for the PCA
df_to_cluster.active <- df_to_cluster[,8:40]

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
             fill.ind = multiplex2$Clinical_group, invisible="quali") +
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
                      fill.ind = multiplex2$Clinical_group,
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

