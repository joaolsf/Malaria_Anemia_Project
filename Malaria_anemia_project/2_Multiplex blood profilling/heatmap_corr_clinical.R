
setwd("/Users/joaoluizsfilho/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/2_Luminex/2_PCA_UMAP_heatmap")

library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(viridis)

# reading file (importar o arquivo)
multiplex <- read.csv("cohort_anemia_luminex_data_noCXCL12_nooutlier_heatmap.csv")

# category 1
group <- multiplex$Clinical_group2
cats <- ifelse(group <= 1, 'Moderate anemia','Severe anemia')

# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$PatientID
df_to_cluster.active <- df_to_cluster[,6:39]

# Prepare data
mydata <- scale(df_to_cluster.active)

# Fazendo o heatmap
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
#display.brewer.all(n=10, exact.n=FALSE)
#col = colorRampPalette(rev(brewer.pal(n = 9, name = "Spectral")))(3)
#colorRamp2(c(-0.5, 0, 0.5), c("blue", "yellow", "red"), space = "RGB"),

#cmeth <- c('ward.D', 'ward.D2', "complete", "single", "average", 
 #          "mcquitty", 'median', "centroid")
#dmeth <- c("euclidean", "manhattan", "minkowski", "pearson")

#heatmap virus patterns/macrophage patterns/lymphocyte patterns

rannot <- rowAnnotation(df = data.frame(cats), 
                        col = list(cats = c("Severe anemia" = "salmon", "Moderate anemia" = "#0073C2FF", "Mild anemia" = "grey", "Healthy donor" = "black")), 
                        annotation_width = unit(c(0.5, 0.5), "cm"))

heat <- Heatmap(mydata, cluster_columns = TRUE, cluster_rows = FALSE,
                col = colorRamp2(c(2, 1, 0, -1, -2),brewer.pal(n = 5, name = "RdYlBu"), space = "RGB"),
                heatmap_legend_param = list(color_bar = "continuous"),
                show_row_dend = TRUE, show_column_dend = TRUE,
                row_names_gp = gpar(fontsize = 8),
                column_names_gp = gpar(fontsize = 12))
print(heat+rannot)

#1, 0.6, 0, -0.6, -1
#-1, -0.5, -0.25, 0, 0.25, 0.5, 1
#brewer.pal(n = 5, name = "Spectral"), space = "RGB")

 
pdf('heatmap4.pdf')
for (i in 1:40){
  heat <- Heatmap(mydata, clustering_distance_columns = "euclidean",
                  clustering_method_columns = "ward.D", km = 3, row_km_repeats = i,
                  col = colorRamp2(c(-1, -0.5, 0, 0.5, 1), brewer.pal(n = 5, name = "YlOrRd"), space = "RGB"),
                  heatmap_legend_param = list(color_bar = "continuous"),
                  show_row_dend = TRUE, show_column_dend = TRUE,
                  row_names_gp = gpar(fontsize = 8),
                  column_names_gp = gpar(fontsize = 8))
  print(heat)
}
dev.off()


#Correlation deceased only
mydata2 <- mydata[1:323,]
#correlation only early death
mydata3 <- mydata[1:87,]
#correlation only late death
mydata4 <- mydata[88:323,]
#Correlation recovered only
mydata5 <- mydata[324:573,]


library(ggcorrplot)
corr <- round(cor(mydata2, use = "pairwise.complete.obs"), 2)

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(corr)

#Visualize the correlation matrix
#method = "square" (default)
ggcorrplot(corr)

# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.col = "grey",ggtheme = ggplot2::theme_gray, type = "lower", colors = c("#6D9EC1", "white", "#E46726"), tl.cex = 12,tl.srt = 90, lab = FALSE, p.mat = p.mat, sig.level = 0.05, insig = "blank")

# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(corr, hc.order = TRUE,
           outline.col = "grey",
           ggtheme = ggplot2::theme_gray, tl.cex = 8,tl.srt = 90,
           colors = c("#6D9EC1", "white", "#E46726"), lab = TRUE, type = "lower")

library(Hmisc)
rcorr(mydata)

cor <- rcorr(mydata4, type = "spearman") # rcorr Calcula o p-value das correlacoes
cor$P
# plot das correlacoes de acordo com o p-value
col4 <- colorRampPalette(c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#FFFFFF", "#FDDBC7", "#F4A582","#D6604D",  "#B2182B", "#67001F"))

library(corrplot)
cor_matrix2 <- corrplot(cor$r, method = "square", p.mat = cor$P, sig.level = 0.05, 
                        insig = "blank", type = "full", is.corr=T, 
                        tl.cex=0.5, tl.col = "black", number.cex=0.5, number.font=0.5, diag=T, 
                        mar=c(1,1,1,1), order = "hclust", hclust.method = "ward.D", col = col4(200))



