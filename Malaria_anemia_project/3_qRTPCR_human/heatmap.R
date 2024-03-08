
setwd("/Users/joaoluizsfilho/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/3_qRTPCR_human/3_Analysis/4_PCA_UMAP_heatmap")

library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(viridis)

# 1- Heatmap BM qRTPCR

# reading file (importar o arquivo)
multiplex <- read.csv("qRTPCR_BM_gene_expression_complete.csv")

# category 1
group <- multiplex$Clinical_group2
cats <- ifelse(group <= 1, 'Moderate anemia', 'Severe anemia')

# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$SampleID
df_to_cluster.active <- df_to_cluster[,2:12]

# Prepare data
mydata <- scale(df_to_cluster.active)

# Fazendo o heatmap
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)

rannot <- rowAnnotation(df = data.frame(cats), 
                        col = list(cats = c("Severe anemia" = "salmon", "Moderate anemia" = "#0073C2FF")), 
                        annotation_width = unit(c(0.5, 0.5), "cm"))

heat <- Heatmap(mydata, cluster_columns = TRUE, cluster_rows = FALSE,
                col = colorRamp2(c(2, 1, 0, -1, -2),brewer.pal(n = 5, name = "RdYlBu"), space = "RGB"),
                heatmap_legend_param = list(color_bar = "continuous"),
                show_row_dend = TRUE, show_column_dend = TRUE,
                row_names_gp = gpar(fontsize = 8),
                column_names_gp = gpar(fontsize = 12))
print(heat+rannot)


########################################################################################################################################################################################################################################

# 2- Heatmap PB qRTPCR

# reading file (importar o arquivo)
multiplex <- read.csv("qRTPCR_PB_gene_expression_complete.csv")

# category 1
group <- multiplex$Clinical_group2
cats <- ifelse(group <= 1, 'Moderate anemia', 'Severe anemia')

# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$SampleID
df_to_cluster.active <- df_to_cluster[,4:14]

# Prepare data
mydata <- scale(df_to_cluster.active)

# Fazendo o heatmap
library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)

rannot <- rowAnnotation(df = data.frame(cats), 
                        col = list(cats = c("Severe anemia" = "salmon", "Moderate anemia" = "#0073C2FF")), 
                        annotation_width = unit(c(0.5, 0.5), "cm"))

heat <- Heatmap(mydata, cluster_columns = TRUE, cluster_rows = TRUE,
                col = colorRamp2(c(2, 1, 0, -1, -2),brewer.pal(n = 5, name = "RdYlBu"), space = "RGB"),
                heatmap_legend_param = list(color_bar = "continuous"),
                show_row_dend = TRUE, show_column_dend = TRUE,
                row_names_gp = gpar(fontsize = 8),
                column_names_gp = gpar(fontsize = 12))
print(heat+rannot)

