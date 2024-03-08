
setwd("/Users/joaoluizsfilho/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/4_Nanostring_human/6_EFA_Nanostring_qRTPCT_luminex/BM_nanostring_qRTPCR")

library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(viridis)

# reading file (importar o arquivo)
multiplex <- read.csv("BM_nanostring_qRTPCR_factor_loadings_iteration1_heatmap.csv")

# data frame 
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$Feature

# Prepare data
mydata <- (df_to_cluster)

# Heatmap
heat <- Heatmap(mydata, cluster_columns = FALSE, cluster_rows = FALSE,
                col = colorRamp2(c(0.7, 0.5, 0.3, 0, -0.3), brewer.pal(n = 5, name = "RdYlBu"), space = "RGB"),
                heatmap_legend_param = list(color_bar = "continuous"),
                show_row_dend = TRUE, show_column_dend = TRUE,
                row_names_gp = gpar(fontsize = 12),
                column_names_gp = gpar(fontsize = 10), column_names_rot = 0)

print(heat)



