
setwd("/Users/joaoluizsfilho/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/8_Nanostring_parasite/4_Stage composition")

library(reshape)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(tidyr)
library(forcats)
library(Rmisc)
library(EnhancedVolcano)
library(limma)
library(RColorBrewer)

# 1- Add stage info for each gene
df1 <- read.csv("Genes.csv")
df2 <- read.csv("pelle_clusters_plasmodb.csv")

idx <- match(df1$Genes, df2$plasmodb_id)
df1$stage <- df2$stage[ idx ]
write.csv(df1, "Genes.csv")

df3 <- read.csv("parasite_expression_pb_v1csv")
df4 <- melt(df3)

idx <- match(df4$variable, df1$Genes)
df4$stage <- df1$stage[ idx ]

#"un-melt" df and return to df shape, averaging gene expression by stage per patient
df4$id <- rep(1:31,times = 455)
df5 <- dcast(data = df4,formula = id~stage,fun.aggregate = mean,value.var = c("value"))

write.csv(df4, "parasite_expression_pb_stages.csv")
write.csv(df5, "parasite_expression_pb_stages_average_expression_per_patient.csv")

########################################################################################################################################################################################################################################################################################

library(ComplexHeatmap)
library(circlize)
library(RColorBrewer)
library(NbClust)
library(mclust)
library(M3C)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(viridis)

# reading file (importar o arquivo)
multiplex <- read.csv("parasite_expression_bm_pb_stages_average_expression_per_patient.csv")

multiplexBM <- multiplex[1:29,]
multiplexPB <- multiplex[30:48,]

# category 1
outcome <- multiplexPB$Tissue
cats <- ifelse(outcome <= 1, "Bone marrow", 'Peripheral blood')

# category 2
outcome2 <- multiplexPB$Group
Disease_group <- ifelse(outcome2 <= 1, 'Moderate anemia', 'Severe anemia')

# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$SampleID

#subsetting active variables for the PCA
df_to_cluster.active <- df_to_cluster[,4:9]

# Prepare data
mydata <- scale(df_to_cluster.active)

mydataBM <- mydata[1:29,]
mydataPB <- mydata[30:48,]

#display.brewer.all(n=10, exact.n=FALSE)
#col = colorRampPalette(rev(brewer.pal(n = 9, name = "Spectral")))(3)
#colorRamp2(c(-0.5, 0, 0.5), c("blue", "yellow", "red"), space = "RGB"),

rannot <- rowAnnotation(df = data.frame(cats), 
                        col = list(cats = c("Bone marrow" = "#00AFBB", "Peripheral blood" = "#E7B800")),
                        annotation_width = unit(c(0.5, 0.5), "cm"))

rannot2 <- rowAnnotation(df = data.frame(Disease_group), 
                         col = list(Disease_group = c("Moderate anemia" = "#0073C2FF", "Severe anemia" = "salmon")),
                         annotation_width = unit(c(0.5, 0.5), "cm"))

heat <- Heatmap(mydataPB, cluster_columns = TRUE, cluster_rows = TRUE, show_row_names = TRUE,
                col = colorRamp2(c(-1, -0.6, 0, 0.6, 1),brewer.pal(n = 5, name = "YlOrRd"), space = "RGB"),
                heatmap_legend_param = list(color_bar = "continuous"),
                show_row_dend = FALSE, show_column_dend = TRUE,
                row_names_gp = gpar(fontsize = 12),
                column_names_gp = gpar(fontsize = 12))
print(heat+rannot2)

########################################################################################################################################################################################################################################################################################

# Bar plots stage composition per tissue

library(ggplot2)
df <- read.csv("parasite_expression_pb_stages_average_expression_per_patient.csv")

df$title <- "Mat gam - PB"
p <- ggplot(df, aes(x=Clinical_group, y=mat_gam, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "average expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 22, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", size=1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))




