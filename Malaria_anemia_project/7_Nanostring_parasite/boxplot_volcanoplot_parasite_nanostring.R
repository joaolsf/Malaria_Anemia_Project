
setwd("/Users/joaoluizsfilho/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/3_qRTPCR_human")

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

# 1- Calculate differential gene expression and plot in volcano plots.
multiplex <- read.csv("parasite_expression_bm_v1_limma.csv")
#multiplex <- multiplex[,1:13]

# Define design levels
TS <- paste(multiplex$Clinical_group)
TS

TS <- factor(TS, levels=c("MA", "SA"))
design <- model.matrix(~0+TS)
colnames(design) <- levels(TS)

# Create MA list - transpose the df to match the number of rows in the design with the number of columns in the MAlist
M=t(multiplex[,3:458])
A=t(multiplex[,3:458])
MA <- new("MAList",list(M=M,A=A))

# Fit model
fit <- lmFit(MA, design)

# Define contrasts
cont.matrix <- makeContrasts(Diff = SA-MA, levels=design)

fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)
save(fit2, file = "limma_DGE_BM_Nanostring_Parasite")
#load("limma_DGE_BM_Nanostring_Parasite")

DGE1 = topTable(fit2, coef = "Diff", adjust.method = "BH", number = "456", sort.by = "p")
write.csv(DGE1, "DGE_BM_Nanostring_Parasite_Severe_vs_Moderate.csv")

DGE1 <- read.csv("DGE_BM_Nanostring_Parasite_Severe_vs_Moderate.csv")
options(ggrepel.max.overlaps = 50000)
EnhancedVolcano(toptable = DGE1,
                x = "logFC",
                y = "adj.P.Val",
                lab = DGE1$description,
                xlim = c(-5, +5),
                ylim = c(0,5),
                pCutoff = 0.06,
                FCcutoff = 0.5,
                pointSize = 2,
                labSize = 4.0,
                labCol = 'black',
                boxedLabels = FALSE,
                col = c(brewer.pal(n = 4, name = "Spectral"), space = "RGB"),
                colAlpha = 4/5,
                title = "DGE BM Severe vs Moderate anemia",
                legendLabels = c(
                  'Not significant',
                  'Fold change (but do not pass padj cutoff)',
                  'Pass padj cutoff',
                  'Pass both padj & fold change'),
                legendPosition = 'right',
                legendLabSize = 6.0,
                legendIconSize = 2.0,
                drawConnectors = TRUE,
                widthConnectors = 0.2,
                typeConnectors = "open",
                colConnectors = 'black'
) + theme_light()

# PB genes

multiplex <- read.csv("parasite_expression_pb_v1_limma.csv")
#multiplex <- multiplex[,1:15]

# Define design levels
TS <- paste(multiplex$Clinical_group)
TS

TS <- factor(TS, levels=c("MA", "SA"))
design <- model.matrix(~0+TS)
colnames(design) <- levels(TS)

# Create MA list - transpose the df to match the number of rows in the design with the number of columns in the MAlist
M=t(multiplex[,3:458])
A=t(multiplex[,3:458])
MA <- new("MAList",list(M=M,A=A))

# Fit model
fit <- lmFit(MA, design)

# Define contrasts
cont.matrix <- makeContrasts(Diff = SA-MA, levels=design)

fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)
save(fit2, file = "limma_DGE_PB_Nanostring_Parasite")
#load("limma_DGE_PB_qRTPCR")

DGE1 = topTable(fit2, coef = "Diff", adjust.method = "BH", number = "456", sort.by = "p")
write.csv(DGE1, "DGE_PB_Nanostring_Parasite_Severe_vs_Moderate.csv")

DGE1 <- read.csv("DGE_PB_Nanostring_Parasite_Severe_vs_Moderate.csv")
options(ggrepel.max.overlaps = 50000)
EnhancedVolcano(toptable = DGE1,
                x = "logFC",
                y = "adj.P.Val",
                lab = DGE1$Genes,
                xlim = c(-5, +5),
                ylim = c(0,5),
                pCutoff = 0.06,
                FCcutoff = 0.5,
                pointSize = 2,
                labSize = 4.0,
                labCol = 'black',
                boxedLabels = FALSE,
                col = c(brewer.pal(n = 4, name = "Spectral"), space = "RGB"),
                colAlpha = 4/5,
                title = "DGE PB Severe vs Moderate anemia",
                legendLabels = c(
                  'Not significant',
                  'Fold change (but do not pass padj cutoff)',
                  'Pass padj cutoff',
                  'Pass both padj & fold change'),
                legendPosition = 'right',
                legendLabSize = 6.0,
                legendIconSize = 2.0,
                drawConnectors = TRUE,
                widthConnectors = 0.2,
                typeConnectors = "open",
                colConnectors = 'black'
) + theme_light()

# Add stage and description info for each gene
df1 <- read.csv("DGE_PB_Nanostring_Parasite_Severe_vs_Moderate.csv")
df2 <- read.csv("nanostring_anemia_dge.csv")

idx <- match(df1$Genes, df2$plasmodb_id)
df1$stage <- df2$stage[ idx ]
df1$description <- df2$description[ idx ]
write.csv(df1, "DGE_PB_Nanostring_Parasite_Severe_vs_Moderate_v2.csv")

########################################################################################################################################################################################################################################################################################

library(pheatmap)
library(ComplexHeatmap)
library(circlize)

multiplex <- read.csv("DGE_BM_Nanostring_Parasite_Severe_vs_Moderate.csv")

# Heatmap BM parasite average expression
data <- multiplex[,1:3]
rownames(data) <- data$Genes
data <- data$AveExpr
data <- as.matrix(data)
rownames(data) <- multiplex$Genes
rownames(data) <- multiplex$description

pheatmap(data, color = brewer.pal(6, "YlOrRd"), cluster_rows = T, show_rownames=T,
         annotation_legend = F, annotation_names_row = F, border_color=NA, fontsize = 10, scale="none", fontsize_row = 2)

# Heatmap BM parasite logFC (severe vs moderate)
data <- multiplex[,1:3]
rownames(data) <- data$Genes
data <- data$logFC
data <- as.matrix(data)
rownames(data) <- multiplex$Genes
rownames(data) <- multiplex$description

pheatmap(data, color = colorRamp2(c(4, 2, 0, -1, -2, -4), brewer.pal(n = 6, name = "RdYlBu"), space = "RGB"), cluster_rows = T, show_rownames=T,
         annotation_legend = F, annotation_names_row = F, border_color=NA, fontsize = 10, scale="none", fontsize_row = 2)

########################################################################################################################################################################################################################################################################################

multiplex <- read.csv("DGE_PB_Nanostring_Parasite_Severe_vs_Moderate.csv")

# Heatmap PB parasite average expression
data <- multiplex[,1:3]
rownames(data) <- data$Genes
data <- data$AveExpr
data <- as.matrix(data)
rownames(data) <- multiplex$Genes
rownames(data) <- multiplex$description

pheatmap(data, color = brewer.pal(6, "YlOrRd"), cluster_rows = T, show_rownames=T,
         annotation_legend = F, annotation_names_row = F, border_color=NA, fontsize = 10, scale="none", fontsize_row = 2)

# Heatmap PB parasite logFC (severe vs moderate)
data <- multiplex[,1:3]
rownames(data) <- data$Genes
data <- data$logFC
data <- as.matrix(data)
rownames(data) <- multiplex$Genes
rownames(data) <- multiplex$description

pheatmap(data, color = colorRamp2(c(4, 2, 0, -1, -2, -4), brewer.pal(n = 6, name = "RdYlBu"), space = "RGB"), cluster_rows = T, show_rownames=T,
         annotation_legend = F, annotation_names_row = F, border_color=NA, fontsize = 10, scale="none", fontsize_row = 2)

########################################################################################################################################################################################################################################################################################

# Bar plots logFC vs description colored by stage

library(ggplot2)
weights <- read.csv("DGE_PB_Nanostring_Parasite_Severe_vs_Moderate_top20.csv")

plot_ <- ggplot(weights,
                aes(x= reorder(description,
                               (logFC), decreasing = FALSE), y = (logFC), fill = stage)) +
  geom_bar(stat = "identity", width=0.7, position=position_dodge(width=1)) +
  scale_fill_manual("legend", values = c("circulating" = '#0073C2FF', "sequestered" = "#E7B800", "gam_ring" = "#3B3B3BFF", "imm_gam" = "#800080", "mat_gam" = "#CD534CFF", "NA" = "#00AFBB")) +
  theme_bw(base_size = 10) +
  coord_flip() +
  xlab("") + 
  ylab("logFC") +
  ggtitle("logFC - Severe vs moderate anemia") +
  theme(plot.title = element_text(size=12), 
        axis.text.y = element_text(face = "plain", size=6, angle=0, vjust=.5, hjust=1),
        axis.text.x = element_text(face = "plain", size=12, angle=0, vjust=.5, hjust=1)) 
plot_






