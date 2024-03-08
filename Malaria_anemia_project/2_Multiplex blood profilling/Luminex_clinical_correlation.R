
setwd("/Users/joaoluizsfilho/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/2_Luminex/6_Correlations")

library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(viridis)
library(longitudinal)
library(ggcorrplot)
library(Hmisc)
library(corrplot)

# reading file (importar o arquivo)
multiplex <- read.csv("Luminex_clinical_correlation.csv")

# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$PatientID
df_to_cluster <- df_to_cluster[,2:79]
# Prepare data
mydata <- scale(df_to_cluster)

# Subset by clinical group
moderate <- mydata[1:14,]
severe <- mydata[15:31,]

library(Hmisc)
cor <- rcorr(moderate, type = "spearman") # rcorr Calcula o p-value das correlacoes
library(tabletools)
#calculate adjusted p-values of the correlations: https://rdrr.io/github/JMLuther/tabletools/man/rcorr_padjust.html
cor2 <- rcorr_padjust(cor) # BH by default
#Extract r and p values to plot a corr plot for the correlation of specific variables
write.csv(cor$r , "Correlation_spearman_luminex_clinical_rvalues_moderate.csv")
write.csv(cor$P , "Correlation_spearman_luminex_clinical_pvalues_moderate.csv")
write.csv(cor2$P , "Correlation_spearman_luminex_clinical_adjustpvalues_moderate.csv")

# reading file (importar o arquivo)
multiplex2 <- read.csv("Correlation_spearman_luminex_clinical_rvalues_severe.csv")
multiplex3 <- read.csv("Correlation_spearman_luminex_clinical_pvalues_severe.csv")
multiplex4 <- read.csv("Correlation_spearman_luminex_clinical_adjustpvalues_severe.csv")

rownames(multiplex2) <- multiplex2$Luminex
rownames(multiplex3) <- multiplex3$Luminex
rownames(multiplex4) <- multiplex4$Luminex

multiplex2 <- multiplex2[,-1]
multiplex2 <- as.matrix.data.frame(multiplex2)
multiplex2 <- t(multiplex2)

multiplex3 <- multiplex3[,-1]
multiplex3 <- as.matrix.data.frame(multiplex3)
multiplex3 <- t(multiplex3)

multiplex4 <- multiplex4[,-1]
multiplex4 <- as.matrix.data.frame(multiplex4)
multiplex4 <- t(multiplex4)

#plot the correlation of specific variables using the files above
col4 <- colorRampPalette(c("#053061", "#2166AC", "#4393C3", "#92C5DE", "#D1E5F0", "#FFFFFF", "#FDDBC7", "#F4A582","#D6604D",  "#B2182B", "#67001F"))
library(corrplot)
cor_matrix <- corrplot(multiplex2, method = "square", p.mat = multiplex3, 
                        type = "full", is.corr=T, insig = 'label_sig', sig.level = c(0.001, 0.01, 0.05), pch.cex = 1.5,pch.col = 'white',
                        tl.cex=0.8, tl.col = "black", number.cex=0.5, number.font=0.5, diag=F,cl.cex = 1, cl.ratio = 0.1,
                        mar=c(0.05,0.05,0.05,0.05), col = col4(200), addgrid.col="light grey", order = "hclust")

