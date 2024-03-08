
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

# 1- BOX PLOTS BM qRTPCR

multiplex <- read.csv("qRTPCR_BM_gene_expression.csv")

multiplex$title <- "ANGPT2"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ANGPT2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 20))

multiplex$title <- "CCL2"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CCL2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 20))

multiplex$title <- "CCL5"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CCL5, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 20))

multiplex$title <- "CSF3"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CSF3, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 20))

multiplex$title <- "EPO"
p <- ggplot(multiplex, aes(x=Clinical_group, y=EPO, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 20))

multiplex$title <- "IFNB1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IFNB1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "IFNG"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IFNG, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "SDC1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=SDC1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "SELP"
p <- ggplot(multiplex, aes(x=Clinical_group, y=SELP, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "VCAM1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=VCAM1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "VEGFA"
p <- ggplot(multiplex, aes(x=Clinical_group, y=VEGFA, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

####################################################################################################################################################################################################################################################################################

# 2- #BOX PLOTS PB qRTPCR

multiplex <- read.csv("qRTPCR_PB_gene_expression.csv")

multiplex$title <- "ANGPT2"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ANGPT2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 20))

multiplex$title <- "CCL2"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CCL2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 20))

multiplex$title <- "CCL5"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CCL5, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 20))

multiplex$title <- "CSF3"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CSF3, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 20))

multiplex$title <- "EPO"
p <- ggplot(multiplex, aes(x=Clinical_group, y=EPO, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 20))

multiplex$title <- "IFNB1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IFNB1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "IFNG"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IFNG, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 8))

multiplex$title <- "SDC1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=SDC1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "SELP"
p <- ggplot(multiplex, aes(x=Clinical_group, y=SELP, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "VCAM1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=VCAM1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit=c(0,1.5))

multiplex$title <- "VEGFA"
p <- ggplot(multiplex, aes(x=Clinical_group, y=VEGFA, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "relative expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 


######################################################################################################################################################################################################################################################################################################################################################################################

# 3- Calculate differential gene expression and plot in volcano plots.

library(EnhancedVolcano)
library(limma)
library(RColorBrewer)

multiplex <- read.csv("qRTPCR_BM_gene_expression_complete_limma.csv")
multiplex <- multiplex[,1:13]

# Define design levels

TS <- paste(multiplex$Clinical_group)
TS

TS <- factor(TS, levels=c("MA", "SA"))
design <- model.matrix(~0+TS)
colnames(design) <- levels(TS)

# Create MA list - transpose the df to match the number of rows in the design with the number of columns in the MAlist
M=t(multiplex[,3:13])
A=t(multiplex[,3:13])
MA <- new("MAList",list(M=M,A=A))

# Fit model
fit <- lmFit(MA, design)

# Define contrasts
cont.matrix <- makeContrasts(Diff = SA-MA, levels=design)

fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)
save(fit2, file = "limma_DGE_BM_qRTPCR")
load("limma_DGE_BM_qRTPCR")

DGE1 = topTable(fit2, coef = "Diff", adjust.method = "BH", number = "11", sort.by = "p")
write.csv(DGE1, "DGE_BM_qRTPCR.csv")

DGE1 <- read.csv("DGE_BM_qRTPCR.csv")
options(ggrepel.max.overlaps = 50000)
EnhancedVolcano(toptable = DGE1,
                x = "logFC",
                y = "adj.P.Val",
                lab = DGE1$Genes,
                xlim = c(-5, +5),
                ylim = c(0,5),
                pCutoff = 0.05,
                FCcutoff = 0.5,
                pointSize = 5,
                labSize = 4.0,
                labCol = 'black',
                boxedLabels = FALSE,
                col = c(brewer.pal(n = 4, name = "Spectral"), space = "RGB"),
                colAlpha = 4/5,
                title = "DGE BM qRTPCR",
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

multiplex <- read.csv("qRTPCR_PB_gene_expression_complete_limma.csv")
multiplex <- multiplex[,1:15]

# Define design levels

TS <- paste(multiplex$Clinical_group)
TS

TS <- factor(TS, levels=c("MA", "SA"))
design <- model.matrix(~0+TS)
colnames(design) <- levels(TS)

# Create MA list - transpose the df to match the number of rows in the design with the number of columns in the MAlist
M=t(multiplex[,5:15])
A=t(multiplex[,5:15])
MA <- new("MAList",list(M=M,A=A))

# Fit model
fit <- lmFit(MA, design)

# Define contrasts
cont.matrix <- makeContrasts(Diff = SA-MA, levels=design)

fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)
save(fit2, file = "limma_DGE_PB_qRTPCR")
load("limma_DGE_PB_qRTPCR")

DGE1 = topTable(fit2, coef = "Diff", adjust.method = "BH", number = "11", sort.by = "p")
write.csv(DGE1, "DGE_PB_qRTPCR.csv")

DGE1 <- read.csv("DGE_PB_qRTPCR.csv")
options(ggrepel.max.overlaps = 50000)
EnhancedVolcano(toptable = DGE1,
                x = "logFC",
                y = "adj.P.Val",
                lab = DGE1$Genes,
                xlim = c(-5, +5),
                ylim = c(0,5),
                pCutoff = 0.05,
                FCcutoff = 0.5,
                pointSize = 5,
                labSize = 4.0,
                labCol = 'black',
                boxedLabels = FALSE,
                col = c(brewer.pal(n = 4, name = "Spectral"), space = "RGB"),
                colAlpha = 4/5,
                title = "DGE PB qRTPCR",
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



















