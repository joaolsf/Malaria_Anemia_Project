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


#BOX PLOTS METHOD clinical parameter
multiplex <- read.csv("BM_nanostring_qRTPCR_factor_scores_iteration1_boxplot.csv")

multiplex$title <- "BM signature 4"
p <- ggplot(multiplex, aes(x=Clinical_group, y=BMS4, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Sample loading score") +
  theme_bw(base_size = 24) +
  geom_hline(yintercept=0, linetype = 2) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=24), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)



+ scale_y_continuous(limit = c(0, 15))
