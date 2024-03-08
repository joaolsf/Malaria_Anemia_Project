
setwd("/Users/joaoluizsfilho/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/4_Nanostring_human")

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

#BOX PLOTS BM nanostring
multiplex <- read.csv("cohort_anemia_nanostring_human_BM_complete.csv")

multiplex$title <- "ADRB1 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ADRB1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", size=1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ADRB2 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ADRB2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth =1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ADRB3 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ADRB3, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ALAS1 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ALAS1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "AP - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=AP, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "KIT (SCFR) - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CKIT, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD73 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD73, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD90 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD90, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD34 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD34, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CXCL12 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CXCL12, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))


multiplex$title <- "CXCR4 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CXCR4, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "IL17RA - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL17RA, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ANGPT1 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ANGPT1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "TIE2 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TIE2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD105 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD105, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD146 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD146, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD31 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD31, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD44 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD44, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ICAM1 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ICAM1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "JAGGED1 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=JAGGED1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "Thrombin - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=F2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "Thrombomodulin - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=THBD, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PAR1 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PAR1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PAR2 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PAR2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PAR3 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PAR3, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PAR4 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PAR4, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PROC - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PROC, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "EPCR - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=EPCR, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "LEPTINR - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=LEPTINR, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "MMP2 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MMP2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "MMP9 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MMP9, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "MMP13 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MMP13, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "MMP14 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MMP14, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "OPN - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=OPN, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PDGFRB - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PDGFRB, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PDGF - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PDGF, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "TGFB1 - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TGF, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "TGFRB - BM"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TGFRB, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

#######################################################################################################################################################################################################################################################################################################################################################################################################################################################################

#BOX PLOTS BM nanostring
multiplex <- read.csv("cohort_anemia_nanostring_human_PB_complete.csv")

multiplex$title <- "ADRB1 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ADRB1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ADRB2 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ADRB2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth =1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ADRB3 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ADRB3, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ALAS1 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ALAS1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "AP - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=AP, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "KIT (SCFR) - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CKIT, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 22, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD73 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD73, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD90 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD90, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD34 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD34, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CXCL12 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CXCL12, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CXCR4 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CXCR4, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "IL17RA - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL17RA, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ANGPT1 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ANGPT1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "TIE2 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TIE2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD105 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD105, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD146 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD146, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD31 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD31, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "CD44 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CD44, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "ICAM1 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=ICAM1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "JAGGED1 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=JAGGED1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "Thrombin - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=F2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "Thrombomodulin - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=THBD, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PAR1 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PAR1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PAR2 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PAR2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PAR3 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PAR3, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PAR4 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PAR4, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PROC - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PROC, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "EPCR - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=EPCR, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "LEPTINR - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=LEPTINR, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "MMP2 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MMP2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "MMP9 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MMP9, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "MMP13 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MMP13, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "MMP14 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MMP14, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "OPN - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=OPN, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PDGFRB - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PDGFRB, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "PDGF - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PDGF, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "TGFB1 - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TGF, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "TGFRB - PB"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TGFRB, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "normalised expression") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill ="black", linewidth=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) # + scale_y_continuous(limit = c(0, 15000))






