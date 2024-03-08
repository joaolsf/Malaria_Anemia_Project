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
multiplex <- read.csv("cohort_anemia_luminex_data_noCXCL12_nooutlier.csv")

multiplex$title <- "Ang-1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Ang1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Ang-1 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", linewidth=1.5, linetype="blank"))

p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD',"Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 15000))

multiplex$title <- "Ang-2"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Ang2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Ang-2 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD',"Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 12000))

multiplex$title <- "Ang-2:Ang-1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Ang2_Ang1_ratio, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Ang-2:Ang-1") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))

p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 4))

multiplex$title <- "VCAM-1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=VCAM1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "VCAM-1 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0,10000))

multiplex$title <- "E-selectin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=E_selectin, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "E-selectin (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0,175000))

multiplex$title <- "P-selectin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=P_selectin, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "P-selectin (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0,70000))

multiplex$title <- "Syndecan-1"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Syndecan1, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Syndecan-1 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0,9000))

multiplex$title <- "VEGF"
p <- ggplot(multiplex, aes(x=Clinical_group, y=VEGF, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "VEGF (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "TNF-a"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TNFa, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "TNF-a (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "IL-1a"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL1a, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-1a (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "IL-1b"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL1b, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-1b (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "IL-6"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL6, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-6 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "IL-8"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL8, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-8 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0,200))

multiplex$title <- "IL-10"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL10, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-10 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0,200))

multiplex$title <- "TNFa:IL-10"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TNF_IL10_ratio, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "TNFa:IL-10") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0,5))

multiplex$title <- "IFN-g"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IFNg, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IFN-g (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "IL-27"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL27, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-27 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "G-CSF"
p <- ggplot(multiplex, aes(x=Clinical_group, y=G_CSF, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "G-CSF (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0,125))

multiplex$title <- "GM-CSF"
p <- ggplot(multiplex, aes(x=Clinical_group, y=GM_CSF, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "GM-CSF (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,125))

multiplex$title <- "M-CSF"
p <- ggplot(multiplex, aes(x=Clinical_group, y=M_CSF, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "M-CSF (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0,2500))

multiplex$title <- "IL-11"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL11, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-11 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "TPO"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TPO, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "TPO (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "Ftl-3L"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Ftl_3L, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Ftl-3L (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "IL-7"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL7, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-7 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "IL-15"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL15, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-15 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "IL-21"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL21, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-21 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "IL-3"
p <- ggplot(multiplex, aes(x=Clinical_group, y=IL3, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "IL-3 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "OPN"
p <- ggplot(multiplex, aes(x=Clinical_group, y=OPN, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "OPN (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "SCF"
p <- ggplot(multiplex, aes(x=Clinical_group, y=SCF, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "SCF (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "c-Kit"
p <- ggplot(multiplex, aes(x=Clinical_group, y=cKit, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "c-Kit (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "CCL2"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CCL2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "CCL2 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "CCL2"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CCL2, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "CCL2 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "CCL5"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CCL5, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "CCL5 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

multiplex$title <- "Eotaxin-1/CCL11"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CCL11, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Eotaxin-1/CCL11 (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("light grey", "#0073C2FF", "salmon")) + scale_x_discrete(limits=c('HD', "Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0,2500))

######################################################################################################################################################################################################################################################################################################################################################################################

# 3- Calculate differential gene expression and plot in volcano plots.

library(EnhancedVolcano)
library(limma)
library(RColorBrewer)

multiplex <- read.csv("cohort_anemia_luminex_data_noCXCL12_nooutlier_limma.csv")

# Define design levels
TS <- paste(multiplex$Clinical_group)
TS
TS <- factor(TS, levels=c("MA", "SA"))
design <- model.matrix(~0+TS)
colnames(design) <- levels(TS)

# Create MA list - transpose the df to match the number of rows in the design with the number of columns in the MAlist
M=t(multiplex[,3:36])
A=t(multiplex[,3:36])
MA <- new("MAList",list(M=M,A=A))

# Fit model
fit <- lmFit(MA, design)

# Define contrasts
cont.matrix <- makeContrasts(Diff = SA-MA, levels=design)

fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)
save(fit2, file = "limma_DGE_PB_luminex")
load("limma_DGE_PB_luminex")

DGE1 = topTable(fit2, coef = "Diff", adjust.method = "BH", number = "11", sort.by = "p")
write.csv(DGE1, "DGE_PB_luminex.csv")

DGE1 <- read.csv("DGE_PB_luminex.csv")
options(ggrepel.max.overlaps = 50000)
EnhancedVolcano(toptable = DGE1,
                x = "logFC",
                y = "P.Value",
                lab = DGE1$Feature,
                xlim = c(-200, +1600),
                ylim = c(0,5),
                pCutoff = 0.05,
                FCcutoff = 0.5,
                pointSize = 5,
                labSize = 4.0,
                labCol = 'black',
                boxedLabels = FALSE,
                col = c(brewer.pal(n = 4, name = "Spectral"), space = "RGB"),
                colAlpha = 4/5,
                title = "DE PB luminex",
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




