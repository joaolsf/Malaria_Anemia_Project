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
multiplex <- read.csv("cohort_anemia_clinical_data_removed_mild_anemia_pLDH_outliers_complete.csv")

multiplex$title <- "Hemoglobin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Hemoglobin, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Hemoglobin (g/dL)") +
  geom_hline(yintercept=6, linetype = 2) +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 15))

multiplex$title <- "Age"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Age, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Age (months)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "HR at admission"
p <- ggplot(multiplex, aes(x=Clinical_group, y=HR_admission, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Heart rate") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))

p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Glyceamia"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Glyceamia, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Glyceamia (mmol/L)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(2, 10))

multiplex$title <- "MUAC"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MUAC, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "MUAC") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Plasma iron"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Plasma_iron, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Plasma iron (ug/dL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "TIBC"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TIBC, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "TIBC (mg/L)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Saturated transferrin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Sat_Transf, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Saturated transferrin (%)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Transferrin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Transferrin, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Transferrin (g/L)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "sTfR"
p <- ggplot(multiplex, aes(x=Clinical_group, y=sTfR, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "sTfR (mg/L)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Ferritin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Ferritin, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Ferritin (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "TfR-F-Index"
p <- ggplot(multiplex, aes(x=Clinical_group, y=TfRF_index, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "TfR-F-Index") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "EPO"
p <- ggplot(multiplex, aes(x=Clinical_group, y=EPO, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "EPO (U/L)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Folate"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Folate, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Folate (ng/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Vitamin B12"
p <- ggplot(multiplex, aes(x=Clinical_group, y=VitA, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Vitamin B12 (pg/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "LDH"
p <- ggplot(multiplex, aes(x=Clinical_group, y=LDH, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "LDH (U/L)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "CRP"
p <- ggplot(multiplex, aes(x=Clinical_group, y=CRP, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "CRP (mg/dL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Albumin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Albumin, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Albumin (g/L)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Prealbumin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Prealbumin, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Prealbumin (g/L)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Haptoglobin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Haptoglobin, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Haptoglobin (g/L)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "Ferritin:CRP"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Ferritin_CRP_ratio, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Ferritin:CRP") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 120))

multiplex$title <- "Ferritin:CRP"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Ferritin_CRP_ratio, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Ferritin:CRP") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 120))

multiplex$title <- "RBC counts"
p <- ggplot(multiplex, aes(x=Clinical_group, y=WBC, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "RBC (x10^6/uL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 18))

multiplex$title <- "Hematocrit"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Hematocrit, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Hematocrit (%)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "Mean Cell Volume"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MCV_fL, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "MCV (fL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "Mean Cell Hemoglobin"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MCH_pg, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "MCH (pg)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "MCHC"
p <- ggplot(multiplex, aes(x=Clinical_group, y=MCHC_g_dL, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "MCHC (g/dL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "RBC width"
p <- ggplot(multiplex, aes(x=Clinical_group, y=RBC_width_sd_fL, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "RBC width sd (fL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "Neutrophils counts"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Neutrophils, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Neutrophils (x10^3/uL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "Lymphocytes counts"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Lymphocytes, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Lymphocytes (x10^3/uL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "Platelet counts"
p <- ggplot(multiplex, aes(x=Clinical_group, y=Platelets, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Platelets (x10^3/uL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) 

multiplex$title <- "NLCR"
p <- ggplot(multiplex, aes(x=Clinical_group, y=NLCR, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "NLCR") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 18, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 3))


multiplex$title <- "PB parasitemia"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PB_parasitemia, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "PB parasitemia (%)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 10))

multiplex$title <- "PB parasite density"
p <- ggplot(multiplex, aes(x=Clinical_group, y=PB_density, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Parasites/uL") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 16, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)

multiplex$title <- "BM parasite density"
p <- ggplot(multiplex, aes(x=Clinical_group, y=BM_density, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Parasites/uL") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 16, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)


multiplex$title <- "BM parasitemia"
p <- ggplot(multiplex, aes(x=Clinical_group, y=BM_parasitemia, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "BM parasitemia (%)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 8))

multiplex$title <- "BM asexual density"
p <- ggplot(multiplex, aes(x=Clinical_group, y=BM_asexual_dens, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Parasites/uL") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title)


multiplex$title <- "BM sexual density"
p <- ggplot(multiplex, aes(x=Clinical_group, y=BM_gam_dens, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Parasites/uL") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 22, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 50))

multiplex$title <- "BM_gam_asex_ratio"
p <- ggplot(multiplex, aes(x=Clinical_group, y=BM_gam_asex_ratio, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "Ratio") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 20, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) + scale_y_continuous(limit = c(0, 30))

multiplex$title <- "Parasite biomass"
p <- ggplot(multiplex, aes(x=Clinical_group, y=pLDH, fill=Clinical_group,)) +
  geom_boxplot(outlier.alpha = 0.1, outlier.shape = 2)  + 
  geom_jitter(size = 3, shape=16, position=position_jitter(0.1)) +
  labs(y = "pLDH (pg/mL)") +
  theme_bw(base_size = 24) +
  theme(legend.justification=c(),
        legend.position='right', plot.title = element_text(size=20), 
        axis.text.y = element_text(face = "plain", size=24, angle=0, vjust=1, hjust=0.5),
        axis.text.x = element_text(face = "plain", size=10, angle=0, vjust=1, hjust=0.5), 
        strip.text.x = element_text(size = 24, colour = "white", face = "plain"),
        strip.background = element_rect(color=NULL, fill="black", size=1.5, linetype="blank"))
p + scale_fill_manual(values = c("#0073C2FF", "salmon")) + scale_x_discrete(limits=c("Moderate anemia", "Severe anemia"))  + facet_grid(. ~ title) #+ scale_y_continuous(limit = c(0, 30))

################################################################################################################################################################################################################################################################################

library(ggpubr)
library(Rmisc)

multiplex <- read.csv("cohort_anemia_clinical_data_removed_mild_anemia_pLDH_outliers_complete_v2.csv")
multiplex$Clinical_group <- as.factor(multiplex$Clinical_group)

# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$PatientID
#subsetting active variables for the PCA
df_to_cluster.active <- df_to_cluster[,1:51]
# Prepare data
mydata <- (df_to_cluster.active)

#Standard error or mean calculation for each factor 
df1_1 <- summarySE(mydata, measurevar="Age", groupvars=c("Clinical_group"))
head(df1_1)

# Run in a loop
mydata2 <- df_to_cluster[,2:51]

df = data.frame()
for (i in colnames(mydata2)){
  output <- summarySE(mydata, measurevar=i, groupvars=c("Clinical_group"))
  df = append(df, output)
}

df2 <- data.frame(df)

library (plyr)
df2 <- ldply (df, data.frame)

#Statistical test
compare_means(Age ~ Clinical_group, data = df1_1, method = "wilcox.test", p.adjust.method	= 'fdr')
compare_means(Age ~ Clinical_group, data = df1_1, method = "t.test", p.adjust.method	= 'none')






