

setwd("/Users/joaoluizsfilho/Library/CloudStorage/Dropbox/Work_Files/Matthias_Lab/Projects/Severe_Anemia_project/qRTPCR_human")

# 1- qRTPCR analysis with the pcr package

# Reference: https://cran.r-project.org/web/packages/pcr/vignettes/qpcr_analysis.html
# https://liz-is.github.io/qpcr-analysis-with-r/aio.html

# 1.1- load required libraries
library(pcr)
library(qPCRtools)
library(tidyverse)
library(ggplot2)
library(cowplot)

# 1.2- Mode delta_delta_ct

# 1.2.1- Using GAPDH as ref_gene and PBMC as ref_group
ct1 <- read.csv("qRTPCR_BM_PB.csv")
ct1 <- ct1[1:48,]

## add grouping variable
# group_var <- rep(c('BM', 'PBMC'), each = 6)
group_var <- ct1$Tissue

ct2 <- ct1[,4:15]

# calculate all values and errors in one step
## mode == 'separate_tube' default
res <- pcr_analyze(ct2,
                   method = 'delta_delta_ct',
                   group_var = group_var,
                   reference_gene = 'GAPDH',
                   reference_group = 'PBMC',
                   plot = TRUE, facet = TRUE)
res

# Value
# A data.frame of 8 columns:
# group: The unique entries in group_var
# gene: The column names of df. reference_gene is dropped
# normalized: The CT value (or the average CT value) of target genes after subtracting that of the reference_gene
# calibrated: The normalized average CT value of target genes after subtracting that of the reference_group
# relative_expression: The expression of target genes normalized by a reference_gene and calibrated by a reference_group
# error: The standard deviation of the relative_expression
# lower: The lower interval of the relative_expression
# upper: The upper interval of the relative_expression

res2 <- pcr_analyze(ct2,
                   method = 'delta_delta_ct',
                   group_var = group_var,
                   reference_gene = 'GAPDH',
                   reference_group = 'PBMC',
                   plot = FALSE)
res2
write.csv(res2, "qRTPCR_BM_vs_PB_relative_expression.csv")


ggplot(res2, aes(x = gene, y = normalized, fill=(group))) +
  geom_col(position = "dodge") +
  labs(x = '', y = 'Normalised mRNA expression') +
  theme_bw(base_size = 12) #+ scale_fill_manual(values = c("grey", "purple"))

ggplot(res2, aes(x = gene, y = relative_expression, fill=(group))) +
  geom_col(position = "dodge") +
  labs(x = '', y = 'Relative mRNA expression') +
  geom_errorbar(aes(x = gene, ymin = lower, ymax = upper) , width=.2,
                position=position_dodge(.9)) +
  theme_bw(base_size = 12) #+ scale_fill_manual(values = c("grey", "purple"))


# 1.3- Mode delta ct method to calculate fold change
res3 <- pcr_analyze(ct2,
            group_var = group_var,
            reference_group = 'PBMC',
            method = 'delta_ct')

ggplot(res3, aes(x = gene, y = fold_change, fill=(group))) +
  geom_col(position = "dodge") +
  labs(x = '', y = 'fold change') +
  geom_errorbar(aes(x = gene, ymin = lower, ymax = upper) , width=.2,
                position=position_dodge(.9)) +
  theme_bw(base_size = 12) #+ scale_fill_manual(values = c("grey", "purple"))


# return a plot
pcr_analyze(ct2,
            group_var = group_var,
            reference_group = 'PBMC',
            method = 'delta_ct',
            plot = TRUE)

# 1.4- Comparing PBMC vs blank
ct3 <- read.csv("qRTPCR_PB.csv")

## add grouping variable
# group_var <- rep(c('BM', 'PBMC'), each = 6)
group_var <- ct3$Tissue

ct3 <- ct3[,4:15]

# calculate all values and errors in one step
## mode == 'separate_tube' default
res4 <- pcr_analyze(ct3,
                   method = 'delta_delta_ct',
                   group_var = group_var,
                   reference_gene = 'GAPDH',
                   reference_group = 'Blank',
                   plot = TRUE)
res4

res5 <- pcr_analyze(ct2,
                    method = 'delta_delta_ct',
                    group_var = group_var,
                    reference_gene = 'GAPDH',
                    reference_group = 'Blank',
                    plot = FALSE)
res5

# applying the delta ct method to calculate fold change
pcr_analyze(ct3,
            group_var = group_var,
            reference_group = 'Blank',
            method = 'delta_ct')

# return a plot
pcr_analyze(ct3,
            group_var = group_var,
            reference_group = 'Blank',
            method = 'delta_ct',
            plot = TRUE)

# 1.5- Comparing BM vs blank
ct4 <- read.csv("qRTPCR_BM.csv")

## add grouping variable
# group_var <- rep(c('BM', 'PBMC'), each = 6)
group_var <- ct4$Tissue

ct4 <- ct4[,4:15]

# calculate all values and errors in one step
## mode == 'separate_tube' default
res6 <- pcr_analyze(ct4,
                    method = 'delta_delta_ct',
                    group_var = group_var,
                    reference_gene = 'GAPDH',
                    reference_group = 'Blank',
                    plot = TRUE)
res6

res7 <- pcr_analyze(ct4,
                    method = 'delta_delta_ct',
                    group_var = group_var,
                    reference_gene = 'GAPDH',
                    reference_group = 'Blank',
                    plot = FALSE)
res7

# applying the delta ct method to calculate fold change
pcr_analyze(ct4,
            group_var = group_var,
            reference_group = 'Blank',
            method = 'delta_ct')

# return a plot
pcr_analyze(ct4,
            group_var = group_var,
            reference_group = 'Blank',
            method = 'delta_ct',
            plot = TRUE)

#################################################################################################################################################################################################################

# 2- qRTPCR analysis with the qPCRtools package

# Reference: https://www.web4xiang.top/post/r-package-qpcrtools-examples/

# 2.1- load required libraries
library(pcr)
library(qPCRtools)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(qPCRtools)

# preprocess dataframe
ct1 <- read.csv("qRTPCR_BM_PB.csv")
library(reshape)
ct2 <- melt(ct1)
write.csv(ct2, "qRTPCR_BM_vs_PB_melt.csv")

# df1.path = system.file("examples", "ddct.cq.txt", package = "qPCRtools")
# df2.path = system.file("examples", "ddct.design.txt", package = "qPCRtools")

cq.table = read.csv("cq.table.csv")
design.table = read.csv("design.table.csv")

CalExp2ddCt(cq.table,
            design.table,
            ref.gene = "GAPDH",
            ref.group = "PBMC",
            stat.method = "wilcox.test",
            fig.type = "bar",
            fig.ncol = NULL) -> res

res[["table"]] %>% 
  kableExtra::kable(format = "html") %>% 
  kableExtra::kable_styling("striped")

res[["figure"]]


ortho <- read.csv("qRTPCR_BM_PB.csv")
cluster <- read.csv("FinalSampleSetMay2021.csv")

idx <- match(ortho$SampleID, cluster$nida)
ortho$studyno <- cluster$studyno[ idx ]
ortho$Luminex <- cluster$luminex[ idx ]
ortho$Nanostring_BM <- cluster$nanostring.BM[ idx ]
ortho$Nanostring_PB <- cluster$nanostring.blood[ idx ]

write.csv(ortho, file="qRTPCR_BM_PB.csv")


