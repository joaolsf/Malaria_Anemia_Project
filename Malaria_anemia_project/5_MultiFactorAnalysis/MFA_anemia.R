#Multiple Factor Analysis 
#Load the packaages
library("FactoMineR")
library("factoextra")
library("missMDA")
library(RColorBrewer)
library(ggrepel)
library(viridis)
library(ggpubr)
options(ggrepel.max.overlaps = 100)

# 1- Missing data imputation

#perform PCA on the imputed data set. To this end, we propose to use the PCA function of the FactoMineR package:
multiplex <- read.csv("Clinical_luminex_BMqRTPCR.csv")

# data frame para cluster
df_to_cluster <- multiplex[,-1]
rownames(df_to_cluster) <- multiplex$PatientID

#subsetting active variables for the PCA
df_to_cluster.active <- df_to_cluster[,2:96]

# Prepare data
mydata <- (df_to_cluster.active)

#Perform the (regularized) iterative PCA algorithm with the number of dimensions selected in the previous step, using the function imputePCA:
res.imp <- imputePCA(mydata, ncp = 8)

mydata_complete <- res.imp$completeObs
write.csv(mydata_complete, "Clinical_luminex_BMqRTPCR_complete.csv")

#####################################################################################################################################################################################################################################################################

mydata <- read.csv("Clinical_luminex_BMqRTPCR_complete.csv")
rownames(mydata) <- mydata$PatientID
mydata <- mydata[,-1]

# Prepare data
mydata_clinical <- mydata[,1:36]
mydata_parasite <- mydata[,37:44]
mydata_luminex <- mydata[,45:78]
mydata_qRTPCR <- mydata[,79:89]
mydata_sup <- mydata[,90]

mydata[,90] <- as.factor(mydata[,90])

mydata2 <- MFA(mydata, group = c(36, 8, 34, 11, 1), #clinical, luminex, BM qRTPCR, categorical
               type = c("s", "s", "s", "s", "n"),
               name.group = c("Clinical", "Parasite","PB_biomarker","BM_biomarker",
                              "Clinical_group"),
               num.group.sup = c(5),
               graph = FALSE)
print(mydata2)

# Visualization and interpretation
# We’ll use the factoextra R package to help in the interpretation and the visualization of the multiple factor analysis.
# The functions below [in factoextra package] will be used:
# get_eigenvalue(res.mfa): Extract the eigenvalues/variances retained by each dimension (axis).
# fviz_eig(res.mfa): Visualize the eigenvalues/variances.
# get_mfa_ind(res.mfa): Extract the results for individuals.
# get_mfa_var(res.mfa): Extract the results for quantitative and qualitative variables, as well as, for groups of variables.
# fviz_mfa_ind(res.mfa), fviz_mfa_var(res.mfa): Visualize the results for individuals and variables, respectively.

# 1- Eigenvalues / Variances
# The proportion of variances retained by the different dimensions (axes) can be extracted using the function get_eigenvalue() [factoextra package] as follow:
eig.val <- get_eigenvalue(mydata2)
head(eig.val)
# The function fviz_eig() or fviz_screeplot() [factoextra package] can be used to draw the scree plot:
fviz_screeplot(mydata2)

# 2- Graph of variables
# 2.1- Groups of variables
# The function get_mfa_var() [in factoextra] is used to extract the results for groups of variables.
# This function returns a list containing the coordinates, the cos2 and the contribution of groups.
group <- get_mfa_var(mydata2, "group")
group

# The different components can be accessed as follow: (check the PCA R files with this package to see how to plot the parameters below using corrplot)
# Coordinates of groups
head(group$coord)
# Cos2: quality of representation on the factore map
head(group$cos2)
# Contributions to the  dimensions
head(group$contrib)

# To plot the groups of variables, type this:
# red color = active groups of variables
# green color = supplementary groups of variables
fviz_mfa_var(mydata2, "group")

# To draw a bar plot of groups contribution to the dimensions, use the function fviz_contrib():
# Contribution to the first dimension (check the PCA R files with this package to see how to edit this plot to include more parameters)
fviz_contrib(mydata2, "group", axes = 1)
# Contribution to the second dimension
fviz_contrib(mydata2, "group", axes = 2)

# 2.2- Quantitative variables
#The function get_mfa_var() [in factoextra] is used to extract the results for quantitative variables. 
#This function returns a list containing the coordinates, the cos2 and the contribution of variables:
quanti.var <- get_mfa_var(mydata2, "quanti.var")
quanti.var 

# The different components can be accessed as follow:
# Coordinates
head(quanti.var$coord)
# Cos2: quality on the factor map
head(quanti.var$cos2)
# Contributions to the dimensions
head(quanti.var$contrib)

#In this section, we’ll describe how to visualize quantitative variables colored by groups.
#Next, we’ll highlight variables according to either:
#i) their quality of representation on the factor map or 
#ii) their contributions to the dimensions.

# Correlation between quantitative variables and dimensions. The R code below plots quantitative variables colored by groups. 
# The argument palette is used to change group colors (see ?ggpubr::ggpar for more information about palette). 
# Supplementary quantitative variables are in dashed arrow and violet color. We use repel = TRUE, to avoid text overlapping.
fviz_mfa_var(mydata2, "quanti.var", 
             palette = "jco", 
             repel = TRUE, 
             select.var = list(contrib = 60))

# To make the plot more readable, we can use geom = c(“point”, “text”) instead of geom = c(“arrow”, “text”). 
# We’ll change also the legend position from “right” to “bottom”, using the argument legend = “bottom”:
fviz_mfa_var(mydata2, 
             "quanti.var", 
             palette = c("black", "#21908CFF", "#800080"), 
             repel = TRUE, 
             select.var = list(cos2 = 60),
             geom = c("point", "text"),
             shape.var=19,
             legend = "bottom")

"#440154FF"


fviz_mfa_var(mydata2, 
             "quanti.var", 
             palette = c("#0073C2FF", "#EFB400", "#3B3B3BFF", "#CD534CFF"), 
             repel = TRUE,
             select.var = list(contrib = 95),
             geom = c("point", "text"),
             shape.var= 19, labelsize = 3,
             legend = "bottom")  + theme(legend.justification=c(),
                                         legend.position='bottom', legend.text = element_text(size = 12), text = element_text(size = 5),
                                      axis.title = element_text(size = 12),
                                      axis.text = element_text(size = 12)) 
             
# Color options:
#800080
#440154FF
#F79C79

# The contribution of quantitative variables (in %) to the definition of the dimensions can be visualized using the function fviz_contrib() [factoextra package]. Variables are colored by groups. The R code below shows the top 20 variable categories contributing to the dimensions:
# Contributions to dimension 1
fviz_contrib(mydata2, choice = "quanti.var", axes = 1, top = 60, xtickslab.rt = 90,
             palette = c("#0073C2FF", "#EFB400", "#3B3B3BFF", "#CD534CFF"), ggtheme = theme_light())
# Contributions to dimension 2
fviz_contrib(mydata2, choice = "quanti.var", axes = 2, top = 60,xtickslab.rt = 90,
             palette = c("#0073C2FF", "#EFB400", "#3B3B3BFF", "#CD534CFF"), ggtheme = theme_light())

# The most contributing quantitative variables can be highlighted on the scatter plot using the argument col.var = “contrib”. 
# This produces a gradient colors, which can be customized using the argument gradient.cols.
fviz_mfa_var(mydata2, "quanti.var", col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)
# Similarly, you can highlight quantitative variables using their cos2 values representing the quality of representation on the factor map. 
# If a variable is well represented by two dimensions, the sum of the cos2 is closed to one. 
# For some of the row items, more than 2 dimensions might be required to perfectly represent the data.
# Color by cos2 values: quality on the factor map
fviz_mfa_var(mydata2, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             col.var.sup = "violet", repel = TRUE)
# To create a bar plot of variables cos2, type this:
fviz_cos2(mydata2, choice = "quanti.var", axes = 1)

#2.3- The function dimdesc() [in FactoMineR], for dimension description, can be used to identify the most significantly associated variables with a given principal component 
res.desc <- dimdesc(mydata2, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
# Description of dimension 2
res.desc$Dim.2

write.csv(res.desc$Dim.1, "MFA1_correlation.csv")
write.csv(res.desc$Dim.2, "MFA2_correlation.csv")

#plot factor loadings for PC1
weights <- read.csv("MFA1_correlation.csv")
plot_ <- ggplot(weights,
                aes(x= reorder(Feature,
                               (Correlation), decreasing = TRUE), y = (Correlation), fill = Category)) +
  geom_bar(stat = "identity", width=0.7, position=position_dodge(width=1)) +
  theme_light(base_size = 12) +
  xlab("") + 
  ylab("Dim1 Loadings") + scale_fill_manual("legend", values = c("BM_biomarker" = '#0073C2FF', "Clinical" = "#E7B800", "Parasite" = "#3B3B3BFF", "PB_biomarker" = "#CD534CFF")) +
  theme(plot.title = element_text(size=16), 
        axis.text.y = element_text(face = "plain", size=12, angle=90, vjust=.5, hjust=1),
        axis.text.x = element_text(face = "plain", size=9, angle=90, vjust=.5, hjust=1))
plot_
#  coord_flip() +
# #800080" = purple
# #440154FF" = deep purple
# #F79C79 = orange
# #21908CFF" = green

#plot factor loadings for PC2
weights2 <- read.csv("MFA2_correlation.csv")
plot_ <- ggplot(weights2,
                aes(x= reorder(Feature,
                               (Correlation), decreasing = TRUE), y = (Correlation), fill = Category)) +
  geom_bar(stat = "identity", width=0.7, position=position_dodge(width=1)) +
  theme_classic(base_size = 12) +
  xlab("") + 
  ylab("Dim2 Loadings") + scale_fill_manual("legend", values = c("BM_biomarker" = '#0073C2FF', "Clinical" = "#E7B800", "Parasite" = "#3B3B3BFF", "PB_biomarker" = "#CD534CFF")) +
  theme(plot.title = element_text(size=16), 
        axis.text.y = element_text(face = "plain", size=12, angle=90, vjust=.5, hjust=1),
        axis.text.x = element_text(face = "plain", size=9, angle=90, vjust=.5, hjust=1))
plot_
#  coord_flip() +
# #800080" = purple
# #440154FF" = deep purple
# #F79C79 = orange
# #21908CFF" = green


#3- Graph of individuals
# To get the results for individuals, type this:
ind <- get_mfa_ind(mydata2)
ind
#To plot individuals, use the function fviz_mfa_ind() [in factoextra].
#By default, individuals are colored in blue. 
#However, like variables, it’s also possible to color individuals by their cos2 values:
fviz_mfa_ind(mydata2, col.ind = "contrib", 
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
               repel = TRUE)
# In the plot above, the supplementary qualitative variable categories are shown in black. Env1, Env2, Env3 are the categories of the soil. Saumur, Bourgueuil and Chinon are the categories of the wine Label. 
# If you don’t want to show them on the plot, use the argument invisible = “quali.var”

# Note that, it’s possible to color the individuals using any of the qualitative variables in the initial data table. 
# To do this, the argument habillage is used in the fviz_mfa_ind() function. 
# For example, if you want to color the wines according to the supplementary qualitative variable “Label”, type this:
p <- fviz_mfa_ind(mydata2,
             geom.ind = c("point"),
             repel = TRUE, 
             shape.ind = 19,
             pointsize = 5,
             habillage = "Clinical_group",
             fill.ind = "Clinical_group",
             palette = c("#0073C2FF", "salmon"),
             col.ind = "white", 
             label = "var",
             addEllipses = TRUE, ellipse.type = "confidence", invisible="quali") + theme_bw(base_size = 16)

p + geom_point(aes(shape = mydata$Clinical_group), size=5) + scale_shape_manual(values=c(21, 22))

