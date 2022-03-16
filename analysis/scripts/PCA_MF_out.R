library(here) # for knitr to print out all used packages at the end of the RMarkdown
library(dplyr) # for data manipulation
library(plotly)
require(psych)
require(FactoMineR)
require(FactoInvestigate)
require(factoextra)
require(missMDA)

here("analysis")

source(here('analysis/scripts/PCA.R'), echo = FALSE) #to grab variables from other PCA script

mesh_PCA4
mesh_PCA4_out <- mesh_PCA4[,c(2:16)]
nb <- estim_ncp(mesh_PCA4_out[c(3:8)], scale = TRUE)
imp_data_out <- imputePCA(mesh_PCA4_out, quanti.sup = c(9:14), quali.sup = (1), ncp = nb$ncp, scale = TRUE)
#imp_precision <- MIPCA(mesh_PCA4_out, scale = TRUE, ncp = nb$ncp)

grave_out.pca <- PCA(imp_data_out$completeObs, ncp = 5, quanti.sup = c(9:14), quali.sup = 1, graph = TRUE)
plot(grave_out.pca, chix = "ind", habillage = "SexGender")
summary(grave.pca)

#screeplots
scree1_out <- fviz_contrib(grave_out.pca, choice = "var", axes = 1, top = 10)+
  theme(plot.title = element_text(size=8))+
  theme(axis.title.y = element_text(size=9))#PC1
scree2_out <- fviz_contrib(grave_out.pca, choice = "var", axes = 2, top = 10)+
  theme(plot.title = element_text(size=8))+
  theme(axis.title.y = element_text(size=9))#PC2
scree3_out <- fviz_contrib(grave_out.pca, choice = "var", axes = 3, top = 10)+
  theme(plot.title = element_text(size=8))+
  theme(axis.title.y = element_text(size=9)) #PC3

scree_all_out <- fviz_screeplot(grave_out.pca, ncp=10)+
  theme(axis.title.y = element_text(size=9))

require(ggsci)

ind.km_out <- kmeans(grave_out.pca$ind$coord[,1:3], centers = 3, nstart = 25)
ind.grp_out <- as.factor(ind.km_out$cluster)

#PCA biplot used in main paper
PCA_paper_out <- fviz_pca_biplot(grave_out.pca,
                             # Individuals
                             geom.ind = "point",
                             fill.ind = ind.grp_out, col.ind = "black",
                             pointshape = 21, pointsize = 2, #shape 21 combine well with group colours
                             palette = "aaas",
                             addEllipses = TRUE,
                             mean.point = FALSE,
                             ellipse.type = "convex",
                             ellipse.level = 0.95,
                             ellipse.alpha = 0.2,
                             show.clust.cent = FALSE,
                             # Variables
                             col.var = "contrib",
                             col.quanti.sup = "grey",
                             gradient.cols = "Blues",
                             legend.title = list(fill = "Group",
                                                 color = "Contribution",
                                                 alpha = "Contrib"),
                             repel = TRUE)
