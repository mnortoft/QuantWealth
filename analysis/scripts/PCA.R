library(here) # for knitr to print out all used packages at the end of the RMarkdown
library(dplyr) # for data manipulation
library(plotly)
require(psych)
require(FactoMineR)
require(FactoInvestigate)
require(factoextra)
require(missMDA)

here("analysis")

source(here('analysis/scripts/Gini_calc.R'), echo = FALSE) #to grab allGravesTotals5 and CW_raw
source(here('analysis/scripts/material_total.R'), echo = FALSE) # to grab material_total

# extract normalized grave good value measures, graveDepth and TOT from previous scripts
mesh_PCA <- allGravesTotals5
pre_TOT <- CW_raw[, c(1, 24:28, 31, 35:46)]
Depth <- CW_raw[,c(1,9)]
pre_TOT[is.na(pre_TOT)] <- 0
pre_TOT[,2:19][pre_TOT[,2:19] > 0] <- 1
pre_TOT$TOT <- rowSums(pre_TOT[2:19])
pre_TOT_cut <- pre_TOT[,c(1,20)]

#materials
material_PCA_ready <- material_all[,-2]
names(material_PCA_ready) <- c("Grave_ID", "meat", "bone", "stone", "flint", "metal", "shell", "ceramic")


#merge all measures into one dataframe:
mesh_PCA2 <- merge(x = pre_TOT_cut, y = mesh_PCA, by = "Grave_ID", all = TRUE)
mesh_PCA3 <- merge(x = mesh_PCA2, y = Depth, by = "Grave_ID", all = TRUE)
#fix names
names(mesh_PCA3) <- c("Grave_ID", "TOT", "PH", "prestige", "scarcity", "travel", "skill", "meat", "graveDepth")

mesh_PCA4 <- merge(x = mesh_PCA3, y = material_PCA_ready[,-2], by = "Grave_ID", all = TRUE)

#remove outliers
mesh_PCA_no_out <- mesh_PCA4[-c(14,16),c(2:15)]
mesh_no_missing <- dplyr::filter(mesh_PCA_no_out[rowSums(mesh_PCA_no_out, na.rm = TRUE) >1,])

Total <- data.frame(rowSums(mesh_no_missing, na.rm = TRUE))
nb <- estim_ncp(mesh_no_missing[c(2:7, 9:14)], scale = TRUE)
imp_data <- imputePCA(mesh_PCA_no_out, ncp = nb$ncp, scale = TRUE)
imp_precision <- MIPCA(mesh_PCA_no_out, scale = TRUE, ncp = nb$ncp)

#PCA of wealth and value measures (imputed grave depth), and material groups:
grave.pca <- PCA(imp_data$completeObs, ncp = 5, quanti.sup = c(9:14), graph = FALSE)

#To check if sexGender and flint axes and blades are important factors for any group, seems not
# sex <- merge(x= mesh_PCA4, y=CW_raw[,c(1,16,41,42)], by= "Grave_ID")
# grave_sex.pca <- PCA(sex[-c(14,16),-1], ncp = 5, quanti.sup = c(9:14,16,17), quali.sup = 15)
# plot(grave_sex.pca, cex=0.8, choix="ind", habillage="SexGender", axes = 1:2)

#contribution of variables on each PC
scree1 <- fviz_contrib(grave.pca, choice = "var", axes = 1, top = 10)+
  theme(plot.title = element_text(size=8))+
  theme(axis.title.y = element_text(size=9))#PC1
scree2 <- fviz_contrib(grave.pca, choice = "var", axes = 2, top = 10)+
  theme(plot.title = element_text(size=8))+
  theme(axis.title.y = element_text(size=9))#PC2
scree3 <- fviz_contrib(grave.pca, choice = "var", axes = 3, top = 10)+
  theme(plot.title = element_text(size=8))+
  theme(axis.title.y = element_text(size=9)) #PC3

scree_all <- fviz_screeplot(grave.pca, ncp=10)+
  theme(axis.title.y = element_text(size=9))

#significance of variables on PC1 and PC2 respectively
grave.desc <- dimdesc(grave.pca, axes = c(1,2,3), proba = 0.05)
grave.desc$Dim.1 # PC1: all positive significant correlation
grave.desc$Dim.2 # PC2: positive: meat (highly significant), and then flint and TOT,
grave.desc$Dim.3
#negative: travel, metal, and grave depth significant
#scarcity and PH not significant on PC2

library(cluster)
library(fpc)
imp_complete <- data.frame(imp_data$completeObs)
mesh_scaled <- data.frame(scale(imp_complete[1:8]))


kmeans <- kmeans(grave.pca$ind$coord, 2)
require(clustertend)
set.seed(123)
hopkins(imp_complete[1:8], n = nrow(imp_complete[-1, c(1:8)]))

#draw random dataset from imp_complete and scale it for comparison:
random_df <- apply(imp_complete[1:8], 2,
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)
random_df <- scale(random_df)
# Standardize the data sets

#fviz_pca_ind(prcomp(random_df), title = "PCA - Random data",
#             geom = "point", ggtheme = theme_classic())

# km.res2 <- kmeans(random_df, 2)
# fviz_cluster(list(data = random_df, cluster = km.res2$cluster),
#              ellipse.type = "norm", ellipse.level = 0.95, geom = "point", stand = FALSE,
#              palette = "jco", ggtheme = theme_classic())

set.seed(123)
hopkins(random_df, n = nrow(random_df)-1)

# fviz_dist(dist(grave.pca$ind$coord), show_labels = FALSE)+
#   labs(title = "Iris data")
#
# fviz_dist(dist(random_df), show_labels = FALSE)+
#   labs(title = "Random data")

#dd <- cbind(mesh_scaled, cluster = pam$pamobject)

# fviz_cluster(kmeans,
#              data = mesh_scaled,
#              palette = "aaas", # color palette
#              ellipse.type = "norm", # Concentration ellipse,
#              ellipse.level = 0.95,
#              ellipse.alpha = 0.2,
#              repel = TRUE) # Avoid label overplotting (slow)

require(NbClust)
# nb <- NbClust(grave.pca$ind$coord[,1:3], distance = "manhattan", min.nc = 2,
#               max.nc = 10, method = "ward.D2")


#fviz_nbclust(nb)
require(ggsci) # for scientific journal colour palettes

ind.km <- kmeans(grave.pca$ind$coord[,1:3], centers = 3, nstart = 25)
ind.grp <- as.factor(ind.km$cluster)

#to make counts per group as proportion table:
grp.df <- data.frame(ind.grp)
df.prop <- as.data.frame(summary(grp.df))
tab <- table(ind.grp)

prop_table <- as.data.frame(prop.table(tab))

names(prop_table) <- c("Group_name", "Proportion")
prop_table$percent <- prop_table$Proportion*100
group1 <- prop_table$percent[prop_table$Group_name == '1']
group2 <- prop_table$percent[prop_table$Group_name == '2']
group3 <- prop_table$percent[prop_table$Group_name == '3']

#PCA biplot used in main paper
PCA_paper <- fviz_pca_biplot(grave.pca,
                # Individuals
                geom.ind = "point",
                fill.ind = ind.grp, col.ind = "black",
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

#clustering on PCs 2 and 3
ind.km_2_3 <- kmeans(grave.pca$ind$coord[,2:3], centers = 2, nstart = 25)
ind.grp_2_3 <- as.factor(ind.km_2_3$cluster)

#PCA biplot of PCs 2 and 3 used in SI:
PCA_paper_2_3 <- fviz_pca_biplot(grave.pca, axes = c(2,3),
                                 # Individuals
                                 geom.ind = "point",
                                 fill.ind = ind.grp_2_3, col.ind = "black",
                                 pointshape = 21, pointsize = 2, #shape 21 combine well with group colours
                                 palette = c("blue", "green", "red"),
                                 addEllipses = TRUE,
                                 mean.point = FALSE,
                                 ellipse.type = "convex",
                                 ellipse.level = 0.95,
                                 ellipse.alpha = 0.1,
                                 show.clust.cent = FALSE,
                                 # Variables
                                 col.var = "contrib",
                                 col.quanti.sup = "grey",
                                 gradient.cols = "Blues",
                                 legend.title = list(fill = "Group",
                                                     color = "Contribution",
                                                     alpha = "Contrib"),
                                 repel = TRUE)

#In case wanting a 3d plot
library(rgl)
#plot3d(grave.pca$ind$coord[,1:3], col=ind.km_2_3$cluster)


