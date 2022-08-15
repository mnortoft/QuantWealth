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
source(here('analysis/scripts/age_groups.R'), echo = FALSE) # to grab simplified age groups
source(here('analysis/scripts/animals.R'), echo = FALSE)

# extract normalized grave good value measures, graveDepth and TOT from previous scripts
mesh_PCA <- allGravesTotals5
pre_TOT <- CW_raw[, c(1, 16, 24:28, 31, 35:46)]
Depth <- CW_raw[,c(1,9)]
pre_TOT[is.na(pre_TOT)] <- 0 # makes warning because SexGender (factor) column has NAs. doesn't matter
pre_TOT[,3:20][pre_TOT[,3:20] > 0] <- 1
pre_TOT$TOT <- rowSums(pre_TOT[3:20])  #should be changed if dataset has different number of categories
pre_TOT_cut <- pre_TOT[,c(1,2,21)]

#materials
material_PCA_ready <- material_all[,-2]
names(material_PCA_ready) <- c("Grave_ID", "meat", "bone", "stone", "flint", "metal", "shell", "ceramic")

#merge all measures (adding TOT and Grave Depth) into one dataframe:
mesh_PCA2 <- merge(x = pre_TOT_cut, y = mesh_PCA, by = "Grave_ID", all = TRUE)
mesh_PCA3 <- merge(x = mesh_PCA2, y = Depth, by = "Grave_ID", all = TRUE)
#fix names
names(mesh_PCA3) <- c("Grave_ID", "SexGender", "TOT", "PH", "prestige", "scarcity", "travel", "skill", "meat", "graveDepth")

mesh_PCA4 <- merge(x = mesh_PCA3, y = material_PCA_ready[,-2], by = "Grave_ID", all = TRUE)
age_group <- age_Group_freq[,c("Grave_ID", "ageGroup_simple")]
mesh_PCA5 <- merge(x = mesh_PCA4, y = age_group, by="Grave_ID")
names(mesh_PCA5)[names(mesh_PCA5)=="ageGroup_simple"] <- c("ageGroup")

animals_cut <- animals[,-c(6,10,11,12)] #remove columns: hare, fox, unworked teeth, and tooth species
#animals_cut[animals_cut > 1] <- 1

meat_PCA_ready_raw <- merge(mesh_PCA5, animals_cut, by="Grave_ID", all = TRUE)
meat_PCA_ready <- meat_PCA_ready_raw[-c(14,16),-c(1,2,3,10:16)]
meat_PCA_ready[is.na(meat_PCA_ready)] <- 0
meat_PCA_ready$ageGroup[meat_PCA_ready$ageGroup == "0"] <- NA #fix NA agegroups with zero back to NA

meat.pca <- PCA(meat_PCA_ready, quanti.sup = c(8:14), quali.sup = c(7), graph = FALSE)
meat_PCA_report <- dimdesc(meat.pca, axes = c(1,2,3), proba = 0.2)

#remove outliers
mesh_PCA_no_out <- mesh_PCA5[-c(14,16),c(2:17)]
mesh_no_missing <- dplyr::filter(mesh_PCA_no_out[rowSums(mesh_PCA_no_out[,c(2:15)], na.rm = TRUE) >1,])
#mesh_PCA_no_depth <- mesh_PCA_no_out[,-8]
Total <- data.frame(Total_all = rowSums(mesh_no_missing[,-c(1,16)], na.rm = TRUE))

nb <- estim_ncp(mesh_PCA_no_out[c(3:8)], scale = TRUE)
imp_data <- imputePCA(mesh_PCA_no_out, quanti.sup = c(9:14), quali.sup = (c(1,16)), ncp = nb$ncp, scale = FALSE)
imp_precision <- MIPCA(mesh_PCA_no_out[,-c(1,16)], scale = TRUE, ncp = nb$ncp)

imp_data$completeObs$ageGroup[is.na(imp_data$completeObs$ageGroup)] <- "unknown"
imp_data$completeObs$ageGroup <- as.factor(imp_data$completeObs$ageGroup)
#PCA of wealth and value measures (imputed grave depth), and material groups:
grave.pca <- PCA(imp_data$completeObs, ncp = 5, quanti.sup = c(9:15), quali.sup = c(1,16), graph = FALSE)
#plot(grave.pca, choix = "ind", invisible = "quanti.sup") #hiding quantitative supplementary variables
summary(grave.pca)

#contribution of variables on each PC
scree1 <- fviz_contrib(grave.pca, choice = "var", axes = 1, top = 10)+
  theme(plot.title = element_text(size=5))+
  theme(axis.title = element_text(size=7))+
  theme(axis.text = element_text(size=6))#PC1
scree2 <- fviz_contrib(grave.pca, choice = "var", axes = 2, top = 10)+
  theme(plot.title = element_text(size=5))+
  theme(axis.title = element_text(size=7))+
  theme(axis.text = element_text(size=6)) #PC2
scree3 <- fviz_contrib(grave.pca, choice = "var", axes = 3, top = 10)+
  theme(plot.title = element_text(size=5))+
  theme(axis.title = element_text(size=7))+
  theme(axis.text = element_text(size=6)) #PC3

scree_all <- fviz_screeplot(grave.pca, ncp=10)+
  theme(axis.title = element_text(size=8))+
  ylab("% of explained variances")

#significance of variables on PC1 and PC2 respectively
grave.desc <- dimdesc(grave.pca, axes = c(1,2,3), proba = 0.2)
grave.desc$Dim.1$category # PC1: all positive significant correlation
grave.desc$Dim.2$quanti[1,2] # PC2: positive: meat (highly significant), and then flint and TOT,
grave.desc$Dim.2$category
grave.desc$Dim.3
#negative: travel, metal, and grave depth significant
#scarcity and PH not significant on PC2

#library(cluster)
#library(fpc)

#imp_complete <- data.frame(imp_data$completeObs)
#mesh_scaled <- data.frame(scale(imp_complete[1:8]))


#kmeans <- kmeans(grave.pca$ind$coord, 2)
#require(clustertend)
#set.seed(123)
#hopkins(imp_complete[1:8], n = nrow(imp_complete[-1, c(1:8)]))

require(ggsci) # for scientific journal colour palettes

#ind.km <- kmeans(grave.pca$ind$coord[,1:3], centers = 3, nstart = 25)
#ind.grp <- as.factor(ind.km$cluster)

#to make counts per group as proportion table:
#grp.df <- data.frame(ind.grp)
#df.prop <- as.data.frame(summary(grp.df))
#tab <- table(ind.grp)

#prop_table <- as.data.frame(prop.table(tab))

# names(prop_table) <- c("Group_name", "Proportion")
# prop_table$percent <- prop_table$Proportion*100
# group1 <- prop_table$percent[prop_table$Group_name == '1']
# group2 <- prop_table$percent[prop_table$Group_name == '2']
# group3 <- prop_table$percent[prop_table$Group_name == '3']

#plot meat pca
PCA_paper_meat <- fviz_pca_biplot(meat.pca,
                                 # Individuals
                                 geom.ind = "point",
                                 fill.ind = meat_PCA_ready$ageGroup, col.ind = "black",
                                 pointshape = 21, pointsize = 2, #shape 21 combine well with group colours
                                 palette = "viridis",
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

#PCA biplot used in main paper
PCA_paper_sex <- fviz_pca_biplot(grave.pca,
                # Individuals
                geom.ind = "point",
                fill.ind = mesh_PCA_no_out$SexGender, col.ind = "black",
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

#PCA biplot used in main paper
PCA_paper_age <- fviz_pca_biplot(grave.pca,
                                 # Individuals
                                 geom.ind = "point",
                                 fill.ind = mesh_PCA_no_out$ageGroup, col.ind = "black",
                                 pointshape = 21, pointsize = 2, #shape 21 combine well with group colours
                                 palette = "viridis",
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


PCA_paper_2_3 <- fviz_pca_biplot(grave.pca,
                                 # Individuals
                                 axes = c(2,3), #choosing Dims 2 and 3
                                 geom.ind = "point",
                                 fill.ind = mesh_PCA_no_out$ageGroup, col.ind = "black",
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

#check significance of the 3 groups by putting them as quali.sup in the PCA instead of SexGender
#PCA_group <- data.frame(Group = ind.grp, imp_data$completeObs[,-1])
#grave.pca.group <- PCA(PCA_group, ncp = 5, quanti.sup = c(9:14), quali.sup = 1, graph = FALSE)
#summary(grave.pca.group) #Group significance in quali v-test here
#dimdesc(grave.pca.group, proba = 0.5)
#PCA without ambiguous sex
PCA_MF_ready <- filter(imp_data$completeObs, SexGender != "ambiguous")
grave.pcaMF <- PCA(PCA_MF_ready, ncp = 5, quanti.sup = c(9:14), quali.sup = c(1, 16), graph = FALSE)
#plot(grave.pcaMF, choix = "ind", habillage = "SexGender")
summary(grave.pcaMF) #Group significance in quali v-test here
dimdesc(grave.pcaMF, proba = 0.5)

#extract PCs and merge with sex to do non-parametric (wilcox) test on sex for each dimension
Sex_PCs <- data.frame(grave.pcaMF$ind$coord)
Sex_PCs_sex <- data.frame(SexGender = PCA_MF_ready[,1], Sex_PCs)
PC_male <- filter(Sex_PCs_sex, SexGender == "male")
PC_female <- filter(Sex_PCs_sex, SexGender == "female")
wilcox.test(PC_female[,3], PC_male[,3])

PCA_paperMF <- fviz_pca_biplot(grave.pcaMF,
                             # Individuals
                             geom.ind = "point",
                             fill.ind = PCA_MF_ready$SexGender, col.ind = "black",
                             pointshape = 21, pointsize = 2, #shape 21 combine well with group colours
                             palette = "aaas",
                             #ellipses
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
                             legend.title = list(fill = "SexGender",
                                                 color = "Contribution",
                                                 alpha = "Contrib"),
                             repel = TRUE)

#In case wanting a 3d plot
# library(rgl)
# plot3d(grave.pca$ind$coord[,1:3], type = "l")



