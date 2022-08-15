library(here)
require(ggplot2)
require(DescTools)
require(tidyverse)

here::here()

#Get_data
#Load the raw grave good and demographic data:
CW_raw <- read.csv(here("analysis/data/raw_data/grave_goods_raw_count.csv"),
                       header = TRUE,
                       stringsAsFactors = TRUE,
                       sep = "\t",
                       encoding = "UTF-8",
                       dec = ',')

#CW_raw <- CW_raw[-16,] #only used if wanting to remove extreme Marefy grave
names(CW_raw)[4] <- "Diet"

#choose relevant column range:
CW_binary <- CW_raw[, 24:46]
#cut out polish, decoration and unknown sherds columns:
CW_binary <- CW_binary[,-c(6:7, 9:11)]

#Make binary:
CW_binary[is.na(CW_binary)] <- 0
CW_binary[CW_binary > 0] <- 1

CW_binary$TOT <- rowSums(CW_binary)


#Extract the column and rowSums for each object type where it is present to see
#the relationship between each object type and the TOT.
#We also add a column defining the material of each object type in the process:
Animal_TOT <- subset(CW_binary[,c(1, 19)], unworkedAnimal_spec == 1)
Animal_TOT$unworkedAnimal_spec <- paste(colnames(Animal_TOT[1]), " (", as.character(length(Animal_TOT$TOT)), ")", sep = "")
names(Animal_TOT) <- c("Type", "TOT")
Animal_TOT$Material <- "Bone"

BoneTeethBeads_TOT <- subset(CW_binary[,c(2, 19)], boneToothBeads_perf == 1)
BoneTeethBeads_TOT$boneToothBeads_perf <- paste(colnames(BoneTeethBeads_TOT[1]), " (", as.character(length(BoneTeethBeads_TOT$TOT)), ")", sep = "")
names(BoneTeethBeads_TOT) <- c("Type", "TOT")
BoneTeethBeads_TOT$Material <- "Bone"

BoneToolSimple_TOT <- subset(CW_binary[,c(3, 19)], boneToolSimple == 1)
BoneToolSimple_TOT$boneToolSimple <- paste(colnames(BoneToolSimple_TOT[1]), " (", as.character(length(BoneToolSimple_TOT$TOT)), ")", sep = "")
names(BoneToolSimple_TOT) <- c("Type", "TOT")
BoneToolSimple_TOT$Material <- "Bone"

BoneToolComplex_TOT <- subset(CW_binary[,c(4, 19)], BoneTuskAntler_complex == 1)
BoneToolComplex_TOT$BoneTuskAntler_complex <- paste(colnames(BoneToolComplex_TOT[1]), " (", as.character(length(BoneToolComplex_TOT$TOT)), ")", sep = "")
names(BoneToolComplex_TOT) <- c("Type", "TOT")
BoneToolComplex_TOT$Material <- "Bone"

VesselS_TOT <- subset(CW_binary[,c(5, 19)], small.vessel == 1)
VesselS_TOT$small.vessel <- paste(colnames(VesselS_TOT[1]), " (", as.character(length(VesselS_TOT$TOT)), ")", sep = "")
names(VesselS_TOT) <- c("Type", "TOT")
VesselS_TOT$Material <- "Ceramic"

VesselL_TOT <- subset(CW_binary[,c(6, 19)], vesselLarge == 1)
VesselL_TOT$vesselLarge <- paste(colnames(VesselL_TOT[1]), " (", as.character(length(VesselL_TOT$TOT)), ")", sep = "")
names(VesselL_TOT) <- c("Type", "TOT")
VesselL_TOT$Material <- "Ceramic"

Spindle_TOT <- subset(CW_binary[,c(7, 19)], spindleWhorl == 1)
Spindle_TOT$spindleWhorl <- paste(colnames(Spindle_TOT[1]), " (", as.character(length(Spindle_TOT$TOT)), ")", sep = "")
names(Spindle_TOT) <- c("Type", "TOT")
Spindle_TOT$Material <- "Ceramic"

CopperAwlNeedle_TOT <- subset(CW_binary[,c(8, 19)], copperAwlNeedle == 1)
CopperAwlNeedle_TOT$copperAwlNeedle <- paste(colnames(CopperAwlNeedle_TOT[1]), " (", as.character(length(CopperAwlNeedle_TOT$TOT)), ")", sep = "")
names(CopperAwlNeedle_TOT) <- c("Type", "TOT")
CopperAwlNeedle_TOT$Material <- "Metal"

CopperKnifeRazor_TOT <- subset(CW_binary[,c(9, 19)], copperKnifeRazor == 1)
CopperKnifeRazor_TOT$copperKnifeRazor <- paste(colnames(CopperKnifeRazor_TOT[1]), " (", as.character(length(CopperKnifeRazor_TOT$TOT)), ")", sep = "")
names(CopperKnifeRazor_TOT) <- c("Type", "TOT")
CopperKnifeRazor_TOT$Material <- "Metal"

CopperJewel_TOT <- subset(CW_binary[,c(10, 19)], copperJewellery == 1)
CopperJewel_TOT$copperJewellery <- paste(colnames(CopperJewel_TOT[1]), " (", as.character(length(CopperJewel_TOT$TOT)), ")", sep = "")
names(CopperJewel_TOT) <- c("Type", "TOT")
CopperJewel_TOT$Material <- "Metal"

CopperNeck_TOT <- subset(CW_binary[,c(11, 19)], copperNeckRing == 1)
CopperNeck_TOT$copperNeckRing <- paste(colnames(CopperNeck_TOT[1]), " (", as.character(length(CopperNeck_TOT$TOT)), ")", sep = "")
names(CopperNeck_TOT) <- c("Type", "TOT")
CopperNeck_TOT$Material <- "Metal"

GoldHair_TOT <- subset(CW_binary[,c(12, 19)], GoldHairRing == 1)
GoldHair_TOT$GoldHairRing <- paste(colnames(GoldHair_TOT[1]), " (", as.character(length(GoldHair_TOT$TOT)), ")", sep = "")
names(GoldHair_TOT) <- c("Type", "TOT")
GoldHair_TOT$Material <- "Metal"

FlintAxe_TOT <- subset(CW_binary[,c(13, 19)], flintAxe == 1)
FlintAxe_TOT$flintAxe <- paste(colnames(FlintAxe_TOT[1]), " (", as.character(length(FlintAxe_TOT$TOT)), ")", sep = "")
names(FlintAxe_TOT) <- c("Type", "TOT")
FlintAxe_TOT$Material <- "Flint"

FlintBlade_TOT <- subset(CW_binary[,c(14, 19)], flintBladeScraperFlake == 1)
FlintBlade_TOT$flintBladeScraperFlake <- paste(colnames(FlintBlade_TOT[1]), " (", as.character(length(FlintBlade_TOT$TOT)), ")", sep = "")
names(FlintBlade_TOT) <- c("Type", "TOT")
FlintBlade_TOT$Material <- "Flint"

ShellPerf_TOT <- subset(CW_binary[,c(15, 19)], shellPerf == 1)
ShellPerf_TOT$shellPerf <- paste(colnames(ShellPerf_TOT[1]), " (", as.character(length(ShellPerf_TOT$TOT)), ")", sep = "")
names(ShellPerf_TOT) <- c("Type", "TOT")
ShellPerf_TOT$Material <- "Shell"

BattleAxe_TOT <- subset(CW_binary[,c(16, 19)], battleAxe == 1)
BattleAxe_TOT$battleAxe <- paste(colnames(BattleAxe_TOT[1]), " (", as.character(length(BattleAxe_TOT$TOT)), ")", sep = "")
names(BattleAxe_TOT) <- c("Type", "TOT")
BattleAxe_TOT$Material <- "Stone"

StoneAxe_TOT <- subset(CW_binary[,c(17, 19)], stoneAxe == 1)
StoneAxe_TOT$stoneAxe <- paste(colnames(StoneAxe_TOT[1]), " (", as.character(length(StoneAxe_TOT$TOT)), ")", sep = "")
names(StoneAxe_TOT) <- c("Type", "TOT")
StoneAxe_TOT$Material <- "Stone"

?colnames
UnworkedStone_TOT <- subset(CW_binary[,c(18, 19)], unworkedStoneMineral == 1)
UnworkedStone_TOT$unworkedStoneMineral <- paste(colnames(UnworkedStone_TOT[1]), " (", as.character(length(UnworkedStone_TOT$TOT)), ")", sep = "")
names(UnworkedStone_TOT) <- c("Type", "TOT")
UnworkedStone_TOT$Material <- "Stone"

#Combine them all into one (long) dataframe:
ObjectType_TOT_cor <- rbind(Animal_TOT,
                            BoneTeethBeads_TOT,
                            BoneToolSimple_TOT,
                            BoneToolComplex_TOT,
                            VesselS_TOT,
                            VesselL_TOT,
                            Spindle_TOT,
                            CopperAwlNeedle_TOT,
                            CopperKnifeRazor_TOT,
                            CopperJewel_TOT,
                            CopperNeck_TOT,
                            GoldHair_TOT,
                            FlintAxe_TOT,
                            FlintBlade_TOT,
                            ShellPerf_TOT,
                            BattleAxe_TOT,
                            StoneAxe_TOT,
                            UnworkedStone_TOT)

#Make the categorical variables (type and material) factor to use in the plot:
ObjectType_TOT_cor$Type <- as.factor(ObjectType_TOT_cor$Type)
ObjectType_TOT_cor$Material <- as.factor(ObjectType_TOT_cor$Material)

#I used this to see the total count of each object type added for each type in the dataframe. These were added to the x-axis titles:
summary(ObjectType_TOT_cor$Type)


#Now we can make a boxplot of each type vs. TOT count:
TOT_prestige_plot <- ggplot(ObjectType_TOT_cor, aes(x=reorder(Type, TOT), y=TOT, fill = Material))+
  geom_boxplot(outlier.shape = 21, outlier.size = 2)+
  geom_jitter(width = 0.1, height = 0.1, size = 0.1, alpha = 0.5)+
  theme(axis.text.x = element_text (angle = 90), axis.text.y = element_text (size = 8), axis.title = element_text(size = 9), plot.title = element_text(size=10))+
  labs(title = "Association of each grave good type with total object types", x ="Grave good type (type total)", y = "Total object types")+
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), minor_breaks = NULL)

