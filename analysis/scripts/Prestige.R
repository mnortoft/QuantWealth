require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(stringr)

here("analysis/scripts")

#This script extracts the medians of the prestige plot data from the main paper,
#applies the median prestige (from TOT ranges) values and multiplies them with the
#original count data table to form a new Gini coefficient based on prestige

#load the prestige plot data from the main paper:
source(here('analysis/scripts/prestigeObjectsBoxPlots.R'), echo = FALSE)


#Extract the medians from the prestige plot data:
ObjectType_medians <- ObjectType_TOT_cor %>%
  group_by(Type) %>%
  dplyr::summarise(median = median(TOT))

#
TOT_median_animal <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^unworkedAnimal_spec"))
TOT_median_BoneTeethBeads <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^boneToothBeads_perf"))
TOT_median_BoneToolSimple <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^boneToolSimple"))
TOT_median_BoneToolComplex <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^BoneTuskAntler_complex"))
TOT_median_vesselSmall <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^small.vessel"))
TOT_median_vesselLarge <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^vesselLarge"))
TOT_median_spindleWhorl <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^spindleWhorl"))
TOT_median_CopperAwlNeedle <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^copperAwlNeedle"))
TOT_median_CopperKnifeRazor <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^copperKnifeRazor"))
TOT_median_CopperJewel <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^copperJewellery"))
TOT_median_CopperNeckRing <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^copperNeckRing"))
TOT_median_GoldHair <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^GoldHairRing"))
TOT_median_FlintAxe <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^flintAxe"))
TOT_median_FlintBlade <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^flintBladeScraperFlake"))
TOT_median_ShellPerf <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^shellPerf"))
TOT_median_BattleAxe <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^battleAxe"))
TOT_median_StoneAxe <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^stoneAxe"))
TOT_median_otherStone <- ObjectType_medians %>% dplyr::filter(str_detect(Type, "^unworkedStoneMineral"))

# first extract relevant columns (Grave_ID, and all grave good count columns) from
#the raw grave good count table
Prestige_values <- data.frame(Grave_ID = CW_raw[,1], CW_binary[,-19])


#Now we apply the prestige medians to each object type (i.e. each column) to raw data (simple count data).
Prestige_values$unworkedAnimal_spec <- Prestige_values$unworkedAnimal_spec*TOT_median_animal$median
Prestige_values$boneToothBeads_perf <- Prestige_values$boneToothBeads_perf*TOT_median_BoneTeethBeads$median
Prestige_values$boneToolSimple <- Prestige_values$boneToolSimple*TOT_median_BoneToolSimple$median
Prestige_values$BoneTuskAntler_complex <- Prestige_values$BoneTuskAntler_complex*TOT_median_BoneToolComplex$median
Prestige_values$small.vessel <- Prestige_values$small.vessel*TOT_median_vesselSmall$median
Prestige_values$vesselLarge <- Prestige_values$vesselLarge*TOT_median_vesselLarge$median
Prestige_values$spindleWhorl <- Prestige_values$spindleWhorl*TOT_median_spindleWhorl$median
Prestige_values$copperAwlNeedle <- Prestige_values$copperAwlNeedle*TOT_median_CopperAwlNeedle$median
Prestige_values$copperKnifeRazor <- Prestige_values$copperKnifeRazor*TOT_median_CopperKnifeRazor$median
Prestige_values$copperJewellery <- Prestige_values$copperJewellery*TOT_median_CopperJewel$median
Prestige_values$copperNeckRing <- Prestige_values$copperNeckRing*TOT_median_CopperNeckRing$median
Prestige_values$GoldHairRing <- Prestige_values$GoldHairRing*TOT_median_GoldHair$median
Prestige_values$flintAxe <- Prestige_values$flintAxe*TOT_median_FlintAxe$median
Prestige_values$flintBladeScraperFlake <- Prestige_values$flintBladeScraperFlake*TOT_median_FlintBlade$median
Prestige_values$shellPerf <- Prestige_values$shellPerf*TOT_median_ShellPerf$median
Prestige_values$battleAxe <- Prestige_values$battleAxe*TOT_median_BattleAxe$median
Prestige_values$stoneAxe <- Prestige_values$stoneAxe*TOT_median_otherStone$median

Prestige_values$prestige_total <- rowSums(Prestige_values[,-1], na.rm = TRUE)

#Prestige_values <- Prestige_values[-16,]  #Only used to remove Marefy grave

