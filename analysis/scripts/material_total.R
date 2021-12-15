require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots
require(tidyr) #for gather function

here("analysis")

source(here('analysis/scripts/animals.R'), echo = TRUE)
source(here('analysis/scripts/ceramics_predict.R'), echo = TRUE)
source(here('analysis/scripts/flint_predict.R'), echo = TRUE, encoding = "UTF-8")
source(here('analysis/scripts/stone_predict.R'), echo = TRUE, encoding = "UTF-8")
source(here('analysis/scripts/metals.R'), echo = TRUE, encoding = "UTF-8")
source(here('analysis/scripts/PH_bone.R'), echo = FALSE, encoding = "UTF-8")
source(here('analysis/scripts/PH_shell.R'), echo = TRUE, encoding = "UTF-8")
source(here('analysis/scripts/PH_amber.R'), echo = FALSE, encoding = "UTF-8")

source(here('analysis/scripts/Gini_calc.R'), echo = FALSE)
source(here('analysis/scripts/labour_hour_total.R'), echo = FALSE)
source(here('analysis/scripts/Prestige.R'), echo = FALSE)

material_total0 <- grave_8_skelet %>% dplyr::select(-c(unworkedAnimal_spec, skeletal))
prestige_clean <- Prestige_values%>% dplyr::select(-c(prestige_total, unworkedAnimal_spec))

names(prestige_clean) <- c("Grave_ID",
                           "boneBeads_prest",
                           "boneToolS_prest",
                           "boneToolComp_prest",
                           "vesselSmall_prest",
                           "vesselLarge_prest",
                           "spindWhorl_prest",
                           "copperawl_prest",
                           "copperKnife_prest",
                           "copperJewel_prest",
                           "copperNeck_prest",
                           "goldRing_prest",
                           "flintAxe_prest",
                           "flintBlade_prest",
                           "shell_prest",
                           "battleAxe_prest",
                           "stoneAxe_prest",
                           "unworkMineral_prest")


material_total2 <- merge(x=material_total0, y=prestige_clean, all = TRUE)
Sex <- grave_id[,c("Grave_ID", "meanAge", "SexGender")]
SiteName <- grave_id[,c("Grave_ID", "SiteName")]

mat_animal <- data.frame(Grave_ID = animals_all$Grave_ID, animal = animals_all$total_animal)

mat_bone <- data.frame(Grave_ID = material_total2$Grave_ID,
                           bone_total = rowSums(material_total2 %>% select(contains('bone')), na.rm = TRUE))

mat_stone <- data.frame(Grave_ID = material_total2$Grave_ID,
                           stone_total = rowSums(material_total2 %>% select(contains(c('stone', 'battle', 'Mineral'))), na.rm = TRUE))

mat_flint <- data.frame(Grave_ID = material_total2$Grave_ID,
                        flint_total = rowSums(material_total2 %>% select(contains(c('flint'))), na.rm = TRUE))

mat_metal <- data.frame(Grave_ID = material_total2$Grave_ID,
                        metal_total = rowSums(material_total2 %>% select(contains(c('metal', 'copper', 'gold'))), na.rm = TRUE))

mat_shell <- data.frame(Grave_ID = material_total2$Grave_ID,
                        shell_total = rowSums(material_total2 %>% select(contains('shell')), na.rm = TRUE))

mat_ceramic <- data.frame(Grave_ID = material_total2$Grave_ID,
                                       ceramic_total = rowSums(material_total2 %>% select(contains(c('ceram', 'pot', 'whorl', 'vessel'))), na.rm = TRUE))

material_all <- SiteName %>%
  merge(mat_animal, by="Grave_ID") %>%
  merge(mat_bone, by="Grave_ID") %>%
  merge(mat_stone, by="Grave_ID") %>%
  merge(mat_flint, by="Grave_ID") %>%
  merge(mat_metal, by="Grave_ID") %>%
  merge(mat_shell, by="Grave_ID") %>%
  merge(mat_ceramic, by="Grave_ID")

#Remove outliers for PCA
#material_all_cut <- material_all[-c(14,16),]
#material_PCA <- FactoMineR::PCA(material_all_cut[2:8], graph = FALSE)
#plot(material_PCA)
