require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots
require(DescTools)
require(psych)

here("analysis")

source(here('analysis/scripts/ceramics_predict.R'), echo = TRUE)
source(here('analysis/scripts/flint_predict.R'), echo = TRUE, encoding = "UTF-8")
source(here('analysis/scripts/stone_predict.R'), echo = TRUE, encoding = "UTF-8")
source(here('analysis/scripts/metals.R'), echo = TRUE, encoding = "UTF-8")
source(here('analysis/scripts/PH_bone.R'), echo = FALSE, encoding = "UTF-8")
source(here('analysis/scripts/PH_shell.R'), echo = TRUE, encoding = "UTF-8")
source(here('analysis/scripts/PH_amber.R'), echo = FALSE, encoding = "UTF-8")
source(here('analysis/scripts/prestigeObjectsBoxPlots.R'), echo = FALSE)

grave_id <- read.csv(here("analysis/data/raw_data/grave_goods_raw_count.csv"),
                   header = TRUE,
                   stringsAsFactors = TRUE,
                   sep = "\t",
                   encoding = "UTF-8",
                   dec = ',')

#grave_id <- grave_id[-16,]  #only used if wanting to remove extreme Marefy grave
grave_id$skeletal <- TRUE

#removing data not used in Gini
grave_id_cut <- data.frame(grave_id[,c(Grave_ID = "Grave_ID",
                                       skeletal = "skeletal",
                                       graveDepth = "graveDepth",
                                       animal_species = "unworkedAnimal_spec")])



names(grave_total_bone) <- c("Grave_ID", "bone_PH_raw", "bone_obj_count", "bone_skill")
names(grave_total_pot) <- c("Grave_ID", "pot_PH_raw", "pot_skill")
names(grave_total_ceramic_other) <- c("Grave_ID", "ceramic_PH_raw", "ceramic_skill")
names(grave_total_metals) <- c("Grave_ID", "metal_PH_raw", "metal_skill", "metal_scarcity", "metal_travel")
names(grave_total_shell) <- c("Grave_ID", "shell_PH_raw", "shell_skill", "shell_travel")
names(grave_total_stone_tools) <- c("Grave_ID", "stone_PH_raw", "stone_skill", "stone_scarcity",  "stone_travel")
names(grave_total_flint_axe) <- c("Grave_ID", "flintAxe_PH_raw", "flintAxe_skill", "flintAxe_scarcity", "flintAxe_travel")
names(grave_total_flint_other) <- c("Grave_ID", "flint_other_PH_raw", "flint_other_skill")

#based on Kolar 2018 table 13:
grave_total_bone$bone_scarcity <- 413/83
grave_total_pot$pot_scarcity <- 413/376
grave_total_ceramic_other$ceramic_other_scarcity <- 413/376
grave_total_shell$shell_scarcity <- 413/83
grave_total_stone_tools$stone_scarcity[is.na(grave_total_stone_tools$stone_scarcity)] <- 413/156 #polished + other stone artefacts
grave_total_flint_axe$flintAxe_scarcity[is.na(grave_total_flint_axe$flintAxe_scarcity)] <- 413/155
grave_total_flint_other$flintOther_scarcity <- 413/155

# #collecting all metal value measures to plot them:
# grave_metal_values <- merge(x=grave_meta, y=grave_total_metals, by="Grave_ID", all = TRUE)
# grave_metal_values$total_metal <- rowSums(grave_metal_values[21:24])

# ggplot(grave_metal_values, aes(meanAge, total_metal, color = SexGender))+
#   geom_point()

grave_1 <- merge(x=grave_id_cut, y=grave_total_bone, by="Grave_ID", all = TRUE)
grave_2 <- merge(x=grave_1, y=grave_total_pot, by="Grave_ID", all = TRUE)
grave_3 <- merge(x=grave_2, y=grave_total_ceramic_other, by="Grave_ID", all = TRUE)
grave_4 <- merge(x=grave_3, y=grave_total_metals, by="Grave_ID", all = TRUE)
grave_5 <- merge(x=grave_4, y=grave_total_shell, by="Grave_ID", all = TRUE)
grave_6 <- merge(x=grave_5, y=grave_total_stone_tools, by="Grave_ID", all = TRUE)
grave_7 <- merge(x=grave_6, y=grave_total_flint_axe, by="Grave_ID", all = TRUE)
grave_8 <- merge(x=grave_7, y=grave_total_flint_other, by="Grave_ID", all = TRUE)

#remove graves without skeletal material
#because some graves in Kolar et al. 2011 did not get all gravegoods added here (e.g. metals):
grave_8_skelet <- dplyr::filter(grave_8, skeletal %in% c(TRUE))

