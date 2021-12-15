require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots

here("analysis")

#272 kg/4 hours/10
axe_blank_10kg = 4/(272/10)

mohs_tough_ref_list <- read.csv(here("analysis/data/raw_data/reference_data/mohs_vs_tough_ref.csv"),
         header = TRUE,
         stringsAsFactors = TRUE,
         sep = ";",
         encoding = "UTF-8",
         dec = '.',
         na.strings = '')

# Tentative reference list for Mohs hardness and fracture toughness:
mohs_tough_list <- ggplot(mohs_tough_ref_list, aes(x = mohs_hardness, y = fracture_toughness, color = mineral_type))+
  geom_point()

stone_axe_ref <- read.csv(here("analysis/data/raw_data/reference_data/stone_axe_ref.csv"),
                                              header = TRUE,
                                              stringsAsFactors = TRUE,
                                              sep = ";",
                                              encoding = "UTF-8",
                                              dec = '.',
                                              na.strings = '')

stone_axe_ref$total_PH <- rowSums(stone_axe_ref[c("raw_material_PH", "preform_PH", "grind_polish_PH")], na.rm = T)



stone_axe_ref$mohs_tough <- stone_axe_ref$toughness+log(stone_axe_ref$mohs_hardness)


#plot extraction and blank production by method
stoneaxe_blank_plot <- ggplot(stone_axe_ref[-c(4,6),], aes(x = '', y = raw_material_PH)) +
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(color = extraction_method))

#find median of extracting rock by fire spalling or heavy percussion (sawing very slow and excluded here)
stoneaxe_blank <- median(stone_axe_ref[-c(4,6),]$raw_material_PH)

stoneaxe_preform <- ggplot(stone_axe_ref[-c(3,1),], aes(x = (mohs_tough), y = preform_PH, color = type_simple, shape = knapper)) +
  geom_jitter(width = 0.2, size = 3)

stoneaxe_grind <- ggplot(stone_axe_ref, aes(x = mohs_tough, y = grind_polish_PH, color = type_simple, shape = knapper)) +
  geom_jitter(width = 0.2, size = 3)

stoneaxe_total_plot <- ggplot(stone_axe_ref[-9,], aes(x = type_simple, y = total_PH)) +
  geom_boxplot()+
  geom_jitter(aes(color = grind_area, shape = knapper), width = 0.1, size = 3)

stoneaxe_grind.lm <- lm(grind_polish_PH ~ mohs_tough, stone_axe_ref[-9,])
summary(stoneaxe_grind.lm)
#plot(stoneaxe_grind.lm) abandon model, linear assumptions not met


