require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots

here("analysis/")

shell <- read.csv(here("analysis/data/raw_data/reference_data/shell.csv"),
                 header = TRUE,
                 stringsAsFactors = TRUE,
                 sep = ";",
                 encoding = "UTF-8",
                 dec = '.',
                 na.strings = '')

#sample data for grave assemblage
shell_predict <- read.csv(here("analysis/data/raw_data/shell_predict.csv"),
                  header = TRUE,
                  stringsAsFactors = TRUE,
                  sep = ";",
                  encoding = "UTF-8",
                  dec = '.',
                  na.strings = '')

#Extract time for Unio into a simple object
shell_Unio <- shell$shell_PH[shell$shell_material == "Unio"]
shell_Dentalium <- shell$shell_PH[shell$shell_material == "Dentalium"]
shell_nacre <- shell$shell_PH[shell$shell_material == "Unio"]
#assuming nacre takes as long time as Unio (Neolithic nacre mostly made from local Unio (Sakalauskaite 2019 et al.))


shell_predict$PH_piece <- dplyr::case_when(shell_predict$shell_material == "Unio" ~ shell_Unio,
                                           shell_predict$shell_material == "Dentalium" ~ shell_Dentalium,
                                           shell_predict$shell_material == "nacre" ~ shell_nacre,
                                           shell_predict$shell_material == "shell" ~ shell_Unio,
                                           shell_predict$shell_material == "oyster" ~ shell_Unio,
                                           shell_predict$shell_material == "Theodoxus" ~ shell_Unio, )

shell_predict$PH_predict <- dplyr::case_when(
  shell_predict$shell_count < 100 ~ shell_predict$shell_count*shell_predict$PH_piece,
  shell_predict$shell_count >= 100 & shell_predict$shell_count < 1000
  ~ sqrt(shell_predict$shell_count*shell_predict$PH_piece*1000),
  shell_predict$shell_count >= 1000 ~ sqrt(shell_predict$shell_count*shell_predict$PH_piece*100)
  )

# Skill bonus (data for Spondylus is currently lacking and is thus ignored here)
shell_predict$PH_skill <- dplyr::case_when(shell_predict$shell_material == "Unio" ~ (shell_predict$PH_predict*0.0),
                                           shell_predict$shell_material == "Dentalium" ~ (shell_predict$PH_predict* 0.4),
                                           shell_predict$shell_material == "nacre" ~ shell_predict$PH_predict*0.4,
                                           shell_predict$shell_material == "oyster" ~ shell_predict$PH_predict*0.0,
                                           shell_predict$shell_material == "Theodoxus" ~ shell_predict$PH_predict*0.0,
                                           shell_predict$shell_material == "shell" ~ shell_predict$PH_predict*0.0)
# skill may be a factor in the importance of nacre beads Sakalauskaite et al. 2019)


#shell_predict$scarcity <- shell_predict$total_graves/shell_predict$total_material_grave
#scarcity/rarity (and shiny white appearance) may also be a factor in the
#importance of nacre beads Sakalauskaite et al. 2019)

#assuming travel speed at 7 km/h, and downplaying large counts (sqrt) assuming that shells to some degree were transported in batches:
shell_predict$shell_travel <- sqrt(shell_predict$shell_count)*(shell_predict$source_distance_km/7)


grave_total_shell <- shell_predict %>%
  group_by(Grave_ID) %>%
  dplyr::summarise(PH_raw = sum(PH_predict),
                   skill_bonus = sum(PH_skill),
                   travelBonus = sum(shell_travel))
