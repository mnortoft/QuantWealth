library(here) # for knitr to print out all used packages at the end of the RMarkdown
library(dplyr) # for data manipulation
library(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots

here("analysis")

bone <- read.csv(here("analysis/data/raw_data/reference_data/bone.csv"),
                           header = TRUE,
                           stringsAsFactors = TRUE,
                           sep = ";",
                           encoding = "UTF-8",
                           dec = '.',
                           na.strings = '')


#extract reference PH data:
bone$total_PH <- rowSums(bone[c(4,6)], na.rm = TRUE)

#define skill levels:
bone$skill_factor = dplyr::case_when(
  bone$skill_level == "low" ~ 0.0,
  bone$skill_level == "medium" ~ 0.4,
  bone$skill_level == "medium-high" ~ 0.6,
  bone$skill_level == "high" ~ 2.0,
  bone$skill_level == "very high" ~ 5.0,
)


#estimated time for tooth extraction (estimate based on own experience with wild boar teeth)
tooth_extract <- 0.083

#Extract each reference bone tool as separate value:
PH_T_axe = bone[bone$type == "T-axe",]
PH_boneawl_simple = bone[bone$type == "bone awl simple",]
PH_boneawl_complex = bone[bone$type == "bone awl complex",]
PH_bone_needle = bone[bone$type == "bone needle",]
PH_antler_needle = bone[bone$type == "antler needle",]
PH_bone_chisel = bone[bone$type == "bone chisel",]
PH_harpoon = bone[bone$type == "harpoon",]
PH_bonepin_simple = bone[bone$type == "bone pin simple",]
PH_bonepin_complex = bone[bone$type == "bone pin complex",]
PH_bonepoint_simple = bone[bone$type == "bone point simple",]
PH_bonepoint_complex = bone[bone$type == "bone point complex",]
PH_bone_spearhead = bone[bone$type == "spearhead",]
PH_bone_ring = bone[bone$type == "bone ring (regular)",]
PH_bone_comb = bone[bone$type == "bone comb",]
PH_bone_pendant_button = bone[bone$type == "bone pendant_button",]
PH_bone_plaque_disc = bone[bone$type == "plaque_disc",]
PH_bone_bead = bone[bone$type == "bone bead",]
PH_tooth_bead = bone[bone$type == "tooth bead",]
PH_astragal_perf = bone[bone$type == "astragalus perforated",]
PH_bone_ring_disc = bone[bone$type == "bone ring (disc)",]

## Other ceramics archaeological grave data
bone_predict <- read.csv(here("analysis/data/raw_data/bonetool_predict.csv"),
                         header = TRUE,
                         stringsAsFactors = TRUE,
                         sep = ";",
                         encoding = "UTF-8",
                         dec = '.',
                         na.strings = '')

#if wanting to remove objects in the grave fill:
#bone_predict <- dplyr::filter(bone_predict, is.na(grave_fill))

# list of values in bone_predict grave data
# antler point
# antler tool
# bevel-ended tool
# boar tusk
# boar tusk awl
# boar tusk pendant
# bone awl
# bone bead
# bone chisel
# bone harpoon simple
# bone harpoon
# bone pendant
# bone point
# bone tool
# tooth
# tooth pendant



bone_predict$PH_piece = dplyr::case_when(
  bone_predict$object_type == "antler point" ~ PH_antler_needle$total_PH,
  bone_predict$object_type == "antler tool" ~ PH_antler_needle$total_PH,
  bone_predict$object_type == "bevel-ended tool" ~ PH_bone_chisel$total_PH,
  bone_predict$object_type == "boar tusk" ~ tooth_extract,
  bone_predict$object_type == "boar tusk awl" ~ PH_boneawl_simple$total_PH,
  bone_predict$object_type == "boar tusk pendant" ~ PH_bone_plaque_disc$total_PH, #based on plaque/disc of boar tusk reference data
  bone_predict$object_type == "bone awl" ~ PH_boneawl_simple$total_PH,
  bone_predict$object_type == "bone bead" ~ PH_bone_bead$total_PH,
  bone_predict$object_type == "bone chisel" ~ PH_bone_chisel$total_PH,
  bone_predict$object_type == "bone harpoon simple" ~ (PH_harpoon$total_PH/2),
  bone_predict$object_type == "bone harpoon" ~ PH_harpoon$total_PH,
  bone_predict$object_type == "bone pendant" ~ PH_bone_pendant_button$total_PH,
  bone_predict$object_type == "bone point" ~ PH_bonepoint_simple$total_PH,
  bone_predict$object_type == "bone tool" ~ PH_bonepoint_simple$total_PH,
  bone_predict$object_type == "tooth" ~ tooth_extract,
  bone_predict$object_type == "tooth pendant" ~ PH_tooth_bead$total_PH,
  bone_predict$object_type == "astragalus perforated" ~ PH_astragal_perf$total_PH,
  bone_predict$object_type == "bone ring (regular)" ~ PH_bone_ring$total_PH,
  bone_predict$object_type == "bone ring (disc)" ~ PH_bone_ring_disc$total_PH,
  bone_predict$object_type == "bone spearhead" ~ PH_bone_spearhead$total_PH,

                                       )

bone_predict$PH_raw = dplyr::case_when(
   bone_predict$count <= 100 ~ bone_predict$count*bone_predict$PH_piece,
   bone_predict$count > 100 ~ sqrt(bone_predict$count*bone_predict$PH_piece*100)
)



bone_predict$skill_bonus = dplyr::case_when(
  bone_predict$object_type == "antler point" ~ (bone_predict$PH_raw*PH_antler_needle$skill_factor),
  bone_predict$object_type == "antler tool" ~ (bone_predict$PH_raw*PH_antler_needle$skill_factor),
  bone_predict$object_type == "bevel-ended tool" ~ (bone_predict$PH_raw*PH_bone_chisel$skill_factor),
  bone_predict$object_type == "boar tusk" ~ (tooth_extract*0.0),
  bone_predict$object_type == "boar tusk awl" ~ (bone_predict$PH_raw*PH_boneawl_simple$skill_factor),
  bone_predict$object_type == "boar tusk pendant" ~ (bone_predict$PH_raw*PH_bone_plaque_disc$skill_factor), #based on plaque/disc of boar tusk reference data
  bone_predict$object_type == "bone awl" ~ (bone_predict$PH_raw*PH_boneawl_simple$skill_factor),
  bone_predict$object_type == "bone bead" ~ (bone_predict$PH_raw*PH_bone_bead$skill_factor),
  bone_predict$object_type == "bone chisel" ~ (bone_predict$PH_raw*PH_bone_chisel$skill_factor),
  bone_predict$object_type == "bone harpoon simple" ~ (bone_predict$PH_raw*PH_harpoon$skill_factor),
  bone_predict$object_type == "bone harpoon" ~ (bone_predict$PH_raw*PH_harpoon$skill_factor),
  bone_predict$object_type == "bone pendant" ~ (bone_predict$PH_raw*PH_bone_pendant_button$skill_factor),
  bone_predict$object_type == "bone point" ~ (bone_predict$PH_raw*PH_bonepoint_simple$skill_factor),
  bone_predict$object_type == "bone tool" ~ (bone_predict$PH_raw*PH_bonepoint_simple$skill_factor),
  bone_predict$object_type == "tooth" ~ (tooth_extract*0.0),
  bone_predict$object_type == "tooth pendant" ~ (bone_predict$PH_raw*PH_tooth_bead$skill_factor),
  bone_predict$object_type == "astragalus perforated" ~ (bone_predict$PH_raw*PH_astragal_perf$skill_factor),
  bone_predict$object_type == "bone spearhead" ~ (bone_predict$PH_raw*PH_bone_spearhead$skill_factor)
)


grave_total_bone <- bone_predict %>%
  group_by(Grave_ID) %>%
  dplyr::summarise(object_count = sum(count),
                   PH_raw = sum(PH_raw),
                   skill_bonus = sum(skill_bonus))

