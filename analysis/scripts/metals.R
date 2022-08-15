require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots

here("analysis")

metal_ref <- read.csv(here("analysis/data/raw_data/reference_data/metal_ref.csv"),
                                header = TRUE,
                                stringsAsFactors = TRUE,
                                sep = ";",
                                encoding = "UTF-8",
                                dec = '.',
                                na.strings = '')

metal_density <- data.frame(metal = c("copper", "silver", "gold"),
                            g_cm3 = c(8.92, 10.49, 19.3))

metal_ref$volume_cm3 <- metal_ref$volume_mm3/1000

#plot the reported weight of reference metal objects
metal_refplot <- ggplot(metal_ref, aes(x = volume_cm3, y = weight_g, label = object_simple))+
  #geom_smooth(method = lm)+
  geom_point(aes(color = object_simple, shape = material))+
  #geom_smooth(method = lm)+
  ylab("Reported weight of reference metals (g)")+
  xlab("Volume (cm3)")


#using densities of different reference metals (previously multiplied by 0.289):
metal_ref$pure_weight <- dplyr::case_when(
  metal_ref$material == "copper" ~ metal_ref$volume_cm*8.92,
  metal_ref$material == "silver" ~ metal_ref$volume_cm*10.49,
  metal_ref$material == "gold" ~ metal_ref$volume_cm*19.3,
  metal_ref$material == "bronze" ~ metal_ref$volume_cm*8.6)

#plot the weight estimated from density
metal_pure_plot <- ggplot(metal_ref, aes(x = volume_cm3, y = pure_weight, label = object_simple))+
 # geom_smooth(method = lm)+
  geom_jitter(aes(color = object_simple, shape = material))+
  #geom_smooth(method = lm)+
  ylab("Estimated weight from density (g)")+
  xlab("Estimated volume (cm3)")

#Zooming in on the small ornaments:
metal_ref_small <- dplyr::filter(metal_ref, weight_g < 50)  #reported weight of reference metal objects
metal_est_small <- dplyr::filter(metal_ref, pure_weight < 50) #weight of same objects based on estimated volume if metal is pure

metal_refplot_small <- ggplot(metal_ref_small, aes(x = volume_cm3, y = weight_g, label = object_simple))+
  #geom_smooth(method = lm)+
  geom_point(aes(color = object_simple, shape = material))+
  geom_smooth(method = lm)+
  ylab("Reported weight of reference metals (g)")+
  xlab("Estimated volume (cm3)")

metal_estplot_small <- ggplot(metal_est_small, aes(x = volume_cm3, y = pure_weight, label = object_simple))+
  # geom_smooth(method = lm)+
  geom_jitter(aes(color = object_simple, shape = material))+
  geom_smooth(method = lm)+
  ylab("Estimated weight from density (g)")+
  xlab("Estimated volume (cm3)")

metal_comp_weight.p <- ggplot(metal_est_small, aes(x = weight_g, y = pure_weight, label = object_simple))+
  # geom_smooth(method = lm)+
  geom_jitter(aes(color = object_simple, shape = material))+
  geom_smooth(method = lm)+
  ylab("Estimated weight from volume*density (g)")+
  xlab("Reported weight (g)")


est_rep_weight.lm <- lm(pure_weight~weight_g, data = metal_est_small)
summary(est_rep_weight.lm)

metal_forge_arch <- read.csv(here("analysis/data/raw_data/metal_forge_predict.csv"),
                      header = TRUE,
                      stringsAsFactors = TRUE,
                      sep = ";",
                      encoding = "UTF-8",
                      dec = '.',
                      na.strings = '')

#in case wanting to remove objects in grave fill:
#metal_forge_arch <- filter(metal_forge_arch, is.na(grave_fill))

#make part volume and weight estimates for linear modelling
metal_forge_arch$est_vol_cm3_part <- (metal_forge_arch$est_volume_mm3/1000)/metal_forge_arch$part_count
metal_forge_arch$est_weight_g_part <- metal_forge_arch$est_weight_g_whole_item/metal_forge_arch$part_count

#plot of estimated volume and weight of archaeological metal objects
metal_archplot <- ggplot(metal_forge_arch, aes(x = est_vol_cm3_part, y = est_weight_g_part, label = object_simple))+
  # geom_smooth(method = lm)+
  geom_jitter(aes(color = object_simple, shape = material))+
  geom_smooth(method = lm)+
  ylab("Estimated weight from density (g) per part")+
  xlab("Estimated volume (cm3) per part")

metal_dim_ratio <- metal_forge_arch[-3,c("Grave_ID", "dimension_ratio_numeric", "est_vol_cm3_part", "est_weight_g_part", "object_simple", "material")]
metal_dim_ratio$est_weight_g_vol <- metal_dim_ratio$est_vol_cm3_part*8.92

#make a variable showing the difference in weight (g) when using dim-ratio vs. volume*density
metal_dim_ratio$vol_dim_rat_diff <- metal_dim_ratio$est_weight_g_vol/metal_dim_ratio$est_weight_g_part
summary(metal_dim_ratio$vol_dim_rat_diff)
#do the same but with the raw weight difference between dim-ratio and volume*density:
metal_dim_ratio$vol_dim_raw_diff <- metal_dim_ratio$est_weight_g_vol-metal_dim_ratio$est_weight_g_part
summary(metal_dim_ratio$vol_dim_raw_diff)

metal_dim_rat_complete <- metal_dim_ratio[!is.na(metal_dim_ratio$vol_dim_rat_diff),]
cor(metal_dim_rat_complete$est_weight_g_vol, metal_dim_rat_complete$est_weight_g_part)
wilcox.test(metal_dim_rat_complete$est_weight_g_vol, metal_dim_rat_complete$est_weight_g_part)

est_vol_vs_dimrat_density <- ggplot(metal_dim_rat_complete)+
  geom_density(aes(est_weight_g_vol), color = "green")+
  geom_density(aes(est_weight_g_part), color = "blue")+
  xlab("estimated weight (g)")

dim_ratio_plot <- ggplot(metal_dim_rat_complete)+
  geom_boxplot(aes(x = "dim_ratio x ref weight", y = est_weight_g_part))+
  geom_boxplot(aes(x = "volume x density", y = est_weight_g_vol))+
  geom_jitter(aes(x = "dim_ratio x ref weight", y = est_weight_g_part, color = object_simple, alpha = vol_dim_raw_diff, size = vol_dim_rat_diff), width = 0.1)+
  geom_jitter(aes(x = "volume x density", y = est_weight_g_vol, color = object_simple, alpha = vol_dim_raw_diff, size = vol_dim_rat_diff), width = 0.1)+
  ylab("estimated weight (volume vs. dim.ratio)")

#dim_ratio_plotly <- ggplotly(dim_ratio_plot)

metal_ref_small.p <- ggplot(metal_est_small, aes(x = object_simple, y= weight_g))+
  geom_boxplot()+
  geom_jitter(aes(shape = shape_category), width = 0.05)

#library(patchwork)
#metal_ref_small.p+dim_ratio_plot


#make a boxplot of the dim ratio vs. volume
dim_vol_diff.p <- ggplot(metal_dim_ratio, aes(x = "dim vs. vol ratio", y = vol_dim_rat_diff))+
  geom_boxplot()+
  geom_jitter(width = 0.05)

#ggplotly(dim_vol_diff.p)

#Use a linear model to see how estimated weight and volume relate to each other across objects
metal_vol_weight.lm <- lm(est_weight_g_part~est_vol_cm3_part, data = metal_forge_arch)
metal_vol_weight.sum <- summary(metal_vol_weight.lm) #get results of the linear model
#plot(metal_vol_weight.lm) #check assumptions of linear model


### Metal Chaîne operatoire
# panning river gold in a washing pan may yield about 1 g per 8 hours (but can vary widely) (Urban 2009),
# copper mining based on Brinkmann 2019, mined gold based on Stoellner et al 2016:
metal_forge_arch$mining <- dplyr::case_when(
  metal_forge_arch$material == "gold" ~ metal_forge_arch$est_weight_g_whole_item*8,
  metal_forge_arch$material == "gold mined" ~ metal_forge_arch$est_weight_g_whole_item*41.92,
  metal_forge_arch$material == "copper" ~ 0.5+(metal_forge_arch$est_weight_g_whole_item*1.015*2))

# Processes of copper and mined gold = smelting time per gram, gold = "sintering" time per part
#(beating gold together to a nugget 10-12 PH at 300 degrees or 5 PH at 600 degrees,
#Raub and Gebhard 1995), mined gold smelting 1 PH (Stöllner 2016: fig. 9):
metal_forge_arch$nugget <- dplyr::case_when(
  metal_forge_arch$material == "gold" ~ metal_forge_arch$part_count*5,
  metal_forge_arch$material == "gold mined" ~ metal_forge_arch$est_weight_g_whole_item*1,
  metal_forge_arch$material == "copper" ~ 2+(metal_forge_arch$est_weight_g_whole_item*0.08685)*2)

#forging tools downplayed because they are usually reused, so this may account for tool maintenance:
metal_forge_arch$forging_tools <- metal_forge_arch$est_weight_g_whole_item*0.0071

#Stage one in the forging process:
metal_forge_arch$PH_shape1 <- dplyr::case_when(
  metal_forge_arch$shape_type == "sheet" ~ metal_forge_arch$part_count*(0.22+0.0833),
  metal_forge_arch$shape_type == "wire" ~ metal_forge_arch$part_count*0.6,
  metal_forge_arch$shape_type == "thick wire" ~ metal_forge_arch$part_count*1,
  metal_forge_arch$shape_type == "flat wire" ~ metal_forge_arch$part_count*(0.6+0.22),
  metal_forge_arch$shape_type == "bar" ~ metal_forge_arch$part_count*0.5,
  metal_forge_arch$shape_type == "cast" ~ metal_forge_arch$part_count*
    metal_forge_arch$est_weight_g_whole_item,
)

#Stage two in the forging process:
metal_forge_arch$PH_shape2 <- dplyr::case_when(
  metal_forge_arch$object_simple == "sheet" ~ metal_forge_arch$part_count*0,
  metal_forge_arch$object_simple == "ring" ~ metal_forge_arch$part_count*0.33,
  metal_forge_arch$object_simple == "neck ring" ~ metal_forge_arch$part_count*0.66,
  metal_forge_arch$object_simple == "spiral" ~ metal_forge_arch$part_count*0.33,
  metal_forge_arch$object_simple == "bead" ~ metal_forge_arch$part_count*0.33,
  metal_forge_arch$object_simple == "folded" ~ metal_forge_arch$part_count*0.33,
  metal_forge_arch$object_simple == "tube" ~ metal_forge_arch$part_count*(0.33+0.33),
  metal_forge_arch$object_simple == "awl" ~ metal_forge_arch$part_count*1.25,
  metal_forge_arch$object_simple == "needle" ~ metal_forge_arch$part_count*1.25,
  metal_forge_arch$object_simple == "scoop" ~ metal_forge_arch$part_count*1.25,
  metal_forge_arch$object_simple == "knife" ~ metal_forge_arch$part_count*1,
  metal_forge_arch$object_simple == "razor" ~ metal_forge_arch$part_count*1,

)

metal_forge_arch$PH_total <- rowSums(metal_forge_arch[,c(
  "mining",
  "nugget",
  "forging_tools",
  "PH_shape1",
  "PH_shape2"
)], na.rm = TRUE)

#skill defined as "high" gives 2.0 in skill_bonus
metal_forge_arch$skill_bonus <- (metal_forge_arch$nugget +
                                   metal_forge_arch$PH_shape1+
                                   metal_forge_arch$PH_shape1)*2.0

metal_forge_arch$scarcity_bonus <- dplyr::case_when(
  metal_forge_arch$material == "copper" ~
    metal_forge_arch$total_graves/metal_forge_arch$total_material_grave,
  metal_forge_arch$material == "gold" ~
    metal_forge_arch$total_graves/metal_forge_arch$total_material_grave
  )

#adding travel bonus assuming one trip for each whole object
#(so combined objects such as Marefy necklace count only once):
metal_forge_arch$travel_bonus <- metal_forge_arch$whole_item_count*(metal_forge_arch$source_distance_km/7)

# 9.072 kg of galena may yield c. 5.625 kg (mostly) silver, lead and slag = 62% yield,
#so 0.62 kg silver and lead per kg galena.
silver_lead_perc <- (5.625/9.072)*0.11

#silver can be separated from lead by cupellation with calcium as the agent.
#The cupel can be made of bone ash or shells. One experiment (with modern cupel)
#got 12.86 grams of silver from the 5.625 kg pyramid-shape of silver, lead and slag,
# or 0.23 %, or a general yield of 1.4146 g pure silver/kg silver-lead-slag.
#Sterling silver is 92.5%. For the whole experiment, see https://www.youtube.com/watch?v=VfC-wAJqksU.
#In experiments by L'Heritier et al. 2015 silver cupellation, including heating the furnace (for cupellation per batch) took:
#36 minutes (mean) per cupellation, and 150 mins to heat the furnace (divided by four cupellations):
# or 1.23 PH for cupellation of one 93.2% pure silver button of 0.19 g (average of experiment yields)
#to get time per gram of silver 1/0.19 = 5.26*1.23 PH = 6.44 PH/g silver
silver_cupellation_g <- (((150/4)+36)/60)*5.26

grave_total_metals <- metal_forge_arch %>%
  group_by(Grave_ID) %>%
  dplyr::summarise(PH_total = sum(PH_total),
                   skill_bonus = sum(skill_bonus),
                   scarcityBonus = sum(scarcity_bonus),
                   travelBonus = sum(travel_bonus))
