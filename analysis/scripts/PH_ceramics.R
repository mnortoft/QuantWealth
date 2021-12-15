require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(plotly) # for data manipulation
require(tidyverse) # for interactive plots
require(car)

here("analysis")

#Load experimental reference data:
PH_pottery_uncut <- read.csv(here("analysis/data/raw_data/reference_data/PH_pottery.csv"),
                             header = TRUE,
                             stringsAsFactors = TRUE,
                             sep = ";",
                             encoding = "UTF-8",
                             dec = '.',
                             na.strings = '')

#remove molded pots as they have very different manufacturing time:
PH_pottery <- PH_pottery_uncut[-c(1:7),] #this refers to rows 1-7 in the reference dataset

# chaine operatoire

# raw material (assuming local clay is used)
raw_material <- 1

min_clay_extract <- 1.55
min_water_clay_mix <- 4.32
min_kneading_clay <- 50.5
# clay extraction (1.55 min/kg) and mixing with water (4.32 min/kg)
#(from Martineau et al. 2007: 37) and kneading the clay into a paste (50.5 min/kg)
#= 56.37 min/kg = 0.94 PH/kg (rounded to 2 decimals)
min_clay_all <- min_clay_extract+min_water_clay_mix+min_kneading_clay
PH_clay_extract <- round((min_clay_all/60), 2)

#clay density = 1.75 kg/L, so we divide the PH by the density and get PH/kg
clay_density <- 1/1.75
clay_PH_L <- round(PH_clay_extract*clay_density, 2)

grog_density <- mean(c(1.87, 1.80, 1.76)) #grog densities taken from Iyasara et al. 2016 table 5
#grog temper 0.6 hours/Liter temper (0.6 PH) (Ignat 2016)
PH_temper_grog <-   0.6
# sand temper assuming 0 PH for preparation (adding a bit of time for collection and mixing clay with sand)
PH_temper_sand <- 0.1
# shell temper assuming 0.1 PH/Liter (reference data needed for this) converted
#to kg density assuming 0.85 kg/L (according to aqua-calc.com calculator)
PH_temper_shell <- 0.2
# mica temper assuming 0.2 PH/Liter (reference data needed for this) based on low Mohs hardness
PH_temper_mica <- 0.2
# granite temper 0.33 hours/kg (converted to Liter with density 1.65 kg/L according
#to aqua-calc.com calculator)
#in fire-brittled form (collected from fireplaces) (Inger Heebøl pers.comm. August 2021)
PH_temper_granite <- round(0.33*1.65, 2) #0.55 PH/L
# quartz temper assuming 6.5 hours/Liter according to Palacios 2021
#but fire-brittled quartz probably much less, assuming here slightly more than granite (see below), cf. Crandell 2007 'Fire-cracked Rocks')
PH_temper_quartz <- round(PH_temper_granite*1.5, 2)

# Person-hours for coiling assuming 1,3 mins/cm (max measure) and converting to PH
#temper_overview$PH_coil <- (temper_overview$max_measure_cm * 1.078) /60

# PH of coiling using coefficient of 0.07527 minute per pot weight (g)
PH_shape_coil <- 1.078 #multiply with weight in kg column of dataset

# thickness
PH_thickness <- 0.2

#c. 15 minutes for handles/ears
ear_handle <- (15/60)

# 3 minutes per plastic decoration in hours:
PH_plastic_deco <- (15/60)

#firing times (open fire): the 4 first from Inger Heebøls records, the last (3 hours) is first experience
#from Steinzeitpark Dithmarschen summer of 2021)
#only active firing time is used, as the pots usually cool down slowly with the
#embers without much supervision to prevent breaking.
#the number is multiplied by 2 assuming that two people attend the fire and the
#pottery while firing, and then divided by 20 assuming that a batch of c. 30 pots
#were fired at the same time (own experience from Steinzeitpark Dithmarschen summer 2021)
PH_fire <- c(4.0, 1.7, 2.45 , 2.17, 3.0)
PH_firing <- (median(PH_fire)*2)/20 # median = 2.45 PH * / 20 = 0.245 PH


#First predict the weights of pots because they are often not given for archaeological assemblages:
PH_pottery$weight_predict <- ifelse(is.na(PH_pottery$temper_percent), #if temper percent column is empty
                                    #apply weight_predict of respectively smaller and larger than 10 cm max measure * 0.2
                                    (dplyr::case_when(PH_pottery$max_measure_cm < 10 ~ ((PH_pottery$weight_predict =  PH_pottery$max_measure_cm * 10)*0.2),
                                                     PH_pottery$max_measure_cm >= 10 ~ ((PH_pottery$weight_predict =  (PH_pottery$max_measure_cm-9) * 120))*0.2)),
                                    #if temper_percent column is NOT empty, apply weight_predict same way but with (inverse) temper_percent multiplication
                                    #(to get the percentage that is clay and NOT temper, i.e. the clay amount)
                                    (dplyr::case_when(PH_pottery$max_measure_cm < 10 ~ ((PH_pottery$weight_predict =  PH_pottery$max_measure_cm * 10)*(abs(PH_pottery$temper_percent-1))),
                                   PH_pottery$max_measure_cm >= 10 ~ ((PH_pottery$weight_predict =  (PH_pottery$max_measure_cm-9) * 120))*(abs(PH_pottery$temper_percent-1)),
                                   )))

#plot reference weights against predicted weights
weight_size <- ggplot(PH_pottery)+
  geom_point(aes(max_measure_cm, weight_g), color = "green")+
  geom_point(aes(max_measure_cm, weight_predict), color = "red")

#Now use the predicted weights in temper calculations
# calculating the accumulated PH for each step depending on the dataset given:
PH_pottery$PH_extract = PH_clay_extract * (PH_pottery$weight_predict/1000) #PH per kg extracted clay


#first converting measurements to Liter for each pot (cylinder formula)
diam_int <- (PH_pottery$max_width_cm)-0.68 #internal diameter
diam_ext <- PH_pottery$max_width_cm        #external diameter
PH_pottery$Liter <- (pi*PH_pottery$height_cm*((diam_ext^2)-(diam_int^2))/4)/1000


#then converting the specific temper percents to Liter... (ifelse statements not filled out )
PH_pottery$PH_temper = PH_pottery$Liter * ifelse(is.na(PH_pottery$temper_percent), 0.2, PH_pottery$temper_percent) *
  ifelse(is.na(PH_pottery$temper), 0.3, dplyr::case_when(PH_pottery$temper == "grog" ~ PH_temper_grog, #then applying to the different tempers
                   PH_pottery$temper == "shell" ~ PH_temper_shell,
                   PH_pottery$temper == "sand" ~ PH_temper_sand,
                   PH_pottery$temper == "quartz" ~ PH_temper_quartz,
                   PH_pottery$temper == "granite" ~ PH_temper_granite,
                   TRUE ~ 0))


# the actual time for shaping (coiling) in experimental studies and from informants
PH_pottery$PH_shape_time = PH_pottery$shape_time_mins/60
#using the coefficient of max_measure_cm vs. shaping time to predict shaping PH (with a lower increase at smaller pots)
PH_pottery$PH_shape_predict = dplyr::case_when(PH_pottery$max_measure_cm <= 10 ~ (PH_pottery$PH_shape_predict =  PH_pottery$max_measure_cm * (4/60)),
                                               PH_pottery$max_measure_cm > 10 ~ (PH_pottery$PH_shape_predict =  (PH_pottery$max_measure_cm-8) * (15/60)))

#plot the reference shaping time against the predicted shaping time:
shape_time <- ggplot(PH_pottery)+
  geom_point(aes(max_measure_cm, PH_shape_time), color = "green")+
  geom_point(aes(max_measure_cm, PH_shape_predict), color = "red")

PH_pottery$PH_plastic = PH_pottery$plastic_deco * PH_plastic_deco
PH_pottery$PH_impress_deco = (PH_pottery$PH_shape_predict) * (PH_pottery$impressed_decorations_perc)


#polishing time
PH_pottery$PH_polish_predict = dplyr::case_when(
  PH_pottery$polish == 1 ~ (PH_pottery$max_measure_cm * (4.3/60)), #polish on one side
  PH_pottery$polish == 2 ~ (PH_pottery$max_measure_cm * (4.3/60))*2) #polish on two sides

PH_pottery$PH_smooth = dplyr::case_when(
  PH_pottery$smooth_beat == 1 ~ (PH_pottery$PH_polish_predict* 0.28), #smooth on one side
  PH_pottery$smooth_beat == 2 ~ (PH_pottery$PH_polish_predict* 0.28)*2) # smooth on two sides

#plot the reference polishing time against the predicted polishing time:
polish_time <- ggplot(PH_pottery)+
  geom_point(aes(max_measure_cm, (polish_time_mins/60)), color = "green")+
  geom_point(aes(max_measure_cm, PH_polish_predict), color = "red")

# calculate slip or painting time based on shaping time and slip/paint percentage
PH_pottery$PH_slip_paint = (PH_pottery$PH_shape_time) * PH_pottery$slip_paint_perc

# add the (fixed) firing time to the reference dataframe to include in overall PH
PH_pottery$PH_fire = PH_firing

#boxplot of firing time data range (needs to be a dataframe to work with ggplot):
fireDF <- data.frame(PH_fire)
fire_plot <- ggplot(fireDF, aes('', y = PH_fire))+
  geom_boxplot()+
  geom_point()

## Skill ##
#Three different levels: medium (0.4), medium-high (0.6), and high (2.0)
PH_pottery$skill = dplyr::case_when(PH_pottery$max_measure_cm >= 25 ~ (PH_pottery$skill =  2.0),
                                    PH_pottery$max_measure_cm >= 15 &
                                      PH_pottery$max_measure_cm <25 ~ (PH_pottery$skill = 0.6),
                                    PH_pottery$max_measure_cm < 15 ~ (PH_pottery$skill = 0.4))


# summing up PH for all production stages:
PH_pottery$PH_all <- rowSums(PH_pottery[,c("PH_temper",
                        "PH_shape_predict",
                        "PH_plastic",
                        "PH_impress_deco",
                        "PH_smooth",
                        "PH_polish_predict",
                        "PH_slip_paint",
                        "PH_fire")], na.rm = TRUE)


#Multiplying total hours by skill factor
PH_pottery$PH_all_skill = (rowSums(PH_pottery[,c("PH_shape_predict","PH_polish_predict","PH_fire")],
                                  na.rm = TRUE) * PH_pottery$skill) + PH_pottery$PH_all

# max measure (cm) vs. shaping time (PH)
potter_PH <- ggplot(PH_pottery, aes(max_measure_cm, PH_shape_time, color = potter)) +
  geom_point()+
  geom_line(stat="smooth", method = "glm", se = TRUE, level = 0.95, fullrange = FALSE, alpha = 0.5)+
  scale_y_continuous("Person hours (PH)", minor_breaks = seq(0, 10, 0.5),
                     breaks = c(1, 2,3,4,5,6, 7))


#plot of person-hours vs. max measure for different temper types assuming 20% temper
temper_PH <- ggplot(PH_pottery, aes(x = max_measure_cm,
                                    y = ((((height_cm*(max_width_cm/2)*pi)/1000)*0.2)*PH_temper_quartz),
                                    fill="Quartz hours",
                                    width = 0.2
                                    )) +
  geom_col()+
  geom_col(aes(max_measure_cm, ((((height_cm*(max_width_cm/2)*pi)/1000)*0.2)*PH_temper_grog), fill = "Grog hours"))+
  geom_col(aes(max_measure_cm, ((((height_cm*(max_width_cm/2)*pi)/1000)*0.2)*PH_temper_granite), fill = "Granite hours"))+
  geom_col(aes(max_measure_cm, ((((height_cm*(max_width_cm/2)*pi)/1000)*0.2)*PH_temper_mica), fill = "Mica Shell hours"))+
  scale_fill_manual('', values = c("Quartz hours" = "red",
                                   "Granite hours" = "green",
                                   "Grog hours" = "black",
                                   "Mica Shell hours" = "yellow"))+
                                   #breaks = c("Grog hours", "Quartz hours", "Granite hours", "Mica Shell hours")))+
  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_continuous("Person hours (PH)", minor_breaks = seq(0, 10, 0.5),
                     breaks = c(0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16, 0.18, 0.20, 0.22, 0.24))+
  scale_x_continuous("Max measure (cm)", minor_breaks = seq(0, 100, 5),
                     breaks = c(1,5,10,15,20,25,30,35))


#temper_PH <- ggplotly(temper_PH, tooltip = c("max_measure_cm", "y"))

# Find the ratio between max_measure_cm and thickness_mm (converted first to cm)
PH_pottery$size_ratio = PH_pottery$height_cm/PH_pottery$max_width_cm
PH_pottery$thick_size_ratio = round(PH_pottery$max_measure_cm/(PH_pottery$thickness_mm/10), 2)
PH_pottery$thick_size_ratioSQRT = round(PH_pottery$max_measure_cm/sqrt(PH_pottery$thickness_mm/10), 2)


mesh_time <- ggplot(PH_pottery, aes(max_measure_cm, shape_time_mins))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_y_continuous("Shaping-time (minutes)")+
  scale_x_continuous("Max measure (cm)")+
  theme(text = element_text(size = 8))

ratio_time <- ggplot(PH_pottery, aes(thick_size_ratio, shape_time_mins))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_y_continuous("Shaping-time (minutes)")+
  scale_x_continuous("Max measure/thickness (cm)")+
  theme(text = element_text(size = 8))

ratio_time2 <- ggplot(PH_pottery, aes(thick_size_ratioSQRT, shape_time_mins))+
  geom_point()+
  geom_smooth(method = lm)+
  scale_y_continuous("Shaping-time (minutes)")+
  scale_x_continuous("Max measure/squareroot of thickness (cm)")+
  theme(text = element_text(size = 8))


## other ceramics reference data
other_ceramic_ref <- read.csv(here("analysis/data/raw_data/reference_data/PH_other_ceramic.csv"),
         header = TRUE,
         stringsAsFactors = TRUE,
         sep = ";",
         encoding = "UTF-8",
         dec = '.',
         na.strings = '')

