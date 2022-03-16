require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra) # for tables
require(shiny) #for interactive plots
require(tidyr) #for gather function

here("analysis")

source(here('analysis/scripts/prestigeObjectsBoxPlots.R'), echo = FALSE) # take the animal prestige value from this script
source(here('analysis/scripts/Prestige.R'), echo = FALSE)
source(here('analysis/scripts/age_Groups.R'), echo = FALSE) #get simplified age groups


animals <- read.csv(here("analysis/data/raw_data/animal_bones.csv"),
                     header = TRUE,
                     stringsAsFactors = TRUE,
                     sep = ",",
                     encoding = "UTF-8",
                     dec = ',')
animals2 <- animals
animals2 <- merge(age_Group_freq[,c(1, 48)], animals, by = "Grave_ID")

animal_age_means <- animals2
animal_age_means[is.na(animal_age_means)] <- 0
animal_age_means$total <- rowSums(animal_age_means[,-c(1:2)], na.rm = TRUE)
animal_age_means <- animal_age_means %>%
  filter(total > 0)

animals_age_means2 <- animal_age_means %>%
  group_by(ageGroup_simple) %>%
  dplyr::summarise(means = mean(total, na.rm = TRUE))

chisq.test(animals_age_means2[,c("means")])

animals_age_group <- animals2 %>%
  group_by(ageGroup_simple) %>%
  dplyr::summarise(cattle = sum(cattle, na.rm = TRUE),
                   red_deer = sum(red_deer, na.rm = TRUE),
                   horse = sum(horse, na.rm = TRUE),
                   dog = sum(dog_wolf, na.rm = TRUE),
                   pig = sum(pig, na.rm = TRUE),
                   sheep_goat = sum(sheep_goat, na.rm = TRUE))


animals_age_group.t <- data.frame(t(animals_age_group))
names(animals_age_group.t) <- animals_age_group.t[1,]
animals_age_group.t <- animals_age_group.t[-1,]

animals_age_group.t$infans <- as.numeric(animals_age_group.t$infans)
animals_age_group.t$juvenis <- as.numeric(animals_age_group.t$juvenis)
animals_age_group.t$adultus <- as.numeric(animals_age_group.t$adultus)
animals_age_group.t$maturus <- as.numeric(animals_age_group.t$maturus)
animal_age_fish <- fisher.test(animals_age_group.t)
animal_age_chisq <- chisq.test(animals_age_group.t)
# categories adapted from Kolar 2018 Table 64:
# animals$large <- rowSums(animals[c("cattle", "red_deer", "horse")], na.rm = TRUE)
# animals$medium <- rowSums(animals[c("dog_wolf", "fox", "pig", "sheep_goat")], na.rm = TRUE)
# animals$small <- rowSums(animals[c("small_medium_amphibians", "hare")], na.rm = TRUE)


#usable meat from cow, sheep, and goat representing large and medium animals (Sørensen 2015: Fig III.6)
# boar (incl. presumably Neolithic pigs) lean cuts + fat cuts in kg (from proportion of half carcass weight)
# from  Sales and Kotrba 2013 DOI : 10.1016/j.meatsci.2013.01.012

meat_sheep <- 10.52 # 2.75 (mean of sheep values in Rowley-Conwy et al. 2002 (modified from Binford 1878)
meat_pig_boar <- 45.1 #from Rowley-Conwy et al. 2002
meat_horse <- 126.00 # from Outram and Rowley-Conwy 1998 see notes below
meat_cow <- meat_horse*1.6 # scaled up from horse live weight (181 kg) to match Neolithic cattle (290 kg)
meat_reindeer <- 30.23 #from Metcalfe and Jones 1988
meat_whitetailed_deer <- 11.468+0.133 #meat + marrow kg, meat is mean of three specimens in Madrigal et al


#Neolithic cattle weight 280-300 kg (Cummings and Morris 2018: 2)

#Meat utility indices (incl. marrow and grease)
#Total meat utility of wild boar/domestic pig in Rowley-Conwy et al. 2002:
#32.93 kg (14 months old), 57.20 kg (92 months old) (mean 45.1 kg)
#same for sheep 7.692 kg (6 months old), 13.348 (90 months old) = mean 10.52 kg Rowley-Conwy et al. 2002 modified from Binford 1878

#Horse:
#Przewalski horses weigh on average 300 kg (withers height 144 cm),
#and are often used as the size equivalent of prehistoric horses
#Horse: carcass weight for 2 female horses withers height 144 cm and 149 cm (3rd horse too fat, left out here)
#horse (contd): 193.25 kg and 169.75 kg (mean 181.5 kg) Outram and Rowley-Conwy 1998
#horse (contd): meat weight of the 2 horses 132.25 kg and 119.75 kg (mean 126 kg).
#reindeer (caribou) meat 30.23 kg Metcalfe and Jones 1988 modified from Binford 1978
reference_meat <- data.frame(horse = meat_horse,
                             sheep = meat_sheep,
                             pig_boar = meat_pig_boar,
                             reindeer = meat_reindeer)
ref_meat_long <- tidyr::gather(reference_meat)
names(ref_meat_long) <- c("animal", "meat_kg")

animal_meat <- data.frame(Grave_ID = animals$Grave_ID,
                          cattle = case_when(!is.na(animals$cattle) ~ animals$cattle * meat_cow),
                          red_deer = case_when(!is.na(animals$red_deer) ~ animals$red_deer * meat_reindeer),
                          horse = case_when(!is.na(animals$horse) ~ animals$horse * (meat_horse)),
                          pig_boar = case_when(!is.na(animals$pig) ~ animals$pig * meat_pig_boar),
                          dog_wolf = case_when(!is.na(animals$dog_wolf) ~ animals$dog_wolf * (meat_sheep/2)),
                          fox = case_when(!is.na(animals$fox) ~ animals$fox * (meat_sheep/3)),
                          hare = case_when(!is.na(animals$hare) ~ animals$hare * (meat_sheep/8)),
                          amphibians = case_when(!is.na(animals$small_medium_amphibians) ~ animals$small_medium_amphibians * (meat_sheep/10)),
                          sheep_goat = case_when(!is.na(animals$sheep_goat) ~ animals$sheep_goat * meat_sheep)
                          )

# animal_kg <- data.frame(Grave_ID = animals$Grave_ID,
#                         kg_large = case_when(!is.na(animals$large) ~ animals$large*200),
#                         kg_medium = case_when(!is.na(animals$medium) ~ animals$medium*30),
#                         kg_small = case_when(!is.na(animals$small) ~ animals$small*0.5))

animal_meat$kg_total <- rowSums(animal_meat[-1], na.rm = TRUE)

#join animal prestige values to animal dataframe:
animals_all <- merge(x=animal_meat, y=Prestige_values[c("Grave_ID", "unworkedAnimal_spec")], by="Grave_ID", all = TRUE)
#change name of prestige value column to "prestige":
names(animals_all)[names(animals_all) == 'unworkedAnimal_spec'] <- 'animal_prestige'

animals_all$animal_scarcity <- ifelse(!is.na(animals_all$kg_total), 413/43, 0)
animals_all$kg_total <- ifelse(!is.na(animals_all$kg_total), animals_all$kg_total, 0)

# # multiply the prestige value with the species count
# animals_all$animal_prestige <- animals_all$animal_prestige * animals_all$species_count

# #change NA values to 0s:
# animals_all[,c("species_count", "animal_scarcity", "animal_prestige")][is.na(animals_all[,c("species_count", "animal_scarcity", "animal_prestige")])] <- 0

animals_all$total_animal <- rowSums(animals_all[,c("kg_total", "animal_prestige", "animal_scarcity")], na.rm = TRUE)
summary(animals_all)
#animal_normed <- data.frame(animals_all[,1], lapply(animals_all[,2:4], min_max_norm))

animals_sexgender <- data.frame(merge(CW_raw[,c(1,16)], animals_all, by = "Grave_ID"))

meat_boolean <- data.frame(SexGender = animals_sexgender$SexGender, meat = animals_sexgender$total_animal)
meat_boolean$meat_boolean <- case_when(meat_boolean$meat > 0 ~ TRUE,
                                       meat_boolean$meat == 0 ~ FALSE, )


animals_sex <- merge(CW_raw[,c(1,16)], animals[,-c(6,10)], by = "Grave_ID")
#FactoMineR::PCA(animals_sexgender[,c(2:11)], quali.sup = c(1))

animals_sex_group <- animals_sex %>%
  group_by(SexGender) %>%
  dplyr::summarise(cattle = sum(cattle, na.rm = TRUE),
                   red_deer = sum(red_deer, na.rm = TRUE),
                   horse = sum(horse, na.rm = TRUE),
                   dog = sum(dog_wolf, na.rm = TRUE),
                   pig = sum(pig, na.rm = TRUE),
                   sheep_goat = sum(sheep_goat, na.rm = TRUE))


#make animal species count ready for ggplot and testing
animals_sex_group.t <- data.frame(t(animals_sex_group))
animals_sex_group2 <- janitor::row_to_names(animals_sex_group.t, row_number = 1)
animals_sex_group2 <- as.data.frame(animals_sex_group2)
colnames(animals_sex_group2) <- c("ambiguous", "female", "male", "unknown")
animals_sex_group2$Species <- rownames(animals_sex_group2)
animals_sex_group2[,-5] <- as.data.frame(lapply(animals_sex_group2[,-5], as.numeric))
animals_sex_group_long <- animals_sex_group2 %>%
  pivot_longer(!Species, names_to = "SexGender") #for ggplot

#Do Chi.squared test and fishers' exact test for count data
chisq_spec_sex <- chisq.test(animals_sex_group2[,-5])
fisher_spec_sex <- fisher.test(animals_sex_group2[,-5])

#do Mann-Whitney U/Wilcoxon non-parametric test (two variables only)
wilcox_female <- animals_sex_group2$female
wilcox_male <- animals_sex_group2$male
wilcox.test(wilcox_female, wilcox_male, paired = TRUE)

animal_sex_total <- data.frame(total = colSums(animals_sex_group2[,-5]))
animal_sex_total$SexGender <- rownames(animal_sex_total)
animal_sex_total.p <- ggplot(animal_sex_total, aes(SexGender, total))+
  geom_col(position = "dodge")

#meat_DF <- data.frame(animals_prop)

#Barplot for all animal species present by SexGender
animal_sexgender.p <- ggplot(animals_sex_group_long, aes(Species, value, fill = SexGender))+
  geom_col(position = "dodge")

#Barplot showing presence/absence of animal bones by sex
# meat_sexgender.p <- ggplot(meat_DF, aes(meat, Freq, fill = SexGender))+
#   geom_col(position = "dodge")
#animal_total <- data.frame(Grave_ID = animal_normed[,1], animal_total = rowSums(animal_normed[,-1], na.rm = TRUE))
