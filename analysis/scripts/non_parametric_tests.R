require(here)
require(dplyr)
require(ggplot2)
require(FactoMineR)

here()
source(here('analysis/scripts/PCA.R'), echo = FALSE) #to grab results from PCA script


# Non-parametric test of PCA SexGender and age
## Mann-Whitney-Wilcox rank sum test (only comparing two groups)
PCA_ready_all <- mesh_PCA5[,c(2, 4:9, 17)] #take only columns active in PC , SexGender and agegroup columns
all.pca <- PCA(PCA_ready_all, quali.sup = c(1,8), graph = FALSE)

pre_wilcox <- data.frame(PC1 = all.pca$ind$coord[,1], PC2 = all.pca$ind$coord[,2], SexGender = PCA_ready_all$SexGender)

#choose only males and females (in separate objects)
wilcox_prep_M <- subset(pre_wilcox, SexGender == c('male'))
wilcox_prep_F <- subset(pre_wilcox, SexGender == c('female'))

#Perform the Mann-Whitney-Wilcox test on the two numerical objects
wilcoxMF_PC1 <- wilcox.test(wilcox_prep_M$PC1, wilcox_prep_F$PC1, exact = FALSE) # Perform wilcox test on PC1  for male vs. female
wilcoxMF_PC2 <- wilcox.test(wilcox_prep_M$PC2, wilcox_prep_F$PC2, exact = FALSE) # Perform wilcox test on PC2  for male vs. female

#prepare boxplot of male and female PC1 values (to visualize the basis of the wilcoxon test)
PC1_2_MF <- pre_wilcox %>%
  filter(!is.na(SexGender)) %>%
  filter(SexGender != "ambiguous")
#boxplot PC1 and Sex (MF)
ggplot(PC1_2_MF, aes(SexGender, PC1))+
  geom_boxplot()+
  geom_jitter(width = 0.01, height = 0.01)
#plot PC2 and Sex (MF)
ggplot(PC1_2_MF, aes(SexGender, PC2))+
  geom_boxplot()+
  geom_jitter(width = 0.01, height = 0.01)

## Kruskal-Wallis rank sum test (comparing two or more groups, for age groups)
KruskWall_PCA <- data.frame(PC1 = all.pca$ind$coord[,1], PC2 = all.pca$ind$coord[,2], age_group = PCA_ready_all$ageGroup)
KruskWall_PCA <- filter(KruskWall_PCA, !is.na(KruskWall_PCA$age_group))

#Now prepare the raw meat variable for test with age groups
KruskWall_raw <- filter(PCA_ready_all, !is.na(ageGroup))

#boxplot age and PC1
ggplot(KruskWall_raw, aes(x = ageGroup, y = meat)) +
  geom_boxplot()+
  geom_jitter(width = 0.01, height = 0.01)
#boxplot age and PC1
ggplot(KruskWall_PCA, aes(x = age_group, y = PC1)) +
  geom_boxplot()+
  geom_jitter(width = 0.01, height = 0.01)
#boxplot age and PC2
ggplot(KruskWall_PCA, aes(x = age_group, y = PC2)) +
  geom_boxplot()+
  geom_jitter(width = 0.01, height = 0.01)

#Perform Kruskal-Wallis test on PCs first all groups, then adultus vs. the rest
kruskal.test(PC1 ~ age_group, data = KruskWall_PCA) #PC1 all age groups against each other
kruskal.test(PC2 ~ age_group, data = KruskWall_PCA) #PC2 all age groups against each other
KruskWall_nonAdultus <- filter(KruskWall_PCA, age_group != "adultus")
kruskal.test(PC2 ~ age_group, data = KruskWall_nonAdultus)

#prepare Kruskal Wallis test on adultus and maturus vs infans on PC1
KruskWall_adu_mat_vs_inf <- KruskWall_PCA
KruskWall_adu_mat_vs_inf <- filter(KruskWall_adu_mat_vs_inf, age_group != "juvenis")
KruskWall_adu_mat_vs_inf$adult_vs_subadult <- case_when(KruskWall_adu_mat_vs_inf$age_group == "adultus" |
                                                          KruskWall_adu_mat_vs_inf$age_group == "maturus" ~
                                         "adultus_maturus",
                                         KruskWall_adu_mat_vs_inf$age_group == "infans" ~ "infans")
kruskal.test(PC1 ~ adult_vs_subadult, data = KruskWall_adu_mat_vs_inf)

ggplot(KruskWall_adu_mat_vs_inf, aes(adult_vs_subadult, PC1))+
  geom_boxplot()+
  geom_jitter(width = 0.1)


###prepare Kruskal Wallis test on adultus vs the rest on PC2 used in the main paper:
KruskWall_adu_vs_juvenis <- KruskWall_PCA
KruskWall_adu_vs_juvenis <- filter(KruskWall_adu_vs_juvenis, age_group != "maturus")
KruskWall_adu_vs_juvenis <- filter(KruskWall_adu_vs_juvenis, age_group != "infans")
KW_adu_vs_juv_PC2 <- kruskal.test(PC2 ~ age_group, data = KruskWall_adu_vs_juvenis)

ggplot(KruskWall_adu_vs_juvenis, aes(age_group, PC2))+
  geom_boxplot()+
  geom_jitter(width = 0.1)




###Lav PC1 variance for hver gruppe, og sammenlign med random sample bagefter
adu_mat_PCA <- filter(KruskWall_adu_mat_vs_inf, adult_vs_subadult != "infans")
infans_PCA <- filter(KruskWall_adu_mat_vs_inf, adult_vs_subadult != "adultus_maturus")
var_adult <- var(adu_mat_PCA$PC1) #take variance of adult group
var_infans <- var(infans_PCA$PC1) #take variance of infans group
rat.obs = var_adult/var_infans #ratio of the variances for the two groups
shapiro.test(adu_mat_PCA$PC1) #adults are very clearly not normally distributed
shapiro.test(infans_PCA$PC1) #infans is more normally distributed than adults
var.test(adu_mat_PCA$PC1, infans_PCA$PC1) # the very different normality of the two groups
#makes the F-stat (and thus p-value) of the var-test doubtful
#So we compare the means of the two groups instead
d1 = adu_mat_PCA$PC1-mean(adu_mat_PCA$PC1);  var_adult = var(d1)
d2 = infans_PCA$PC1-mean(infans_PCA$PC1);  var_infans = var(d2)
rat.obs_means = var_adult/var_infans #ratio of the two groups when using the means
#ratio (both on normal variance of the two groups and on variance of mean-corrected groups)
#and F-stat of the var-test are the same
set.seed(0.1415)
rat.prm = replicate(1000, #make 10,000 var.tests on random samples of the data giving different F-stats
                    var.test(KruskWall_adu_mat_vs_inf$PC1~sample(KruskWall_adu_mat_vs_inf$adult_vs_subadult))$stat)
mean(rat.prm >= rat.obs_means) #gives p-value of one-sided permutation test

#plot the randomly sampled var.test F-stats (i.e. ratios) with the F-stat of the observed data
#This shows how likely the red line (actual F-stat) is under the null-hypothesis
#of no difference between the group variances
var_F_stat.p <- ggplot()+
  geom_density(aes(rat.prm))+
  geom_vline(aes(xintercept = rat.obs_means), color = "red")











########Testing the raw meat column agains age groups
#First prepare for Wilcoxon test (two groups only)
wilcox_age_raw <- KruskWall_raw
wilcox_age_raw$adultus_boolean <- case_when(wilcox_age_raw$ageGroup == "infans" |
                                           wilcox_age_raw$ageGroup == "juvenis" |
                                           wilcox_age_raw$ageGroup == "maturus" ~ "non-adultus",
                                         wilcox_age_raw$ageGroup == "adultus" ~ "adultus")

#choose only males and females (in separate objects)
wilcox_raw_adultus <- subset(wilcox_age_raw, adultus_boolean == c('adultus'))
wilcox_raw_non_adultus <- subset(wilcox_age_raw, adultus_boolean == c('non-adultus'))

wilcox_age_meat <- wilcox.test(wilcox_raw_adultus$meat, wilcox_raw_non_adultus$meat, exact = FALSE)

kruskal.test(meat ~ ageGroup, data = KruskWall_raw) #all age groups on raw meat

KruskWall_nonJuvenis_raw <- filter(KruskWall_raw, ageGroup != "juvenis")
kruskal.test(meat ~ ageGroup, data = KruskWall_nonJuvenis_raw)

#Test juvenis and adultus against each other
KruskWall_juven_adultus <- KruskWall_raw %>%
  filter(ageGroup != "maturus") %>%
  filter(ageGroup != "infans")
kruskal.test(meat ~ ageGroup, data = KruskWall_juven_adultus)

#Test meat column between all other groups but adultus
KruskWall_non_adultus <- KruskWall_raw %>%
  filter(ageGroup != "adultus")
KW_non_adultus <- kruskal.test(meat ~ ageGroup, data = KruskWall_non_adultus)

#Kruskal-Wallis test for meat between adultus and infans
KruskWall_adultus_infans <- KruskWall_raw %>%
  filter(ageGroup != "maturus") %>%
  filter(ageGroup != "juvenis")
kruskal.test(meat ~ ageGroup, data = KruskWall_adultus_infans)

#Kruskal-Wallis test for meat between maturus and juvenis
KruskWall_maturus_juvenis <- KruskWall_raw %>%
  filter(ageGroup != "adultus") %>%
  filter(ageGroup != "infans")
kruskal.test(meat ~ ageGroup, data = KruskWall_maturus_juvenis)

#Test all groups but juvenis against each other
KruskWall_nonJuvenis <- filter(KruskWall_raw, ageGroup != "juvenis")
kruskal.test(meat ~ ageGroup, data = KruskWall_nonJuvenis)

wilcox_age <- KruskWall_raw
#Wilcox test of adultus vs. non-adultus (in separate objects)
wilcox_age$age_combined <- case_when(KruskWall_raw$ageGroup == "infans" |
                                          KruskWall_raw$ageGroup == "juvenis" |
                                          KruskWall_raw$ageGroup == "maturus" ~ "non-adultus",
                                     KruskWall_raw$ageGroup == "adultus" ~ "adultus")

wilcox_adultus <- subset(wilcox_age, age_combined == c('adultus'))
wilcox_non_adultus <- subset(wilcox_age, age_combined == c('non-adultus'))
wilcox_age_adult <- wilcox.test(wilcox_adultus$meat, wilcox_non_adultus$meat, exact = FALSE)

#Wilcox test meat for maturus against adultus
wilcox_maturus <- subset(KruskWall_raw, ageGroup == c('maturus'))
wilcox_adultus <- subset(KruskWall_raw, ageGroup == c('adultus'))
wilcox_age_mat_adult <- wilcox.test(wilcox_maturus$meat, wilcox_adultus$meat, exact = FALSE)

#Wilcox test of juvenis vs. non-juvenis (in separate objects)
wilcox_age$age_comb_juv_non_juv <- case_when(KruskWall_raw$ageGroup == "infans" |
                                       KruskWall_raw$ageGroup == "adultus" |
                                       KruskWall_raw$ageGroup == "maturus" ~ "non-juvenis",
                                     KruskWall_raw$ageGroup == "juvenis" ~ "juvenis")

wilcox_juvenis <- subset(wilcox_age, age_comb_juv_non_juv == c('juvenis'))
wilcox_non_juvenis <- subset(wilcox_age, age_comb_juv_non_juv == c('non-juvenis'))
wilcox_age_juvenis <- wilcox.test(wilcox_juvenis$meat, wilcox_non_juvenis$meat, exact = FALSE)
