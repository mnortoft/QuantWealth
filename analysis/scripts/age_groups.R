require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(dplyr) # for data manipulation
require(plotly) # for interactive plots
require(kableExtra)

here()
source(here('analysis/scripts/prestigeObjectsBoxPlots.R'), echo = FALSE)

age_Group_freq <- CW_raw #takes the raw grave data table loaded in the prestigeobjectsboxplot script

age_Group_freq$ageGroup_simple <- case_when(age_Group_freq$ageGroup == "maturus" |
                                              age_Group_freq$ageGroup == "maturus I" |
                                              age_Group_freq$ageGroup == "maturus II" ~ "maturus",
                                            age_Group_freq$ageGroup == "adultus" |
                                              age_Group_freq$ageGroup == "adultus I" |
                                              age_Group_freq$ageGroup == "adultus II" ~ "adultus",
                                            age_Group_freq$ageGroup == "juvenis" ~ "juvenis",
                                            age_Group_freq$ageGroup == "infans" |
                                              age_Group_freq$ageGroup == "infans I" |
                                              age_Group_freq$ageGroup == "infans II" |
                                              age_Group_freq$ageGroup == "infans III" ~ "infans")

SexAge <- data.frame(SexGender = age_Group_freq$SexGender, age_group = age_Group_freq$ageGroup_simple)
Age_stats <- data.frame(age_group = age_Group_freq$ageGroup_simple)

SexAge.tab <- table(SexAge)
sex_age.chisq <- chisq.test(SexAge.tab)

Age.tab <- table(Age_stats)
age_chisq <- chisq.test(Age.tab)
Age.prop <- prop.table(Age.tab)
age.propDF <- data.frame(Age.prop)
age.propDF$Age_stats <- factor(age.propDF$Age_stats, levels=c("infans", "juvenis", "adultus", "maturus"))
SexAge.prop <- prop.table(SexAge.tab)

Age.tabDF <- data.frame(Age.tab)
SexAge.propDF <- data.frame(SexAge.prop)

SexAge.propDF$age_group <- factor(SexAge.propDF$age_group, levels=c("infans", "juvenis", "adultus", "maturus"))
#levels(SexAge.propDF$age_group) <- factor(c("infans", "juvenis", "adultus", "maturus"))

#plot age and sex groups of sample population
SexAge.p <- ggplot(SexAge.propDF, aes(age_group, (Freq*100), fill = SexGender))+
  geom_col(position = "dodge")+
  scale_y_continuous(limits = c(0,30), breaks = c(0, 5, 10, 15, 20, 25, 30))+
  labs(x = "Sex-Gender and age group", y = "Percentage")+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 8))

#plot age groups of sample population
Age.p <- ggplot(age.propDF, aes(Age_stats, (Freq*100)))+
  geom_col()+
  #scale_y_continuous(limits = c(0,45), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45))+
  labs(x = "Age group", y = "Percentage")+
  theme(axis.text = element_text(size = 6), axis.title = element_text(size = 8))


#make a dataframe with frequency of meat or no meat for each age group
#(ageGroup NAs not included, so three individuals ignored here)
has_meat <- age_Group_freq[c("ageGroup_simple", "unworkedAnimal_spec")] %>%
  filter(!is.na(unworkedAnimal_spec))
has_meat.tab <- data.frame(has_meat = as.factor(table(has_meat$ageGroup_simple)))
has_meat.tab$ageGroup <- as.factor(rownames(has_meat.tab))

no_meat <- age_Group_freq[c("ageGroup_simple", "unworkedAnimal_spec")] %>%
  filter(is.na(unworkedAnimal_spec))

no_meat.tab <- data.frame(no_meat = as.factor(table(no_meat$ageGroup_simple)))
no_meat.tab$ageGroup <- as.factor(rownames(no_meat.tab))
age_meat <- merge(has_meat.tab, no_meat.tab, by="ageGroup")

age_meat.t <- data.frame(t(age_meat))
names(age_meat.t) <- age_meat.t[1,]
age_meat.t <- age_meat.t[-1,]

age_meat.t$infans <- as.numeric(as.character(age_meat.t$infans))
age_meat.t$juvenis <- as.numeric(as.character(age_meat.t$juvenis))
age_meat.t$adultus <- as.numeric(as.character(age_meat.t$adultus))
age_meat.t$maturus <- as.numeric(as.character(age_meat.t$maturus))

chisq_all <- chisq.test(age_meat.t) #test significance between all groups
chisq_non_adult <- chisq.test(age_meat.t[,c(2:4)])#test significance of all groups but adultus
fisher_all <- fisher.test(age_meat.t)
fish_non_adult <- fisher.test(age_meat.t[,c(2:4)])
age_meat_simple <- data.frame(adultus = age_meat.t$adultus, non_adultus = rowSums(age_meat.t[,c(2:4)]))
chisq_adult_rest <- chisq.test(age_meat_simple)
fisher.test(age_meat_simple)
age_meat.prop <- data.frame((age_meat.t[1,]/(age_meat.t[1,]+age_meat.t[2,]))*100)
rownames(age_meat.prop) <- c("meat_percent")
age_meat_tab_all <- rbind(age_meat.t, age_meat.prop)

