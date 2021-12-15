require(here) # for knitr to print out all used packages at the end of the RMarkdown
require(tidyverse) # for data manipulation
require(plotly) # for interactive plots
require(shiny) #for interactive plots
require(DescTools) #for Ginis
require(gglorenz) # for lorenz curves
#require(ggh4x) # for dual scale plots (gini vs. mean over time)


#Lausitz house sizes
Lausitz_houses <- read.csv(here("analysis/data/raw_data/house_size_data/house_sizes_MBA-LBA.csv"),
                           header = TRUE,
                           stringsAsFactors = TRUE,
                           sep = ";",
                           encoding = "UTF-8",
                           dec = '.')

Lausitz_houses$house_m2 <- round((Lausitz_houses$length_mean*Lausitz_houses$width_mean), 2)

Lausitz_houses_Germ <- Lausitz_houses[Lausitz_houses$Country== "Germany",]

#Make Gini with and without added 10 m2
house_lausitz_num <- unlist(Lausitz_houses_Germ$house_m2+10)
gini_house_lausitz_Germ <- round(Gini(house_lausitz_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
house_lausitz_num_raw <- unlist(Lausitz_houses_Germ$house_m2)
gini_house_lausitz_raw <- round(Gini(house_lausitz_num_raw, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)


hist_house_lausitz <- ggplot(Lausitz_houses, aes(house_m2))+
  geom_density()

box_house_lausitz <- ggplot(Lausitz_houses, aes(x = Country, y = house_m2))+
  geom_boxplot()+
  geom_jitter(width = 0.1)


LC_house_lausitz.p <- ggplot(Lausitz_houses, aes(house_m2))+
  stat_lorenz()+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("house size %")+
  geom_text(aes(0.2, 1, size = 36, label=paste("Gini ", as.name(gini_house_lausitz))))+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")

#Unetice houses
unetice_houses <- read.csv(here("analysis/data/raw_data/house_size_data/house_sizes_Unetice.csv"),
                   header = TRUE,
                   stringsAsFactors = TRUE,
                   sep = ";",
                   encoding = "UTF-8",
                   dec = '.')

unetice_houses$house_m2 <- round((unetice_houses$length_approx*unetice_houses$width_approx), 2)

house_unetice_num <- unlist(unetice_houses$house_m2+10)
gini_house_unetice <- round(Gini(house_unetice_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
house_unetice_num_raw <- unlist(unetice_houses$house_m2)
gini_house_unetice_raw <- round(Gini(house_unetice_num_raw, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)


hist_house_unetice <- ggplot(unetice_houses, aes(house_m2))+
  geom_density()

LC_house_unetice.p <- ggplot(unetice_houses, aes(house_m2))+
  stat_lorenz()+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("house size %")+
  geom_text(aes(0.2, 1, size = 36, label=paste("Gini ", as.name(gini_house_unetice))))+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")



#Bellbeaker house sizes
BBC_houses_raw <- read.csv(here("analysis/data/raw_data/house_size_data/house_sizes_BBC.csv"),
                             header = TRUE,
                             stringsAsFactors = TRUE,
                             sep = ";",
                             encoding = "UTF-8",
                             dec = '.')

BBC_houses <- data.frame(na.omit(BBC_houses_raw))
#BBC_houses <- data.frame(country = BBC_houses_raw$Country, width= na.omit(BBC_houses_raw$width_mean), length = na.omit(BBC_houses_raw$length_mean))

BBC_houses$house_m2 <- round((BBC_houses$length_mean*BBC_houses$width_mean), 2)
BBC_houses_cut <- BBC_houses[BBC_houses$Country!= "Italy",]


house_BBC_num <- unlist(BBC_houses_cut$house_m2+10)
gini_house_BBC <- round(Gini(house_BBC_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
house_BBC_num_raw <- unlist(BBC_houses_cut$house_m2)
gini_house_BBC_raw <- round(Gini(house_BBC_num_raw, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)


hist_house_BBC <- ggplot(BBC_houses_cut, aes(house_m2))+
  geom_density()

box_house_BBC <- ggplot(BBC_houses_cut, aes(x = Country, y = house_m2))+
  geom_boxplot()+
  geom_jitter(width = 0.1)

LC_house_BBC.p <- ggplot(BBC_houses, aes(house_m2))+
  stat_lorenz()+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("house size %")+
  geom_text(aes(0.2, 1, size = 36, label=paste("Gini ", as.name(gini_house_BBC))))+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")




# CWC houses
CWC_houses <- read.csv(here("analysis/data/raw_data/house_size_data/house_sizes_CWC.csv"),
                           header = TRUE,
                           stringsAsFactors = TRUE,
                           sep = ";",
                           encoding = "UTF-8",
                           dec = '.')

#First SGC/BAC (lumped as CWC) houses:
#house_size_CWC <- Scand_MN_houses[Scand_MN_houses$Culture == "CWC",]
house_CW_num <- unlist(CWC_houses$house_m2+10)
gini_house_CWC <- round(Gini(house_CW_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
house_CW_num_raw <- unlist(CWC_houses$house_m2)
gini_house_CWC_raw <- round(Gini(house_CW_num_raw, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)



#MN houses
MN_houses <- read.csv(here("analysis/data/raw_data/house_size_data/house_sizes_MLF_Neo.csv"),
                       header = TRUE,
                       stringsAsFactors = TRUE,
                       sep = ";",
                       encoding = "UTF-8",
                       dec = '.')

#Then late TRB houses (here named TRB III):
Scand_MN_house_cut <- MN_houses[MN_houses$Culture == "TRB",]
LN_house_cut <- MN_houses[grepl("KC-Mako|Schonfeld", MN_houses$Culture),]
GAC_house_cut <- MN_houses[grepl("GAC", MN_houses$Culture),]

Scand_MN_house_num <- unlist(Scand_MN_house_cut$house_m2+10)
gini_house_Scand_MN <- round(Gini(Scand_MN_house_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
Scand_MN_house_num_raw <- unlist(Scand_MN_house_cut$house_m2)
gini_house_Scand_MN_raw <- round(Gini(Scand_MN_house_num_raw, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)

GAC_house_num <- unlist(GAC_house_cut$house_m2+10)
gini_house_GAC <- round(Gini(GAC_house_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
GAC_house_num_raw <- unlist(GAC_house_cut$house_m2)
gini_house_GAC_raw <- round(Gini(GAC_house_num_raw, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)

LN_house_num <- unlist(LN_house_cut$house_m2+10)
gini_house_LN <- round(Gini(LN_house_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
LN_house_num_raw <- unlist(LN_house_cut$house_m2)
gini_house_LN_raw <- round(Gini(LN_house_num_raw, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)


#Scandinavian EN2 houses
Scand_EN_houses <- read.csv(here("analysis/data/raw_data/house_size_data/house_size_EN.csv"),
                            header = TRUE,
                            stringsAsFactors = TRUE,
                            sep = "\t",
                            encoding = "UTF-8",
                            dec = '.')

scand_EN_house_num <- unlist(Scand_EN_houses$house_m2+10)
gini_house_scand_EN <- round(Gini(scand_EN_house_num, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)
scand_EN_house_num_raw <- unlist(Scand_EN_houses$house_m2)
gini_house_scand_EN_raw <- round(Gini(scand_EN_house_num_raw, conf.level = 0.80, unbiased = TRUE, type = "bca", R = 4000), 3)


hist_house_scand_EN <- ggplot(Scand_EN_houses, aes(house_m2))+
  geom_density()


LC_house_scand_EN.p <- ggplot(Scand_EN_houses, aes(house_m2))+
  stat_lorenz()+
  geom_abline(color = "grey")+
  theme_bw()+
  xlab("Sample population %")+
  ylab("house size %")+
  geom_text(aes(0.2, 1, size = 36, label=paste("Gini ", as.name(gini_house_scand_EN))))+
  theme(axis.title = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "None")


#Merge Gini and mean data into one dataframe:
Gini_mean_lausitz_Germ <- data.frame(t(gini_house_lausitz_Germ), gini_original = gini_house_lausitz_raw[1], mean = mean(Lausitz_houses$house_m2))
Gini_mean_unetice <- data.frame(t(gini_house_unetice), gini_original = gini_house_unetice_raw[1], mean = mean(unetice_houses$house_m2))
Gini_mean_BBC <- data.frame(t(gini_house_BBC), gini_original = gini_house_BBC_raw[1], mean = mean(BBC_houses$house_m2))
Gini_mean_CWC <- data.frame(t(gini_house_CWC), gini_original = gini_house_CWC_raw[1], mean = mean(CWC_houses$house_m2))
Gini_mean_LN <- data.frame(t(gini_house_LN), gini_original = gini_house_LN_raw[1], mean = mean(LN_house_cut$house_m2))
Gini_mean_Scand_MN <- data.frame(t(gini_house_Scand_MN), gini_original = gini_house_Scand_MN_raw[1], mean = mean(Scand_MN_house_cut$house_m2))
Gini_mean_GAC <- data.frame(t(gini_house_GAC), gini_original = gini_house_GAC_raw[1], mean = mean(GAC_house_cut$house_m2))
Gini_mean_Scand_EN <- data.frame(t(gini_house_scand_EN), gini_original = gini_house_scand_EN_raw[1], mean = mean(Scand_EN_houses$house_m2))


Gini_house_merge <- rbind(Gini_mean_Scand_EN,
                          Gini_mean_Scand_MN,
                          Gini_mean_GAC,
                          Gini_mean_LN,
                          Gini_mean_CWC,
                          Gini_mean_BBC,
                          Gini_mean_unetice,
                          Gini_mean_lausitz_Germ)
Gini_house_merge$name <- c("EN",
                           "Scand MN",
                           "GAC",
                           "FN_CA",
                           "CWC",
                           "BBC N Europe",
                           "Unetice",
                           "Lausitz Germany")

Gini_house_merge$mean_date <- c(3600,
                                3000,
                                2900,
                                2600,
                                2400,
                                2200,
                                1950,
                                1300)

Gini_house_merge$gini_perc <- Gini_house_merge$gini*100
#defines primary and secondary axis in dual gini plot below:
#sec <- help_secondary(Gini_house_merge, primary = gini_perc, secondary = mean)

Gini_house_merge <- Gini_house_merge[ , c("name", "mean_date", "gini", "lwr.ci", "upr.ci", "gini_original", "mean", "gini_perc")]

#reorder culture names according to time for ggplot legend:
Gini_house_merge$name  <- with(Gini_house_merge, reorder(name, rev(mean_date)))

row.names(Gini_house_merge) <- NULL

#make the dual gini plot:
dual_ginis <- ggplot(Gini_house_merge, aes(mean_date))+
  geom_line(aes(y = gini_perc, group = "Gini"), linetype = 3)+
  geom_point(aes(y = gini_perc, color = name), size = 2)+
  geom_line(aes(y = mean), group = "mean", linetype = 3)+
  geom_point(aes(y = mean, color = name), size = 2, shape = 18)+
  labs(x= "Mean date", y= expression(paste("Gini ",x10^2, " (bottom), house size (",m^2,") mean (top)")))+
  theme(axis.text.y = element_text(size = 7),
        legend.title = element_text(size=10))+
  guides(color = guide_legend(title="Culture/Period"))+
  #geom_line(aes(y = sec$proj(mean)), color = "red")+
  #geom_point(aes(y = sec$proj(mean), color = "red"))+
  #scale_y_continuous(sec.axis = sec, limits = c(15, 40))+
  scale_y_continuous(limits = c(5, 170), breaks = c(5, 10, 15, 25, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 170))+
  scale_x_reverse(limits =c(3600,1200), breaks = c(3600, 3000, 2400, 2200, 1950, 1300))

house_gini.p <- ggplotly(dual_ginis, showlegend = T)

#Merge raw data into one dataframe for comparable house_m2 by culture boxplot:
Lausitz_house_cut <- Lausitz_houses[c("Period", "Culture", "house_m2", "Country")]
Unetice_house_cut <- unetice_houses[c("Period", "Culture", "house_m2", "Country")]
BBC_house_cut <- BBC_houses_cut[c("Period", "Culture", "house_m2", "Country")]
Scand_MN_house_cut2 <- Scand_MN_house_cut[c("Period", "Culture", "house_m2", "Country")]
LN_house_cut2 <- LN_house_cut[c("Period", "Culture", "house_m2", "Country")]
CWC_house_cut <- CWC_houses[c("Period", "Culture", "house_m2", "Country")]
Scand_EN_house_cut <- Scand_EN_houses[c("Period", "Culture", "house_m2", "Country")]
GAC_house_cut2 <- GAC_house_cut[c("Period", "Culture", "house_m2", "Country")]

house_size_merge <- rbind(Scand_EN_house_cut,
                          Scand_MN_house_cut2,
                          GAC_house_cut2,
                          LN_house_cut2,
                          CWC_house_cut,
                          BBC_house_cut,
                          Unetice_house_cut,
                          Lausitz_house_cut)



house_size_merge$Period =
  factor(house_size_merge$Period, c("EN", "Scand MN", "FN_CA", "GAC", "CWC", "BBC", "EBA", "MBA-LBA"))

levels(house_size_merge$Period)

levels(house_size_merge$Culture)

Culture_shape <- c()

house_m2_box <- ggplot(house_size_merge, aes(Period, house_m2))+
  geom_boxplot()+
  geom_jitter(width = 0.1, aes(colour = Country, shape = Culture))+
  scale_shape_manual(values=c(21, 22, 23, 24, 25, 8, 9, 3))+
  theme(legend.title = element_text(size=10),
        legend.key.height = unit(0.4, 'cm'))+
  ylab("House plan area (m2)")

house_box.p <- ggplotly(house_m2_box)

