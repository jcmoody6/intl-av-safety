#title: International Comparison of AV Safety Summary Statistics and Bayesian MSEM Results
#author: Joanna Moody
#date: June 30, 2019

library(car)  #for recode function
library(dplyr)
library(reshape2) #for melt function
library(ggplot2)
library(scales)
library(maps)
library(mapproj)
library(data.table) #for setDT function
library(viridis)
library(grid)
library(gridExtra)
library(gtable)

#########################################
### Data preparation
#Load complete raw dataset
Data_raw <- read.csv("./ipf_weightsCap_July.csv")

#Exclude all observations from Taiwan, leaving 41932 observations (in 51 countries) of 149 variables
rows_TW <- which(Data_raw$iso == "TW")
Data_clean <- Data_raw[!(rownames(Data_raw)%in%rows_TW),]
rownames(Data_clean) <- 1:length(Data_clean$iso)

#Exclude observations with missing values for monthly household income or education level covariates
Data_clean$age <- 2016-Data_clean$yob

#Income: Categorical; used midpoint to proxy continuous; 6749 missing observations
rows_missinc <- which(is.na(Data_clean$inc)) #6749 observations
Data_clean <- Data_clean[!(rownames(Data_clean)%in%rows_missinc),]
rownames(Data_clean) <- 1:length(Data_clean$inc)

Data_clean$edu_high <- car::recode(Data_clean$edu,"c(0,1,2)=0; 3=1")
rows_missedu <- which(is.na(Data_clean$edu_high)) #1225 observations
Data_clean <- Data_clean[!(rownames(Data_clean)%in%rows_missedu),]
rownames(Data_clean) <- 1:length(Data_clean$edu_high)

# Cleaned dataset contains 33,958 obervations 
# having excluded a total of 7,974 observations with missing covariate valeus


#########################################
### AV Awareness
#q16 -- "Have you seen, heard, or read anything about self-driving cars?"
#1 = No
#2 = Yes, a bit
#3 = Yes, a lot

Data_clean$q16 <- as.factor(Data_clean$q16)
levels(Data_clean$q16) <- c("Not", "A bit", "Very")

graph1 <- subset(Data_clean, select=c("Country", "q16", "ipf_weights"))
graph1 <- dplyr::group_by(graph1, q16) %>% count()
graph1$perc <- (graph1$n/33958)*100
graph1$pos <- c(12.4, 52.7, 90.3)

p1 <- ggplot(graph1) +
  geom_bar(aes(x=1, y=perc, fill=forcats::fct_rev(q16)), stat="identity") +
  geom_text(size=4, aes(x=0.5, y=pos, label=round(perc,1))) +
  ggtitle("(a) Awareness of AV Technology") +
  labs(x="",y="Percent") +
  scale_fill_viridis(option="cividis", discrete=TRUE) +
  coord_cartesian(ylim = c(0,100)) +
  theme(legend.position="right", legend.title = element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_flip()
p1

#########################################
### AV Safety Perception
#q17 --	"How safe do you think self-driving cars are now?"
#1 = "Not safe at all" 
#2 = "Not very safe"
#3 = "Somewhat safe"
#4 = "Very safe"
#"NA" = "Not sure"

Data_clean$q17 <- as.factor(Data_clean$q17)
levels(Data_clean$q17) <- c("Not at all", "Not very", "Somewhat", "Very")
graph2 <- subset(Data_clean, select=c("Country", "q17", "ipf_weights"))

graph2 <- dplyr::group_by(graph2, q17) %>% count()
graph2$perc <- (graph2$n/33958)*100
graph2 <- graph2[order(graph2$q17),]
graph2$pos <- c(28.3, 43.7, 72.6, 94.8, 12.5)

p2 <- ggplot(graph2) +
  geom_bar(aes(x=1, y=perc, fill=forcats::fct_rev(q17)), stat="identity") + #color="black"
  geom_text(size=4, aes(x=0.5, y=pos, label=round(perc,1))) +
  ggtitle("(b) Current Perceptions of AV Safety")+
  labs(x="",y="Percent") +
  coord_cartesian(ylim = c(0,100)) +
  scale_fill_viridis(option="cividis", discrete=TRUE, na.value="#E8E8E8") +
  theme(legend.position="right", legend.title = element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_flip()
p2


#########################################
### AV Safety Prediction
#q18	AV safety prediction -- "How soon, if at all, do you think self-driving cars will be safe enough for you to consider using one?"
#100 ='Never'
#2 = 'Within the next 2 years'
#5 = 'Within the next 5 years'
#10 = 'Within the next 10 years'
#20 = 'Within the next 20 years'
#30 = 'More than 20 years'
#NA = "Don't know"

Data_clean$q18 <- car::recode(Data_clean$q18,"2='Within 2 years'; 5='2-5 years'; 
                              10='5-10 years'; 20='10-20 years'; 
                              30='More than 20 years'; 100='Never'")
#Data_clean$q18 <- car::recode(Data_clean$q18,"2=1; 5=3.5; 10=7.5; 20=15; 30=25; 100=50")
Data_clean$AV_VerySafe <- car::recode(Data_clean$q17, "c('Not at all', 'Not very', 'Somewhat')=0; c('Very')=1")
data.table::setDT(Data_clean)[AV_VerySafe == 1, q18 := 'Already very safe']
graph3 <- subset(Data_clean, select=c("Country", "q18", "ipf_weights"))

graph3 <- dplyr::group_by(graph3, q18) %>% count()
graph3$perc <- (graph3$n/33958)*100
graph3$q18 <- as.factor(graph3$q18)
graph3$q18 <- factor(graph3$q18, levels=c(NA, "Already very safe", "Within 2 years", "2-5 years", "5-10 years", "10-20 years", "More than 20 years", "Never"))
graph3 <- graph3[order(graph3$q18),]
graph3$pos <- c(24.4, 33.8, 48.3, 69.8, 85.3, 91.7, 96.9, 9.58)

p3 <- ggplot(graph3) +
  geom_bar(aes(x=1, y=perc, fill=forcats::fct_rev(q18)), stat="identity") + #color="black"
  geom_text(size=4, aes(x=0.5, y=pos, label=round(perc,1))) +
  ggtitle("(c) Years Until AVs will be Safe Enough to Use")+
  labs(x=" ",y="Percent") +
  coord_cartesian(ylim = c(0,100)) +
  scale_fill_viridis(option="cividis", discrete=TRUE, na.value="#E8E8E8") +
  theme(legend.position="right", legend.title = element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_flip()
p3

Data_clean$q18 <- car::recode(Data_clean$q18,"'Within 2 years'=1; '2-5 years'=3.5; 
                              '5-10 years'=7.5; '10-20 years'=15; 
                              'More than 20 years'=25; 'Never'=50;
                              'Already very safe'=0")
mean(Data_clean$q18, na.rm=TRUE)


#########################################
### Figure 2
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
grid.draw(rbind(g1, g2, g3, size = "last"))


#########################################
### Load Mplus Model Output File
FSCORES <- read.table("./FullModel_Bayes_pairwisecomp_200000.dat", header=FALSE)
colnames(FSCORES) <- c("Q16", "SAFEPERC", "SAFEPRED", "AGE", "GEN",
                       "EMPC", "LOG_ESS", "INC", "EDU_HIGH",
                       "CAROWN", "Q01D",
                       paste0("B_Q16", 1:70), "B_Q16_mean", "B_Q16_median", 
                       "B_Q16_stddev", "B_Q16_CIlow", "B_Q16_CIupp", 
                       paste0("B_PERC", 1:70), "B_PERC_mean", "B_PERC_median", 
                       "B_PERC_stddev", "B_PERC_CIlow", "B_PERC_CIupp",
                       paste0("B_PRED", 1:70), "B_PRED_mean", "B_PRED_median", 
                       "B_PRED_stddev", "B_PRED_CIlow", "B_PRED_CIupp",
                       "R_ID", "iso_num")

FSCORES_co <- unique(subset(FSCORES, select=c("iso_num", "B_Q16_mean", "B_Q16_stddev",
                                              "B_PERC_mean", "B_PERC_stddev",
                                              "B_PRED_mean", "B_PRED_stddev")))

#Load file with country names and remove Taiwan
Country_lookup <- read.csv("./Country-level data/Country_ISO_Lookup2.csv")
Country_lookup <- Country_lookup[!(rownames(Country_lookup)%in%
                                     which(Country_lookup$iso == "TW")),]
Country_lookup$iso_num <- as.numeric(Country_lookup$iso)
FSCORES_co <- merge(FSCORES_co, Country_lookup, by = "iso_num")


### Visualize Country Intercepts of the Three Outcomes
#https://stackoverflow.com/questions/30706124/ploting-the-world-map-in-r
WorldData <- map_data('world')
WorldData <- WorldData %>% filter(region != "Antarctica")
WorldData <- fortify(WorldData)

FSCORES_co$region <- FSCORES_co$Country


# Awareness
summary(FSCORES_co$B_Q16_mean)
m1 <- ggplot()
m1 <- m1 + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region),
                  fill="white", colour="#7f7f7f", size=0.1)
m1 <- m1 + geom_map(data=FSCORES_co, map=WorldData,
                  aes(fill=B_Q16_mean, map_id=region),
                  colour="#7f7f7f", size=0.1)
m1 <- m1 + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
m1 <- m1 + scale_fill_viridis(option="cividis", discrete=FALSE)
m1 <- m1 + labs(fill="", title = element_blank(), x="", y="") 
m1 <- m1 + ggtitle("(a) AV Awareness (country intercept)")
m1 <- m1 + theme_bw()
m1 <- m1 + theme(panel.border = element_blank(), rect = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank(),
               axis.ticks = element_blank(), panel.grid.major = element_blank())
m1

# Safety Perceptions
summary(FSCORES_co$B_PERC_mean)
m2 <- ggplot()
m2 <- m2 + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region),
                  fill="white", colour="#7f7f7f", size=0.1)
m2 <- m2 + geom_map(data=FSCORES_co, map=WorldData,
                  aes(fill=B_PERC_mean, map_id=region),
                  colour="#7f7f7f", size=0.1)
m2 <- m2 + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
m2 <- m2 + scale_fill_viridis(option="cividis", discrete=FALSE)
m2 <- m2 + labs(fill="", title = element_blank(), x="", y="") 
m2 <- m2 + ggtitle("(b) Current Perceptions of AV Safety (country intercept)")
m2 <- m2 + theme_bw()
m2 <- m2 + theme(panel.border = element_blank(), rect = element_blank(), 
               axis.text.x = element_blank(), axis.text.y = element_blank(),
               axis.ticks = element_blank(), panel.grid.major = element_blank())
m2 

### Safety Predictions
summary(FSCORES_co$B_PRED_mean)
m3 <- ggplot()
m3 <- m3 + geom_map(data=WorldData, map=WorldData,
                    aes(x=long, y=lat, group=group, map_id=region),
                    fill="white", colour="#7f7f7f", size=0.1)
m3 <- m3 + geom_map(data=FSCORES_co, map=WorldData,
                    aes(fill=B_PRED_mean, map_id=region),
                    colour="#7f7f7f", size=0.1)
m3 <- m3 + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90))
m3 <- m3 + scale_fill_viridis(option="cividis", discrete=FALSE, direction=-1)
m3 <- m3 + labs(fill="years", title = element_blank(), x="", y="")
m3 <- m3 + ggtitle("(c) Years until AVs are Safe Enough to Use (country intercept)")
m3 <- m3 + theme_bw()
m3 <- m3 + theme(panel.border = element_blank(), rect = element_blank(), 
                 axis.text.x = element_blank(), axis.text.y = element_blank(),
                 axis.ticks = element_blank(), panel.grid.major = element_blank())
m3 

grid.arrange(m1, m2, m3, nrow=3)

## Correlations of Country Intercepts and Country Covariates
FSCORES_co_sub <- subset(FSCORES_co, select = c("B_Q16_mean", "B_PERC_mean", "B_PRED_mean",
                                                "GDP_percap_PPP", "Gini_index", 
                                                "PassKm_perCap", "Motor_Rate",
                                                "RoadDeathRate", "PercRdDeath_4wheel", 
                                                "PercRdDeath_2wheel", "PercRdDeath_Ped"))
colnames(FSCORES_co_sub) <- c("AV awareness", "Current perceptions of AV safety", 
                              "Years until AVs are safe",
                              "GDP per capita, PPP", "Gini index",
                              "Pass-km by road per capita", "Motorization rate", 
                              "Road death rate", "PercRdDeath_4wheel", 
                              "PercRdDeath_2wheel", "PercRdDeath_Ped")
cor_df <- cor(FSCORES_co_sub, use = "pairwise.complete.obs", method = "pearson")
cor_df <- subset(cor_df, select=c("B_Q16_mean", "B_PERC_mean", "B_PRED_mean"))
write.csv(cor_df, "20190630_CountryCorrelations.csv")
#corrplot(cor_df)

# Save appendix table of country intercepts and rank for each of the three outcomes
FSCORES_app <- subset(FSCORES_co, select = c("Country", "B_Q16_mean", "B_PERC_mean", "B_PRED_mean"))
FSCORES_app$Q16_rank <- 52-round(rank(FSCORES_app$B_Q16_mean), 0)
FSCORES_app$PERC_rank <- 52-round(rank(FSCORES_app$B_PERC_mean), 0)
FSCORES_app$PRED_rank <- 52-round(rank(FSCORES_app$B_PRED_mean), 0)
FSCORES_app <- FSCORES_app[order(FSCORES_app$B_Q16_mean),] 
write.csv(FSCORES_app, "20190630_AppendixTab.csv")

