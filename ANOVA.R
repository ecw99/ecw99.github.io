getwd()
setwd('/Users/ethan/Desktop/streetmatrix/data')
install.packages("tidyverse")
install.packages("readr")
library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# data processing: The orginal csv (downloaded from red)

df <- read_csv("CoolPavementSurvey_Edit.csv")

df <- df %>%
  rename(SunShade = `Is this surface in the sun, or in the shade?`,Surface_Comp = `Surface Composition`)
  

df <- mutate(df, TempMean = rowMeans(select(df, starts_with("Temperature")), na.rm = TRUE))



df$`Survey Timestamp`<- round(as.POSIXct(df$`Survey Timestamp`, format= "%m/%d/%Y %H:%M"), units="hours")

df <- df %>% 
  mutate(hours=hour(strptime(`Survey Timestamp`, '%Y-%m-%d %H:%M:%S')) %>% as.integer() )

df <- df %>% # add early morning (before 9am)
  mutate( 
         TimeofDay  = case_when(between(hours, 6, 9) ~ "Early Morning", 
                            between(hours,9,12) ~ "Morning",
                            between(hours, 12, 17) ~"Afternoon")) 
                         
                           

surface_profiles <- group_by(df,Surface_Comp,TimeofDay,SunShade)


surface_profilesmeans <- summarise(surface_profiles,S_mean = mean(TempMean),sc_se = sd(TempMean)/sqrt(n()), count = n())
head(surface_comp)


surface_profiles_SunShade <- group_by(df,Surface_Comp,SunShade)

surface_profiles_SunShademeans <- summarise(surface_profiles_SunShade,S_mean = mean(TempMean),sc_se = sd(TempMean)/sqrt(n()), count = n())

surface_profiles_hours <- group_by(df,Surface_Comp,hours)
surface_profiles_hoursmeans <- summarise(surface_profiles_hours,S_mean = mean(TempMean),sc_se = sd(TempMean)/sqrt(n()), count = n())


df %>% ggplot(aes(x = Surface_Comp, y = TempMean)) + 
  geom_point(aes(colour = Surface_Comp), show.legend = FALSE)+
  labs(x = "Surface Composition")+
  labs(y = "Surface Temperature (F)")+
  labs(title = "Raw Surface Temperature")+
  theme(plot.title = element_text(hjust = 0.5))
 
surface_profiles_SunShademeans %>% ggplot(aes(x = Surface_Comp, y = S_mean)) + 
  geom_point(aes(colour =  SunShade), show.legend = TRUE)+
  geom_text(aes(label=  count, colour = SunShade),hjust=-1, vjust=0,, show.legend = FALSE)+
  scale_color_manual(values=c("blue", "red", "black"))+
  geom_errorbar(aes(ymin=S_mean-sc_se, ymax= S_mean+sc_se, colour =  SunShade), width=.4,
                position=position_dodge(0.01))+
  labs(x = "Surface Composition")+
  labs(y = "Surface Temperature (F)")+
  labs(title = "Shade and Surface Temperature",color = "Legend")+
  theme(plot.title = element_text(hjust = 0.5))

surface_profiles_hoursmeans %>% ggplot(aes(x = hours, y = S_mean,group = Surface_Comp, color = Surface_Comp)) + 
  geom_point(aes(colour = Surface_Comp), show.legend = FALSE)+
  geom_line()+
  labs(x = "Time")+
  labs(y = "Surface Temperature (F)")+
  labs(title = "Surface Temperature Over Time",color = "Surface")+
  theme(plot.title = element_text(hjust = 0.5))

# convert for ANOVA
stats_df <- df %>% 
  mutate(Surface_Comp = factor(Surface_Comp, 
                                 levels = c("Road not treated", "Concrete/Sidewalk", "Gravel/Dirt","Grass")),
         SunShade = factor(SunShade, 
                      labels = c("Sun", "Shade","Partial Shade")),
         TimeofDay = factor(TimeofDay, 
                                levels = c("Morning", "Afternoon")))
#ANOVA

ad_aov <- aov(TempMean ~ Surface_Comp* TimeofDay* SunShade,data = stats_df)

summary(ad_aov)

tidy_ad_aov <- tidy(ad_aov)

#Post hoc

ad_pairwise <- tidy(pairwise.t.test(surface_profiles$TempMean,surface_profiles$`Surface Composition`:surface_profiles$TimeofDay:surface_profiles$SunShade, 
                               p.adj = "none"))
tidy_ad_tukey <- tidy(TukeyHSD(ad_aov, which = 'Surface_Comp:TimeofDay:SunShade'))

summary(tidy_ad_tukey)
