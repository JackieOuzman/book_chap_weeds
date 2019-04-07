

library(tidyverse)
library(dplyr)
library(ggplot2)
#install.packages('DT')
library(DT)
library(readxl)
#test
#setwd("W:/Weeds/book_chapter_data")
setwd("C:/Users/ouz001/book_chapter_data")


#postcodes_with_coord <- read.csv("../Australian_Post_Codes_Lat_Lon.csv")

#postcodes_with_coord_dist <- distinct(postcodes_with_coord, postcode, .keep_all = TRUE)
#write.csv(postcodes_with_coord_dist, "postcodes_with_coord_dist.csv")




#bring in the PA study

advisors_PA <- read.csv("C:/Users/ouz001/book_chapter_data/PA/PA_advisors_adoption.csv")

advisors_PA <- select(advisors_PA,
                    farmers = Respondents,
                    GRDC_RegionQ3,
                    state,
                    region_ReZoned,
                    X12postcodes)
glimpse(advisors_PA)

#Try bring in more data from orginal study
PA_survey <- read_excel("C:/Users/ouz001/book_chapter_data/PA/PA_data_rawish.xlsx")
#SELECT JUST A  FEW VARAIABLES FROM SURVEY
PA_survey <- select(PA_survey,
                    farmers = Respondents,
                    Agro_Yr_StartQ34,
                    Yr_No_Till_PA = NoTill_YrQ20,
                    Yr_AutoSteer = Asteer_YrQ46,
                    Yr_yld_map = Ymap_StartYearQ54,
                    Yr_soil_testing = SoilTest_StartYearQ75) 
                    
#JOIN THESE TWO TOGETHER
glimpse(advisors_PA)
glimpse(PA_survey)

PA_survey = left_join(advisors_PA, PA_survey, by = "farmers")
glimpse(PA_survey)

#NOW GET THE ZONE AND REGIONS - so I need a file that has farmer

XYadvisors_postcodes_join <- read.csv("C:/Users/ouz001/book_chapter_data/adoption_data/XYadvisors_postcodes_join.csv") %>% 
  select(farmers = farmer,
         postcode,
         study,
         AGROECOLOG,
         REGIONS,
         state
         )
glimpse(XYadvisors_postcodes_join)

##JOIN TO THE PA SURVEY ####

glimpse(XYadvisors_postcodes_join)
glimpse(PA_survey)


PA_survey_zone = left_join(XYadvisors_postcodes_join, PA_survey, by = "farmers") %>% 
  select(farmers,
         postcode,
         study,
         state = state.x,
         AGROECOLOG,
         REGIONS,
         Agro_Yr_StartQ34,
         Yr_No_Till_PA,
         Yr_AutoSteer,
         Yr_yld_map,
         Yr_soil_testing)

glimpse(PA_survey_zone)
PA_survey_zone <- mutate(PA_survey_zone, 
                         Yr_Agro =as.factor(Agro_Yr_StartQ34),
                         Yr_No_Till_PA=as.factor(Yr_No_Till_PA),
                         Yr_AutoSteer_PA=as.factor(Yr_AutoSteer),
                         Yr_yld_map_PA=as.factor(Yr_yld_map),
                         Yr_soil_testing_PA=as.factor(Yr_soil_testing))


###I THINK THIS IS THE DATA SET I NEED TO USE#####

####PA_survey_zone#####


###NOW I WANT TO CHECK OUT HOW THIS THESE adoption curves compare###
#use data clm called PA_survey_zone
# use zone as states
#number of farmers in state
# year_as_factor is the adoption clm I want to use
#glimpse(PA_survey_zone)
#test <- count(PA_survey_zone, Yr_Agro)

fun_test2 <- function(df, zone, numb_farmers) {
  df_subset <- filter(df, state == zone)
  count_adoption_year <- count(df_subset,Yr_Agro)
  count_adoption_year <- select(count_adoption_year, year = Yr_Agro, n) #this clm year is a factor
  years_of_study <- data.frame(year = 1950:2014, #this is int 
                               id = 1:65,
                               zone = zone)
  years_of_study <- mutate(years_of_study, year = as.factor(year))
  
  adoption_df <- left_join(years_of_study, count_adoption_year, by= "year" )
  adoption_df1 <- mutate(adoption_df, n = replace_na(adoption_df$n, 0))
  adoption_df2 <- mutate(adoption_df1, cummulative = cumsum(adoption_df1$n))
  adoption_df3 <- mutate(adoption_df2, cumm_percent = (adoption_df2$cummulative/numb_farmers)*100)
}

###PA Advisors BY state######
###PA data for agro use - chcek to see if it looks the same as other work - yes so far so good.
PA_AgroNSW <- fun_test2(PA_survey_zone, "NSW", 105)
PA_AgroSA <- fun_test2(PA_survey_zone, "SA", 186)
PA_AgroVIC <- fun_test2(PA_survey_zone, "VIC", 150)
PA_AgroWA <- fun_test2(PA_survey_zone, "WA", 128)
PA_Agro_state <- rbind(PA_AgroNSW,
                       PA_AgroSA,
                       PA_AgroVIC,
                       PA_AgroWA) %>% 
  mutate(year = as.integer(year))
glimpse(PA_Agro_state)



ggplot(PA_Agro_state, aes(year, cumm_percent))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)+
  labs(x = "Years",
       y = "percenatge of farmers",
       title = "Adoption of advisor use per states",
       subtitle = "This study has no farmers in QLD")



###PA Yr_No_Till_PA BY state######


fun_test3 <- function(df, zone, numb_farmers) {
  df_subset <- filter(df, state == zone)
  count_adoption_year <- count(df_subset,Yr_No_Till_PA)
  count_adoption_year <- select(count_adoption_year, year = Yr_No_Till_PA, n) #this clm year is a factor
  years_of_study <- data.frame(year = 1950:2014, #this is int 
                               id = 1:65,
                               zone = zone)
  years_of_study <- mutate(years_of_study, year = as.factor(year))
  
  adoption_df <- left_join(years_of_study, count_adoption_year, by= "year" )
  adoption_df1 <- mutate(adoption_df, n = replace_na(adoption_df$n, 0))
  adoption_df2 <- mutate(adoption_df1, cummulative = cumsum(adoption_df1$n))
  adoption_df3 <- mutate(adoption_df2, cumm_percent = (adoption_df2$cummulative/numb_farmers)*100)
}


Yr_No_Till_PANSW <- fun_test3(PA_survey_zone, "NSW", 105)
Yr_No_Till_PASA <- fun_test3(PA_survey_zone, "SA", 186)
Yr_No_Till_PAVIC <- fun_test3(PA_survey_zone, "VIC", 150)
Yr_No_Till_PAWA <- fun_test3(PA_survey_zone, "WA", 128)
Yr_No_Till_PA_state <- rbind(Yr_No_Till_PANSW,
                             Yr_No_Till_PASA,
                             Yr_No_Till_PAVIC,
                             Yr_No_Till_PAWA) %>% 
  mutate(year = as.integer(year))
glimpse(Yr_No_Till_PA_state)

ggplot(Yr_No_Till_PA_state, aes(year, cumm_percent))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)



###put the no till data from PA survey over the advisor PA data###
glimpse(PA_Agro_state)
glimpse(Yr_No_Till_PA_state)

PA_Agro_state <- mutate(PA_Agro_state, adoption = "advisors")
Yr_No_Till_PA_state <- mutate(Yr_No_Till_PA_state, adoption = "No_till_PA")

PA_no_till_advisors <- rbind(PA_Agro_state, Yr_No_Till_PA_state )

ggplot(PA_no_till_advisors, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)
##this says its the same as the no till study in 2014



###OK for the 2014 weeds adoption data### no till


XYNoTill_postcodes_join <- read.csv("C:/Users/ouz001/book_chapter_data/adoption_data/XYNoTill_postcodes_join_GRDC_SLA.csv")

#change the year from interger to factor
XYNoTill_postcodes_join <- mutate(XYNoTill_postcodes_join, year_as_factor = as.factor(Q15_year_f))
glimpse(XYNoTill_postcodes_join)

adoption_curve_function_NoTill_S <- function(df, zone, numb_farmers) {
  df_subset <- filter(df, state == zone)
  count_adoption_year <- count(df_subset,year_as_factor)
  count_adoption_year <- select(count_adoption_year, year = year_as_factor, n)
  years_of_study <- data.frame(year = 1950:2014, 
                               id = 1:65,
                               zone = zone)
  years_of_study <- mutate(years_of_study, year = as.factor(year))
  adoption_df <- left_join(years_of_study, count_adoption_year, by= "year" )
  adoption_df <- mutate(adoption_df, n = replace_na(adoption_df$n, 0))
  adoption_df <- mutate(adoption_df, cummulative = cumsum(adoption_df$n))
  adoption_df <- mutate(adoption_df, cumm_percent = (adoption_df$cummulative/numb_farmers)*100)
  adoption_df$year <- as.double(adoption_df$year)
  
  #return(df_subset)
  return(adoption_df)
}
QLD_NT_weeds <- adoption_curve_function_NoTill_S(XYNoTill_postcodes_join, "QLD", 59)
NSW_NT_weeds <- adoption_curve_function_NoTill_S(XYNoTill_postcodes_join, "NSW", 153)
SA_NT_weeds <- adoption_curve_function_NoTill_S(XYNoTill_postcodes_join, "SA", 65)
VIC_NT_weeds <- adoption_curve_function_NoTill_S(XYNoTill_postcodes_join, "VIC", 141)
WA_NT_weeds <- adoption_curve_function_NoTill_S(XYNoTill_postcodes_join, "WA", 179)

NoTill_adoption_state_NT_weeds <- rbind(QLD_NT_weeds, NSW_NT_weeds, SA_NT_weeds,VIC_NT_weeds, WA_NT_weeds ) 
NoTill_adoption_state_NT_weeds <- mutate(NoTill_adoption_state_NT_weeds, adoption = "No_till_Weeds")

####Join adoption data for weeds - no till , PA no till and advisors
glimpse(PA_no_till_advisors)
glimpse(NoTill_adoption_state_NT_weeds)
PA_no_till_advisors_weeds_noTill <- rbind(PA_no_till_advisors, NoTill_adoption_state_NT_weeds)


ggplot(PA_no_till_advisors_weeds_noTill, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)

ggsave("1Adoption_no_till_and_advisor_PA_No_till_weeds.png", width = 9.8, height = 5.6, units = "in")

glimpse(PA_survey_zone)





###PA Autosteer  Yr_AutoSteer_PA
fun_test4 <- function(df, zone, numb_farmers) {
  df_subset <- filter(df, state == zone)
  count_adoption_year <- count(df_subset,Yr_AutoSteer_PA)
  count_adoption_year <- select(count_adoption_year, year = Yr_AutoSteer_PA, n) #this clm year is a factor
  years_of_study <- data.frame(year = 1950:2014, #this is int 
                               id = 1:65,
                               zone = zone)
  years_of_study <- mutate(years_of_study, year = as.factor(year))
  
  adoption_df <- left_join(years_of_study, count_adoption_year, by= "year" )
  adoption_df1 <- mutate(adoption_df, n = replace_na(adoption_df$n, 0))
  adoption_df2 <- mutate(adoption_df1, cummulative = cumsum(adoption_df1$n))
  adoption_df3 <- mutate(adoption_df2, cumm_percent = (adoption_df2$cummulative/numb_farmers)*100)
}


###PA Autosteer

Yr_AutoSteer_PANSW <- fun_test4(PA_survey_zone, "NSW", 105)
Yr_AutoSteer_PASA <- fun_test4(PA_survey_zone, "SA", 186)
Yr_AutoSteer_PAVIC <- fun_test4(PA_survey_zone, "VIC", 150)
Yr_AutoSteer_PAWA <- fun_test4(PA_survey_zone, "WA", 128)
Yr_AutoSteer_PA_state <- rbind(Yr_AutoSteer_PANSW,
                               Yr_AutoSteer_PASA,
                               Yr_AutoSteer_PAVIC,
                               Yr_AutoSteer_PAWA) %>% 
  mutate(year = as.integer(year))
glimpse(Yr_No_Till_PA_state)
Yr_AutoSteer_PA_state <- mutate(Yr_AutoSteer_PA_state, adoption = "PA_Auto_steer")

glimpse(Yr_AutoSteer_PA_state)
glimpse(NoTill_adoption_state_NT_weeds)
Yr_AutoSteer_PA_weeds_noTill <- rbind(Yr_AutoSteer_PA_state, NoTill_adoption_state_NT_weeds, PA_Agro_state)


ggplot(Yr_AutoSteer_PA_weeds_noTill, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)

ggsave("Yr_AutoSteer_PA_weeds_noTill.png", width = 9.8, height = 5.6, units = "in")


Yr_AutoSteer_PA_Agro <- rbind(Yr_AutoSteer_PA_state, PA_Agro_state)


ggplot(Yr_AutoSteer_PA_Agro, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)

ggsave("Yr_AutoSteer_PA_Agro.png", width = 9.8, height = 5.6, units = "in")







###Yr_yld_map
glimpse(PA_survey_zone)
fun_test5 <- function(df, zone, numb_farmers) {
  df_subset <- filter(df, state == zone)
  count_adoption_year <- count(df_subset,Yr_yld_map_PA)
  count_adoption_year <- select(count_adoption_year, year = Yr_yld_map_PA, n) #this clm year is a factor
  years_of_study <- data.frame(year = 1950:2014, #this is int 
                               id = 1:65,
                               zone = zone)
  years_of_study <- mutate(years_of_study, year = as.factor(year))
  
  adoption_df <- left_join(years_of_study, count_adoption_year, by= "year" )
  adoption_df1 <- mutate(adoption_df, n = replace_na(adoption_df$n, 0))
  adoption_df2 <- mutate(adoption_df1, cummulative = cumsum(adoption_df1$n))
  adoption_df3 <- mutate(adoption_df2, cumm_percent = (adoption_df2$cummulative/numb_farmers)*100)
}


###PA Yr_yld_map

Yr_yld_map_PANSW <- fun_test5(PA_survey_zone, "NSW", 105)
Yr_yld_map_PASA <- fun_test5(PA_survey_zone, "SA", 186)
Yr_yld_map_PAVIC <- fun_test5(PA_survey_zone, "VIC", 150)
Yr_yld_map_PAWA <- fun_test5(PA_survey_zone, "WA", 128)
Yr_yld_map_PA_state <- rbind(Yr_yld_map_PANSW,
                             Yr_yld_map_PASA,
                             Yr_yld_map_PAVIC,
                             Yr_yld_map_PAWA) %>% 
  mutate(year = as.integer(year))
glimpse(Yr_yld_map_PA_state)
Yr_yld_map_PA_state <- mutate(Yr_yld_map_PA_state, adoption = "PA_Yld_map")

glimpse(Yr_yld_map_PA_state)
glimpse(NoTill_adoption_state_NT_weeds)
Yr_yld_map_PA_weeds_noTill <- rbind(Yr_yld_map_PA_state, NoTill_adoption_state_NT_weeds)


ggplot(Yr_yld_map_PA_weeds_noTill, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)

ggsave("Yr_yld_map_PA_weeds_noTill.png", width = 9.8, height = 5.6, units = "in")

Yr_yld_map_PA_Agro_state <- rbind(Yr_yld_map_PA_state, PA_Agro_state)
ggplot(Yr_yld_map_PA_Agro_state, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)

ggsave("Yr_yld_map_PA_Agro_state.png", width = 9.8, height = 5.6, units = "in")




###Yr_soil_testing_PA
glimpse(PA_survey_zone)
fun_test6 <- function(df, zone, numb_farmers) {
  df_subset <- filter(df, state == zone)
  count_adoption_year <- count(df_subset,Yr_soil_testing_PA)
  count_adoption_year <- select(count_adoption_year, year = Yr_soil_testing_PA, n) #this clm year is a factor
  years_of_study <- data.frame(year = 1950:2014, #this is int 
                               id = 1:65,
                               zone = zone)
  years_of_study <- mutate(years_of_study, year = as.factor(year))
  
  adoption_df <- left_join(years_of_study, count_adoption_year, by= "year" )
  adoption_df1 <- mutate(adoption_df, n = replace_na(adoption_df$n, 0))
  adoption_df2 <- mutate(adoption_df1, cummulative = cumsum(adoption_df1$n))
  adoption_df3 <- mutate(adoption_df2, cumm_percent = (adoption_df2$cummulative/numb_farmers)*100)
}


###PA Yr_soil_testing_PA

Yr_soil_testing_PANSW <- fun_test6(PA_survey_zone, "NSW", 105)
Yr_soil_testing_PASA <- fun_test6(PA_survey_zone, "SA", 186)
Yr_soil_testing_PAVIC <- fun_test6(PA_survey_zone, "VIC", 150)
Yr_soil_testing_PAWA <- fun_test6(PA_survey_zone, "WA", 128)
Yr_soil_testing_PA_state <- rbind(Yr_soil_testing_PANSW,
                                  Yr_soil_testing_PASA,
                                  Yr_soil_testing_PAVIC,
                                  Yr_soil_testing_PAWA) %>% 
  mutate(year = as.integer(year))
glimpse(Yr_soil_testing_PA_state)
Yr_soil_testing_PA_state <- mutate(Yr_soil_testing_PA_state, adoption = "PA_soil_test")

glimpse(Yr_soil_testing_PA_state)
glimpse(NoTill_adoption_state_NT_weeds)
Yr_soil_testing_PA_state_weeds_noTill <- rbind(Yr_soil_testing_PA_state, NoTill_adoption_state_NT_weeds)


ggplot(Yr_soil_testing_PA_state_weeds_noTill, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)

ggsave("Yr_soil_testing_PA_weeds_noTill.png", width = 9.8, height = 5.6, units = "in")




Yr_soil_testing_PA_Agro_state <- rbind(Yr_soil_testing_PA_state, PA_Agro_state)
ggplot(Yr_soil_testing_PA_Agro_state, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)

ggsave("Yr_soil_testing_PA_Agro_state.png", width = 9.8, height = 5.6, units = "in")











###OK for the 2014 weeds adoption data### no till


XYNoTill_postcodes_join <- read.csv("C:/Users/ouz001/book_chapter_data/adoption_data/XYNoTill_postcodes_join_GRDC_SLA.csv") %>% 
  select(farmer,
         postcode,
         study,
         AGROECOLOG,
         REGIONS,
         state)
glimpse(XYNoTill_postcodes_join)
crop_top <- read_excel("C:/Users/ouz001/book_chapter_data/Weeds/Raw_data_Weeds_with_postcodes.xlsx")%>% 
            select(KEY,
                   Q20l1)

crop_top <- mutate(crop_top, Yr_crop_top = if_else(Q20l1  ==-99, 0, Q20l1)) 
glimpse(crop_top)
crop_top <- mutate(crop_top, Yr_crop_top = as.factor(Yr_crop_top),
                   farmer = KEY)
  
#join the data togther to get clm for crop topping and for states etc..
crop_top1 <- left_join(crop_top, XYNoTill_postcodes_join, by = "farmer")
glimpse(crop_top1)


adoption_curve_fun7 <- function(df, zone, numb_farmers) {
  df_subset <- filter(df, state == zone)
  count_adoption_year <- count(df_subset,Yr_crop_top)
  count_adoption_year <- select(count_adoption_year, year = Yr_crop_top, n)
  years_of_study <- data.frame(year = 1950:2014, 
                               id = 1:65,
                               zone = zone)
  years_of_study <- mutate(years_of_study, year = as.factor(year))
  adoption_df <- left_join(years_of_study, count_adoption_year, by= "year" )
  adoption_df <- mutate(adoption_df, n = replace_na(adoption_df$n, 0))
  adoption_df <- mutate(adoption_df, cummulative = cumsum(adoption_df$n))
  adoption_df <- mutate(adoption_df, cumm_percent = (adoption_df$cummulative/numb_farmers)*100)
  adoption_df$year <- as.double(adoption_df$year)
  
  #return(df_subset)
  return(adoption_df)
}
QLD_croptop <- adoption_curve_fun7(crop_top1, "QLD", 59)
NSW_croptop <- adoption_curve_fun7(crop_top1, "NSW", 153)
SA_croptop <- adoption_curve_fun7(crop_top1, "SA", 65)
VIC_croptop <- adoption_curve_fun7(crop_top1, "VIC", 141)
WA_croptop <- adoption_curve_fun7(crop_top1, "WA", 179)

croptop_states <- rbind(QLD_croptop, NSW_croptop, SA_croptop,VIC_croptop, WA_croptop ) 
croptop_states <- mutate(croptop_states, adoption = "crop top")


ggplot(croptop_states, aes(year, cumm_percent))+
  geom_line(aes(linetype=adoption))+
  #scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)+
  labs(x = "Years",
       y = "Percentage of farmers")


glimpse(NoTill_adoption_state_NT_weeds)
croptop_advisors <- rbind(PA_Agro_state, croptop_states)
croptop_advisors <- filter(croptop_advisors, zone != "QLD")
glimpse(croptop_advisors)

ggplot(croptop_advisors, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  #scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)+
  labs(x = "Years",
       y = "Percentage of farmers")

ggsave("croptop_advisors.png", width = 9.8, height = 5.6, units = "in")









####make a data set that has Advisor (PA study), no till, soil test, auto steer and crop topping######



glimpse(PA_Agro_state)
glimpse(NoTill_adoption_state_NT_weeds)
glimpse(Yr_soil_testing_PA_state)
glimpse(Yr_AutoSteer_PA_state)
glimpse(croptop_states)


Agro_noTill_soil_test_autoSteer_crop_top <- rbind(PA_Agro_state, 
                                                  NoTill_adoption_state_NT_weeds,
                                                  Yr_soil_testing_PA_state,
                                                  Yr_AutoSteer_PA_state,
                                                  croptop_states)
Agro_noTill_soil_test_autoSteer_crop_top<- filter(Agro_noTill_soil_test_autoSteer_crop_top, zone!= "QLD")

ggplot(Agro_noTill_soil_test_autoSteer_crop_top, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  #scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)+
  labs(x = "Years",
       y = "Percentage of farmers")
ggsave("Agro_noTill_soil_test_autoSteer_crop_top.png", width = 9.8, height = 5.6, units = "in")

#########make a data set that has Advisor (PA study), no till, soil test, and auto steer ######
glimpse(PA_Agro_state)
glimpse(NoTill_adoption_state_NT_weeds)
glimpse(Yr_soil_testing_PA_state)
glimpse(Yr_AutoSteer_PA_state)



Agro_noTill_soil_test_autoSteer <- rbind(PA_Agro_state, 
                                                  NoTill_adoption_state_NT_weeds,
                                                  Yr_soil_testing_PA_state,
                                                  Yr_AutoSteer_PA_state)
                                                  
Agro_noTill_soil_test_autoSteer<- filter(Agro_noTill_soil_test_autoSteer, zone!= "QLD")

ggplot(Agro_noTill_soil_test_autoSteer, aes(year, cumm_percent, group = adoption))+
  geom_line(aes(linetype=adoption))+
  #scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  xlim(1980, 2015)+
  ylim(0,100)+
  facet_wrap(.~zone)+
  labs(x = "Years",
       y = "Percentage of farmers")
ggsave("Agro_noTill_soil_test_autoSteer.png", width = 9.8, height = 5.6, units = "in")


