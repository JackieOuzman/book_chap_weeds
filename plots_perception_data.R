library(ggplot2)
library(dplyr)
library(plyr)

#"file://portjervis-gw/C$/Users/ouz001/book_chapter_data/adoption_data/perception_question_REGION.csv"

plot_perception <- read.csv("C:/Users/ouz001/book_chapter_data/adoption_data/perception_question_REGION.csv" )
 glimpse(plot_perception)
 
#getting the data clm into the right format
 plot_perception <- mutate(plot_perception, ans_factors = as.factor(Ans))

 #getting the data clm to display the names I want     
plot_perception$ans_factors <- mapvalues(plot_perception$Ans, from = c("1", "2", "3"), to = c("Less", "Same", "More"))
plot_perception$question_text <- mapvalues(plot_perception$question, from = c("Q26_1", "Q26_3", "Q26_4", "Q26_5", "Q26_6", "Q26_8"), 
                                                                to = c("Crop disease", "Weed costs", "Nitrogen fertiliser costs",
                                                                       "Pest costs", "Effectiveness of pre-emergent herbicides",
                                                                       "Reliability of wheat yields")) 
#filter out the region classed as Excluded
plot_perception <- filter(plot_perception, REGIONS != "Excluded",
                          ans_factors!= -99)



####CROP DISEASE####
#filter out to disply one question 
 plot_perception_crop_disease <- filter(plot_perception, question == "Q26_1")
 
 
ggplot(data = plot_perception_crop_disease, aes(REGIONS, fill = ans_factors ))+
  geom_bar(aes(weight = perc))+
  scale_fill_brewer(palette = "Blues")+
  theme_bw()+
  theme(legend.position = "bottom")+
  guides(fill = "none")+
  labs(x = "Regions",
       y = "percentage of farmers indicating less, more, same",
       title = "Perception of no-till and stubble retention per region")



####ALL QUESTIONS AS FACET WRAP #####
ggplot(data = plot_perception, aes(REGIONS, fill = ans_factors ))+
  geom_bar(aes(weight = perc))+
  scale_fill_brewer(palette = "Blues")+
  theme_bw()+
  facet_wrap(.~question_text)+
  theme(legend.position = "bottom")+
  guides(fill = "none")+
  labs(x = "Regions",
       y = "percentage of farmers indicating less, more, same",
       title = "Perception of no-till and stubble retention per region")


#####Just plot MORE values for crop disease, weed, fert and pest
plot_perception_more_q1_5 <- filter(plot_perception, ans_factors == "More") %>% 
                            filter(question != "Q26_6") %>%  
                            filter(question != "Q26_8") 
                          
ggplot(data = plot_perception_more_q1_5, aes(REGIONS, fill = ans_factors ))+
  geom_bar(aes(weight = perc))+
  scale_fill_brewer(palette = "Blues")+
  theme_bw()+
  facet_wrap(.~question_text)+
  ylim(0,100)+
  theme(legend.position = "bottom")+
        guides(fill = "none")+
  labs(x = "Regions",
       y = "percentage of farmers indicating MORE",
       title = "Perception of no-till and stubble retention per region")

##Just plot LESS values for Effectiveness of pre-emergent herbicides Reliability of wheat yields

plot_perception_more_q6_8 <- filter(plot_perception, ans_factors == "Less") %>% 
  filter(question != "Q26_1") %>%  
  filter(question != "Q26_3") %>% 
  filter(question != "Q26_4") %>% 
  filter(question != "Q26_5")



ggplot(data = plot_perception_more_q6_8, aes(REGIONS, fill = ans_factors ))+
  geom_bar(aes(weight = perc))+
  scale_fill_brewer(palette = "Blues")+
  theme_bw()+
  facet_wrap(.~question_text)+
  ylim(0,100)+
  theme(legend.position = "bottom")+
  guides(fill = "none")+
  labs(x = "Regions",
       y = "percentage of farmers indicating LESS",
       title = "Perception of no-till and stubble retention per region")




#### NOW ONLY display more or less and bras not stacked  #####
glimpse(plot_perception)
#create df with just more or less not the same
plot_perception_more_less <- filter(plot_perception, ans_factors != "Same")
glimpse(plot_perception_more_less)

#this is stacked
ggplot(data = plot_perception_more_less, aes(REGIONS, fill = ans_factors ))+
  geom_bar(aes(weight = perc))+
  scale_fill_brewer(palette = "Blues")+
  theme_bw()+
  facet_wrap(.~question_text)+
  theme(legend.position = "bottom")+
  guides(fill = "none")+
  labs(x = "Regions",
       y = "percentage of farmers indicating less, more",
       title = "Perception of no-till and stubble retention per region")

#question Q26_6 to 8
plot_perception_more_lessq6_8 <- filter(plot_perception_more_less, question != "Q26_5") %>% 
  filter(question != "Q26_1") %>%  
  filter(question != "Q26_3") %>% 
  filter(question != "Q26_4") 
  

glimpse(plot_perception_more_lessq6_8)
ggplot(data = plot_perception_more_lessq6_8, aes(ans_factors, y = perc, fill = ans_factors ))+
  geom_bar(stat = "identity")+  
  facet_grid(REGIONS ~question_text)+
  scale_fill_manual(values = c("grey77", "grey50"))+
  theme_bw()+
  
  theme(legend.position = "bottom")+
  guides(fill = "none")+
  labs(x = "",
       y = "percentage of farmers",
       title = "Perception of no-till and stubble retention per region")
ggsave("plot_perception_more_lessq6_8.png", width = 9.8, height = 5.6, units = "in")

#question Q26_1 Q26_3 Q26_4 Q26_5
plot_perception_more_lessq1_5 <- filter(plot_perception_more_less, question != "Q26_8") %>% 
  filter(question != "Q26_6") 

glimpse(plot_perception_more_lessq1_5)
ggplot(data = plot_perception_more_lessq1_5, aes(ans_factors, y = perc, fill = ans_factors ))+
  geom_bar(stat = "identity", width = .3)+  
  facet_grid(REGIONS ~question_text)+
  scale_fill_manual(values = c("grey77", "grey50"))+
  theme_bw()+
  
  theme(legend.position = "bottom")+
  guides(fill = "none")+
  labs(x = "",
       y = "percentage of farmers",
       title = "Perception of no-till and stubble retention per region")
ggsave("plot_perception_more_lessq1_5.png", width = 9.8, height = 5.6, units = "in")


glimpse(plot_perception_more_lessq6_8)
glimpse(plot_perception_more_lessq1_5)

plot_perception_more_lessq6_8_and1_5 <- rbind(plot_perception_more_lessq6_8,plot_perception_more_lessq1_5)
glimpse(plot_perception_more_lessq1_5)
ggplot(data = plot_perception_more_lessq6_8_and1_5, aes(ans_factors, y = perc, fill = ans_factors ))+
  geom_bar(stat = "identity", width = .5)+  
  facet_grid(REGIONS ~question_text, labeller = labeller(question_text = label_wrap_gen()))+
  scale_fill_manual(values = c("grey77", "grey50"))+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x  = element_text(angle=90, size = 8),
        axis.text.y  = element_text(size = 8),
        strip.text = element_text(size = 8))+
  guides(fill = "none")+
  labs(x = "",
       y = "percentage of farmers",
       title = "Perception of no-till and stubble retention per region")
# I still have problem wrapping my text -- try the below
#https://stackoverflow.com/questions/37174316/how-to-fit-long-text-into-ggplot2-facet-titles/37174810
