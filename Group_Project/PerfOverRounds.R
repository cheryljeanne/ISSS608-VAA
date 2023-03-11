

swimdata <- read_csv("Group_Project/data/swimdata_clean.csv")

final_swimmers <- swimdata %>%
  filter(Round == 'Final' & Style != 'Relay') %>%
  select(Name, Event)

final_swimmers_full <- inner_join(swimdata, final_swimmers) %>%
  select(Round, Name, Finals_Time, Event) %>%
  pivot_wider(names_from = Round,
              values_from = Finals_Time) %>%
  na.omit() %>%                                               #>> only retain events that has heats/semis/finals
  mutate(Semi = seconds_to_period(Semi), 
         Final = seconds_to_period(Final),
         Heat = seconds_to_period(Heat),
         SemitoFinal = (Semi-Final)/Semi,
         HeattoFinal = (Heat-Final)/Heat
  ) %>%
  pivot_longer(cols = c("SemitoFinal", "HeattoFinal"), 
               names_to = "RoundsCompare",
               values_transform = as.numeric,
               values_to = "Time")

write_csv(final_swimmers_full, "final_swimmers_full.csv")




#####

pacman::p_load(tidyverse, dplyr, ggplot2,lubridate, ggdist) 

swimdata <- read_csv("Group_Project/data/final_swimmers_full.csv")


##CDF is the probability that random variable values less than or equal to x
  
ggplot(final_swimmers_full, aes(Time, colour = RoundsCompare)) + 
  stat_ecdf(geom = "point")+
  labs(title="Probability of Improvement in Finals Performance",
       y = "Cumulative Distribution Function", x="Fractional Improvement in Swim Time")+
  theme_classic() +
  geom_vline(xintercept=mean(final_swimmers_full$Time, na.rm=TRUE), linetype="dashed", color = "red")


final_swimmers_full %>%
  ggplot(aes(x = RoundsCompare, 
             y = Time)) +
  stat_gradientinterval(   
    fill = "skyblue",      
    show.legend = TRUE     
  ) +                        
  labs(
    title = "Visualising confidence intervals of Improvement in Performances between Swim Rounds",
    subtitle = "Gradient + interval plot")


  final_swimmers_full %>% 
    select(RoundsCompare, Time) %>%
    ggplot(aes(x = Time, y = RoundsCompare)) +
    stat_cdfinterval()
  
  
  ##pass in values: stroke // distance -> create function to compute p-value one way anova? 
  #else just visualize one way anova based on Fractional performance improvement
  
  ##Mann Kendall Test for trend? see if there's a trend detected for swimmers that are consistent 

  