swimdata <- read_csv("Group_Project/data/swimdata_clean.csv")

final_swimmers <- swimdata %>%
  filter(Round == 'Final' & !(Style == 'Relay' | Style == 'Medley')) %>%
  select(Name, Event)

final_swimmers_full <- inner_join(swimdata, final_swimmers) %>%
  select(Round, Name, Finals_Time, Event)%>%
  pivot_wider(names_from = Round,
              values_from = Finals_Time) %>%
  na.omit() %>%                                                        #>> only retain events that has heats/semis/finals
  mutate(Semi = period_to_seconds(ms(Semi)), 
         Final = period_to_seconds(ms(Final)),
         Heat = period_to_seconds(ms(Heat)),
         SemitoFinal = (Semi-Final)/Semi,
         HeattoFinal = (Heat-Final)/Heat
  ) %>%
  pivot_longer(cols = c("SemitoFinal", "HeattoFinal"), 
               names_to = "RoundsCompare",
               values_transform = as.numeric,
               values_to = "Time")

write_csv(final_swimmers_full, "final_swimmers_full.csv")


############################################################

pacman::p_load(tidyverse, dplyr, ggplot2, ggdist, plotly, scales, thematic, shiny, ragg) 

swimdata <- read_csv("Group_Project/data/final_swimmers_PR.csv")

##CDF is the probability that random variable values less than or equal to x
  
plot <- ggplot(swimdata, aes(Time, colour = RoundsCompare)) + 
  stat_ecdf(geom = "point")+
  labs(title="Probability of Improvement in Finals Performance",
       y = "Cumulative Distribution Function", 
       x = "% Improvement in Swim Time") +
  theme_classic() +
  geom_vline(xintercept=mean(swimdata$Time, na.rm=TRUE), linetype="dashed", color = "red") +
  annotate("text", x= -0.001, y=1, label="Avg Improvement in \nFinals Swim Time", size=2.5, color = "red", angle=0)
  
ggly <- ggplotly(plot)

text_xHF <- number(
  ggly$x$data[[1]]$x,
  scale = 100,
  suffix = "% Improvement in Swim Time",
  accuracy = 0.01
)

text_yHF <- number(
  ggly$x$data[[1]]$y,
  scale = 100,
  accuracy = 0.1,
  prefix = "Cumul. distribution: ",
  suffix = "%"
)

text_xSF <- number(
  ggly$x$data[[2]]$x,
  scale = 100,
  suffix = "% Improvement in Swim Time",
  accuracy = 0.01
)

text_ySF <- number(
  ggly$x$data[[2]]$y,
  scale = 100,
  accuracy = 0.1,
  prefix = "Cumul. distribution: ",
  suffix = "%"
)

ggly %>%
  style(text = paste0(text_xHF, "</br></br>", text_yHF), traces = 1) %>%
  style(text = paste0(text_xSF, "</br></br>", text_ySF), traces = 2)

#>>Pass in slider scale for fractional improvement range?
#>Filter for rounds compare multi select

##Mann Kendall Test for trend? see if there's a trend detected for swimmers that are consistent 

library("Kendall")

MKdata <- final_swimmers_full %>%
  select(Name, Event, Final, Heat, Semi) %>%
  unique() %>%
  na.omit() %>%
  unite(NameEvent, Name:Event, remove = TRUE) %>%
  rename("1" = "Final",
         "2" = "Semi",
         "3" = "Heat") %>%
  pivot_longer(cols = c("1", "2","3"), 
               names_to = "Rounds",
               values_transform = as.numeric,
               values_to = "Time") %>%
  arrange(NameEvent, Rounds) %>%
  select (NameEvent, Time)

MKdata <- split(MKdata$Time, MKdata$NameEvent)

MKOutput <- lapply(MKdata, MannKendall)

MKdata

MKOutput

data <- final_swimmers_full %>%
  select(Name, Event, Final, Heat, Semi) %>%
  unique() %>%
  na.omit() %>%
  unite(NameEvent, Name:Event, remove = TRUE)%>%
  rename("1" = "Final",
         "2" = "Semi",
         "3" = "Heat")

data <- data[,c(1,2,4,3)]


##>> CI Visualization

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

#>> 2 Sample T-Test based on Fractional Performance Improvement
#>> P-value is < 0.05, reject the null hypothesis that there's no difference between the means
#>> Conclude that a significant difference btw, Heat/Finals swim time vs Semi/Finals swim time. 

final_swimmers_full

ggbetweenstats(
  data = final_swimmers_full,
  x = RoundsCompare, 
  y = Time,
  type = "p",
  mean.ci = TRUE, 
  pairwise.comparisons = TRUE, 
  pairwise.display = "s",
  p.adjust.method = "fdr",
  messages = FALSE
)



#>>pass in input parameter for % CI 

final_swimmers_full %>% 
  select(RoundsCompare, Time) %>%
  ggplot(aes(x = Time, y = RoundsCompare)) +
  stat_cdfinterval() +
  labs(title="Probability of Improvement in Finals Performance",
       x = "% Improvement in Swim Time",
       y = "Comparison of Rounds to Final") +
  theme_classic()

  
