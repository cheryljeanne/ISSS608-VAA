pacman::p_load(shiny,
               shinythemes,
               ggiraph,
               dplyr, 
               ggplot2,
               plotly,
               ggstatsplot, 
               tidyverse, 
               dendextend, 
               patchwork, 
               scales, 
               rstantools,
               RColorBrewer,
               lubridate,
               PMCMRplus,
               gapminder, 
               gt, 
               performance, tidyr, ggiraphExtra, logistf, sjPlot)

######## DATA FILES ########

swimdata <- read_csv("Group_Project/data/swimdata_clean.csv")

all_swimmers2 <- swimdata %>%
  filter(!(Style == 'Relay' | Style == 'Medley')) %>%
  select(Place, Round, Name, Finals_Time, Event)%>%
  select(Round, Name, Finals_Time, Event)%>%
  mutate(Finals_Time = as.character(Finals_Time), 
         LEN = nchar(Finals_Time)) %>%
  filter(LEN > 5) %>%
  mutate(Finals_Time2 = period_to_seconds(ms(Finals_Time)), 
         Finals_Time2 = as.character(Finals_Time2))

all_swimmers <- swimdata %>%
  filter(!(Style == 'Relay' | Style == 'Medley')) %>%
  select(Place, Round, Name, Finals_Time, Event)%>%
  select(Round, Name, Finals_Time, Event)
  
all_swimmers_final <- left_join(x=all_swimmers, y=all_swimmers2, 
                                by = c("Name","Event","Round")) %>%
    

all_swimmers_final2 <- mutate_at(Finals_Time, ~replace_na(.,Finals_Time.x))


write_csv(all_swimmers_final, "all_swimmers_Reg_final.csv")


#######



Olymp_Qual_Time2 <- Olymp_Qual_Time %>%
  mutate(OlypQualTime = as.character(OlypQualTime), 
                           OlypRecord = as.character(OlypRecord),
                           LENQT = nchar(OlypQualTime), 
                           LENQR = nchar(OlypRecord)) %>%
  filter(LENQT > 5 | LENQR > 5) %>%
  mutate(OlypQualTime2 = period_to_seconds(ms(OlypQualTime)), 
         OlypQualTime2 = as.character(OlypQualTime2), 
         OlypRecord2 = period_to_seconds(ms(OlypRecord)), 
         OlypRecord2 = as.character(OlypRecord2))


Olymp_Qual_Time_final <- left_join(x=Olymp_Qual_Time, y=Olymp_Qual_Time2, 
                                by = c("Event"))


write_csv(Olymp_Qual_Time_final, "Olymp_Qual_Time_final.csv")

#######

MLR_swimdata <- read_csv("Group_Project/data/all_swimmers_Reg.csv")
Olymp_Qual_Time <- read_csv("Group_Project/data/OlympicQualTime.csv")

MLR_swimdata_joined <- inner_join(MLR_swimdata, Olymp_Qual_Time) %>%
  mutate(Finals_Time = period_to_seconds(ms(Finals_Time)), 
         OlypQualTime = period_to_seconds(ms(OlypQualTime)),
         OlypRecord = period_to_seconds(ms(OlypRecord))) %>%
  pivot_wider(names_from = Round,
              values_from = Finals_Time)


write_csv(MLR_swimdata_joined, "MLR_swimdata_final.csv")

########

MLR_swimdata <- read_csv("Group_Project/data/MLR_swimdata_final.csv")

MLR_swimdata2 <- MLR_swimdata %>%
  select (Name, Event, Podium, AvgTime, RelTime_OQT, RelTime_ORT, RelTime_OQT_LR) %>%
 filter(Event == "Men's 200m Backstroke")

model <- glm( RelTime_OQT_LR ~ AvgTime + Podium,
     data = MLR_swimdata2, family=binomial)

summary(model)

check_c <- check_collinearity(model)
plot(check_c)


#package in R to perform Firth logistic regression on an unbalanced dataset
model <- logistf(RelTime_OQT_LR ~ AvgTime + Podium, data = MLR_swimdata2)

summary(model)

library(sjPlot)

plot_model(model, axisLimits = c(0.05, 25), transformTicks = T)

MLR_swimdata2$RelTime_OQT_LR = predict(model, MLR_swimdata2, type="response")
plot(RelTime_OQT_LR ~ AvgTime, data=MLR_swimdata2, col="steelblue")


lines(RelTime_OQT_LR ~ AvgTime, MLR_swimdata2, lwd=2)

ggPredict(model,se=TRUE,interactive=TRUE,digits=3)

head(MLR_swimdata2)

install.packages("sjPlot")

library(sjPlot)
library(logistf)
data(sex2)
fit<-logistf(case ~ age+oc+vic+vicl+vis+dia, data=sex2)
# for this example, axisLimits need to be specified manually
plot_model(fit, axisLimits = c(0.05, 25), transformTicks = T)



#+ 
  theme_sjplot2() + scale_color_sjplot("simply") + 
  ggplot2::labs(title= "Predicted probabilities of Hunger", x= "Race", y="Percentage")
  
##################LOG REG ON FINAL SWIMMERS#######################
  
LogReg_swimdata <- read_csv("Group_Project/data/MLR_swimdata_final_finals_.csv")
  
names(LogReg_swimdata)
  
LogReg_swimdata2 <- LogReg_swimdata %>%
    select (OlypQualTime, OlypRecord, Final, Heat, Semi, AvgTime, SDTime, RelTime_OQT, RelTime_ORT, Podium, RelTime_OQT_LR, RelTime_ORT_LR) 
#%>% filter(Event == "Men's 200m Backstroke")

model <- glm( RelTime_OQT_LR ~ OlypQualTime + #OlypRecord + Final + Heat + Semi + 
                #AvgTime + 
                SDTime + 
                RelTime_OQT + RelTime_ORT + Podium + RelTime_OQT_LR + RelTime_ORT_LR,
              data = LogReg_swimdata2, family=binomial)

model <- lm( AvgTime ~ #OlypQualTime + OlypRecord + 
               #Final + 
               Heat + 
               #Semi + 
                #RelTime_OQT_LR + 
                SDTime #+ 
                #RelTime_OQT + RelTime_ORT #+ Podium + RelTime_OQT_LR + RelTime_ORT_LR
             ,
              data = LogReg_swimdata2)
#package in R to perform Firth logistic regression on an unbalanced dataset
model1 <- logistf(formula = RelTime_OQT_LR ~ OlypQualTime + #OlypRecord + Final + Heat + Semi + 
                    #AvgTime + 
                    #SDTime + 
                    RelTime_OQT + #RelTime_ORT + 
                    Podium + RelTime_OQT_LR, #+ RelTime_ORT_LR,
                 data = LogReg_swimdata2)

select <- c('OlypQualTime', 'OlypRecord', 'Final', 'Heat', 
            'Semi', 'SDTime', 'RelTime_OQT', 'RelTime_ORT', 'Podium', 'RelTime_OQT_LR', 'RelTime_ORT_LR')
forms

inputs <- paste(select, collapse = " + ")
forms <- as.formula(paste0("AvgTime", "~",  inputs))
model <- lm(forms, data = LogReg_swimdata2)


m <- summary(model)
print(m)

check_c <- check_collinearity(model)
plot(check_c)


ggcoefstats(model, 
            output = "plot")

plot_model(model1, axisLimits = c(0.001, 4000), transformTicks = T)


LogReg_swimdata2$RelTime_OQT_LR = predict(model1, LogReg_swimdata2, type="response")

plot(RelTime_OQT_LR ~ OlypQualTime+RelTime_OQT+Podium, data=LogReg_swimdata2, col="steelblue")

  
  

########
MLR_swimdata <- read_csv("Group_Project/data/all_swimmers_Reg_final.csv")
Olymp_Qual_Time <- read_csv("Group_Project/data/Olymp_Qual_Time_final.csv")
##https://resources.fina.org/fina/document/2022/07/15/e0f75e3c-88d5-4356-9ec6-852c8ce237b7/Paris-2024-SW-Qualification-System_ENG_H.pdf

##https://en.wikipedia.org/wiki/List_of_Olympic_records_in_swimming


head(MLR_swimdata)

MLR_swimdata_join <- inner_join(MLR_swimdata, Olymp_Qual_Time) %>%
  mutate(OlypQualTime_conv = period_to_seconds(ms(OlypQualTime))
         )

check_c <- check_collinearity(model)

plot(check_c)
