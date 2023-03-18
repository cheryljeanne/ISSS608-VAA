pacman::p_load(shiny,
               shinythemes,
               ggiraph, 
               ggstatsplot, 
               tidyverse, 
               dendextend, 
               patchwork, 
               scales, 
               rstantools,
               RColorBrewer,
               lubridate,
               PMCMRplus,
               gapminder)

######## DATA FILES ########

S_plits <- read_csv("splits.csv")
### new input 17/3 ###
ST_swimdata <- read_csv("swimdata_clean3.csv")
ST_continents <- read_csv("continents.csv")
### new input 17/3 ###



######## END OF DATA FILES ########

### new input 17/3 ###
averagespeedPanel <- tabPanel(
  "Average Speed",
  sidebarLayout(
    sidebarPanel(
      # Gender, Style, Distance, Round
      selectInput("ST_style",
                  label = h5("Style:"),
                  choices = unique(ST_swimdata$Style),
                  multiple = TRUE,
                  selected = unique(ST_swimdata$Style),
                  ),
      selectInput("ST_gender",
                  label=h5("Gender:"),
                  choices=unique(ST_swimdata$Gender),
                  multiple = TRUE,
                  selected = unique(ST_swimdata$Gender),
                  ),
      selectInput("ST_distance",
                  label=h5("Distance:"),
                  choices=unique(ST_swimdata$Distance),
                  multiple = TRUE,
                  selected = unique(ST_swimdata$Distance)
                  ),
      selectInput("ST_round",
                  label=h5("Round:"),
                  choices=unique(ST_swimdata$Round),
                  multiple = TRUE,
                  selected = unique(ST_swimdata$Round),
                  ),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Distribution",
          titlePanel("Average Speed of Swimmers"),
          fluidRow(
            column(12,
              h3("Histogram Graph"),
              p("The chart here shows the distribution of average speed by the selected variables."),
              ggiraphOutput("ST_average_speed_dist", width="100%"),
            )
          ),
          fluidRow(
            column(12,
               h3("Continent Scatterstats Plot"),
               p("The chart here shows the distribution of average speed of continents by selected variables."),
               plotOutput("ST_average_speed_continent_dist", width="100%"),
            ),
            
          ),
        ),
        tabPanel(
          "Compare Mean",
          titlePanel("Comparison of Average Speeds"),
          fluidRow(
            column(12,
              h3("Style"),
              p("The chart here shows the comparison of average speed by style."),
              plotOutput("ST_average_speed_compare_mean_style", width="100%"),
            )
          ),
          fluidRow(
            column(12,
              h3("Gender"),
              p("The chart here shows the comparison of average speed by gender"),
              plotOutput("ST_average_speed_compare_mean_gender", width="100%"),
            )
          ),
          fluidRow(
            column(12,
              h3("Distance"),
              p("The chart here shows the comparison of average speed by distance"),
              plotOutput("ST_average_speed_compare_mean_distance", width="100%"),
            )
          ),
          fluidRow(
            column(12,
              h3("Round"),
              p("The chart here shows the comparison of average speed by round"),
              plotOutput("ST_average_speed_compare_mean_round", width="100%"),
            )
          ),
        ),
        tabPanel(
          "Correlation",
          titlePanel("Correlation of Average Speeds with Reaction Time"),
          fluidRow(
            column(12,
              p("Shows the correlation between Average Speed and Reaction Time by selected events"),
              plotOutput("ST_average_speed_correlation", width="60%", height="600px"),
            )
          ),
        ),
      )
    )
  )
)
### new input 17/3 ###

######## UI ########

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML(".selectize-input, .selectize-dropdown {font-size: 90%;}"))),
  navbarPage(HTML("<b>Swim Analytica</b>"),
             tabPanel("Data"),
             ### START new input 17/3 ###
             navbarMenu(title = "Speed/Time",
                        averagespeedPanel,
                        tabPanel("Reaction Time")),
             ### END new input 17/3 ###
             tabPanel("Split Times",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("S_event",
                                      label=h5("Event:"),
                                      choices=list(
                                        "Men's 200m Backstroke" = "M200BA",
                                        "Men's 200m Breaststroke" = "M200BR",
                                        "Men's 200m Butterfly" = "M200BU",
                                        "Men's 200m Freestyle" = "M200FR",
                                        "Men's 400m Freestyle" = "M400FR",
                                        "Men's 800m Freestyle" = "M800FR",
                                        "Men's 1500m Freestyle" = "M1500FR",
                                        "Women's 200m Backstroke" = "W200BA",
                                        "Women's 200m Breaststroke" = "W200BR",
                                        "Women's 200m Butterfly" = "W200BU",
                                        "Women's 200m Freestyle" = "W200FR",
                                        "Women's 400m Freestyle" = "W400FR",
                                        "Women's 800m Freestyle" = "W800FR",
                                        "Women's 1500m Freestyle" = "W1500FR"
                                        )),
                          p(HTML("Default pacing categories are 'Positive Split', 'Negative Split' or 'Even',
                                 or use hierarchical clustering to identify others.")),
                          sliderInput("S_range", label = h5("Range for even pacing"), min = 90, 
                                      max = 110, value = c(98, 101)),
                          selectInput("S_dmethod",
                                      label=h5("Distance method for clustering:"),
                                      choices=list(
                                        "Euclidean" = "euclidean",
                                        "Maximum" = "maximum",
                                        "Manhattan" = "manhattan",
                                        "Canberra" = "canberra",
                                        "Minkowski" = "minkowski"
                                        )),
                          selectInput("S_cmethod",
                                      label=h5("Hierarchical clustering method:"),
                                      choices=list(
                                        "Complete" = "complete",
                                        "Average" = "average",
                                        "Single" = "single",
                                        "Ward" = "ward.D"
                                        )),
                          numericInput("S_clusters", label = h5("Number of clusters:"), value = 4),
                          width=3),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Overview",
                                     titlePanel("Swimmer speed by splits and average speed by event"),
                                     girafeOutput("S_overview", height=1000)),
                            tabPanel("Distance",
                                     titlePanel("Correlation between speed at split and distance at split"),
                                     fluidRow(
                                       column(6,
                                              plotOutput("S_distance1")),
                                       column(6,
                                              plotOutput("S_distance2"))
                                     ),
                                     fluidRow(
                                       column(6,
                                              verbatimTextOutput("S_distance3")),
                                       column(6,
                                              verbatimTextOutput("S_distance4"))
                                     )
                                     ),
                            tabPanel("Pacing (Default)",
                                     titlePanel("Identifying positive splits, negative splits and even pacing"),
                                     fluidRow(
                                       column(5,
                                              plotOutput("S_pacing1")),
                                       column(7,
                                              girafeOutput("S_pacing2")))
                                     ),
                            tabPanel("Pacing (Clusters)",
                                     titlePanel("Identifying alternative pacing categories"),
                                     h4("Dendrogram of pacing clusters"),
                                     plotOutput("S_pacing3", height=900),
                                     fluidRow(
                                       column(7,
                                              girafeOutput("S_pacing4"))
                                     )),
                            tabPanel("Performance",
                                     titlePanel("Comparing median average speed across pacing categories, clusters"),
                                     plotOutput("S_perform1"),
                                     plotOutput("S_perform2"))
                                     ),
                          width=9
                          )
                        )),
             tabPanel("Performance Over Rounds"))
)

######## END OF UI ########


######## SERVER ########

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # SECTION: AVERAGE SPEED
  # SUB-SECTION: DISTRIBUTION
  output$ST_average_speed_dist <- renderGirafe({
    ST_swimdata_subset <- ST_swimdata
    
    if (!is_null( input$ST_style)) {
      ST_swimdata_subset <- ST_swimdata %>% filter(Style %in% input$ST_style)
    }
    
    if (!is_null(input$ST_distance)) {
      ST_swimdata_subset <- ST_swimdata_subset %>% filter(Distance %in% input$ST_distance)
    }
    
    if (!is_null( input$ST_gender)) {
      ST_swimdata_subset <- ST_swimdata_subset %>% filter(Gender %in% input$ST_gender)
    }
    
    if (!is_null(input$ST_round)) {
      ST_swimdata_subset <- ST_swimdata_subset %>% filter(Round %in% input$ST_round)
    }
    
    ST_swimdata_subset$tooltip <- c(paste0(
      'Average Speed = ', ST_swimdata_subset$Average_speed,
      "\n Name = ", ST_swimdata_subset$Name
    ))
    
    p1 <- ggplot(data=ST_swimdata_subset, aes(x=Average_speed)) + 
      geom_histogram_interactive(bins=10, aes(tooltip=ST_swimdata_subset$tooltip))
    
    p2 <- ggplot(data=ST_swimdata_subset, aes(x='', y=Average_speed)) +
      geom_boxplot() +
      coord_flip()
    
    p3 <- p2 + p1 + plot_layout(nrow = 2, heights = c(1, 5))
    girafe(
      code = print(p3),
      ggobj = p3,
      width_svg = 12,
      height_svg = 12*0.618,
    )
  })
  
  output$ST_average_speed_continent_dist <- renderPlot({
    ggdotplotstats(
      data       = ST_swimdata,
      y          = Continent,
      x          = Average_speed,
      type       = "robust",
      xlab       = "Average Speed",
      ylab.      = "Continent"
    )
  })

  # SUB-SECTION: COMPARE MEAN
  output$ST_average_speed_compare_mean_style <- renderPlot({
    ggbetweenstats(
      data = ST_swimdata,
      x = Style,
      y = Average_speed
    )
  })
  output$ST_average_speed_compare_mean_gender <- renderPlot({
    ggbetweenstats(
      data = ST_swimdata,
      x = Gender,
      y = Average_speed
    )
  })
  output$ST_average_speed_compare_mean_distance <- renderPlot({
    ggbetweenstats(
      data = ST_swimdata,
      x = Distance,
      y = Average_speed
    )
  })
  output$ST_average_speed_compare_mean_round <- renderPlot({
    ggbetweenstats(
      data = ST_swimdata,
      x = Round,
      y = Average_speed
    )
  })
  # SUB-SECTION: CORRELATION
  output$ST_average_speed_correlation <- renderPlot({
    ggscatterstats(
      data = ST_swimdata,
      x = Average_speed,
      y = Reaction_Time,
      marginal = FALSE,
    )
  })
  
  # SECTION: REACTION TIME
  # SUB-SECTION: DISTRIBUTION
  
  output$ST_reaction_time_dist <- renderGirafe({
    
    if (!is_null( input$ST_style)) {
      ST_swimdata_subset <- ST_swimdata %>% filter(Style %in% input$ST_style)
    }
    
    if (!is_null(input$ST_distance)) {
      ST_swimdata_subset <- ST_swimdata_subset %>% filter(Distance %in% input$ST_distance)
    }
    
    if (!is_null( input$ST_gender)) {
      ST_swimdata_subset <- ST_swimdata_subset %>% filter(Gender %in% input$ST_gender)
    }
    
    if (!is_null(input$ST_round)) {
      ST_swimdata_subset <- ST_swimdata_subset %>% filter(Round %in% input$ST_round)
    }
    
    ST_swimdata_subset$tooltip <- c(paste0(
      'Reaction Time = ', ST_swimdata_subset$Reaction_Time,
      "\n Name = ", ST_swimdata_subset$Name
    ))
    
    p1 <- ggplot(data=ST_swimdata_subset, aes(x=Reaction_Time)) + 
      geom_histogram_interactive(bins=10, aes(tooltip=ST_swimdata_subset$tooltip))
    
    p2 <- ggplot(data=ST_swimdata_subset, aes(x='', y=Reaction_Time)) +
      geom_boxplot() +
      coord_flip()
    
    p3 <- p2 + p1 + plot_layout(nrow = 2, heights = c(1, 5))
    girafe(
      code = print(p3),
      ggobj = p3,
      width_svg = 12,
      height_svg = 12*0.618,
    )
  })

  # SECTION: SPLITS
  # SUB-SECTION: OVERVIEW
  output$S_overview <- renderGirafe({
    ## Data prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    ## Plot 1A 
    S_plit1A <- S_plit1 %>%
      select(-c(40:68)) %>%
      pivot_longer(c(4:S_1), names_to="Split", values_to="Time") %>%
      mutate(Split = as.numeric(Split)) %>%
      mutate(Speed = 50/Time)
    S_1A_tooltip <- paste0(S_plit1A$ID, "\n", "Place: ", S_plit1A$Place, "\n", "Time: ", S_plit1A$Finals_Time)
    S_1A <- ggplot(data=S_plit1A, aes(x=Split, y=Speed, group=ID)) +
      geom_line_interactive(aes(data_id=ID, tooltip=S_1A_tooltip)) +
      scale_x_continuous(name="Distance (m)", 
                         breaks=seq(50,S_distance,ifelse(S_distance==1500, 100, 50))) +
      scale_y_continuous(name="Speed (m/s)") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            axis.text=element_text(size=7),
            axis.title=element_text(size=7)) 
    ## Plot 1B (Position:Right)
    S_plit1B <- S_plit1A %>%
      group_by(ID) %>%
      summarise(AvgSpeed = mean(AvgSpeed))
    S_1B <- ggplot(data=S_plit1B, aes(x=AvgSpeed, y=reorder(ID,AvgSpeed))) +
      geom_col_interactive(aes(data_id=ID), fill="lightseagreen") +
      scale_x_continuous(name="Average Speed (m/s)", limits=c(1.3, NA), oob = rescale_none) +
      theme(axis.title.y=element_blank(),
            axis.text=element_text(size=5),
            axis.title=element_text(size=7))
    ## Together:
    girafe(code = print(S_1A + S_1B))
  })
  

    
  
  # SECTION: SPLITS
  # SUB-SECTION: DISTANCE(1)
  output$S_distance1 <- renderPlot({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    ## Data Prep for Plot
    S_plit1A <- S_plit1 %>%
      select(-c(40:68)) %>%
      pivot_longer(c(4:S_1), names_to="Split", values_to="Time") %>%
      mutate(Split = as.numeric(Split)) %>%
      mutate(Speed = 50/Time)
    ## Plot
    ggplot(data=S_plit1A, aes(x=Split, y=Speed)) +
      geom_point() +
      geom_smooth(method=lm) +
      ggtitle("All splits") +
      scale_x_continuous(name="Distance (m)", 
                         breaks=seq(50,S_distance,ifelse(S_distance==1500, 100, 50))) +
      scale_y_continuous(name="Speed (m/s)", limits=c(1.3,2.0)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=15)) 
  })
  
  # SECTION: SPLITS
  # SUB-SECTION: DISTANCE(2)
  output$S_distance2 <- renderPlot({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    ## Data Prep for Plot
    S_plit1A <- S_plit1 %>%
      select(-c(40:68)) %>%
      pivot_longer(c(4:S_1), names_to="Split", values_to="Time") %>%
      mutate(Split = as.numeric(Split)) %>%
      mutate(Speed = 50/Time)
    S_plit2B <- S_plit1A %>%
      filter(Split!=50, Split!=S_distance)
    # Plot
    ggplot(data=S_plit2B, aes(x=Split, y=Speed)) +
      geom_point() +
      geom_smooth(method=lm) +
      ggtitle("Drop first and last splits") +
      scale_x_continuous(name="Distance (m)", 
                         breaks=seq(50,S_distance,ifelse(S_distance==1500, 100, 50))) +
      scale_y_continuous(name="Speed (m/s)", limits=c(1.3,2.0)) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=15)) 
  })
  
  # SECTION: SPLITS
  # SUB-SECTION: DISTANCE(3)
  output$S_distance3 <- renderPrint({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    ## Data Prep for Plot
    S_plit1A <- S_plit1 %>%
      select(-c(40:68)) %>%
      pivot_longer(c(4:S_1), names_to="Split", values_to="Time") %>%
      mutate(Split = as.numeric(Split)) %>%
      mutate(Speed = 50/Time)
    ## Test
    S_result1 <- cor.test(S_plit1A$Split, S_plit1A$Speed, method = "pearson")
    print(S_result1)
  })
  
  # SECTION: SPLITS
  # SUB-SECTION: DISTANCE(4)
  output$S_distance4 <- renderPrint({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    ## Data Prep for Plot
    S_plit1A <- S_plit1 %>%
      select(-c(40:68)) %>%
      pivot_longer(c(4:S_1), names_to="Split", values_to="Time") %>%
      mutate(Split = as.numeric(Split)) %>%
      mutate(Speed = 50/Time)
    S_plit2B <- S_plit1A %>%
      filter(Split!=50, Split!=S_distance)
    ## Test
    S_result2 <- cor.test(S_plit2B$Split, S_plit2B$Speed, method = "pearson")
    print(S_result2)
  })
  
  # SECTION: SPLITS
  # SUB-SECTION: PACING(1)
  output$S_pacing1 <- renderPlot({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    S_2 <- (((S_distance/50)-2)/2)+3
    S_3 <- S_2+1
    S_4 <- S_2+(((S_distance/50)-2)/2)
    S_5 <- (S_distance - 100)/2
    lower <- input$S_range[1]
    upper <- input$S_range[2]
    ## Data Prep for Plot
    S_plit3A <-  S_plit1 %>%
      select(-c(40:68)) %>%
      select(-c("50",as.character(S_distance))) %>%
      mutate(Speed1 = (S_5/rowSums(.[4:S_2]))) %>%
      mutate(Speed2 = (S_5/rowSums(.[S_3:S_4]))) %>%
      mutate(Change = as.numeric(format(round((Speed2*100)/Speed1, 1), nsmall=1)))
    ## Plot
    ggplot(data=S_plit3A, aes(x=Change)) +
      geom_histogram(fill='lightseagreen', col='darkslategray', bins=10) +
      geom_vline(xintercept=lower, linetype='dashed', color='orange', size=1) +
      geom_vline(xintercept=upper, linetype='dashed', color='orange', size=1) +
      labs(title="Frequency Table", x="Speed during second half as percentage of \n speed during first half",
           y="Number of swimmers") +
      scale_x_continuous(n.breaks=20) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=15)) 
  })
  
  # SECTION: SPLITS
  # SUB-SECTION: PACING(2)
  output$S_pacing2 <- renderGirafe({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    S_2 <- (((S_distance/50)-2)/2)+3
    S_3 <- S_2+1
    S_4 <- S_2+(((S_distance/50)-2)/2)
    S_5 <- (S_distance - 100)/2
    lower <- input$S_range[1]
    upper <- input$S_range[2]
    ## Data Prep for Plot
    S_plit3A <-  S_plit1 %>%
      select(-c(40:68)) %>%
      select(-c("50",as.character(S_distance))) %>%
      mutate(Speed1 = (S_5/rowSums(.[4:S_2]))) %>%
      mutate(Speed2 = (S_5/rowSums(.[S_3:S_4]))) %>%
      mutate(Change = as.numeric(format(round((Speed2*100)/Speed1, 1), nsmall=1)))
    S_plit3B <- S_plit3A %>%
      mutate(Category = ifelse(Change<lower, "Positive", "Even")) %>%
      mutate(Category = ifelse(Change>upper, "Negative", Category)) %>%
      pivot_longer(c(4:(S_1-2)), names_to="Split", values_to="Time") %>%
      mutate(Split = as.numeric(Split)) %>%
      mutate(Speed = 50/Time)
    S_3B <- ggplot(data=S_plit3B, aes(x=Split, y=Speed, group=ID, color=Category)) +
      geom_line_interactive(aes(data_id=Category, tooltip=Category)) +
      scale_color_brewer(palette = "Dark2") +
      ggtitle("Splits by pacing category") +
      scale_x_continuous(name="Distance (m)", 
                         breaks=seq(50,S_distance,ifelse(S_distance==1500, 100, 50))) +
      scale_y_continuous(name="Speed (m/s)") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=10.5)) 
    girafe(code = print(S_3B), ggobj=S_3B, options = list(opts_hover_inv(css = "opacity:0.2;")))
  })
  
  # SECTION: SPLITS
  # SUB-SECTION: PACING(3)
  output$S_pacing3 <- renderPlot({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_6 <- 40+(((S_distance)/50)-2)
    S_plit4A <- S_plit1 %>%
      select(c(40:all_of(S_6)), ID) %>%
      remove_rownames %>% 
      column_to_rownames(var="ID") %>%
      scale()
    # Distance matrix
    S_d <- dist(S_plit4A, method=input$S_dmethod)
    # Hierarchical clustering
    S_hc <- hclust(S_d, method=input$S_cmethod)
    # Dendrogram
    S_dend <- color_branches(as.dendrogram(S_hc), k=input$S_clusters, groupLabels = TRUE)
    S_dend <- color_labels(S_dend, k=input$S_clusters)
    par(mar=c(2,2,2,10))
    plot(S_dend, horiz=TRUE, axes=FALSE)
  })
  
  # SECTION: SPLITS
  # SUB-SECTION: PACING(4)
  output$S_pacing4 <- renderGirafe({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    S_6 <- 40+(((S_distance)/50)-2)
    S_plit4A <- S_plit1 %>%
      select(c(40:all_of(S_6)), ID) %>%
      remove_rownames %>% 
      column_to_rownames(var="ID") %>%
      scale()
    ## Distance matrix
    S_d <- dist(S_plit4A, method=input$S_dmethod)
    ## Hierarchical clustering
    S_hc <- hclust(S_d, method=input$S_cmethod)
    ## append clusters to table
    S_groups <- cutree(S_hc, k=input$S_clusters)
    S_plit4B <- S_plit1 %>%
      cbind(Cluster = S_groups) %>%
      as.data.frame() %>%
      mutate(Cluster = as.character(Cluster)) %>%
      pivot_longer(c(4:all_of(S_1)), names_to="Split", values_to="Time") %>%
      mutate(Split = as.numeric(Split)) %>%
      mutate(Speed = 50/Time)
    ## Plot
    S_4B <- ggplot(data=S_plit4B, aes(x=Split, y=Speed, group=ID, color=Cluster)) +
      geom_line_interactive(aes(data_id=Cluster, tooltip=paste("Cluster ", Cluster))) +
      scale_color_brewer(palette = "Dark2") +
      ggtitle("Splits by pacing cluster") +
      scale_x_continuous(name="Distance (m)", 
                         breaks=seq(50,S_distance,ifelse(S_distance==1500, 100, 50))) +
      scale_y_continuous(name="Speed (m/s)") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=10.5)) 
    girafe(code = print(S_4B), ggobj=S_4B, options = list(opts_hover_inv(css = "opacity:0.2;")))
  })
  
  # SECTION: SPLITS
  # SUB-SECTION: PERFORM(1)
  output$S_perform1 <- renderPlot({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    S_2 <- (((S_distance/50)-2)/2)+3
    S_3 <- S_2+1
    S_4 <- S_2+(((S_distance/50)-2)/2)
    S_5 <- (S_distance - 100)/2
    lower <- input$S_range[1]
    upper <- input$S_range[2]
    ## Data Prep for Plot
    S_plit3A <-  S_plit1 %>%
      select(-c(40:68)) %>%
      select(-c("50",as.character(S_distance))) %>%
      mutate(Speed1 = (S_5/rowSums(.[4:S_2]))) %>%
      mutate(Speed2 = (S_5/rowSums(.[S_3:S_4]))) %>%
      mutate(Change = as.numeric(format(round((Speed2*100)/Speed1, 1), nsmall=1)))
    S_plit3B <- S_plit3A %>%
      mutate(Category = ifelse(Change<lower, "Positive", "Even")) %>%
      mutate(Category = ifelse(Change>upper, "Negative", Category)) %>%
      pivot_longer(c(4:(S_1-2)), names_to="Split", values_to="Time") %>%
      mutate(Split = as.numeric(Split)) %>%
      mutate(Speed = 50/Time)
    S_plit5A <- S_plit3B %>%
      select(ID, Category, AvgSpeed) %>%
      distinct()
    ggbetweenstats(data=S_plit5A, 
                   x=Category, 
                   y=AvgSpeed, 
                   type="np",
                   pairwise.comparisons=FALSE,
                   title="Median average speed by category",
                   centrality.label.args = list(size  = 6))
  })
  
  # SECTION: SPLITS
  # SUB-SECTION: PERFORM(2)
  output$S_perform2 <- renderPlot({
    ## Initial Data Prep
    S_plit1 <- S_plits %>%
      mutate(ID = as.factor(ID)) %>%
      filter(Event==input$S_event)
    ## Create Values
    S_distance <- as.numeric(S_plit1[1,"Distance"])
    S_1 <- (S_distance/50)+3
    S_6 <- 40+(((S_distance)/50)-2)
    S_plit4A <- S_plit1 %>%
      select(c(40:all_of(S_6)), ID) %>%
      remove_rownames %>% 
      column_to_rownames(var="ID") %>%
      scale()
    ## Distance matrix
    S_d <- dist(S_plit4A, method=input$S_dmethod)
    ## Hierarchical clustering
    S_hc <- hclust(S_d, method=input$S_cmethod)
    ## append clusters to table
    S_groups <- cutree(S_hc, k=input$S_clusters)
    S_plit4B <- S_plit1 %>%
      cbind(Cluster = S_groups) %>%
      as.data.frame() %>%
      mutate(Cluster = as.character(Cluster)) %>%
      pivot_longer(c(4:all_of(S_1)), names_to="Split", values_to="Time") %>%
      mutate(Split = as.numeric(Split)) %>%
      mutate(Speed = 50/Time)
    S_plit5B <- S_plit4B %>%
      select(ID, Cluster, AvgSpeed) %>%
      distinct()
    ggbetweenstats(data=S_plit5B, 
                   x=Cluster, 
                   y=AvgSpeed, 
                   type="np",
                   pairwise.comparisons=FALSE,
                   title="Median average speed by cluster",
                   centrality.label.args = list(size  = 6))
  })
  

}

######## END OF SERVER ########


######## RUN APPLICATION ########

shinyApp(ui = ui, server = server)

######## END ########