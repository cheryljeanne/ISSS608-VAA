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
               ggdist, 
               DT, 
               performance,
               htmltools)

######## DATA FILES ########

S_plits <- read_csv("data/splits.csv")
ST_swimdata <- read_csv("data/swimdata_clean3.csv")
ST_continents <- read_csv("data/continents.csv")
PR_swimdata <- read_csv("data/final_swimmers_PR.csv")
LM_swimdata <- read_csv("data/final_swimmers_MLR.csv")

######## END OF DATA FILES ########

### Apple input 18/3###
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
          "Compare Average Speed",
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

reactiontimePanel <- tabPanel(
  "Reaction Time",
  sidebarLayout(
    sidebarPanel(
      # Gender, Style, Distance, Round
      selectInput("RT_style",
                  label = h5("Style:"),
                  choices = unique(ST_swimdata$Style),
                  multiple = TRUE,
                  selected = unique(ST_swimdata$Style),
      ),
      selectInput("RT_gender",
                  label=h5("Gender:"),
                  choices=unique(ST_swimdata$Gender),
                  multiple = TRUE,
                  selected = unique(ST_swimdata$Gender),
      ),
      selectInput("RT_distance",
                  label=h5("Distance:"),
                  choices=unique(ST_swimdata$Distance),
                  multiple = TRUE,
                  selected = unique(ST_swimdata$Distance)
      ),
      selectInput("RT_round",
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
          titlePanel("Reaction Time of Swimmers"),
          fluidRow(
            column(12,
                   h3("Histogram Graph"),
                   p("The chart here shows the distribution of Reaction Time by the selected variables."),
                   girafeOutput("RT_reaction_time_dist", width="100%")
            )
          ),
          fluidRow(
            column(12,
                   h3("Continent Scatterstats Plot"),
                   p("The chart here shows the distribution of Reaction Time of continents by selected variables."),
                   plotOutput("RT_reaction_time_continent_dist", width="100%")
            ),
            
          ),
        ),
        tabPanel(
          "Compare Reaction Time",
          titlePanel("Comparison of Reaction Time"),
          fluidRow(
            column(12,
                   h3("Style"),
                   p("The chart here shows the comparison of Reaction Time by style."),
                   plotOutput("RT_reaction_time_compare_mean_style", width="100%"),
            )
          ),
          fluidRow(
            column(12,
                   h3("Gender"),
                   p("The chart here shows the comparison of Reaction Time by gender"),
                   plotOutput("RT_reaction_time_compare_mean_gender", width="100%"),
            )
          ),
          fluidRow(
            column(12,
                   h3("Distance"),
                   p("The chart here shows the comparison of Reaction Time by distance"),
                   plotOutput("RT_reaction_time_compare_mean_distance", width="100%"),
            )
          ),
          fluidRow(
            column(12,
                   h3("Round"),
                   p("The chart here shows the comparison of Reaction Time by round"),
                   plotOutput("RT_reaction_time_compare_mean_round", width="100%"),
            )
          ),
        ),
        tabPanel(
          "Correlation",
          titlePanel("Correlation of Reaction Time with Average Speed"),
          fluidRow(
            column(12,
                   p("Shows the correlation between Reaction Time with Average Speed by selected events"),
                   plotOutput("RT_reaction_time_correlation", width="60%", height="600px"),
            )
          ),
        ),
      )
    )
  )
)
### Apple input 18/3###

######## UI ########

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML(".selectize-input, .selectize-dropdown {font-size: 90%;}"))),
  navbarPage(HTML("<b>Swim Analytica</b>"),
             ### START Cheryl input 20/3###
             tabPanel("About",icon = icon("person-swimming"),
                      fluidPage(img(src='swimming-wallpapers.png', 
                                    align = "center", width="100%"))
                      ),
             tabPanel("Data",icon = icon("table"),
                      tags$div(
                        tags$h4("DATASET"),
                        "This tab showcases the dataset in which this shiny app and its analysis is built upon."),
                      tags$br(),tags$br(),
                      fluidRow(column(DT::dataTableOutput("rawtable"), width = 12))
                      ),
             ### END Cheryl input 20/3###
             ### START new input 17/3 ###
             navbarMenu(title = "Speed/Time", icon = icon("timer"),
                        averagespeedPanel,
                        reactiontimePanel),
             ### END new input 17/3 ###
             tabPanel("Split Times", icon = icon("water-ladder"),
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
             ### START - Cheryl 20/3 ###
             tabPanel("Performance Over Rounds", icon = icon("ranking-star"),
                      sidebarPanel(
                          selectInput("S_RoundsCompare",
                                      label=h5("Comparison of Rounds:"),
                                      choices=unique(PR_swimdata$RoundsCompare),
                                      multiple = TRUE,
                                      selected = unique(PR_swimdata$RoundsCompare)
                          )
                        ),
                        mainPanel(titlePanel("Comparison of Performances Over Rounds"),
                                  fluidRow(
                                    column(12,
                                           h3("Cumulative Distribution Function Plot"),
                                           p("The chart here shows the cumulative distribution of the comparison of swimmers' fractional performance changes over rounds."),
                                           plotlyOutput("PR_perf_over_rounds", width="100%", height = "600px"))
                                  ),
                                  fluidRow(
                                    column(12,
                                           h3("Gradient Interval Plot"),
                                           p("The chart here shows the confidence intervals of improvement in performances between swim rounds."),
                                           plotOutput("PR_perf_over_rounds_CI", width="100%", height = "500px"),
                                    )
                                  ))
                      ),
             ### END - Cheryl 20/3 ###
             ### START - Cheryl 22/3 ###
             tabPanel("Prediction", icon = icon("chart-line"),
                      sidebarPanel(
                        tags$p(HTML("<b>Predicted response variable used for the regression model is the <u>Final Swim Time.</u></b>")),
                        selectInput("LM_var",
                                    label=h5("Selection of Independent Variables for Regression Model:"),
                                    choices=c('OlypQualTime', 'OlypRecord', 'AvgTime', 'Heat', 
                                              'Semi', 'SDTime', 'RelTime_OQT', 'RelTime_ORT', 'Podium', 'RelTime_OQT_LR', 'RelTime_ORT_LR'),
                                    multiple = TRUE,
                                    selected = c('OlypQualTime', 'OlypRecord', 'AvgTime', 'Heat', 
                                                 'Semi', 'SDTime', 'RelTime_OQT', 'RelTime_ORT', 'Podium', 'RelTime_OQT_LR', 'RelTime_ORT_LR')
                        )),
                        mainPanel(titlePanel("Prediction of Final Swim Time"),
                                  fluidRow(
                                    column(12,
                                           h3("Model Diagnostic: Check for Multicolinearity"),
                                           p("The chart here visualises if multicolinearity exists within the selected variables for the regression model."),
                                           plotOutput("LM_colinearity", width="100%", height = "400px"))
                                  ),
                                  fluidRow(
                                    column(6,
                                           h3("Regression Analysis"),
                                           verbatimTextOutput("LM_output")),
                                    column(6,
                                           h3("Regression Parameters Plot"),
                                           p("The chart here visualises the parameters of the regression model built."),
                                           plotOutput("LM_regression", width="100%", height = "500px")
                                           )
                                  )
                                )
             )
             ### END - Cheryl 22/3 ###
             )
             
)
######## END OF UI ########


######## SERVER ########

# Define server logic required to draw a histogram
server <- function(input, output) {
  # SECTION: LANDING PAGE
  image_path <- reactive({
      "swimming-wallpapers.png"
  })
  
  # Display the image using the reactive expression
  output$image <- renderUI({
    img(src = image_path())
  })
  
  # SECTION: DATA
  ### START - Cheryl 20/3###
  output$rawtable <- DT::renderDataTable(
    DT::datatable({
      ST_swimdata
    },
    options = list(lengthMenu=list(c(5,10,20),c('5','10','20')), pageLength=10,
                   autoWidth = TRUE,
                   columnDefs = list(list(width = '200px', targets = "_all")),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    class = 'cell-border stripe',
    rownames = FALSE))
  ### END - Cheryl 20/3###
  
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
      geom_histogram_interactive(bins=10, aes(tooltip=ST_swimdata_subset$tooltip))+
      labs(x = "Average Speed (m/s)")+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
    
    p2 <- ggplot(data=ST_swimdata_subset, aes(x='', y=Average_speed)) +
      geom_boxplot() +
      coord_flip()+
      labs(x = "", y = "Average Speed (m/s)")
    
    p3 <- p2 + p1 + plot_layout(nrow = 2, heights = c(1, 5))
    girafe(
      code = print(p3),
      ggobj = p3,
      width_svg = 12,
      height_svg = 12*0.618,
    )
  })
  # SECTION: AVERAGE SPEED
  #SUB SUB SECTION: CONTINENT
  output$ST_average_speed_continent_dist <- renderPlot({
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
    
    ggdotplotstats(
      data       = ST_swimdata_subset,
      y          = Continent,
      x          = Average_speed,
      type       = "robust",
      xlab       = "Average Speed (m/s)",
      ylab.      = "Continent"
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })

  # SECTION: AVERAGE SPEED
  # SUB-SECTION: COMPARE MEAN
  output$ST_average_speed_compare_mean_style <- renderPlot({
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
    
    ggbetweenstats(
      data = ST_swimdata_subset,
      x = Style,
      y = Average_speed,
      type = "nonparametric",
      pairwise.comparisons = FALSE,
      centrality.label.args = list(size  = 4)
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
             text=element_text(size=12))
  })
  output$ST_average_speed_compare_mean_gender <- renderPlot({
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
    
      ggbetweenstats(
      data = ST_swimdata_subset,
      x = Gender,
      y = Average_speed,
      type = "nonparametric",
      pairwise.comparisons = FALSE,
      centrality.label.args = list(size  = 4)
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })
  output$ST_average_speed_compare_mean_distance <- renderPlot({
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
    
    ggbetweenstats(
      data = ST_swimdata_subset,
      x = Distance,
      y = Average_speed,
      type = "nonparametric",
      pairwise.comparisons = FALSE,
      centrality.label.args = list(size  = 4)
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })
  output$ST_average_speed_compare_mean_round <- renderPlot({
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
    
    ggbetweenstats(
      data = ST_swimdata_subset,
      x = Round,
      y = Average_speed,
      type = "nonparametric",
      pairwise.comparisons = FALSE,
      centrality.label.args = list(size  = 4)
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })
  # SECTION: AVERAGE SPEED
  # SUB-SECTION: CORRELATION
  output$ST_average_speed_correlation <- renderPlot({
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
    
    ggscatterstats(
      data = ST_swimdata_subset,
      x = Average_speed,
      y = Reaction_Time,
      type = "nonparametric",
      marginal = FALSE,
    )+
      labs(x = "Average Speed (m/s)", y = "Reaction Time (s)")+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })
  
  # SECTION: REACTION TIME
  # SUB-SECTION: DISTRIBUTION
  
  output$RT_reaction_time_dist <- renderGirafe({
    RT_swimdata_subset <- ST_swimdata
    
    if (!is_null( input$RT_style)) {
      RT_swimdata_subset <- ST_swimdata %>% filter(Style %in% input$RT_style)
    }
    
    if (!is_null(input$RT_distance)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Distance %in% input$RT_distance)
    }
    
    if (!is_null( input$RT_gender)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Gender %in% input$RT_gender)
    }
    
    if (!is_null(input$RT_round)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Round %in% input$RT_round)
    }
    
    RT_swimdata_subset$tooltip <- c(paste0(
      'Reaction Time = ', RT_swimdata_subset$Reaction_Time,
      "\n Name = ", RT_swimdata_subset$Name
    ))
    
    p1 <- ggplot(data=RT_swimdata_subset, aes(x=Reaction_Time)) + 
      geom_histogram_interactive(bins=10, aes(tooltip=RT_swimdata_subset$tooltip))+
      labs(x = "Reaction Time (s)")+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
    
    p2 <- ggplot(data=RT_swimdata_subset, aes(x='', y=Reaction_Time)) +
      geom_boxplot() +
      coord_flip()+
      labs(x = "", y = "Reaction Time (s)")
    
    p3 <- p2 + p1 + plot_layout(nrow = 2, heights = c(1, 5))
    girafe(
      code = print(p3),
      ggobj = p3,
      width_svg = 12,
      height_svg = 12*0.618,
    )
  })
  # SECTION: REACTION TIME
  #SUB SUB SECTION: CONTINENT
  output$RT_reaction_time_continent_dist <- renderPlot({
    RT_swimdata_subset <- ST_swimdata
    
    if (!is_null( input$RT_style)) {
      RT_swimdata_subset <- ST_swimdata %>% filter(Style %in% input$RT_style)
    }
    
    if (!is_null(input$RT_distance)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Distance %in% input$RT_distance)
    }
    
    if (!is_null( input$RT_gender)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Gender %in% input$RT_gender)
    }
    
    if (!is_null(input$RT_round)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Round %in% input$RT_round)
    }
    
    ggdotplotstats(
      data       = RT_swimdata_subset,
      y          = Continent,
      x          = Reaction_Time,
      type       = "robust",
      xlab       = "Reaction Time (s)",
      ylab.      = "Continent"
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })
  # SECTION: REACTION TIME
  # SUB-SECTION: COMPARE MEAN
  output$RT_reaction_time_compare_mean_style <- renderPlot({
    RT_swimdata_subset <- ST_swimdata
    
    if (!is_null( input$RT_style)) {
      RT_swimdata_subset <- ST_swimdata %>% filter(Style %in% input$RT_style)
    }
    
    if (!is_null(input$RT_distance)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Distance %in% input$RT_distance)
    }
    
    if (!is_null( input$RT_gender)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Gender %in% input$RT_gender)
    }
    
    if (!is_null(input$RT_round)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Round %in% input$RT_round)
    }
    
    ggbetweenstats(
      data = RT_swimdata_subset,
      x = Style,
      y = Reaction_Time,
      type = "nonparametric",
      pairwise.comparisons = FALSE,
      centrality.label.args = list(size  = 4)
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })
  output$RT_reaction_time_compare_mean_gender <- renderPlot({
    RT_swimdata_subset <- ST_swimdata
    
    if (!is_null( input$RT_style)) {
      RT_swimdata_subset <- ST_swimdata %>% filter(Style %in% input$RT_style)
    }
    
    if (!is_null(input$RT_distance)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Distance %in% input$RT_distance)
    }
    
    if (!is_null( input$RT_gender)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Gender %in% input$RT_gender)
    }
    
    if (!is_null(input$RT_round)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Round %in% input$RT_round)
    }
    
    ggbetweenstats(
      data = RT_swimdata_subset,
      x = Gender,
      y = Average_speed,
      type = "nonparametric",
      pairwise.comparisons = FALSE,
      centrality.label.args = list(size  = 4)
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })
  output$RT_reaction_time_compare_mean_distance <- renderPlot({
    RT_swimdata_subset <- ST_swimdata
    
    if (!is_null( input$RT_style)) {
      RT_swimdata_subset <- ST_swimdata %>% filter(Style %in% input$RT_style)
    }
    
    if (!is_null(input$RT_distance)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Distance %in% input$RT_distance)
    }
    
    if (!is_null( input$RT_gender)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Gender %in% input$RT_gender)
    }
    
    if (!is_null(input$RT_round)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Round %in% input$RT_round)
    }
    ggbetweenstats(
      data = RT_swimdata_subset,
      x = Distance,
      y = Reaction_Time,
      type = "nonparametric",
      pairwise.comparisons = FALSE,
      centrality.label.args = list(size  = 4)
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })
  output$RT_reaction_time_compare_mean_round <- renderPlot({
    RT_swimdata_subset <- ST_swimdata
    
    if (!is_null( input$RT_style)) {
      RT_swimdata_subset <- ST_swimdata %>% filter(Style %in% input$RT_style)
    }
    
    if (!is_null(input$RT_distance)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Distance %in% input$RT_distance)
    }
    
    if (!is_null( input$RT_gender)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Gender %in% input$RT_gender)
    }
    
    if (!is_null(input$RT_round)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Round %in% input$RT_round)
    }

        ggbetweenstats(
      data = RT_swimdata_subset,
      x = Round,
      y = Average_speed,
      type = "nonparametric",
      pairwise.comparisons = FALSE,
      centrality.label.args = list(size  = 4)
    )+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
  })
  # SECTION: REACTION TIME
  # SUB-SECTION: CORRELATION
  output$RT_reaction_time_correlation <- renderPlot({
    RT_swimdata_subset <- ST_swimdata
    
    if (!is_null( input$RT_style)) {
      RT_swimdata_subset <- ST_swimdata %>% filter(Style %in% input$RT_style)
    }
    
    if (!is_null(input$RT_distance)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Distance %in% input$RT_distance)
    }
    
    if (!is_null( input$RT_gender)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Gender %in% input$RT_gender)
    }
    
    if (!is_null(input$RT_round)) {
      RT_swimdata_subset <- RT_swimdata_subset %>% filter(Round %in% input$RT_round)
    }
    
    ggscatterstats(
      data = RT_swimdata_subset,
      x = Reaction_Time,
      y = Average_speed,
      type = "nonparametric",
      marginal = FALSE,
    )+
      labs(x = "Reaction Time (s)", y = "Average Speed (m/s)")+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
            text=element_text(size=12))
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
  
  # SECTION: PERFOVERROUNDS
  output$PR_perf_over_rounds <- renderPlotly({
    PR_swimdata_subset <- PR_swimdata
    
    if (!is_null(input$S_RoundsCompare)) {
      PR_swimdata_subset <- PR_swimdata %>% filter(RoundsCompare %in% input$S_RoundsCompare)
    }
    
    ## Plotting on ggplotly
    graphics.off()
    pdf(NULL)
    ggly <- ggplotly(
      ggplot(PR_swimdata_subset, aes(Time, colour = RoundsCompare)) + 
      stat_ecdf(geom = "point")+
      labs(title="Probability of Improvement in Finals Performance",
           y = "Cumulative Distribution Function", 
           x = "% Improvement in Swim Time") +
      theme_classic() +
      geom_vline(xintercept=mean(PR_swimdata$Time, na.rm=TRUE), linetype="dashed", color = "red") +
      annotate("text", x= -0.001, y=1, label="Avg Improvement in \nFinals Swim Time", size=4, color = "red", angle=0) +
      theme(axis.text=element_text(size=8),
              axis.title=element_text(size=10,face="bold"))
    )
    
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
    
  })
  output$PR_perf_over_rounds_CI <- renderPlot({
    PR_swimdata_subset <- PR_swimdata
    
    if (!is_null(input$S_RoundsCompare)) {
      PR_swimdata_subset <- PR_swimdata %>% filter(RoundsCompare %in% input$S_RoundsCompare)
    }
    
    ## Plotting on ggplot
    PR_swimdata_subset %>%
      ggplot(aes(x = RoundsCompare, 
                 y = Time)) +
      stat_gradientinterval(   
        fill = "skyblue"
      ) +                        
      labs(
        subtitle = "Line width is defined at 66% and 95% intervals respectively.") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")
      )
    
  })
  
  # SECTION: PREDICTION
  output$LM_colinearity <- renderPlot({
      LM_swimdata_subset <- LM_swimdata
      
      if (!is_null(input$LM_var)) {
        LM_swimdata_subset <- LM_swimdata %>% 
          select (OlypQualTime, OlypRecord, Final, Heat, Semi, AvgTime, SDTime, RelTime_OQT, RelTime_ORT, Podium, RelTime_OQT_LR, RelTime_ORT_LR)
      }
    
      inputs <- paste(c(input$LM_var), collapse = " + ")
      forms <- as.formula(paste0("Final", "~",  inputs))
      model <- lm(forms, data = LM_swimdata_subset)
      
      check_c <- check_collinearity(model)
      plot(check_c)
    })
    
  output$LM_output <- renderPrint({
    LM_swimdata_subset <- LM_swimdata
    
    if (!is_null(input$LM_var)) {
      LM_swimdata_subset <- LM_swimdata %>% 
        select (OlypQualTime, OlypRecord, Final, Heat, Semi, AvgTime, SDTime, RelTime_OQT, RelTime_ORT, Podium, RelTime_OQT_LR, RelTime_ORT_LR)
    }
    
    inputs <- paste(c(input$LM_var), collapse = " + ")
    forms <- as.formula(paste0("Final", "~",  inputs))
    model <- lm(forms, data = LM_swimdata_subset)
    
    output <- summary(model)
    print(output)
  })
  
  output$LM_regression <- renderPlot({
    LM_swimdata_subset <- LM_swimdata
    
    if (!is_null(input$LM_var)) {
      LM_swimdata_subset <- LM_swimdata %>% 
        select (OlypQualTime, OlypRecord, Final, Heat, Semi, AvgTime, SDTime, RelTime_OQT, RelTime_ORT, Podium, RelTime_OQT_LR, RelTime_ORT_LR)
    }
    
    inputs <- paste(c(input$LM_var), collapse = " + ")
    forms <- as.formula(paste0("Final", "~",  inputs))
    model <- lm(forms, data = LM_swimdata_subset)
    
    ggcoefstats(model, 
                output = "plot")
  })

} 

######## END OF SERVER ########


######## RUN APPLICATION ########

shinyApp(ui = ui, server = server)

######## END ########