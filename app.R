{library(shinythemes)
  library(shiny)
  library(foreign)
  library(sets)
  library(dplyr)
  library(haven)
  library(ggplot2)
  library(expss)
  library(grid)
  library(gridExtra)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rgeos)
  library(gdata)
  library(shinyWidgets)
  library(survey)
  library(splitstackshape)} # Load in libraries

{
  `%notin%` = Negate(`%in%`)
  tol <- as.data.frame(read.spss("data/tol.sav"))
  agg <- as.data.frame(read.spss("data/agg.sav"))
  agg_3 <- as.data.frame(read.spss("data/agg_3.sav"))
  agg_9 <- as.data.frame(read.spss("data/agg_9.sav"))
  theme_set(theme_bw())
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  world <- select(world, name, continent)
  world <- subset(world, (continent %in% c("Europe","Asia","Africa")))
  world1 <- world
  world1$name <- recode(world$name,"Austria" ~ "AT", "Belgium" ~ "BE", "Bulgaria" ~ "BG", "Switzerland"~ "CH",
                        "Cyprus" ~ "CY",  "Germany" ~ "DE", "Denmark"  ~ "DK", "Estonia"    ~ "EE",
                        "Spain" ~ "ES",   "Finland" ~ "FI", "France" ~"FR",    "United Kingdom" ~"UK",
                        "Hungary"~"HU",   "Ireland"~"EI",   "Netherlands" ~"NL",
                        "Norway"~"NO",    "Poland"~"PL",    "Portugal"~"PT",   "Russia" ~ "RU",
                        "Sweden"~"SE",    "Slovenia"~"SL",  "Slovakia"~"SK",   "Ukraine"~"UA",
                        "Czech Rep."~"CZ",   "Italy"~"IT",     "Serbia"~"RS", "Belarus" ~ "BY",
                        "Lithuania" ~ "LT", "Latvia" ~ "LV", "Iceland" ~ "IS", "Morocco" ~ "MA",
                        "Liechtenstein" ~ "LI", "Luxembourg" ~ "LU", "Algeria" ~ "DZ",
                        "Tunisia" ~ "TN", "Turkey" ~ "TR", "Georgia" ~ "GE", "Azerbaijan" ~ "AZ")
  world1 <- na.omit(world1)
  #colnames(agg)[1] <- "name"
  world_2006 <- merge(world1,agg_3, by = "name")
  world_2006$tygpnt_f <- as.factor(world_2006$tygpnt_f)
  world_2006$tygpnt_m <- as.factor(world_2006$tygpnt_m)
  world_2006$iagpnt_f <- as.factor(world_2006$iagpnt_f)
  world_2006$iagpnt_m <- as.factor(world_2006$iagpnt_m)
  world_2006$tochld_f <- as.factor(world_2006$tochld_f)
  world_2006$tochld_m <- as.factor(world_2006$tochld_m)
  
  world_2018 <- merge(world1,agg_9, by = "name")
  world_2018$tygpnt_f <- as.factor(world_2018$tygpnt_f)
  world_2018$tygpnt_m <- as.factor(world_2018$tygpnt_m)
  world_2018$iagpnt_f <- as.factor(world_2018$iagpnt_f)
  world_2018$iagpnt_m <- as.factor(world_2018$iagpnt_m)
  world_2018$tochld_f <- as.factor(world_2018$tochld_f)
  world_2018$tochld_m <- as.factor(world_2018$tochld_m)
  
  # world$tygpnt_f <-as.factor(world$tygpnt_f)
  # world$tygpnt_m <-as.factor(world$tygpnt_m)
  # world$iagpnt_f <-as.factor(world$iagpnt_f)
  # world$iagpnt_m <-as.factor(world$iagpnt_m)
  # world$tochld_f <-as.factor(world$iagpnt_f)
  # world$tochld_m <-as.factor(world$iagpnt_m)
  
} # Data setup and functions

ui <- {navbarPage("ESS Timing of Life",
                  theme = shinythemes::shinytheme("sandstone"),
                  windowTitle = "ESS Timing of Life",
                  {tabPanel("Main page",
                            {fluidPage(
                              {fluidRow(
                                column(2,
                                       img(src = "logo.png", height = "10%", width = "100%")
                                ),
                                column(9,
                                       h1("Welcome to the ESS Timing of Life interactive dashboard!", align = "center")
                                ),
                                column(1,
                                       
                                )
                              )}, # Title
                              {fluidRow(
                                hr(),
                                br(),
                                column(3,
                                       img(src = "background1.png", width = "70%",style="display: block; margin-left: auto; margin-right: auto;")
                                ),
                                column(6,
                                       p("The European Social Survey (ESS) is an 
            academically driven cross-national survey using high methodological standards to provide freely 
            available data for 38 countries. All data used is available on the", 
                                         a("ESS website", href = "https://www.europeansocialsurvey.org/", inline = T, .noWS = "after"),
                                         ". The aim of this web app is to help understand data from the 'Timing of Life' module, using powerful, 
            interactive visualisation. The Timing of life module was fielded in ESS3 (2006) and repeated in ESS9 
            (2018). It aims to understand the views of European citizens on the organisation of the life course 
            and of their strategies to influence and plan their own lives. It also includes measures on youngest 
            age and oldest age of life events, planning for retirement and the timing of key life events.", 
                                         style = "text-align: justify"),
                                       h3("Box plots and country codes", align = "center"),
                                       br(),
                                       fluidRow(
                                         column(2,
                                                img(src = "legend3.png",style="display: block; margin-left: auto; margin-right: auto;",
                                                    width= "100%")
                                         ),
                                         column(10,
                                                p("In box plots, the central horizontal line indicates the median, the central rectangle shows 
              the interquartile range, and the dots at the top and bottom indicate any outliers that may be 
              present. In some questions, the ballot was split evenly (asking about women or men). This will be 
              indicated where applicable. Data for some countries may be missing. The plots below use very many 
              data points, and many box plots need to be rendered, so the plots may take a few seconds to load.", 
                                                  style = "text-align: justify")
                                         )
                                         
                                       ),
                                       br(),
                                       p("Countries are represented by their 2-letter country ISO code. A full list of ISO codes is available ",
                                         a("here", href = "https://www.iban.com/country-codes", inline = T, .noWS = "after"),". Some lesser 
            known country codes are:"),
                                       fluidRow(
                                         column(4,
                                                tags$div(tags$ul(
                                                  tags$li(strong("CH"),"for Switzerland")
                                                )),
                                                
                                                tags$div(tags$ul(
                                                  tags$li(strong("RS"),"for Serbia")
                                                ))
                                         ),
                                         column(4,
                                                tags$div(tags$ul(
                                                  tags$li(strong("UA"),"for Ukraine")
                                                )),
                                                
                                                tags$div(tags$ul(
                                                  tags$li(strong("EI"),"for Ireland")
                                                ))
                                         ),
                                         column(4,
                                                tags$div(tags$ul(
                                                  tags$li(strong("DE"),"for Germany")
                                                )),
                                                
                                                tags$div(tags$ul(
                                                  tags$li(strong("EE"),"for Estonia")
                                                ))
                                         )
                                       )
                                       
                                ),
                                column(3,
                                       img(src = "background2.png", width = "70%",style="display: block; margin-left: auto; margin-right: auto;")
                                )
                              )}, # Introduction
                              {fluidRow(
                                br(),
                                hr(),
                                h3("Demographics selector", align = "center"),
                                br(),
                                column(3,
                                       img(src = "background_alt1.png", width = "70%", style="display: block; margin-left: auto; margin-right: auto;")
                                ),
                                column(6,
                                       p("In this section, you can subset the data that the plotting tabs below will take in to formulate box 
              plots. Please note that for certain comparitive plots, some of the selectors will",
                                         strong("not"),
                                         "work. For example, the gender comparison plots will include both genders, regardless of the selection 
              specified here, but the year of collection and age of respondents can be selected normally.",
                                         style = "text-align: justify"),
                                       br(),
                                       {fluidRow(
                                         column(6,
                                                sliderInput("age",
                                                            label = "Select Age Group",
                                                            min = 0,
                                                            max = 100,
                                                            value = c(0, 100)),
                                                selectInput("gender",
                                                            label = "Select Gender",
                                                            choices = c("Female and Male","Female","Male"),
                                                            selected = "Female and Male"
                                                ),
                                                selectInput("year",
                                                            label = "Select data collection Year",
                                                            choices = c("2006 and 2018","2006","2018"),
                                                            selected = "2018"),
                                                checkboxGroupInput("edu",
                                                            label = "Select Highest Education Level",
                                                            choices = list("ES-ISCED I , less than lower secondary" = 1,
                                                                           "ES-ISCED II, lower secondary" = 2,
                                                                           "ES-ISCED IIIb, lower tier upper secondary" = 3,
                                                                           "ES-ISCED IIIa, upper tier upper secondary" = 4,
                                                                           "ES-ISCED IV, advanced vocational, sub-degree" = 5,
                                                                           "ES-ISCED V1, lower tertiary education, BA level" = 6,
                                                                           "ES-ISCED V2, higher tertiary education, >= MA level" = 7,
                                                                           "Other/Missing data" =0),
                                                            selected = c("ES-ISCED I , less than lower secondary" = 1,
                                                                         "ES-ISCED II, lower secondary" = 2,
                                                                         "ES-ISCED IIIb, lower tier upper secondary" = 3,
                                                                         "ES-ISCED IIIa, upper tier upper secondary" = 4,
                                                                         "ES-ISCED IV, advanced vocational, sub-degree" = 5,
                                                                         "ES-ISCED V1, lower tertiary education, BA level" = 6,
                                                                         "ES-ISCED V2, higher tertiary education, >= MA level" = 7,
                                                                         "Other/Missing data" =0)),
                                                actionButton("selectalledu", label = "Select/Deselect all")
                                         ),
                                         column(6,
                                                fluidRow(
                                                  column(12,
                                                         checkboxGroupInput(
                                                           "cntry",
                                                           label = "Select Country",
                                                           inline = T,
                                                           choices = list("Austria" = "AT","Belgium" = "BE", "Bulgaria" = "BG","Cyprus" = "CY","Czechia" = "CZ",
                                                                          "Germany" = "DE","Denmark" = "DK","Estonia" = "EE","Spain" = "ES","Finland" = "FI",
                                                                          "France" = "FR","Hungary" = "HU","Ireland" = "EI","Italy" = "IT","Netherlands" = "NL",
                                                                          "Norway" = "NO","Poland" = "PL","Portugal" = "PT","Russia" = "RU","Serbia" = "RS",
                                                                          "Sweden" = "SE","Slovakia" = "SK","Slovenia" = "SL","Switzerland" = "CH",
                                                                          "Ukraine" = "UA","UK" = "UK"),
                                                           selected = c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EI", "ES", "FI", "FR", "HU",
                                                                        "IT", "NL", "NO", "PL", "PT", "RS", "RU", "SE", "SK", "SL", "UA", "UK","EI","RS"))
                                                  ),
                                                  actionButton("selectall", label="Select/Deselect all")
                                                )
                                         )
                                       )}
                                ),
                                column(3,
                                       img(src = "background_alt2.png", width = "70%", style="display: block; margin-left: auto; margin-right: auto;")
                                ),
                                fluidRow(
                                  column(12,
                                         hr(),
                                         {fluidRow(
                                           h4(align = "center",
                                              "You have selected",
                                              strong(span(textOutput("selected_gen", inline = T), style = "color:darkred")),
                                              "respondents",
                                              #                    strong(span(textOutput("selected_cntry", inline = T), style = "color:darkred")),
                                              "in",
                                              strong(span(textOutput("selected_year", inline = T), style = "color:darkred")),
                                              "between the ages of",
                                              strong(span(textOutput("selected_age_min", inline = T), style = "color:darkred")),
                                              "and",
                                              strong(span(textOutput("selected_age_max", inline = T), style = "color:darkred")),
                                              "from the following countries:", tags$br(),
                                              strong(span(textOutput("selected_cntry", inline = T), style = "color:darkred")),
                                              "(n =",
                                              strong(span(textOutput("selected_n", inline = T), style = "color:darkred")),
                                              span(").", .noWS = "outside")
                                           )
                                         )},  # Show selected N
                                         br(),
                                         #                textOutput("selected_cntry")
                                  )
                                )
                              )}, # Demographics selector
                              {navbarPage("Question of interest",
                                          navbarMenu("Child-bearing ages",
                                                     {tabPanel("Age too young to become a parent",
                                                               {fluidRow(
                                                                 h3("TYGPNT: Before what age would you say a woman/man is too young to become a mother/father?", 
                                                                    align = "center"),
                                                                 br(),
                                                                 {tabsetPanel(
                                                                   {tabPanel("Overview",
                                                                             br(),
                                                                             p("In this tab you can subset the data as you like, using the buttons above. This serves as an overview of
                    the responses given to this question, to see the survey results as a whole. Use the tabs to the right to 
                    see comparisons."),
                                                                             plotOutput("tygpnt_overview", height = 600),
                                                                   )}, # Overview
                                                                   {tabPanel("By gender (respondent)",
                                                                             br(),
                                                                             p("The plots below show the difference between the responses of men and women, by country. Women's 
                    responses are depicted by",
                                                                               strong(span("red", style = "color:red")),
                                                                               "boxplots, and men's are depicted by their",
                                                                               strong(span("blue", style = "color:blue")),
                                                                               "counterparts. For this plot, 
                    the gender selection tool above will not work (as both genders are represented)."),
                                                                             plotOutput("tygpnt_by_gender", height = 600)
                                                                   )}, # By gender
                                                                   {tabPanel("By year",
                                                                             br(),
                                                                             p("The plots below show the difference between the responses from 2006 and 2018, by country. 
                    Responses from 2006 are depicted by",
                                                                               strong(span("yellow", style = "color:gold")),
                                                                               "boxplots, and responses from 2018 by their",
                                                                               strong(span("green", style = "color:green")),
                                                                               "counterparts. For this plot, the year selection tool above will not work (as data from both ESS waves 
                    are presented). Data for the following countries is missing for either 2006 or 2018, and will be excluded 
                    from the plot: ", strong("Czechia, Denmark, Spain, Italy, Portugal, Serbia, Russia, Sweden, Slovakia, and Ukraine.")),
                                                                             plotOutput("tygpnt_by_year", height = 600)
                                                                   )}, # By year
                                                                   {tabPanel("By gender asked about", 
                                                                             br(),
                                                                             p("The respondents were split into two roughly equally numerous groups, and the question was posed to them 
                    featuring women for one group, and men for another. The following plots show the difference between the 
                    answers men and women gave to the question, depending on which gender was asked about. Responses to 
                    questions asked about women are shown using",
                                                                               strong(span("red", style = "color:red")),
                                                                               "boxplots, and responses to questions about men by their",
                                                                               strong(span("blue", style = "color:blue")),
                                                                               "counterparts. For this plot, the gender selection tool above will not work (as data for both are 
                    presented)."),
                                                                             plotOutput("tygpnt_by_ballot", height = 600)
                                                                   )}, # By ballot
                                                                   {tabPanel("By cohort",
                                                                             br(),
                                                                             p("The plots below show the difference between the responses of people born before 
                    1960, between 1960 and 1990, and those born after 1990. The age selector tool will
                    not work for these plots."),
                                                                             plotOutput("tygpnt_by_cohort", height = 600)
                                                                   )}  # By cohort
                                                                 )} # TYGPNT Plotting tabs
                                                               )}  # TYGPNT question
                                                     )}, # TYGPNT tab
                                                     {tabPanel("Ideal age to become a parent",
                                                               {fluidRow(
                                                                 h3("IAGPNT: In your opinion, what is the ideal age for a girl/boy or woman/man to become a mother/father?",
                                                                    align = "center"),
                                                                 br(),
                                                                 {tabsetPanel(
                                                                   {tabPanel("Overview",
                                                                             br(),
                                                                             p("In this tab you can subset the data as you like, using the buttons above. This serves as an overview of
                    the responses given to this question, to see the survey results as a whole. Use the tabs to the right to 
                    see comparisons."),
                                                                             plotOutput("iagpnt_overview", height = 600)
                                                                   )}, # Overview
                                                                   {tabPanel("By gender (respondent)",
                                                                             br(),
                                                                             p("The plots below show the difference between the responses of men and women, by country. Women's 
                    responses are depicted by",
                                                                               strong(span("red", style = "color:red")),
                                                                               "boxplots, and men's are depicted by their",
                                                                               strong(span("blue", style = "color:blue")),
                                                                               "counterparts. For this plot, 
                    the gender selection tool above will not work (as both genders are represented)."),
                                                                             plotOutput("iagpnt_by_gender", height = 600)
                                                                   )}, # By gender
                                                                   {tabPanel("By year",
                                                                             br(),
                                                                             p("The plots below show the difference between the responses from 2006 and 2018, by country. 
                    Responses from 2006 are depicted by",
                                                                               strong(span("yellow", style = "color:gold")),
                                                                               "boxplots, and responses from 2018 by their",
                                                                               strong(span("green", style = "color:green")),
                                                                               "counterparts. For this plot, the year selection tool above will not work (as data from both ESS waves 
                    are presented). Data for the following countries is missing for either 2006 or 2018, and will be excluded 
                    from the plot: ", strong("Czechia, Denmark, Spain, Italy, Portugal, Serbia, Russia, Sweden, Slovakia, and Ukraine.")),
                                                                             plotOutput("iagpnt_by_year", height = 600)
                                                                   )}, # By year
                                                                   {tabPanel("By gender asked about",
                                                                             br(),
                                                                             p("The respondents were split into two roughly equally numerous groups, and the question was posed to them 
                    featuring women for one group, and men for another. The following plots show the difference between the 
                    answers men and women gave to the question, depending on which gender was asked about. Responses to 
                    questions asked about women are shown using",
                                                                               strong(span("red", style = "color:red")),
                                                                               "boxplots, and responses to questions about men by their",
                                                                               strong(span("blue", style = "color:blue")),
                                                                               "counterparts. For this plot, the gender selection tool above will not work (as data for both are 
                    presented)."),
                                                                             plotOutput("iagpnt_by_ballot", height = 600)
                                                                   )}, # By ballot
                                                                   {tabPanel("By cohort",
                                                                             br(),
                                                                             p("The plots below show the difference between the responses of people born before 
                    1960, between 1960 and 1990, and those born after 1990. The age selector tool will
                    not work for these plots."),
                                                                             plotOutput("iagpnt_by_cohort", height = 600)
                                                                   )}  # By cohort
                                                                 )} # IAGPNT plotting tabs
                                                               )} # IAGPNT question
                                                     )}, # IAGPNT tab
                                                     {tabPanel("Age too old to have more children",
                                                               {fluidRow(
                                                                 h3("TOCHLD: After what age would you say a woman/man is generally too old to consider having any more children?", 
                                                                    align = "center"),
                                                                 br(),
                                                                 {tabsetPanel(
                                                                   {tabPanel("Overview",
                                                                             br(),
                                                                             p("In this tab you can subset the data as you like, using the buttons above. This serves as an overview of
                    the responses given to this question, to see the survey results as a whole. Use the tabs to the right to 
                    see comparisons."),
                                                                             plotOutput("tochld_overview", height = 600),
                                                                   )}, # Overview
                                                                   {tabPanel("By gender (respondent)",
                                                                             br(),
                                                                             p("The plots below show the difference between the responses of men and women, by country. Women's 
                    responses are depicted by",
                                                                               strong(span("red", style = "color:red")),
                                                                               "boxplots, and men's are depicted by their",
                                                                               strong(span("blue", style = "color:blue")),
                                                                               "counterparts. For this plot, 
                    the gender selection tool above will not work (as both genders are represented)."),
                                                                             plotOutput("tochld_by_gender", height = 600)
                                                                   )}, # By gender
                                                                   {tabPanel("By year",
                                                                             br(),
                                                                             p("The plots below show the difference between the responses from 2006 and 2018, by country. 
                    Responses from 2006 are depicted by",
                                                                               strong(span("yellow", style = "color:gold")),
                                                                               "boxplots, and responses from 2018 by their",
                                                                               strong(span("green", style = "color:green")),
                                                                               "counterparts. For this plot, the year selection tool above will not work (as data from both ESS waves 
                    are presented). Data for the following countries is missing for either 2006 or 2018, and will be excluded 
                    from the plot: ", strong("Czechia, Denmark, Spain, Italy, Portugal, Serbia, Russia, Sweden, Slovakia, and Ukraine.")),
                                                                             plotOutput("tochld_by_year", height = 600)
                                                                   )}, # By year
                                                                   {tabPanel("By gender asked about", 
                                                                             br(),
                                                                             p("The respondents were split into two roughly equally numerous groups, and the question was posed to them 
                    featuring women for one group, and men for another. The following plots show the difference between the 
                    answers men and women gave to the question, depending on which gender was asked about. Responses to 
                    questions asked about women are shown using",
                                                                               strong(span("red", style = "color:red")),
                                                                               "boxplots, and responses to questions about men by their",
                                                                               strong(span("blue", style = "color:blue")),
                                                                               "counterparts. For this plot, the gender selection tool above will not work (as data for both are 
                    presented)."),
                                                                             plotOutput("tochld_by_ballot", height = 600)
                                                                   )},  # By ballot
                                                                   {tabPanel("By cohort",
                                                                             br(),
                                                                             p("The plots below show the difference between the responses of people born before 
                    1960, between 1960 and 1990, and those born after 1990. The age selector tool will
                    not work for these plots."),
                                                                             plotOutput("tochld_by_cohort", height = 600)
                                                                   )}  # By cohort
                                                                 )} # TOCHLD Plotting tabs
                                                               )}  # TOCHLD question
                                                     )}  # TOCHLD tab
                                          )
                                          #          ,tabPanel("Collapse menu")
                              )}, # Question selector menu
                            )} # First page
                  )}, # Main Page
                  {tabPanel("Map drawer",
                            p("In this page you will be able to view responses to survey questions using a map drawer."),
                            selectInput("map_question",
                                        label = "Select variable",
                                        choices = c("Too young to become parent" = "tygpnt_",
                                                    "Ideal age to become parent" = "iagpnt_",
                                                    "Too old to have more children" = "tochld_")),
                            selectInput("map_ballot",
                                        label = "Select gender asked about",
                                        choices = c("Women" = "f","Men" = "m")),
                            plotOutput("map")
                  )}  # Map drawer
)}

server <- function(input, output, session) {
  
  {
    output$selected_gen <- renderText({ 
      paste(input$gender)
    })
    
    output$selected_year <- renderText({ 
      paste(input$year)
    })
    
    output$selected_age_min <- renderText({ 
      paste(input$age[1])
    })
    
    output$selected_age_max <- renderText({ 
      paste(input$age[2])
    })
    
    output$selected_cntry <- renderText(input$cntry)
    
    output$selected_n <- renderText({ 
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      paste(
        nrow(tol %>%
               subset(gender %in% chosen_gender) %>%
               subset(year %in% chosen_year) %>%
               subset(cntry %in% chosen_cntry) %>%
               subset(agea >= input$age[1] & agea <= input$age[2])
        )
      )
    })
  } # Selected N text
  
  {
    output$tygpnt_overview <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$tygpnt, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tygpnt, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tygpnt) %>% round(digits = 2)
      }
      
      length(subset(data, cntry == "HU")$tygpnt)
      length(data$tygpnt[which(data$cntry == "HU")])
      
      p1 <- ggplot(data %>% subset(ballot == 1),
                   mapping = aes(y = tygpnt))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,50),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"Before what age would you say a woman is generally too young to become a mother?"')
      
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      tbl1 <- tableGrob(t(data_agg1), rows=c("Country","N","Weighted Mean", "Mean SE", "Median"), theme=tt)
      
      p2 <- ggplot(data %>% subset(ballot == 2),
                   mapping = aes(y = tygpnt))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,50),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"Before what age would you say a man is generally too young to become a father?"')
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tygpnt, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tygpnt, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$tygpnt) %>% round(digits = 2)
      }
      
      tbl2 <- tableGrob(t(data_agg2), rows=c("Country","N","Weighted Mean", "Mean SE", "Median"), theme=tt)
      
      grid.arrange(p1,tbl1,p2,tbl2,nrow = 4)
      
    })
    
    output$tygpnt_by_gender <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      data <- tol %>%
        subset(ballot == 1) %>%
        subset(gender != "No answer") %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      ballot1 <- ggplot(data,
                        mapping = aes(y = tygpnt, fill = gender))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"Before what age would you say a woman is generally too young to become a mother?"')
      
      ballot2 <- ggplot(tol %>%
                          subset(ballot == 2) %>%
                          subset(gender != "No answer") %>%
                          subset(year %in% chosen_year) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(agea >= input$age[1] & agea <= input$age[2]),
                        mapping = aes(y = tygpnt, fill = gender))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"Before what age would you say a man is generally too young to become a father?"')
      
      grid.arrange(ballot1,ballot2, nrow = 2)
    })
    
    output$tygpnt_by_year <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      year1 <- ggplot(tol %>%
                        subset(ballot == 1) %>%
                        subset(gender != "No answer") %>%
                        subset(gender %in% chosen_gender) %>%
                        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
                        subset(cntry %in% chosen_cntry) %>%
                        subset(agea >= input$age[1] & agea <= input$age[2]),
                      mapping = aes(y = tygpnt, fill = year))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"Before what age would you say a woman is generally too young to become a mother?"')+
        scale_fill_manual(values=c("gold", "forestgreen"))
      
      year2 <- ggplot(tol %>%
                        subset(ballot == 2) %>%
                        subset(gender != "No answer") %>%
                        subset(gender %in% chosen_gender) %>%
                        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
                        subset(cntry %in% chosen_cntry) %>%
                        subset(agea >= input$age[1] & agea <= input$age[2]),
                      mapping = aes(y = tygpnt, fill = year))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"Before what age would you say a man is generally too young to become a father?"')+
        scale_fill_manual(values=c("gold", "forestgreen"))
      
      grid.arrange(year1,year2, nrow = 2)
    })
    
    output$tygpnt_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      women <- ggplot(tol %>%
                        subset(gender == "Female") %>%
                        subset(year %in% chosen_year) %>%
                        subset(cntry %in% chosen_cntry) %>%
                        subset(agea >= input$age[1] & agea <= input$age[2]),
                      mapping = aes(y = tygpnt, fill = ballot))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title='"Before what age would you say a ____ is generally too young to become a parent?" (WOMEN\'s responses)')+
        scale_fill_discrete(name = "gender asked",labels = c("woman","man"))
      
      men <- ggplot(tol %>%
                      subset(gender == "Male") %>%
                      subset(year %in% chosen_year) %>%
                      subset(cntry %in% chosen_cntry) %>%
                      subset(agea >= input$age[1] & agea <= input$age[2]),
                    mapping = aes(y = tygpnt, fill = ballot))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title ='"Before what age would you say a ____ is generally too young to become a parent?" (MEN\'s responses)')
      
      grid.arrange(women,men, nrow = 2)
    })
    
    output$tygpnt_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      cohort1 <- ggplot(tol %>%
                          subset(ballot == "1") %>%
                          subset(gender %in% chosen_gender) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(year %in% chosen_year),
                        mapping = aes(y = tygpnt, fill = cohort))+
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        geom_boxplot() +
        facet_wrap(~cntry, nrow = 1)+
        labs(title='"Before what age would you say a woman is generally too young to become a parent?"')+
        scale_fill_discrete(name = "Cohort",labels = c("- 1959","1960 - 1989","1990 - "))
      
      cohort2 <- ggplot(tol %>%
                          subset(ballot == "2") %>%
                          subset(gender %in% chosen_gender) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(year %in% chosen_year),
                        mapping = aes(y = tygpnt, fill = cohort))+
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        geom_boxplot() +
        facet_wrap(~cntry, nrow = 1)+
        labs(title='"Before what age would you say a man is generally too young to become a parent?"')
      
      grid.arrange(cohort1, cohort2, nrow = 2)
    })
  } # TYGPNT plots
  
  {
    output$iagpnt_overview <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      p1 <- ggplot(tol %>%
                     subset(ballot == 1) %>%
                     subset(gender != "No answer") %>%
                     subset(gender %in% chosen_gender) %>%
                     subset(year %in% chosen_year) %>%
                     subset(cntry %in% chosen_cntry) %>%
                     subset(agea >= input$age[1] & agea <= input$age[2]),
                   mapping = aes(y = iagpnt))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,50),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"In your opinion, what is the ideal age for a girl or woman to become a mother?"')
      
      p2 <- ggplot(tol %>%
                     subset(ballot == 2) %>%
                     subset(gender != "No answer") %>%
                     subset(gender %in% chosen_gender) %>%
                     subset(year %in% chosen_year) %>%
                     subset(cntry %in% chosen_cntry) %>%
                     subset(agea >= input$age[1] & agea <= input$age[2]),
                   mapping = aes(y = tygpnt))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,50),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"In your opinion, what is the ideal age for a boy or man to become a father?"')
      
      grid.arrange(p1,p2,nrow = 2)
      
    })
    
    output$iagpnt_by_gender <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      ballot1 <- ggplot(tol %>%
                          subset(ballot == 1) %>%
                          subset(gender != "No answer") %>%
                          subset(year %in% chosen_year) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(agea >= input$age[1] & agea <= input$age[2]),
                        mapping = aes(y = iagpnt, fill = gender))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"In your opinion, what is the ideal age for a girl or woman to become a mother?"')
      
      ballot2 <- ggplot(tol %>%
                          subset(ballot == 2) %>%
                          subset(gender != "No answer") %>%
                          subset(year %in% chosen_year) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(agea >= input$age[1] & agea <= input$age[2]),
                        mapping = aes(y = tygpnt, fill = gender))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"In your opinion, what is the ideal age for a boy or man to become a father?"')
      
      grid.arrange(ballot1,ballot2, nrow = 2)
    })
    
    output$iagpnt_by_year <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      year1 <- ggplot(tol %>%
                        subset(ballot == 1) %>%
                        subset(gender != "No answer") %>%
                        subset(gender %in% chosen_gender) %>%
                        subset(cntry %in% chosen_cntry) %>%
                        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
                        subset(agea >= input$age[1] & agea <= input$age[2]),
                      mapping = aes(y = iagpnt, fill = year))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"In your opinion, what is the ideal age for a girl or woman to become a mother?"')+
        scale_fill_manual(values=c("gold", "forestgreen"))
      
      year2 <- ggplot(tol %>%
                        subset(ballot == 2) %>%
                        subset(gender != "No answer") %>%
                        subset(gender %in% chosen_gender) %>%
                        subset(cntry %in% chosen_cntry) %>%
                        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
                        subset(agea >= input$age[1] & agea <= input$age[2]),
                      mapping = aes(y = tygpnt, fill = year))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"In your opinion, what is the ideal age for a boy or man to become a father?"')+
        scale_fill_manual(values=c("gold", "forestgreen"))
      
      grid.arrange(year1,year2, nrow = 2)
    })
    
    output$iagpnt_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      women <- ggplot(tol %>%
                        subset(gender == "Female") %>%
                        subset(year %in% chosen_year) %>%
                        subset(cntry %in% chosen_cntry) %>%
                        subset(agea >= input$age[1] & agea <= input$age[2]),
                      mapping = aes(y = iagpnt, fill = ballot))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title='"In your opinion, what is the ideal age for a ____ to become a parent?" (WOMEN\'s responses)')+
        scale_fill_discrete(name = "gender asked",labels = c("woman","man"))
      
      men <- ggplot(tol %>%
                      subset(gender == "Male") %>%
                      subset(year %in% chosen_year) %>%
                      subset(cntry %in% chosen_cntry) %>%
                      subset(agea >= input$age[1] & agea <= input$age[2]),
                    mapping = aes(y = tygpnt, fill = ballot))+
        geom_boxplot() +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title ='"In your opinion, what is the ideal age for a ____ to become a mother/father?" (MEN\'s responses)')
      
      grid.arrange(women,men, nrow = 2)
    })
    
    output$iagpnt_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      cohort1 <- ggplot(tol %>%
                          subset(ballot == "1") %>%
                          subset(gender %in% chosen_gender) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(year %in% chosen_year),
                        mapping = aes(y = iagpnt, fill = cohort))+
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        geom_boxplot() +
        facet_wrap(~cntry, nrow = 1)+
        labs(title='"Before what age would you say a woman is generally too young to become a parent?"')+
        scale_fill_discrete(name = "Cohort",labels = c("- 1959","1960 - 1989","1990 - "))
      
      cohort2 <- ggplot(tol %>%
                          subset(ballot == "2") %>%
                          subset(gender %in% chosen_gender) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(year %in% chosen_year),
                        mapping = aes(y = iagpnt, fill = cohort))+
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        geom_boxplot() +
        facet_wrap(~cntry, nrow = 1)+
        labs(title='"Before what age would you say a man is generally too young to become a parent?"')
      
      grid.arrange(cohort1, cohort2, nrow = 2)
    })
  } # IAGPNT plots
  
  {
    output$tochld_overview <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      p1 <- ggplot(tol %>%
                     subset(ballot == 1) %>%
                     subset(gender != "No answer") %>%
                     subset(gender %in% chosen_gender) %>%
                     subset(year %in% chosen_year) %>%
                     subset(cntry %in% chosen_cntry) %>%
                     subset(agea >= input$age[1] & agea <= input$age[2]),
                   mapping = aes(y = tochld))+
        geom_boxplot() +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"After what age would you say a woman is generally too old to consider having any more children?"')
      
      p2 <- ggplot(tol %>%
                     subset(ballot == 2) %>%
                     subset(gender != "No answer") %>%
                     subset(gender %in% chosen_gender) %>%
                     subset(year %in% chosen_year) %>%
                     subset(cntry %in% chosen_cntry) %>%
                     subset(agea >= input$age[1] & agea <= input$age[2]),
                   mapping = aes(y = tochld))+
        geom_boxplot() +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"After what age would you say a man is generally too old to consider having any more children?"')
      
      grid.arrange(p1,p2,nrow = 2)
      
    })
    
    output$tochld_by_gender <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      ballot1 <- ggplot(tol %>%
                          subset(ballot == 1) %>%
                          subset(gender != "No answer") %>%
                          subset(year %in% chosen_year) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(agea >= input$age[1] & agea <= input$age[2]),
                        mapping = aes(y = tochld, fill = gender))+
        geom_boxplot() +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"After what age would you say a woman is generally too old to consider having any more children?"')
      
      ballot2 <- ggplot(tol %>%
                          subset(ballot == 2) %>%
                          subset(gender != "No answer") %>%
                          subset(year %in% chosen_year) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(agea >= input$age[1] & agea <= input$age[2]),
                        mapping = aes(y = tochld, fill = gender))+
        geom_boxplot() +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"After what age would you say a man is generally too old to consider having any more children?"')
      
      grid.arrange(ballot1,ballot2, nrow = 2)
    })
    
    output$tochld_by_year <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      year1 <- ggplot(tol %>%
                        subset(ballot == 1) %>%
                        subset(gender != "No answer") %>%
                        subset(gender %in% chosen_gender) %>%
                        subset(cntry %in% chosen_cntry) %>%
                        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
                        subset(agea >= input$age[1] & agea <= input$age[2]),
                      mapping = aes(y = tochld, fill = year))+
        geom_boxplot() +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"After what age would you say a woman is generally too old to consider having any more children?"')+
        scale_fill_manual(values=c("gold", "forestgreen"))
      
      year2 <- ggplot(tol %>%
                        subset(ballot == 2) %>%
                        subset(gender != "No answer") %>%
                        subset(gender %in% chosen_gender) %>%
                        subset(cntry %in% chosen_cntry) %>%
                        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
                        subset(agea >= input$age[1] & agea <= input$age[2]),
                      mapping = aes(y = tochld, fill = year))+
        geom_boxplot() +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"After what age would you say a man is generally too old to consider having any more children?"')+
        scale_fill_manual(values=c("gold", "forestgreen"))
      
      grid.arrange(year1,year2, nrow = 2)
    })
    
    output$tochld_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      women <- ggplot(tol %>%
                        subset(gender == "Female") %>%
                        subset(year %in% chosen_year) %>%
                        subset(cntry %in% chosen_cntry) %>%
                        subset(agea >= input$age[1] & agea <= input$age[2]),
                      mapping = aes(y = tochld, fill = ballot))+
        geom_boxplot() +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title='"After what age would you say a ____ is generally too old to consider having any more children?" (WOMEN\'s responses)')+
        scale_fill_discrete(name = "gender asked",labels = c("woman","man"))
      
      men <- ggplot(tol %>%
                      subset(gender == "Male") %>%
                      subset(year %in% chosen_year) %>%
                      subset(cntry %in% chosen_cntry) %>%
                      subset(agea >= input$age[1] & agea <= input$age[2]),
                    mapping = aes(y = tochld, fill = ballot))+
        geom_boxplot() +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title ='"After what age would you say a ____ is generally too old to consider having any more children?" (MEN\'s responses)')
      
      grid.arrange(women,men, nrow = 2)
    })
    
    output$tochld_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      cohort1 <- ggplot(tol %>%
                          subset(ballot == "1") %>%
                          subset(gender %in% chosen_gender) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(year %in% chosen_year),
                        mapping = aes(y = tochld, fill = cohort))+
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = c(0.96,0.80)) +
        geom_boxplot() +
        facet_wrap(~cntry, nrow = 1)+
        labs(title='"Before what age would you say a woman is generally too young to become a parent?"')+
        scale_fill_discrete(name = "Cohort",labels = c("- 1959","1960 - 1989","1990 - "))
      
      cohort2 <- ggplot(tol %>%
                          subset(ballot == "2") %>%
                          subset(gender %in% chosen_gender) %>%
                          subset(cntry %in% chosen_cntry) %>%
                          subset(year %in% chosen_year),
                        mapping = aes(y = tochld, fill = cohort))+
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        geom_boxplot() +
        facet_wrap(~cntry, nrow = 1)+
        labs(title='"Before what age would you say a man is generally too young to become a parent?"')
      
      grid.arrange(cohort1, cohort2, nrow = 2)
    })
  } # TOCHLD plots
  
  output$map <- renderPlot({
    
    map_var1 <- paste(input$map_question, input$map_ballot, sep = "")
    map_var2 <- paste(input$map_question, input$map_ballot, sep = "")
    
    m1 <- ggplot(data = world_2006) +
      geom_sf(aes(fill = world_2006[[map_var1]])) +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("2006") +
      scale_x_continuous(limits = c(-20,50)) +
      scale_y_continuous(limits = c(35,70)) +
      theme(axis.text.y=element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x = element_blank()) +
      scale_fill_hue(limits = c(18:50))
    
    m2 <-ggplot(data = world_2018) +
      geom_sf(aes(fill = world_2018[[map_var2]])) +
      xlab("Longitude") + ylab("Latitude") +
      ggtitle("2018") +
      scale_x_continuous(limits = c(-20,50)) +
      scale_y_continuous(limits = c(35,70)) +
      theme(axis.text.y=element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x = element_blank())+
      scale_fill_hue(limits = c(18:50))
    
    grid.arrange(m1, m2, ncol = 2)
    
  })
  
  output$selected_cntry <- renderText(sort(input$cntry))
  
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="cntry",
                                 inline = T,
                                 choices = list(
                                   "Austria" = "AT","Belgium" = "BE", "Bulgaria" = "BG",
                                   "Cyprus" = "CY","Czechia" = "CZ","Germany" = "DE",
                                   "Denmark" = "DK","Estonia" = "EE","Spain" = "ES",
                                   "Finland" = "FI","France" = "FR","Hungary" = "HU",
                                   "Ireland" = "EI","Italy" = "IT","Netherlands" = "NL",
                                   "Norway" = "NO","Poland" = "PL","Portugal" = "PT",
                                   "Russia" = "RU","Serbia" = "RS","Sweden" = "SE",
                                   "Slovakia" = "SK","Slovenia" = "SL","Switzerland" = "CH",
                                   "Ukraine" = "UA","UK" = "UK"),
                                 selected = c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE",
                                              "EI", "ES", "FI", "FR", "HU", "IT", "NL", "NO", "PL",
                                              "PT", "RS", "RU", "SE", "SK", "SL", "UA", "UK", "EI",
                                              "RS","selectall"))
        
      } else {
        updateCheckboxGroupInput(session=session,
                                 inline = T,
                                 inputId="cntry",
                                 choices = list(
                                   "Austria" = "AT","Belgium" = "BE", "Bulgaria" = "BG",
                                   "Cyprus" = "CY","Czechia" = "CZ","Germany" = "DE",
                                   "Denmark" = "DK","Estonia" = "EE","Spain" = "ES",
                                   "Finland" = "FI","France" = "FR","Hungary" = "HU",
                                   "Ireland" = "EI","Italy" = "IT","Netherlands" = "NL",
                                   "Norway" = "NO","Poland" = "PL","Portugal" = "PT",
                                   "Russia" = "RU","Serbia" = "RS","Sweden" = "SE",
                                   "Slovakia" = "SK","Slovenia" = "SL","Switzerland" = "CH",
                                   "Ukraine" = "UA","UK" = "UK"),
                                 selected = c())
      }}
  })
  
  observe({
    if (input$selectalledu > 0) {
      if (input$selectalledu %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="edu",
                                 label = "Select Highest Education Level",
                                 choices = list("ES-ISCED I , less than lower secondary" = 1,
                                                "ES-ISCED II, lower secondary" = 2,
                                                "ES-ISCED IIIb, lower tier upper secondary" = 3,
                                                "ES-ISCED IIIa, upper tier upper secondary" = 4,
                                                "ES-ISCED IV, advanced vocational, sub-degree" = 5,
                                                "ES-ISCED V1, lower tertiary education, BA level" = 6,
                                                "ES-ISCED V2, higher tertiary education, >= MA level" = 7,
                                                "Other/Missing data"=0),
                                 selected = c("ES-ISCED I , less than lower secondary" = 1,
                                              "ES-ISCED II, lower secondary" = 2,
                                              "ES-ISCED IIIb, lower tier upper secondary" = 3,
                                              "ES-ISCED IIIa, upper tier upper secondary" = 4,
                                              "ES-ISCED IV, advanced vocational, sub-degree" = 5,
                                              "ES-ISCED V1, lower tertiary education, BA level" = 6,
                                              "ES-ISCED V2, higher tertiary education, >= MA level" = 7,
                                              "Other/Missing data"=0)
                                 )
        
      } else {
        updateCheckboxGroupInput(session=session,
                                 inputId="edu",
                                 label = "Select Highest Education Level",
                                 choices = list("ES-ISCED I , less than lower secondary" = 1,
                                                "ES-ISCED II, lower secondary" = 2,
                                                "ES-ISCED IIIb, lower tier upper secondary" = 3,
                                                "ES-ISCED IIIa, upper tier upper secondary" = 4,
                                                "ES-ISCED IV, advanced vocational, sub-degree" = 5,
                                                "ES-ISCED V1, lower tertiary education, BA level" = 6,
                                                "ES-ISCED V2, higher tertiary education, >= MA level" = 7,
                                                "Other/Missing data"=0),
                                 selected = c())
      }}
  })
}

shinyApp(ui, server)

##### Old plot    #####
# p1 <- ggplot(subset(tol, ballot == 1 & gender != "No answer"), mapping = aes(y = tygpnt, fill = gender))+
#   geom_boxplot() +
#   scale_y_continuous(limits = c(0,100),
#                      breaks = seq(0,100,10)) +
#   theme(axis.title.y = element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         legend.position = "none") +
#   facet_wrap(~ cntry, nrow = 1) +
#   labs(title = "TYGPNT asked about women")
# 
# p2 <- ggplot(subset(tol, ballot == 2 & gender != "No answer"), mapping = aes(y = tygpnt, fill = gender))+
#   geom_boxplot() +
#   scale_y_continuous(limits = c(0,100),
#                      breaks = seq(0,100,10)) +
#   theme(axis.title.y = element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         legend.position = "none") +
#   facet_wrap(~ cntry, nrow = 1) +
#   labs(title = "TYGPNT asked about men")
# 
# grid.arrange(p1,p2, nrow =2)



##### Oldest plot #####
# output$plot <- renderPlot({
# 
#   if(input$cntry == "All countries"){
#     chosen_cntry <- c("Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland",
#                       "France","Germany","Hungary","Ireland","Italy","Latvia","Netherlands","Norway",
#                       "Poland","Portugal","Romania","Russian Federation","Serbia","Slovakia","Slovenia",
#                       "Spain","Sweden","Switzerland","Ukraine","United Kingdom")
#   }else{chosen_cntry <- c(input$cntry)}
# 
#   if(input$year == "2006 and 2018"){
#     chosen_year <- c("2006","2018")
#   }else{chosen_year <- c(input$year)}
# 
#   if(input$gender == "Female and Male"){
#     chosen_gender <- c("Female", "Male")
#   }else{chosen_gender <- c(input$gender)}
# 
#   p1 <- ggplot(tol %>%
#                  subset(cntry %in% chosen_cntry) %>%
#                  subset(gender %in% chosen_gender) %>%
#                  subset(year %in% chosen_year) %>%
#                  subset(agea >= input$age[1] & agea <= input$age[2]),
#                mapping = aes(y = tygpnt))+
#     geom_boxplot() +
#     scale_y_continuous(limits = c(0,100),
#                        breaks = seq(0,100,10)) +
#     theme(axis.title.y = element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank()) +
#     labs(x = "Too young to become a parent")
# 
# 
#   p2 <- ggplot(tol %>%
#                  subset(cntry %in% chosen_cntry) %>%
#                  subset(gender %in% chosen_gender) %>%
#                  subset(year %in% chosen_year) %>%
#                  subset(agea >= input$age[1] & agea <= input$age[2]),
#                mapping = aes(y = iagpnt))+
#     geom_boxplot() +
#     scale_y_continuous(limits = c(0,100),
#                        breaks = seq(0,100,10)) +
#     theme(axis.title.y = element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())+
#     labs(x = "Ideal age to become a parent")
# 
# 
#   p3 <- ggplot(tol %>%
#                  subset(cntry %in% chosen_cntry) %>%
#                  subset(gender %in% chosen_gender) %>%
#                  subset(year %in% chosen_year) %>%
#                  subset(agea >= input$age[1] & agea <= input$age[2]),
#                mapping = aes(y = tochld))+
#     geom_boxplot() +
#     scale_y_continuous(limits = c(0,100),
#                        breaks = seq(0,100,10)) +
#     theme(axis.title.y = element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())+
#     labs(x = "Too old to have more children")
# 
#   grid.arrange(p1,p2,p3, nrow = 1)
# 
# })