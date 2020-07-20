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
  library(splitstackshape)
  library(rsconnect)
  library(ggflags)
  library(ggthemes)
  library(gridExtra)
  library(ggpubr)
  library(gt)} # Load in libraries

{
  `%notin%` = Negate(`%in%`)
  tol <- as.data.frame(read.spss("data/tol.sav"))
  tol_full <- tol
  # tol_full$cntry[which(tol_full$cntry == "EI")] <- "IE"
  # tol_full$cntry[which(tol_full$cntry == "ES")] <- "EA"
  # tol_full$cntry[which(tol_full$cntry == "SL")] <- "SI"
  # tol_full$cntry[which(tol_full$cntry == "UK")] <- "GB"
  tol <- na.omit(tol)
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
  
  world_2006 <- merge(world1, agg_3, by = "name")
  world_2018 <- merge(world1, agg_9, by = "name")
  
  world_2006$year <- 2006
  world_2018$year <- 2018
  world <- rbind(world_2006, world_2018)
  
  world$year <- as.factor(world$year)
  
  world$name[which(world$name == "EI")] <- "IE"
  world$name[which(world$name == "ES")] <- "EA"
  world$name[which(world$name == "SL")] <- "SI"
  world$name[which(world$name == "UK")] <- "GB"
  

  
  world$code <- tolower(world$name)
  
  
  {code <- sort(c("AT","BE","BG","CH","CY","DE","DK","EA","EE","FI","FR","GB","HU",
                 "IE","NL","NO","PL","PT","RU","SE","SI","SK","UA","CZ","IT","RS"))
  iso <- data.frame(code)
  iso$country <- recode(code, "AT"~"Austria","BE"~"Belgium","BG"~"Bulgaria","CH"~"Switzerland","CY"~"Cyprus",
                        "DE"~"Germany","DK"~"Denmark","EA"~"Spain","FI"~"Finland","FR"~"France",
                        "GB"~"United Kingdom","HU"~"Hungary","IE"~"Ireland","NL"~"Netherlands","NO"~"Norway",
                        "PL"~"Poland","PT"~"Portugal","RU"~"Russia","SE"~"Sweden","SI"~"Slovenia",
                        "SK"~"Slovakia","UA"~"Ukraine","CZ"~"Czechia","IT"~"Italy","RS"~"Serbia",
                        "EE"~"Estonia")
  iso1 <- iso[1:9,]
  iso2 <- iso[10:18,]
  iso3 <- iso[19:26,]} # ISO
  
} # Data setup

ui <- {navbarPage("ESS Timing of Life", collapsible = TRUE,
  # setBackgroundColor(
  #   color = c("#FFFFFF", "#FF9595"),
  #   gradient = c("radial"),
  #   direction = c("bottom"),
  #   shinydashboard = FALSE
  # ),
  theme = shinythemes::shinytheme("sandstone"),
  windowTitle = "ESS Timing of Life",
  {tabPanel("Main page",
    {fluidRow(
      column(2,
             img(src = "logo.png", height = "10%", width = "100%")
      ),
      column(9,
             h1("Welcome to the ESS Timing of Life interactive dashboard!", align = "center")
      )
    )}, # Title
    {fluidRow(
      hr(),
      br(),
      column(3,
        img(src = "background1.png", width = "70%",style="display: block; margin-left: auto; margin-right: auto;"),
        br(),
        br(),
        br(),
        br(),
        br(),
        img(src = "background_alt1.png", width = "70%", style="display: block; margin-left: auto; margin-right: auto;")
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
             
             {fluidRow(

               h3("Demographics selector", align = "center"),
               br(),
               # column(3,
               #        img(src = "background_alt1.png", width = "70%", style="display: block; margin-left: auto; margin-right: auto;")
               # ),
               column(12,
                      p("In this section, you can subset the data that the plotting tabs below will take in to formulate box 
        plots. Please note that for certain comparitive plots, some of the selectors will", strong("not"),
                        "work. For example, the gender comparison plots will include both genders, regardless of the selection 
        specified here, but the year of collection and age of respondents can be selected normally.",
                        style = "text-align: justify"),
                      br(),
                      {fluidRow(
                        column(6,
                               sliderInput("age",
                                           label = "Select Age Group",
                                           min = 15,
                                           max = 101,
                                           value = c(15, 101)),
                               selectInput("gender",
                                           label = "Select Gender",
                                           choices = c("Female and Male","Female","Male"),
                                           selected = "Female and Male"
                               ),
                               selectInput("year",
                                           label = "Select data collection Year",
                                           choices = c("2006 and 2018","2006","2018"),
                                           selected = "2018")
                        ),
                        column(6,
                               fluidRow(
                                 column(12,
                                        
                                        checkboxGroupInput("edu",
                                                           label = "Select Highest Education Level",
                                                           choices = list("ES-ISCED I, less than lower secondary" = 1,
                                                                          "ES-ISCED II, lower secondary" = 2,
                                                                          "ES-ISCED IIIb, lower tier upper secondary" = 3,
                                                                          "ES-ISCED IIIa, upper tier upper secondary" = 4,
                                                                          "ES-ISCED IV, adv. vocational, sub-degree" = 5,
                                                                          "ES-ISCED V1, lower tertiary, BA level" = 6,
                                                                          "ES-ISCED V2, higher tertiary, >= MA level" = 7,
                                                                          "Other/Missing data" =0),
                                                           selected = c("ES-ISCED I, less than lower secondary" = 1,
                                                                        "ES-ISCED II, lower secondary" = 2,
                                                                        "ES-ISCED IIIb, lower tier upper secondary" = 3,
                                                                        "ES-ISCED IIIa, upper tier upper secondary" = 4,
                                                                        "ES-ISCED IV, adv. vocational, sub-degree" = 5,
                                                                        "ES-ISCED V1, lower tertiary, BA level" = 6,
                                                                        "ES-ISCED V2, higher tertiary, >= MA level" = 7,
                                                                        "Other/Missing data" =0)),
                                        actionButton("selectalledu", label = "Select/Deselect all")
                                 )
                               )),
                        column(12,
                               checkboxGroupInput("cntry",
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
                        
                      )}
               ),
               # column(3,
               #        img(src = "background_alt2.png", width = "70%", style="display: block; margin-left: auto; margin-right: auto;")
               # ),

             )} # Demographics selector
      ),
      column(3,
             img(src = "background2.png", width = "70%",style="display: block; margin-left: auto; margin-right: auto;"),
             br(),
             br(),
             br(),
             br(),
             br(),
             img(src = "background_alt2.png", width = "70%", style="display: block; margin-left: auto; margin-right: auto;")
      ),
      fluidRow(
        column(12,
               fluidRow(
                 column(12, align= "center",
                        hr(),
                        {fluidRow(
                          column(1),
                          column(10,
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
                                    strong(span(textOutput("selected_cntry", inline = T), style = "color:darkred")), tags$br(),
                                    "with highest achieved education being one of the following: ",
                                    strong(span(textOutput("selected_edu", inline = T), style = "color:darkred")),
                                    "(n =",
                                    strong(span(textOutput("selected_n", inline = T), style = "color:darkred")),
                                    span(").", .noWS = "outside")
                                 )
                          )
                        )},  # Show selected N
                        br(),
                        #                textOutput("selected_cntry")
                 )
               )
        )
      )
    )}, # Introduction & Demographics
    {navbarPage("Question of interest", collapsable = TRUE,
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
      ),
      navbarMenu("Stages of life",
        {tabPanel("Beginning of adulthood",
          h3("AGEADLT: At what age, approximately, would you say girls/boys or women/men become adults?", 
          align = "center"),
          br(),
          tabsetPanel(
            tabPanel("Overview",
              plotOutput("ageadlt_overview", height = 600)
            ),
            tabPanel("By gender",
              plotOutput("ageadlt_by_gender", height = 600)
            ),
            tabPanel("By year",
              plotOutput("ageadlt_by_year", height = 600)
            ),
            tabPanel("By gender asked about",
              plotOutput("ageadlt_by_ballot", height = 600)
            ),
            tabPanel("By cohort",
              plotOutput("ageadlt_by_cohort", height = 600)
            )
          )
        )}, # AGEADLT tab
        {tabPanel("Beginning of middle age",
          h3("AGEMAGE: At what age, approximately, would you say girls/boys or women/men become middle aged?", 
             align = "center"),
          br(),
          tabsetPanel(
            tabPanel("Overview",
              plotOutput("agemage_overview", height = 600)
            ),
            tabPanel("By gender",
              plotOutput("agemage_by_gender", height = 600)
              ),
            tabPanel("By year",
              plotOutput("agemage_by_year", height = 600)
            ),
            tabPanel("By gender asked about",
              plotOutput("agemage_by_ballot", height = 600)
            ),
            tabPanel("By cohort",
              plotOutput("agemage_by_cohort", height = 600)
            )
          )
        )}, # AGEMAGE tab
        {tabPanel("Beginning of old age",
          h3("AGEOAGE: At what age, approximately, would you say women/men reach old age?", 
             align = "center"),
          br(),
          tabsetPanel(
            tabPanel("Overview",
              plotOutput("ageoage_overview", height = 600)
            ),
            tabPanel("By gender",
              plotOutput("ageoage_by_gender", height = 600)
            ),
            tabPanel("By year",
              plotOutput("ageoage_by_year", height = 600)
            ),
            tabPanel("By gender asked about",
              plotOutput("ageoage_by_ballot", height = 600)
            ),
            tabPanel("By cohort",
              plotOutput("ageoage_by_cohort", height = 600)
            )
          )
        )}  # AGEOAGE tab
      ),
      navbarMenu("Retirement",
        {tabPanel("Age too young to retire",
         h3("TYGRTR: Before what age, approximately, would you say women/men are too young to retire?",
            align = "center"),
         br(),
         tabsetPanel(
           tabPanel("Overview",
                    plotOutput("tygrtr_overview", height = 600)
           ),
           tabPanel("By gender",
                    plotOutput("tygrtr_by_gender", height = 600)
           ),
           tabPanel("By year",
                    plotOutput("tygrtr_by_year", height = 600)
           ),
           tabPanel("By gender asked about",
                    plotOutput("tygrtr_by_ballot", height = 600)
           ),
           tabPanel("By cohort",
                    plotOutput("tygrtr_by_cohort", height = 600)
           )
         )
       )}, # TYGRTR tab
        {tabPanel("Ideal age to retire",
         h3("IAGRTR: In your opinion, what is the ideal age for a woman/man to retire permanently?",
            align = "center"),
         br(),
         tabsetPanel(
           tabPanel("Overview",
                    plotOutput("iagrtr_overview", height = 600)
           ),
           tabPanel("By gender",
                    plotOutput("iagrtr_by_gender", height = 600)
           ),
           tabPanel("By year",
                    plotOutput("iagrtr_by_year", height = 600)
           ),
           tabPanel("By gender asked about",
                    plotOutput("iagrtr_by_ballot", height = 600)
           ),
           tabPanel("By cohort",
                    plotOutput("iagrtr_by_cohort", height = 600)
           )
         )
       )}, # IAGRTR tab
        {tabPanel("Age too old to be working 20 hrs",
         h3("TOWKHT: After what age would you say a woman/man is generally too old to be working 20 hours or more per week?",
            align = "center"),
         br(),
         tabsetPanel(
           tabPanel("Overview",
                    plotOutput("towkht_overview", height = 600)
           ),
           tabPanel("By gender",
                    plotOutput("towkht_by_gender", height = 600)
           ),
           tabPanel("By year",
                    plotOutput("towkht_by_year", height = 600)
           ),
           tabPanel("By gender asked about",
                    plotOutput("towkht_by_ballot", height = 600)
           ),
           tabPanel("By cohort",
                    plotOutput("towkht_by_cohort", height = 600)
           )
         )
       )}  # TOWKHT tab
      ),
      navbarMenu("Living arrangement",
        {tabPanel("Age too young to move in with partner",
         h3("TYGLVP: Before what age would you say a woman/man is generally too young to start living with a partner she/he is not married to?",
            align = "center"),
         br(),
         tabsetPanel(
           tabPanel("Overview",
                    plotOutput("tyglvp_overview", height = 600)
           ),
           tabPanel("By gender",
                    plotOutput("tyglvp_by_gender", height = 600)
           ),
           tabPanel("By year",
                    plotOutput("tyglvp_by_year", height = 600)
           ),
           tabPanel("By gender asked about",
                    plotOutput("tyglvp_by_ballot", height = 600)
           ),
           tabPanel("By cohort",
                    plotOutput("tyglvp_by_cohort", height = 600)
           )
         )
       )}, # TYGRTR tab
        {tabPanel("Ideal age to move in with partner",
         h3("IAGLPTN: In your opinion, what is the ideal age for a girl/boy or woman/man to start living with a partner she/he is not married to?",
            align = "center"),
         br(),
         tabsetPanel(
           tabPanel("Overview",
                    plotOutput("iaglptn_overview", height = 600)
           ),
           tabPanel("By gender",
                    plotOutput("iaglptn_by_gender", height = 600)
           ),
           tabPanel("By year",
                    plotOutput("iaglptn_by_year", height = 600)
           ),
           tabPanel("By gender asked about",
                    plotOutput("iaglptn_by_ballot", height = 600)
           ),
           tabPanel("By cohort",
                    plotOutput("iaglptn_by_cohort", height = 600)
           )
         )
       )}, # IAGLPTN tab
        {tabPanel("Age too old to still be living with parents",
         h3("TOLVPNT: After what age would you say a woman/man is generally too old to still be living with her/his parents?",
            align = "center"),
         br(),
         tabsetPanel(
           tabPanel("Overview",
                    plotOutput("tolvpnt_overview", height = 600)
           ),
           tabPanel("By gender",
                    plotOutput("tolvpnt_by_gender", height = 600)
           ),
           tabPanel("By year",
                    plotOutput("tolvpnt_by_year", height = 600)
           ),
           tabPanel("By gender asked about",
                    plotOutput("tolvpnt_by_ballot", height = 600)
           ),
           tabPanel("By cohort",
                    plotOutput("tolvpnt_by_cohort", height = 600)
           )
         )
       )}  # TOWKHT tab
      ),
      tabPanel("Hide plots",
        br(),
        br(),
        br(),
        br(),
        br()
      )
                      )}, # Question selector menu
  )}, # Main Page
  {tabPanel("Map drawer",
    h2("In this page you can view median responses to the survey questions using a map drawer.", align = "center"),
    hr(),
    fluidRow(
      column(3),
      column(3, align = "left",
         selectInput("map_question",
           label = "Select variable",
           choices = list("Child-bearing ages"= list("Too young to become parent" = "tygpnt_",
                                                     "Ideal age to become parent" = "iagpnt_",
                                                     "Too old to have more children" = "tochld_"),
                           "Stages of life" = list("ageadlt","agemage","ageoage")))
      ),
      column(3, align = "left",
        selectInput("map_ballot",
          label = "Select gender asked about",
          choices = c("Women" = "f","Men" = "m"))
      )
    ),
    fluidRow(
      column(2),
      column(8,
        plotOutput("map", height = 1200)
        )
      )
    )}, # Map drawer
  {tabPanel("Data export",
    h2("In this page you can download the data used in the app in .csv and .sav formats", align = "center"),
    hr(),
    fluidRow(
      column(3, align = "center",
        fluidRow(
          h4("Data used in main page"),
        ),
        fluidRow(
          downloadButton("downloadcsv", "Download CSV", inline = T)
        ),
        br(),
        fluidRow(
          downloadButton("downloadsav", "Download SPSS", inline = T)
        )
      ),
      column(7,
        p("These files contain the data that was used to create the boxplots and tables on the main page. 
          Each variable is denoted by its code (tygpnt = Too young to become a parent, iagpnt = Ideal age 
          to become a parent, tochld = Too old to have more children, age = age at time of ESS round, 
          yrbrn = Year born, edu = education). Year denotes the data collection year, this is 2006 for data
          collected during ESS3, and 2018 during ESS9. Cohort is a variable derived from yrbrn, where 1, 2 and 3
          denote those born after 1990, between 1960 and 1989, and before 1959, respectively. Ballot refers to 
          the question asked where the questions were split between asking about men or women. Here 1 
          corrseponds to Women, and 2 to Men. Dweight and Pweight are the weights used in the survey. Dweight 
          is the design weight and was used to compute weighted means and standard errors. Pweight stands for 
          population size weight, this should be used when comparing countries to one another. A complete guide 
          to ESS weights can be found", 
          a("here.", href = "https://www.europeansocialsurvey.org/docs/methodology/ESS_weighting_data_1.pdf",
          inline = T),
          "And finally, cntry refers to the respondent's home country, 
          by its 2-letter ISO code.")
      )
    ),
    hr(),
    fluidRow(
      column(3, align = "center",
             fluidRow(
               h4("Data used in map drawer"),
               br()
             ),
             fluidRow(
               downloadButton("downloadmapcsv", "Download CSV", inline = T)
             ),
             br(),
             fluidRow(
               downloadButton("downloadmapsav", "Download SPSS", inline = T)
             )
      ),
      column(7,
         p("This dataset was derived from the one above, aggregating the data by country and year, and merging 
         it with the map drawing data in the",tags$em("rnaturalearth"),"and",tags$em("rnaturalearthdata"),
         "libraries. The data presented here is the median value of each variable.")
      )
    )
  )}  # Data Export
)}

{
  overview_plots  <- function(data, table_data1, table_data2, var, limits, titles){
    p1 <- ggplot(data %>% subset(ballot == 1),
                 mapping = aes(y = .data[[var]]))+
      geom_boxplot(fill = "#F37E7E") +
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none") +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title = titles[1])
    
    tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
    tbl1 <- tableGrob(t(table_data1), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
    
    p2 <- ggplot(data %>% subset(ballot == 2),
                 mapping = aes(y = .data[[var]]))+
      geom_boxplot(fill = "#F37E7E") +
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none") +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title = titles[2])
    
    tbl2 <- tableGrob(t(table_data2), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
    
    grid.arrange(p1,tbl1,p2,tbl2,nrow = 4)
  }
  
  by_gender_plots <- function(data, var, limits, titles){
    
    ballot1 <- ggplot(data %>% subset(ballot == 1),
                      mapping = aes(y = .data[[var]], fill = gender))+
      geom_boxplot() +
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = c(0.96,0.80)) +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title = titles[1])
    
    ballot2 <- ggplot(data %>% subset(ballot == 2),
                      mapping = aes(y = .data[[var]], fill = gender))+
      geom_boxplot() +
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none") +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title = titles[2])
    
    return(grid.arrange(ballot1,ballot2, nrow = 2))
  }
  
  by_year_plots   <- function(data, var, limits, titles){
    
    year1 <- ggplot(data %>% subset(ballot == 1),
                    mapping = aes(y = .data[[var]], fill = year))+
      geom_boxplot() +
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = c(0.96,0.80)) +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title = titles[1])+
      scale_fill_manual(values=c("gold", "forestgreen"))
    
    year2 <- ggplot(data %>% subset(ballot == 2),
                    mapping = aes(y = .data[[var]], fill = year))+
      geom_boxplot() +
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none") +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title = titles[2])+
      scale_fill_manual(values=c("gold", "forestgreen"))
    
    grid.arrange(year1,year2, nrow = 2)
  }
  
  by_ballot_plots <- function(data, var, limits, titles){
    women <- ggplot(data %>%
                      subset(gender == "Female"),
                    mapping = aes(y = .data[[var]], fill = ballot))+
      geom_boxplot() +
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = c(0.96,0.80)) +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title= titles[1])+
      scale_fill_discrete(name = "gender asked",labels = c("woman","man"))
    
    men <- ggplot(data %>%
                    subset(gender == "Male"),
                  mapping = aes(y = .data[[var]], fill = ballot))+
      geom_boxplot() +
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none") +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title = titles[2])
    
    grid.arrange(women,men, nrow = 2)
  }
  
  by_cohort_plots <- function(data, var, limits, titles){
    cohort1 <- ggplot(data %>% subset(ballot == "1"),
                      mapping = aes(y = .data[[var]], fill = cohort))+
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = c(0.96,0.80)) +
      geom_boxplot() +
      facet_wrap(~cntry, nrow = 1)+
      labs(title= titles[1])+
      scale_fill_discrete(name = "Cohort",labels = c("- 1959","1960 - 1989","1990 - "))
    
    cohort2 <- ggplot(data %>% subset(ballot == "2"),
                      mapping = aes(y = .data[[var]], fill = cohort))+
      scale_y_continuous(limits = limits,
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none") +
      geom_boxplot() +
      facet_wrap(~cntry, nrow = 1)+
      labs(title= titles[2])
    
    grid.arrange(cohort1, cohort2, nrow = 2)
  }
} # Functions

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
    
    output$selected_edu <- renderText({recode(input$edu, 
                                             1~"I (Less than lower secondary),", 
                                             2~"II (Lower secondary),", 
                                             3~"IIIa (Lower tier upper secondary),", 
                                             4~"IIIb (Upper tier upper secondary),", 
                                             5~"IV (Adv. vocational, Sub-degree),", 
                                             6~"V1 (Lower tertiary, BA level),", 
                                             7~"V2 (Higher tertiary, over MA level),", 
                                             0 ~ "Other/missing,")})
    
    output$selected_n <- renderText({ 
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      paste(
        nrow(tol_full %>%
               subset(gender %in% chosen_gender) %>%
               subset(year %in% chosen_year) %>%
               subset(cntry %in% chosen_cntry) %>%
               subset(agea >= input$age[1] & agea <= input$age[2]) %>%
               subset(edu %in% chosen_edu)
        )
      )
    })
  } # Selected N text
  
  {
  output$selected_cntry <- 
      renderText(sort(input$cntry %>% recode("AT"~"Austria (AT),", "BE" ~ "Belgium (BE),","BG" ~ "Bulgaria (BG),", "CY" ~ "Cyprus (CY),",
                                             "CZ"~"Czechia (CZ),", "DK" ~ "Denmark (DK),", "EE"~"Estonia(EE),", "FI"~"Finland (FI),",
                                             "FR"~"France (FR),", "DE"~"Germany (DE),", "HU"~"Hungary (HU),", "IE"~"Ireland (IE),",
                                             "IT"~"Italy (IT),", "NL"~"Netherlands (NL),", "NO"~"Norway (NO),", "PL"~"Poland (PL),",
                                             "PT"~"Portugal (PT),","RO"~"Romania (RO),","RU"~"Russia (RU),","RS"~"Serbia (RS),",
                                             "SK"~"Slovakia (SK),","SI"~"Slovenia (SI),","ES"~"Spain (EA),", "SE"~"Sweden (SE),",
                                             "CH"~"Switzerland (CH),", "UA"~"Ukraine (UA),","GB"~"United Kingdom (GB),")))
  } # Selected country text
  
  ### PLOTS ###
  
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
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, tygpnt))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median", "n")
      data_agg1 <- data_agg1[, col_order]
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$tygpnt, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tygpnt, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tygpnt) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tygpnt, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tygpnt, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$tygpnt) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      overview_plots(data = data, table_data1 = data_agg1, table_data2 = data_agg2, var = "tygpnt", limits = c(10,40),
                     titles = c('"Before what age would you say a woman is generally too young to become a mother?"',
                                '"Before what age would you say a man is generally too young to become a father?"'))
    }) # Function
    
    output$tygpnt_by_gender <- renderPlot({
      
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "tygpnt", limits = c(10,40), 
                      titles = c('"Before what age would you say a woman is generally too young to become a mother?"',
                                 '"Before what age would you say a man is generally too young to become a father?"'))
    }) # Function
    
    output$tygpnt_by_year <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
                      subset(gender %in% chosen_gender) %>%
                      subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
                      subset(cntry %in% chosen_cntry) %>%
                      subset(edu %in% chosen_edu) %>%
                      subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "tygpnt", limits = c(10,40), 
                    titles = c('"Before what age would you say a woman is generally too young to become a mother?"',
                               '"Before what age would you say a man is generally too young to become a father?"'))
    }) # Function
    
    output$tygpnt_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "tygpnt", limits = c(10,40), 
                      titles = c('"Before what age would you say a ____ is generally too young to become a parent?" (WOMEN\'s responses)',
                                 '"Before what age would you say a ____ is generally too young to become a parent?" (MEN\'s responses)'))
    }) # Function
    
    output$tygpnt_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "tygpnt", limits =c(10,40),
                      titles = c('"Before what age would you say a woman is generally too young to become a parent?"',
                                 '"Before what age would you say a man is generally too young to become a parent?"'))
    }) # Function
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
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, iagpnt, ballot))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median","n")
      data_agg1 <- data_agg1[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$iagpnt, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$iagpnt, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$iagpnt) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      p1 <- ggplot(data %>% subset(ballot == 1),
                   mapping = aes(y = iagpnt))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(10,50),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"In your opinion, what age is ideal for a woman to become a mother?"')
      
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      tbl1 <- tableGrob(t(data_agg1), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      p2 <- ggplot(data %>% subset(ballot == 2),
                   mapping = aes(y = iagpnt))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(10,50),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"In your opinion, what age is ideal for a man to become a father?"')
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$iagpnt, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$iagpnt, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$iagpnt) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg2$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      tbl2 <- tableGrob(t(data_agg2), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      grid.arrange(p1,tbl1,p2,tbl2,nrow = 4)
      
    })
    
    output$iagpnt_by_gender <- renderPlot({
      
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "iagpnt", limits = c(10,40), 
                      titles = c('"In your opinion, what is the ideal age for a girl or woman to become a mother?"',
                                 '"In your opinion, what is the ideal age for a boy or man to become a father?"'))
    }) # Function
    
    output$iagpnt_by_year <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "iagpnt", limits = c(10,40), 
                    titles = c('"In your opinion, what is the ideal age for a girl or woman to become a mother?"',
                               '"In your opinion, what is the ideal age for a boy or man to become a father?"'))
    }) # Function
    
    output$iagpnt_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "iagpnt", limits = c(10,40), 
                      titles = c('"In your opinion, what is the ideal age for a ____ to become a parent?" (WOMEN\'s responses)',
                                 '"In your opinion, what is the ideal age for a ____ to become a parent?" (MEN\'s responses)'))
    }) # Function
    
    output$iagpnt_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "iagpnt", limits =c(10,40),
                      titles = c('"In your opinion, what is the ideal age for a girl or woman to become a mother?"',
                                 '"In your opinion, what is the ideal age for a boy or man to become a father?"'))
    }) # Function
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
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, tochld))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median","n")
      data_agg1 <- data_agg1[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$tochld, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tochld, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tochld) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      p1 <- ggplot(data %>% subset(ballot == 1),
                   mapping = aes(y = tochld))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"After what age would you say a woman is generally too old to consider having any more children?"')
      
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      tbl1 <- tableGrob(t(data_agg1), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      p2 <- ggplot(data %>% subset(ballot == 2),
                   mapping = aes(y = tochld))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"After what age would you say a man is generally too old to consider having any more children?"')
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tochld, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tochld, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$tochld) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      tbl2 <- tableGrob(t(data_agg2), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      grid.arrange(p1,tbl1,p2,tbl2,nrow = 4)
      
    })
    
    output$tochld_by_gender <- renderPlot({
      
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "tochld", limits = c(20,60), 
                      titles = c('"After what age would you say a woman is generally too old to consider having any more children?"',
                                 '"After what age would you say a man is generally too old to consider having any more children?"'))
    }) # Function
    
    output$tochld_by_year <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "iagpnt", limits = c(10,40), 
                    titles = c('"After what age would you say a woman is generally too old to consider having any more children?"',
                               '"After what age would you say a man is generally too old to consider having any more children?"'))
    }) # Function
    
    output$tochld_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "iagpnt", limits = c(10,40), 
                      titles = c('"After what age would you say a ____ is generally too old to consider having any more children?" (WOMEN\'s responses)',
                                 '"After what age would you say a ____ is generally too old to consider having any more children?" (MEN\'s responses)'))
    }) # Function
    
    output$tochld_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "tochld", limits =c(10,40),
                      titles = c('"After what age would you say a woman is generally too old to consider having any more children?"',
                                 '"After what age would you say a man is generally too old to consider having any more children?"'))
    }) # Function
  } # TOCHLD plots
  
  {
    output$ageadlt_overview <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, ageadlt))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median","n")
      data_agg1 <- data_agg1[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$ageadlt, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$ageadlt, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$ageadlt) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      p1 <- ggplot(data %>% subset(ballot == 1),
                   mapping = aes(y = ageadlt))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"At what age, approximately, would you say girls or women become adults?"')
      
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      tbl1 <- tableGrob(t(data_agg1), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      p2 <- ggplot(data %>% subset(ballot == 2),
                   mapping = aes(y = ageadlt))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(10,40),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"At what age, approximately, would you say boys or men become adults?"')
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$ageadlt, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$ageadlt, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$ageadlt) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      tbl2 <- tableGrob(t(data_agg2), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      grid.arrange(p1,tbl1,p2,tbl2,nrow = 4)
    })
    
    output$ageadlt_by_gender <- renderPlot({
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "ageadlt", limits = c(10,40), titles = c("Title1","Title2"))
    }) # Function
    
    output$ageadlt_by_year <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "ageadlt", limits = c(10,40), 
                    titles = c('"At what age, approximately, would you say girls or women become adults?"',
                               '"At what age, approximately, would you say boys or men become adults?"'))
    }) # Function
    
    output$ageadlt_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "ageadlt", limits = c(10,40), 
                      titles = c('"At what age, approximately, would you say girls or women become adults?"',
                                 '"At what age, approximately, would you say boys or men become adults?"'))
    }) # Function
    
    output$ageadlt_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "ageadlt", limits =c(10,40),
                      titles = c('"At what age, approximately, would you say ____ become adults?" (WOMEN\'s responses)',
                                 '"At what age, approximately, would you say ____ become adults?" (MEN\'s responses)'))
    }) # Function
  } # AGEADLT plots
  
  {
    output$agemage_overview <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, agemage))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median","n")
      data_agg1 <- data_agg1[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$agemage, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$agemage, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$agemage) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      p1 <- ggplot(data %>% subset(ballot == 1),
                   mapping = aes(y = agemage))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"At what age, approximately, would you say girls or women become middle aged?"')
      
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      tbl1 <- tableGrob(t(data_agg1), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      p2 <- ggplot(data %>% subset(ballot == 2),
                   mapping = aes(y = agemage))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(20,60),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"AGEMAGE: At what age, approximately, would you say boys or men become middle aged?"')
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$agemage, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$agemage, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$agemage) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      tbl2 <- tableGrob(t(data_agg2), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      grid.arrange(p1,tbl1,p2,tbl2,nrow = 4)
    })
    
    output$agemage_by_gender <- renderPlot({
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "agemage", limits = c(20,60), titles = c("Title1","Title2"))
    }) # Function
    
    output$agemage_by_year <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "agemage", limits = c(20,60), 
                    titles = c('"At what age, approximately, would you say girls or women become middle aged?"',
                               '"At what age, approximately, would you say boys or men become middle aged?"'))
    }) # Function
    
    output$agemage_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "agemage", limits = c(20,60), 
                      titles = c('"At what age, approximately, would you say girls or women reach middle age?"',
                                 '"At what age, approximately, would you say boys or men reach middle age?"'))
    }) # Function
    
    output$agemage_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "agemage", limits =c(20,60),
                      titles = c('"At what age, approximately, would you say ____ reach middle age?" (WOMEN\'s responses)',
                                 '"At what age, approximately, would you say ____ reach middle age?" (MEN\'s responses)'))
    }) # Function
  } # AGEMAGE plots
  
  {
    output$ageoage_overview <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, ageoage))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median","n")
      data_agg1 <- data_agg1[, col_order]
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$ageoage, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$ageoage, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$ageoage) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$ageoage, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$ageoage, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$ageoage) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      p1 <- ggplot(data %>% subset(ballot == 1),
                   mapping = aes(y = ageoage))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(30,80),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"At what age, approximately, would you say women reach old age?"')
      
      tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
      tbl1 <- tableGrob(t(data_agg1), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      p2 <- ggplot(data %>% subset(ballot == 2),
                   mapping = aes(y = ageoage))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(30,80),
                           breaks = seq(0,100,10)) +
        theme(axis.title.y = element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              legend.position = "none") +
        facet_wrap(~ cntry, nrow = 1) +
        labs(title = '"At what age, approximately, would you say men reach old age?"')
      

      
      tbl2 <- tableGrob(t(data_agg2), rows=c("Country","Weighted Mean", "Mean SE", "Median", "Valid N", "Total N"), theme=tt)
      
      grid.arrange(p1,tbl1,p2,tbl2,nrow = 4)
    })
    
    output$ageoage_by_gender <- renderPlot({
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "ageoage", limits = c(30,80), titles = c("Title1","Title2"))
    }) # Function
    
    output$ageoage_by_year <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "ageoage", limits = c(30,80), 
                    titles = c('"At what age, approximately, would you say girls or women reach old age?"',
                               '"At what age, approximately, would you say boys or men reach old age?"'))
    }) # Function
    
    output$ageoage_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "ageoage", limits = c(30,80),
                      titles = c('"At what age, approximately, would you say girls or women reach old age?"',
                                 '"At what age, approximately, would you say boys or men reach old age?"'))
    }) # Function
    
    output$ageoage_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "ageoage", limits =c(30,80),
                      titles = c('"At what age, approximately, would you say ____ reach old age?" (WOMEN\'s responses)',
                                 '"At what age, approximately, would you say ____ reach old age?" (MEN\'s responses)'))
    }) # Function
  } # AGEOAGE plots
  
  {
    output$tygrtr_overview <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, tygrtr))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median", "n")
      data_agg1 <- data_agg1[, col_order]
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$tygrtr, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tygrtr, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tygrtr) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tygrtr, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tygrtr, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$tygrtr) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      overview_plots(data = data, table_data1 = data_agg1, table_data2 = data_agg2, var = "tygrtr", limits = c(40,80),
                     titles = c('"Before what age would you say a woman is generally too young to retire?"',
                                '"Before what age would you say a man is generally too young to retire?"'))
    }) # Function
    
    output$tygrtr_by_gender <- renderPlot({
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "tygrtr", limits = c(40,80), 
                      titles = c('"Before what age would you say a woman is generally too young to retire?"',
                                 '"Before what age would you say a man is generally too young to retire?"'))
    }) # Function
    
    output$tygrtr_by_year <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "tygrtr", limits = c(40,80), 
                    titles = c('"Before what age would you say a woman is generally too young to retire?"',
                               '"Before what age would you say a man is generally too young to retire?"'))
    }) # Function
    
    output$tygrtr_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "tygrtr", limits = c(40,80),
                      titles = c('"Before what age would you say a ___ is generally too young to retire?" (WOMENS\'s responses)',
                                 '"Before what age would you say a ___ is generally too young to retire?" (MENS\'s responses)'))
    }) # Function
    
    output$tygrtr_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "tygrtr", limits =c(40,80),
                      titles = c('"Before what age would you say a woman is generally too young to retire?"',
                                 '"Before what age would you say a man is generally too young to retire?"'))
    }) # Function
  } # TYGRTR plots
  
  {
    output$iagrtr_overview <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, iagrtr))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median", "n")
      data_agg1 <- data_agg1[, col_order]
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$iagrtr, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$iagrtr, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$iagrtr) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$iagrtr, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$iagrtr, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$iagrtr) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      overview_plots(data = data, table_data1 = data_agg1, table_data2 = data_agg2, var = "iagrtr", limits = c(40,80),
                     titles = c('"In your opinion, what is the ideal age for a woman to retire permanently?"',
                                '"In your opinion, what is the ideal age for a man to retire permanently?"'))
    }) # Function
    
    output$iagrtr_by_gender <- renderPlot({
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "iagrtr", limits = c(40,80), 
                      titles = c('"In your opinion, what is the ideal age for a woman to retire permanently?"',
                                 '"In your opinion, what is the ideal age for a man to retire permanently?"'))
    }) # Function
    
    output$iagrtr_by_year <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "tygrtr", limits = c(40,80), 
                    titles = c('"In your opinion, what is the ideal age for a woman to retire permanently?"',
                               '"In your opinion, what is the ideal age for a man to retire permanently?"'))
    }) # Function
    
    output$iagrtr_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "iagrtr", limits = c(40,80),
                      titles = c('"In your opinion, what is the ideal age for a ____ to retire permanently?" (WOMENS\'s responses)',
                                 '"In your opinion, what is the ideal age for a ____ to retire permanently?" (MENS\'s responses)'))
    }) # Function
    
    output$iagrtr_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "iagrtr", limits =c(40,80),
                      titles = c('"In your opinion, what is the ideal age for a woman to retire permanently?"',
                                 '"In your opinion, what is the ideal age for a man to retire permanently?"'))
    }) # Function
  } # IAGRTR plots
  
  {
    output$towkht_overview <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, towkht))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median", "n")
      data_agg1 <- data_agg1[, col_order]
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$towkht, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$towkht, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$towkht) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$towkht, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$towkht, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$towkht) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      overview_plots(data = data, table_data1 = data_agg1, table_data2 = data_agg2, var = "towkht", limits = c(50,90),
                     titles = c('"After what age would you say a woman is generally too old to be working 20 hours or more per week?"',
                                '"After what age would you say a man is generally too old to be working 20 hours or more per week?"'))
    }) # Function
    
    output$towkht_by_gender <- renderPlot({
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "towkht", limits = c(50,90), 
                      titles = c('"After what age would you say a woman is generally too old to be working 20 hours or more per week?"',
                                 '"After what age would you say a man is generally too old to be working 20 hours or more per week?"'))
    }) # Function
    
    output$towkht_by_year <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "tygrtr", limits = c(40,80), 
                    titles = c('"In your opinion, what is the ideal age for a woman to retire permanently?"',
                               '"In your opinion, what is the ideal age for a man to retire permanently?"'))
    }) # Function
    
    output$towkht_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "towkht", limits = c(50,90),
                      titles = c('"After what age would you say a ____ is generally too old to be working 20 hours or more per week?" (WOMENS\'s responses)',
                                 '"After what age would you say a ____ is generally too old to be working 20 hours or more per week?" (MENS\'s responses)'))
    }) # Function
    
    output$towkht_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "towkht", limits =c(50,90),
                      titles = c('"After what age would you say a woman is generally too old to be working 20 hours or more per week?"',
                                 '"After what age would you say a man is generally too old to be working 20 hours or more per week?"'))
    }) # Function
  } # TOWKHT plots
  
  {
    output$tyglvp_overview <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, tyglvp))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median", "n")
      data_agg1 <- data_agg1[, col_order]
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$tyglvp, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tyglvp, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tyglvp) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tyglvp, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tyglvp, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$tyglvp) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      overview_plots(data = data, table_data1 = data_agg1, table_data2 = data_agg2, var = "tyglvp", limits = c(10,50),
                     titles = c('"Before what age would you say a woman is generally too young to start living with a partner she is not married to?"',
                                '"Before what age would you say a man is generally too young to start living with a partner he is not married to?"'))
    }) # Function
    
    output$tyglvp_by_gender <- renderPlot({
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "tyglvp", limits = c(10,50), 
                      titles = c('"Before what age would you say a woman is generally too young to start living with a partner she is not married to?"',
                                 '"Before what age would you say a man is generally too young to start living with a partner he is not married to?"'))
    }) # Function
    
    output$tyglvp_by_year <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "tyglvp", limits = c(10,50), 
                    titles = c('"Before what age would you say a woman is generally too young to start living with a partner she is not married to?"',
                               '"Before what age would you say a man is generally too young to start living with a partner he is not married to?"'))
    }) # Function
    
    output$tyglvp_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "tyglvp", limits = c(10,50),
                      titles = c('"Before what age would you say a ____ is generally too young to start living with a partner they are not married to?" (WOMENS\'s responses)',
                                 '"Before what age would you say a ____ is generally too young to start living with a partner they are not married to?" (MENS\'s responses)'))
    }) # Function
    
    output$tyglvp_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "tyglvp", limits =c(10,50),
                      titles = c('"Before what age would you say a woman is generally too young to start living with a partner she is not married to?"',
                                 '"Before what age would you say a man is generally too young to start living with a partner he is not married to?"'))
    }) # Function
  } # TYGLVP plots
  
  {
    output$iaglptn_overview <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, iaglptn))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median", "n")
      data_agg1 <- data_agg1[, col_order]
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$iaglptn, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$iaglptn, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$iaglptn) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$iaglptn, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$iaglptn, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$iaglptn) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      overview_plots(data = data, table_data1 = data_agg1, table_data2 = data_agg2, var = "iaglptn", limits = c(10,50),
                     titles = c('"In your opinion, what is the ideal age for a girl or woman to start living with a partner she is not married to?"',
                                '"In your opinion, what is the ideal age for a boy or man to start living with a partner he is not married to?"'))
    }) # Function
    
    output$iaglptn_by_gender <- renderPlot({
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "iaglptn", limits = c(10,50), 
                      titles = c('"In your opinion, what is the ideal age for a girl or woman to start living with a partner she is not married to?"',
                                 '"In your opinion, what is the ideal age for a boy or man to start living with a partner he is not married to?"'))
    }) # Function
    
    output$iaglptn_by_year <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "iaglptn", limits = c(10,50), 
                    titles = c('"In your opinion, what is the ideal age for a girl or woman to start living with a partner she is not married to?"',
                               '"In your opinion, what is the ideal age for a boy or man to start living with a partner he is not married to?"'))
    }) # Function
    
    output$iaglptn_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "iaglptn", limits = c(10,50),
                      titles = c('"In your opinion, what is the ideal age for a ____ or woman/man to start living with a partner they are not married to?" (WOMENS\'s responses)',
                                 '"In your opinion, what is the ideal age for a ____ or woman/man to start living with a partner they are not married to?" (MENS\'s responses)'))
    }) # Function
    
    output$iaglptn_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "iaglptn", limits =c(10,50),
                      titles = c('"In your opinion, what is the ideal age for a girl or woman to start living with a partner she is not married to?"',
                                 '"In your opinion, what is the ideal age for a boy or man to start living with a partner he is not married to?"'))
    }) # Function
  } # IAGLPTN plots
  
  {
    output$tolvpnt_overview <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data_full <- tol_full %>%
        subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      data <- na.omit(data_full %>% select(cntry, ballot, tolvpnt))
      
      data_agg1 <- count(data %>% subset(ballot == 1), cntry)
      data_agg1$mean <- 0
      data_agg1$se <- 0
      data_agg1$median <- 0
      data_agg1$total_N <- 0
      
      data_agg2 <- count(data %>% subset(ballot == 2), cntry)
      data_agg2$mean <- 0
      data_agg2$se <- 0
      data_agg2$median <- 0
      data_agg2$total_N <- 0
      
      col_order <- c("cntry", "mean", "se", "median", "n")
      data_agg1 <- data_agg1[, col_order]
      data_agg2 <- data_agg2[, col_order]
      
      for (i in data_agg1$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 1), weights = subset(data, cntry == i & ballot == 1)$dweight)
        data_agg1$mean[which(data_agg1$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 1)$tolvpnt, design = design)[1] %>% round(digits = 2)
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tolvpnt, design = design)) %>% round(digits = 5)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tolvpnt) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tolvpnt, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tolvpnt, design = design)) %>% round(digits = 5)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$tolvpnt) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      overview_plots(data = data, table_data1 = data_agg1, table_data2 = data_agg2, var = "tolvpnt", limits = c(10,50),
                     titles = c('"After what age would you say a woman is generally too old to still be living with her parents?"',
                                '"After what age would you say a man is generally too old to still be living with his parents?"'))
    }) # Function
    
    output$tolvpnt_by_gender <- renderPlot({
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
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(agea >= input$age[1] & agea <= input$age[2]) %>%
        subset(edu %in% chosen_edu)
      
      by_gender_plots(data = data, var = "tolvpnt", limits = c(10,50), 
                      titles = c('"After what age would you say a woman is generally too old to still be living with her parents?"',
                                 '"After what age would you say a man is generally too old to still be living with his parents?"'))
    }) # Function
    
    output$tolvpnt_by_year <- renderPlot({
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>% subset(gender != "No answer") %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %notin% c("CZ","DK","ES","IT","PT","RS","RU","SE","SK","UA")) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_year_plots(data = data, var = "tolvpnt", limits = c(10,50), 
                    titles = c('"After what age would you say a woman is generally too old to still be living with her parents?"',
                               '"After what age would you say a man is generally too old to still be living with his parents?"'))
    }) # Function
    
    output$tolvpnt_by_ballot <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(year %in% chosen_year) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(edu %in% chosen_edu) %>%
        subset(agea >= input$age[1] & agea <= input$age[2])
      
      by_ballot_plots(data = data, var = "tolvpnt", limits = c(10,50),
                      titles = c('"After what age would you say a ____ is generally too old to still be living with their parents?" (WOMENS\'s responses)',
                                 '"After what age would you say a ____ is generally too old to still be living with their parents?" (MENS\'s responses)'))
    }) # Function
    
    output$tolvpnt_by_cohort <- renderPlot({
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      chosen_cntry <- input$cntry
      
      chosen_edu <- input$edu
      
      data <- tol %>%
        subset(gender %in% chosen_gender) %>%
        subset(cntry %in% chosen_cntry) %>%
        subset(year %in% chosen_year) %>%
        subset(edu %in% chosen_edu)
      
      by_cohort_plots(data = data, var = "tolvpnt", limits =c(10,50),
                      titles = c('"After what age would you say a woman is generally too old to still be living with her parents?"',
                                 '"After what age would you say a man is generally too old to still be living with his parents?"'))
    }) # Function
  } # TOLVPNT plots
  
  {
  output$map <- renderPlot({
    
    map_var1 <- paste(input$map_question, input$map_ballot, sep = "")
    map_var2 <- paste(input$map_question, input$map_ballot, sep = "")
    
    map <- {ggplot(data = world) +
      geom_sf(aes(fill = world[[map_var1]])) +
      ggtitle(paste('',
          recode(map_var1, 
  "tygpnt_f"~'"Before what age would you say a woman is generally too young to become a mother?"',
  "tygpnt_m"~'"Before what age would you say a man is generally too young to become a father?"',
  "iagpnt_f"~'"In your opinion, what age is ideal for a woman to become a mother?"',
  "iagpnt_m"~'"In your opinion, what age is ideal for a man to become a father?"',
  "tochld_f"~'"After what age would you say a woman is generally too old to consider having any more children?"',
  "tochld_m"~'"After what age would you say a man is generally too old to consider having any more children?"'),
                    '')) +
      scale_x_continuous(limits = c(-20,50)) +
      scale_y_continuous(limits = c(35,70)) +
      theme(axis.text.y=element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.x = element_blank(),
            legend.position = c(0.04, 0.94),
            panel.grid = element_blank(),)+
      facet_wrap(~year, nrow = 2)+
      scale_alpha_discrete(range = c(0.4,1)) +
      scale_fill_brewer(palette = "Dark"
                        , name = "year")+
      labs(fill = "Age") +
      scale_fill_continuous(limits = if(map_var1 == "tygpnt_f") {c(18,20)}
                                else if(map_var1 == "tygpnt_m") {c(20,23)}
                                else if(map_var1 == "iagpnt_f") {c(22,28)}
                                else if(map_var1 == "iagpnt_m") {c(25,30)}
                                else if(map_var1 == "tochld_f") {c(40,45)}
                                else if(map_var1 == "tochld_m") {c(45,50)}
                                else {c(18,50)},
                             high = "#132B43", low = "#56B1F7")}
    
    legend1 <- {ggplot(world %>% subset(year == 2006) %>% na.omit()) +
      geom_bar(stat = "identity", position = "dodge", fill = "#56B1F7", aes(y = reorder(name, .data[[map_var1]]), x = .data[[map_var1]]))+
      coord_cartesian(xlim=if(map_var1 == "tygpnt_f") {c(17,21)}
                      else if(map_var1 == "tygpnt_m") {c(18,24)}
                      else if(map_var1 == "iagpnt_f") {c(21,29)}
                      else if(map_var1 == "iagpnt_m") {c(24,31)}
                      else if(map_var1 == "tochld_f") {c(39,46)}
                      else if(map_var1 == "tochld_m") {c(44,51)}
                      else {c(18,50)})+
      geom_flag(aes(country = code, y = reorder(name, .data[[map_var1]]), x = .data[[map_var1]]), size = 9)+
      ggtitle("2006")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.x = element_blank())}
    
    legend2 <- ggplot(world %>% subset(year == 2018) %>% na.omit()) +
      geom_bar(stat = "identity", position = "dodge", fill = "#56B1F7", aes(y = reorder(name, .data[[map_var1]]), x = .data[[map_var1]]))+
      coord_cartesian(xlim=if(map_var1 == "tygpnt_f") {c(17,21)}
                      else if(map_var1 == "tygpnt_m") {c(18,24)}
                      else if(map_var1 == "iagpnt_f") {c(21,29)}
                      else if(map_var1 == "iagpnt_m") {c(24,31)}
                      else if(map_var1 == "tochld_f") {c(39,46)}
                      else if(map_var1 == "tochld_m") {c(44,51)}
                      else {c(18,50)})+
      geom_flag(aes(country = code, y = reorder(name, .data[[map_var1]]), x = .data[[map_var1]]), size = 10)+
      ggtitle("2018")+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
    
    legend <- grid.arrange(legend1, legend2, nrow = 2)
    
    ggarrange(map, legend, ncol = 2, widths = c(6,1), heights = c(1,1), align = "v")
    
  })
  } # Map drawer
  
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
  }) # cntry selectall button
  
  observe({
    if (input$selectalledu > 0) {
      if (input$selectalledu %% 2 == 0){
        updateCheckboxGroupInput(session=session, 
                                 inputId="edu",
                                 label = "Select Highest Education Level",
                                 choices = list("ES-ISCED I, less than lower secondary" = 1,
                                                "ES-ISCED II, lower secondary" = 2,
                                                "ES-ISCED IIIb, lower tier upper secondary" = 3,
                                                "ES-ISCED IIIa, upper tier upper secondary" = 4,
                                                "ES-ISCED IV, adv. vocational, sub-degree" = 5,
                                                "ES-ISCED V1, lower tertiary, BA level" = 6,
                                                "ES-ISCED V2, higher tertiary, >= MA level" = 7,
                                                "Other/Missing data"=0),
                                 selected = c("ES-ISCED I, less than lower secondary" = 1,
                                              "ES-ISCED II, lower secondary" = 2,
                                              "ES-ISCED IIIb, lower tier upper secondary" = 3,
                                              "ES-ISCED IIIa, upper tier upper secondary" = 4,
                                              "ES-ISCED IV, adv. vocational, sub-degree" = 5,
                                              "ES-ISCED V1, lower tertiary, BA level" = 6,
                                              "ES-ISCED V2, higher tertiary, >= MA level" = 7,
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
                                                "ES-ISCED IV, adv. vocational, sub-degree" = 5,
                                                "ES-ISCED V1, lower tertiary, BA level" = 6,
                                                "ES-ISCED V2, higher tertiary, >= MA level" = 7,
                                                "Other/Missing data"=0),
                                 selected = c())
      }}
  }) # edu selectall button
  
  {data <- as.data.frame(read.spss("data/tol.sav"))
  
  mapdata <- st_drop_geometry(world)
  
  output$downloadcsv <- downloadHandler(
    
    filename = function() { 
      paste("ESS-dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    })
  
  output$downloadsav <- downloadHandler(
    
    filename = function() { 
      paste("ESS-dataset-", Sys.Date(), ".sav", sep="")
    },
    content = function(file) {
      write.spss(data, file)
    })
  
  output$downloadmapcsv <- downloadHandler(
    
    filename = function() { 
      paste("ESS-map-dataset-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(mapdata, file)
    })
  
  output$downloadmapsav <- downloadHandler(
    
    filename = function() { 
      paste("ESS-map-dataset-", Sys.Date(), ".sav", sep="")
    },
    content = function(file) {
      write.spss(mapdata, file)
    })
  } # Download buttons
}

shinyApp(ui, server)

  # "Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland",
  # "France","Germany","Hungary","Ireland","Italy","Latvia","Netherlands","Norway",
  # "Poland","Portugal","Romania","Russian Federation","Serbia","Slovakia","Slovenia",
  # "Spain","Sweden","Switzerland","Ukraine","United Kingdom"