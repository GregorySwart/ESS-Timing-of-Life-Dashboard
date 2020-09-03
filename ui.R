library(shiny)
source("libraries.R")  # Read in Libraries
source("functions.R")  # Define functions
source("data_setup.R") # Do some data manipulation

ui <- {navbarPage("ESS Timing of Life", collapsible = TRUE, responsive = TRUE,
                  theme = shinythemes::shinytheme("sandstone"),
                  windowTitle = "ESS Timing of Life",
                  {tabPanel("Main page",
                            {fluidRow(
                              column(1, align = "center",
                                     img(src = "logo.png", height = "220%", width = "220%", style="display: block; margin-left: auto; margin-right: auto;")
                              ),
                              column(10,
                                     h1("Welcome to the ESS Timing of Life interactive dashboard!", align = "center")
                              ),
                              column(1, align = "center",
                                     img(src = "szi_logo2.png", height = "150%", width = "150%", align = "right")
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
                                                                   choices = c("2006","2018"),
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
                                                   questionTab(var = "tygpnt", text = "Before what age would you say a woman/man is too young to become a mother/father?", title = "Age too young to become a parent"), # TYGPNT tab
                                                   questionTab(var = "iagpnt", text = "In your opinion, what is the ideal age for a girl/boy or woman/man to become a mother/father?", title = "Ideal age to become a parent"), # IAGPNT tab
                                                   questionTab(var = "tochld", text = "After what age would you say a woman/man is generally too old to consider having any more children?", title = "Age to old to have more children")  # TOCHLD tab
                                        ),
                                        navbarMenu("Stages of life",
                                                   questionTab(var = "ageadlt", text = "At what age, approximately, would you say girls/boys or women/men become adults?", title = "Beginning of adulthood"), # AGEADLT tab
                                                   questionTab(var = "agemage", text = "At what age, approximately, would you say girls/boys or women/men become middle aged?", title = "Beginning of middle age"), # AGEMAGE tab
                                                   questionTab(var = "ageoage", text = "At what age, approximately, would you say women/men reach old age?", title = "Beginning of old age")  # AGEOAGE tab
                                        ),
                                        navbarMenu("Retirement",
                                                   questionTab(var = "tygrtr", text = "Before what age, approximately, would you say women/men are too young to retire?", title = "Age too young to retire"), # TYGRTR tab
                                                   questionTab(var = "iagrtr", text = "In your opinion, what is the ideal age for a woman/man to retire permanently?", title = "Ideal age to retire"), # IAGRTR tab
                                                   questionTab(var = "towkht", text = "After what age would you say a woman/man is generally too old to be working 20 hours or more per week?", title = "Age too old to be working 20 hrs/week")  # TOWKHT tab
                                        ),
                                        navbarMenu("Living arrangement",
                                                   questionTab(var = "tyglvp", text = "Before what age would you say a woman/man is generally too young to start living with a partner she/he is not married to?", title = "Age too young to live with partner"), # TYGLVP tab
                                                   questionTab(var = "iaglptn", text = "In your opinion, what is the ideal age for a girl/boy or woman/man to start living with a partner she/he is not married to?", title = "Ideal age to move in with partner"), # IAGLPTN tab
                                                   questionTab(var = "tolvpnt", text = "After what age would you say a woman/man is generally too old to still be living with her/his parents?", title = "Age too old to be living with parents")  # TOLVPNT tab
                                        ),
                                        tabPanel("Hide plots",
                                                 br(),
                                                 br(),
                                                 h4("The boxplots are hidden at the moment. The demographics selector tool will work much faster this way!", align = "center"),
                                                 br(),
                                                 br()
                                        )
                            )}, # Question selector menu
                  hr(),
                  p("ESS App Version 1.0 developed and designed by Gregory Alexander Swart", align = "center")
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
                                                                "Stages of life"    = list("Age become adult" = "ageadlt_",
                                                                                           "Age become middle aged" = "agemage_",
                                                                                           "Age reach old age" = "ageoage_"),
                                                                "Retirement ages"   = list("Age too young to retire" = "tygrtr_",
                                                                                           "Ideal age to retire" = "iagrtr_",
                                                                                           "Age too old to be working 20 hrs" = "towkht_"),
                                                                "Living arrangement"= list("Age too young to live with partner" = "tyglvp_",
                                                                                           "Ideal age to live with partner" = "iaglptn_",
                                                                                           "Age too old to still be living with parents" = "tolvpnt_")
                                                 )
                                     )
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
                            ),
                            hr(),
                            p("ESS App Version 1.0 developed and designed by Gregory Alexander Swart", align = "center")
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