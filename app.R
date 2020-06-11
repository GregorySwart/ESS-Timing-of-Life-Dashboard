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
library(gdata)
library(splitstackshape)} # Load in libraries

tol <- as.data.frame(read.spss("data/tol.sav"))

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
                    width= "100%", height = "100%")
              ),
              column(10,
                p("In boxplots, the central horizontal line indicates the median, the central rectangle show 
              the interquartile range, and the dots at the top and bottom indicate any outliers that may be 
              present. In some questions, the ballot was split evenly (asking about women or men). This will be 
              indicated where applicable. Data for some countries may be missing.", style = "text-align: justify")
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
            h3("Demographics selector", 
               align = "center"),
            br(),
            column(3),
            column(6,
                   p("In this section, you can subset the data that the plotting tabs below will take in to formulate box 
              plots. Please note that for certain comparitive plots, some of the selectors will",
                     strong("not"),
                     "work. For example, the gender comparison plots will include both genders, regardless of the selection 
              specified here, but the year of collection and age of respondents can be selected normally.",
                     style = "text-align: justify"),
            br(),
              {fluidRow(
                column(2),
                column(6,
                sliderInput("age",
                  label = "Select Age Group",
                  min = 0,
                  max = 100,
                  value = c(0, 100)),
                selectInput("gender",
                            label = "Select Gender",
                            choices = c("Female and Male", "Female","Male"),
                            selected = "Female and Male"
                ),
              selectInput("year",
                          label = "Select Year",
                          choices = c("2006 and 2018","2006","2018"),
                          selected = "2006 and 2018")
                ),
                column(3,
                  img(src = "agelogo.png", height = 90, width = 90),
                  img(src = "genderlogo.png", height = 90, width = 90),
                  img(src = "calendarlogo.png", height = 90, width = 90),
                  
                ),
            )}
            ),
            column(3),
            fluidRow(
              column(12,
                br(),
                {fluidRow(
                  p(align = "center",
                    "You have selected",
                    strong(span(textOutput("selected_gen", inline = T), style = "color:darkred")),
                    "respondents from",
                    strong(span(textOutput("selected_cntry", inline = T), style = "color:darkred")),
                    "in",
                    strong(span(textOutput("selected_year", inline = T), style = "color:darkred")),
                    "between the ages of",
                    strong(span(textOutput("selected_age_min", inline = T), style = "color:darkred")),
                    "and",
                    strong(span(textOutput("selected_age_max", inline = T), style = "color:darkred")),
                    "(n =",
                    strong(span(textOutput("selected_n", inline = T), style = "color:darkred")),
                    span(").", .noWS = "outside")
                  )
                )},  # Show selected N
                hr()
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
                    "counterparts. For this plot, the year selection tool above will not work (as data from both ESS vawes 
                    are presented). Data for some countries is missing for both 2006 and 2018, in these cases a single 
                    box plot will be displayed."),
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
                )}  # By ballot
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
                    "counterparts. For this plot, the year selection tool above will not work (as data from both ESS vawes 
                    are presented). Data for some countries is missing for both 2006 and 2018, in these cases a single 
                    box plot will be displayed."),
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
                  )} # By ballot
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
                                      "counterparts. For this plot, the year selection tool above will not work (as data from both ESS vawes 
                    are presented). Data for some countries is missing for both 2006 and 2018, in these cases a single 
                    box plot will be displayed."),
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
                          )}  # By ballot
                        )} # TOCHLD Plotting tabs
                      )}  # TOCHLD question
            )}  # TOCHLD tab
          )
#          ,tabPanel("Collapse menu")
        )}, # Question selector menu
    )} # First page
  )}, # Main Page
  {tabPanel("Navbar 1",
         p("We are still working on this page")
  )}, # 2nd Page
  {tabPanel("Navbar 2",
         p("We are still working on this page")
  )}  # 3rd Page
)}

server <- function(input, output) {
  
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
    
    output$selected_n <- renderText({ 
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      paste(
        nrow(tol %>%
          subset(gender %in% chosen_gender) %>%
          subset(year %in% chosen_year) %>%
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
      
      p1 <- ggplot(tol %>%
               subset(ballot == 1) %>%
               subset(gender != "No answer") %>%
               subset(gender %in% chosen_gender) %>%
               subset(year %in% chosen_year) %>%
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
        labs(title = '"Before what age would you say a woman is generally too young to become a mother?"')
      
      p2 <- ggplot(tol %>%
               subset(ballot == 2) %>%
               subset(gender != "No answer") %>%
               subset(gender %in% chosen_gender) %>%
               subset(year %in% chosen_year) %>%
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
        labs(title = '"Before what age would you say a man is generally too young to become a father?"')
      
      grid.arrange(p1,p2,nrow = 2)
    
    })
    
    output$tygpnt_by_gender <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      ballot1 <- ggplot(tol %>%
                     subset(ballot == 1) %>%
                     subset(gender != "No answer") %>%
                     subset(year %in% chosen_year) %>%
                     subset(agea >= input$age[1] & agea <= input$age[2]),
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
      
      year1 <- ggplot(tol %>%
                          subset(ballot == 1) %>%
                          subset(gender != "No answer") %>%
                          subset(gender %in% chosen_gender) %>%
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
      
      women <- ggplot(tol %>%
                          subset(gender == "Female") %>%
                          subset(year %in% chosen_year) %>%
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
  } # TYGPNT plots
  
  {
    output$iagpnt_overview <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      p1 <- ggplot(tol %>%
                     subset(ballot == 1) %>%
                     subset(gender != "No answer") %>%
                     subset(gender %in% chosen_gender) %>%
                     subset(year %in% chosen_year) %>%
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
      
      ballot1 <- ggplot(tol %>%
                          subset(ballot == 1) %>%
                          subset(gender != "No answer") %>%
                          subset(year %in% chosen_year) %>%
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
      
      year1 <- ggplot(tol %>%
                        subset(ballot == 1) %>%
                        subset(gender != "No answer") %>%
                        subset(gender %in% chosen_gender) %>%
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
      
      women <- ggplot(tol %>%
                        subset(gender == "Female") %>%
                        subset(year %in% chosen_year) %>%
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
  } # IAGPNT plots

  {
    output$tochld_overview <- renderPlot({
      
      if(input$year == "2006 and 2018"){
        chosen_year <- c("2006","2018")
      }else{chosen_year <- c(input$year)}
      
      if(input$gender == "Female and Male"){
        chosen_gender <- c("Female", "Male")
      }else{chosen_gender <- c(input$gender)}
      
      p1 <- ggplot(tol %>%
                     subset(ballot == 1) %>%
                     subset(gender != "No answer") %>%
                     subset(gender %in% chosen_gender) %>%
                     subset(year %in% chosen_year) %>%
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
      
      ballot1 <- ggplot(tol %>%
                          subset(ballot == 1) %>%
                          subset(gender != "No answer") %>%
                          subset(year %in% chosen_year) %>%
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
      
      year1 <- ggplot(tol %>%
                        subset(ballot == 1) %>%
                        subset(gender != "No answer") %>%
                        subset(gender %in% chosen_gender) %>%
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
      
      women <- ggplot(tol %>%
                        subset(gender == "Female") %>%
                        subset(year %in% chosen_year) %>%
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
  } # TOCHLD plots


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

#`%notin%` <- Negate(`%in%`)

