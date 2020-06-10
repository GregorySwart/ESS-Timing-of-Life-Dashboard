library(shinythemes)
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
library(splitstackshape)


tol <- as.data.frame(read.spss("data/tol.sav"))
tol$year <- as.factor(tol$year)
factor(tol$cntry, levels = sort(unique(tol$cntry)))

ui <- 
  navbarPage("ESS Timing of Life",
  theme = shinythemes::shinytheme("sandstone"),
  windowTitle = "ESS Timing of Life",
  tabPanel("Main page",
    fluidPage(
  #     sidebarPanel(width = 3,
  #       selectInput("gender",
  #         label = "Select Gender",
  #         choices = c("Female and Male", "Female","Male"),
  #         selected = "Female and Male"),
  # 
  #       ##### Country selection #####
  #       # selectInput("cntry",
  #       #   label = "Select Country",
  #       #     choices = c("All countries", "Austria","Belgium","Bulgaria","Cyprus","Czechia", "Denmark","Estonia",
  #       #                 "Finland","France","Germany","Hungary","Ireland","Italy","Latvia","Netherlands","Norway",
  #       #                 "Poland","Portugal","Romania","Russian Federation", "Serbia","Slovakia","Slovenia","Spain",
  #       #                 "Sweden","Switzerland","Ukraine","United Kingdom"),
  #       #     selected = "All countries"),
  #       #####
  # 
  #       selectInput("year",
  #         label = "Select Year",
  #         choices = c("2006 and 2018","2006","2018"),
  #         selected = "2006 and 2018"),
  # 
  #       sliderInput("age", "Select Age Group",
  #          min = 0,
  #          max = 100,
  #          value = c(0, 100))
  # 
  # 
  # ),
    
      fluidRow(width = 12,
        fluidRow(
          column(2,
                 img(src = "logo.png", height = "10%", width = "100%")
          ),
          column(9,
            h1("Welcome to the ESS Timing of Life interactive dashboard!", align = "center")
          ),
          column(1,
                 
          )
        ),
        
        br(),
        
        fluidRow(
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
            fluidRow(
              column(2,
                img(src = "legend3.png",style="display: block; margin-left: auto; margin-right: 0;")
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
        ),
        br(),
        fluidRow(
          column(3)
          
        ),

        
        h3("TYGPNT: Before what age would you say a woman/man is generally too young to become a mother/father?", 
           align = "center"),
        br(),
        fluidRow(
          column(2),
          column(3,
            selectInput("gender",
              label = "Select Gender",
              choices = c("Female and Male", "Female","Male"),
              selected = "Female and Male"
            ),
          ),
          column(2,
            sliderInput("age",
              label = "Select Age Group",
              min = 0,
              max = 100,
              value = c(0, 100))
          ),
          column(3,
            selectInput("year",
              label = "Select Year",
              choices = c("2006 and 2018","2006","2018"),
              selected = "2006 and 2018"),
          ),
          column(1,
                 
          )
        ),
        tabsetPanel(
          tabPanel("Overview",
            br(),
            p("In this tab you can subset the data as you like, using the buttons above. This serves as an overview of
              the responses given to this question, to see the survey results as a whole. Use the tabs to the right to 
              see comparisons."),
            plotOutput("tygpnt_overview", height = 600),
          ),
          tabPanel("By gender (respondent)",
            br(),
            p("The plots below show the difference between the responses of men and women, by country. Women's 
              responses are depicted by",
            strong(span("red", style = "color:red")),
            "boxplots, and men's are depicted by their",
            strong(span("blue", style = "color:blue")),
            "counterparts. For this plot, 
              the gender selection tool above will not work (as both genders are represented)."),
            plotOutput("tygpnt_by_gender", height = 600)
          ),
          tabPanel("By year",
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
          ),
          
          tabPanel("By gender asked about", 
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
          )
        ),
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
        strong(span(textOutput("selected_n", inline = T), style = "color:darkred")), span(").", .noWS = "outside")
      )
    )
  ),
  
  tabPanel("Navbar 1",
         p("We are still working on this page")
  ),
  
  tabPanel("Navbar 2",
         p("We are still working on this page")
  )
)

server <- function(input, output) {
  
  output$selected_gen <- renderText({ 
    paste(input$gender)
  })
  
  output$selected_cntry <- renderText({ 
    paste(input$cntry)
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
    
    ##### Country selection #####
    # if(input$cntry == "All countries"){
    #   chosen_cntry <- c("Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland",
    #                     "France","Germany","Hungary","Ireland","Italy","Latvia","Netherlands","Norway",
    #                     "Poland","Portugal","Romania","Russian Federation","Serbia","Slovakia","Slovenia",
    #                     "Spain","Sweden","Switzerland","Ukraine","United Kingdom")
    # }else{chosen_cntry <- c(input$cntry)}
    #####
    
    if(input$year == "2006 and 2018"){
      chosen_year <- c("2006","2018")
    }else{chosen_year <- c(input$year)}
    
    if(input$gender == "Female and Male"){
      chosen_gender <- c("Female", "Male")
    }else{chosen_gender <- c(input$gender)}
    
    paste(nrow(tol %>%
          subset(gender %in% chosen_gender) %>%
          subset(year %in% chosen_year) %>%
          subset(agea >= input$age[1] & agea <= input$age[2])
      )
    )
  })
  
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









  }

shinyApp(ui, server)

##### Old plot #####
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



#Oldest plot #####
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

