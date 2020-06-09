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

ui <- 
  navbarPage("ESS Timing of Life",
  theme = shinythemes::shinytheme("sandstone"),
  windowTitle = "ESS Timing of Life",
  tabPanel("Main page",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("gender", 
          label = "Select Gender",
          choices = c("Female and Male", "Female","Male"),
          selected = "Female and Male"),
        
        ##### Country selection #####
        # selectInput("cntry", 
        #   label = "Select Country",
        #     choices = c("All countries", "Austria","Belgium","Bulgaria","Cyprus","Czechia", "Denmark","Estonia",
        #                 "Finland","France","Germany","Hungary","Ireland","Italy","Latvia","Netherlands","Norway",
        #                 "Poland","Portugal","Romania","Russian Federation", "Serbia","Slovakia","Slovenia","Spain",
        #                 "Sweden","Switzerland","Ukraine","United Kingdom"),
        #     selected = "All countries"),
        #####
        
        selectInput("year", 
          label = "Select Year",
          choices = c("2006 and 2018","2006","2018"),
          selected = "2006 and 2018"),
        
        sliderInput("age", "Select Age Group",
           min = 0,
           max = 100,
           value = c(0, 100)),
        
      helpText("Data for some countries may be missing.")
  ),
    
      mainPanel(width = 9,
        fluidRow(
          column(10,
            h2("Welcome to the ESS Timing of Life interactive dashboard!", align = "center")
          ),
          column(2,
            img(src = "logo.png", height = "10%", width = "100%")
          )
        ),
        
        br(),
        
        fluidRow(
          column(2,
            img(src = "legend1.png",style="display: block; margin-left: auto; margin-right: auto;")
          ),
          column(10,
            p("The European Social Survey (ESS) is an 
            academically driven cross-national survey using high methodological standards to provide freely 
            available data for 38 countries. You can subset the data by respondent demographics and data collection 
            year in the panel on the left."),
            helpText("In the boxplots below the horizontal thick line indicates the median, the central rectangle show 
                     the interquartile range, and the dots at the top and bottom indicate any outliers that may be 
                     present. In some questions, the ballot was split evenly (asking about women or men). This will be 
                     indicated where applicable.")
          )
        ),
        br(),
        
        h3("TYGPNT: Before what age would you say a woman/man is generally too young to become a mother/father?"),
        
        tabsetPanel(
          tabPanel("Overview",
            br(),
            p("In this tab you can "),
            plotOutput("facet"),
            br(),
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
            strong(span(textOutput("selected_n", inline = T), style = "color:darkred")), span(").", .noWS = "outside"),
          ),
          tabPanel("By gender (respondent)",
            p("We are still working on this page")
          ),
          tabPanel("By year",
            p("We are still working on this page")
          ),
          
          tabPanel("By gender (asked)",
                   p("We are still working on this page")
          )
        )
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
  
  output$facet <- renderPlot({
    
    if(input$year == "2006 and 2018"){
      chosen_year <- c("2006","2018")
    }else{chosen_year <- c(input$year)}

    if(input$gender == "Female and Male"){
      chosen_gender <- c("Female", "Male")
    }else{chosen_gender <- c(input$gender)}
    
    p1 <- ggplot(subset(tol, ballot == 1 & gender != "No answer"), mapping = aes(y = tygpnt, fill = gender))+
      geom_boxplot() +
      scale_y_continuous(limits = c(0,100),
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none") +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title = "TYGPNT asked about women")
    
    p2 <- ggplot(subset(tol, ballot == 2 & gender != "No answer"), mapping = aes(y = tygpnt, fill = gender))+
      geom_boxplot() +
      scale_y_continuous(limits = c(0,100),
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none") +
      facet_wrap(~ cntry, nrow = 1) +
      labs(title = "TYGPNT asked about men")
    
    grid.arrange(p1,p2, nrow =2)
  
  })
  
  #Old plot #####
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

  }

shinyApp(ui, server)
