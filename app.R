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

ui <- fluidPage(
  titlePanel("Attitudes towards child-bearing age in European countries", windowTitle = "ESS Timing of Life"),
  
  sidebarLayout(
    sidebarPanel(
      img(src = "logo.png", height = "10%", width = "100%"),
      helpText("Create box plots showing attitues towards the minimum acceptable and ideal ages to become
               a parent, and the oldest aceptable age to have another child."),
      selectInput("gender", 
                  label = "Select Gender",
                  choices = c("Female and Male", "Female","Male"),
                  selected = "Female and Male"),
      
      selectInput("cntry", 
                  label = "Select Country",
                  choices = c("All countries", "Austria","Belgium","Bulgaria","Cyprus","Czechia",
 "Denmark","Estonia","Finland","France","Germany","Hungary","Ireland","Italy","Latvia","Netherlands",
  "Norway","Poland","Portugal","Romania","Russian Federation",  "Serbia","Slovakia","Slovenia","Spain","Sweden",
  "Switzerland","Ukraine","United Kingdom"),
                  selected = "All countries"),
      
      selectInput("year", 
                  label = "Select Year",
                  choices = c("2006 and 2018","2006","2018"),
                  selected = "2006 and 2018"),
      
      sliderInput("age", "Select Age Group",
                   min = 0, max = 100, value = c(0, 100))
      
      
 
      
#sliderInput("range", label = "Range of interest:", min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(
      tabsetPanel(
          tabPanel("Tab1",
            plotOutput("plot"),
            br(),
            p("You have selected",
              strong(span(textOutput("selected_gen", inline = T), style = "color:red")),
              "respondents from",
              strong(span(textOutput("selected_cntry", inline = T), style = "color:red")),
              "in",
              strong(span(textOutput("selected_year", inline = T), style = "color:red")),
              "between the ages of",
              strong(span(textOutput("selected_age_min", inline = T), style = "color:red")),
              "and",
              strong(span(textOutput("selected_age_max", inline = T), style = "color:red")),
              "(n =",
              strong(span(textOutput("selected_n", inline = T), style = "color:red")),
              ")."),
      
            helpText("The collection for the 9th round of the European Social Survey is still ongoing, and as a 
               result, data for some countries may be missing. For missing data an empty plot background will 
               be displayed.")
          ),
          tabPanel("tab 2",
            p("This page is intentionally empty for now.")),
          tabPanel("tab 3",
            p("This page is intentionally empty for now."))
      )
    )
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
    
    if(input$cntry == "All countries"){
      chosen_cntry <- c("Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland",
                        "France","Germany","Hungary","Ireland","Italy","Latvia","Netherlands","Norway",
                        "Poland","Portugal","Romania","Russian Federation","Serbia","Slovakia","Slovenia",
                        "Spain","Sweden","Switzerland","Ukraine","United Kingdom")
    }else{chosen_cntry <- c(input$cntry)}
    
    if(input$year == "2006 and 2018"){
      chosen_year <- c("2006","2018")
    }else{chosen_year <- c(input$year)}
    
    if(input$gender == "Female and Male"){
      chosen_gender <- c("Female", "Male")
    }else{chosen_gender <- c(input$gender)}
    
    paste(nrow(tol %>%
          subset(cntry %in% chosen_cntry) %>%
          subset(gender %in% chosen_gender) %>%
          subset(year %in% chosen_year) %>%
          subset(agea >= input$age[1] & agea <= input$age[2])
      )
    )
  })
  
  output$plot <- renderPlot({ 
    
    if(input$cntry == "All countries"){
      chosen_cntry <- c("Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland",
                        "France","Germany","Hungary","Ireland","Italy","Latvia","Netherlands","Norway",
                        "Poland","Portugal","Romania","Russian Federation","Serbia","Slovakia","Slovenia",
                        "Spain","Sweden","Switzerland","Ukraine","United Kingdom")
    }else{chosen_cntry <- c(input$cntry)}
    
    if(input$year == "2006 and 2018"){
      chosen_year <- c("2006","2018")
    }else{chosen_year <- c(input$year)}
    
    if(input$gender == "Female and Male"){
      chosen_gender <- c("Female", "Male")
    }else{chosen_gender <- c(input$gender)}
    
    p1 <- ggplot(tol %>%
                   subset(cntry %in% chosen_cntry) %>%
                   subset(gender %in% chosen_gender) %>%
                   subset(year %in% chosen_year) %>%
                   subset(agea >= input$age[1] & agea <= input$age[2]), 
                 mapping = aes(y = tygpnt))+
      geom_boxplot() +
      scale_y_continuous(limits = c(0,100),
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) +
      labs(x = "Too young to become a parent")
    
    
    p2 <- ggplot(tol %>%
                   subset(cntry %in% chosen_cntry) %>%
                   subset(gender %in% chosen_gender) %>%
                   subset(year %in% chosen_year) %>%
                   subset(agea >= input$age[1] & agea <= input$age[2]), 
                 mapping = aes(y = iagpnt))+
      geom_boxplot() +
      scale_y_continuous(limits = c(0,100),
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      labs(x = "Ideal age to become a parent")
    
    
    p3 <- ggplot(tol %>%
                   subset(cntry %in% chosen_cntry) %>%
                   subset(gender %in% chosen_gender) %>%
                   subset(year %in% chosen_year) %>%
                   subset(agea >= input$age[1] & agea <= input$age[2]),
                 mapping = aes(y = tochld))+
      geom_boxplot() +
      scale_y_continuous(limits = c(0,100),
                         breaks = seq(0,100,10)) +
      theme(axis.title.y = element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      labs(x = "Too old to have more children")
    
    grid.arrange(p1,p2,p3, nrow = 1#, 
                 #top=textGrob("",gp=gpar(fontsize=20,font=1))
                 )
    
  })
  
}

shinyApp(ui, server)
