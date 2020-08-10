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
  
  grid.arrange(ggplotGrob(p1),tbl1,ggplotGrob(p2),tbl2,nrow = 4)
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

questionTab <- function(var, text, title){
  tabPanel(title,
           fluidRow(
             h3(paste(toupper(var), ": ", text, sep = ""), align = "center"),
             br(),
             tabsetPanel(
               {tabPanel("Overview",
                         br(),
                         p("In this tab you can subset the data as you like, using the buttons above. This serves as an overview of
            the responses given to this question, to see the survey results as a whole. Use the tabs to the right to 
            see comparisons."),
                         plotOutput(paste(var,"_overview", sep = ""), height = 600),
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
                         plotOutput(paste(var,"_by_gender", sep = ""), height = 600)
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
                         plotOutput(paste(var,"_by_year",sep = ""), height = 600)
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
                         plotOutput(paste(var,"_by_ballot",sep = ""), height = 600)
               )}, # By ballot
               {tabPanel("By cohort",
                         br(),
                         p("The plots below show the difference between the responses of people born before 
            1960, between 1960 and 1990, and those born after 1990. The age selector tool will
            not work for these plots. It is important to note that. It is important to note that members of the 
            eldest cohort (born before 1959) only made up ~10% of the respondents."),
                         plotOutput(paste(var,"_by_cohort",sep = ""), height = 600)
               )}  # By cohort
             ) # TYGPNT Plotting tabs
           )  # TYGPNT question
  )
}