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
  
  {
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tygpnt, design = design)) %>% round(digits = 3)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tygpnt) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tygpnt, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tygpnt, design = design)) %>% round(digits = 3)
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$iagpnt, design = design)) %>% round(digits = 3)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$iagpnt) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      p1 <- ggplot(data %>% subset(ballot == 1),
                   mapping = aes(y = iagpnt))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(10,40),
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
        scale_y_continuous(limits = c(10,40),
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
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$iagpnt, design = design)) %>% round(digits = 3)
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tochld, design = design)) %>% round(digits = 3)
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
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tochld, design = design)) %>% round(digits = 3)
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
      
      by_year_plots(data = data, var = "tochld", limits = c(20,60), 
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
      
      by_ballot_plots(data = data, var = "tochld", limits = c(20,60), 
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
      
      by_cohort_plots(data = data, var = "tochld", limits =c(20,60),
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$ageadlt, design = design)) %>% round(digits = 3)
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
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$ageadlt, design = design)) %>% round(digits = 3)
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
      
      by_gender_plots(data = data, var = "ageadlt", limits = c(10,40), titles = c("At what age, approximately, would you say girls or women become adults?",
                                                                                  "At what age, approximately, would you say boys or men become adults?"))
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
                      titles = c('"At what age, approximately, would you say ____ become adults? (WOMEN\'s responses)"',
                                 '"At what age, approximately, would you say ____ become adults? (MEN\'s responses)"'))
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
                      titles = c('"At what age, approximately, would you say women or girls become adults?"',
                                 '"At what age, approximately, would you say men or boys become adults?"'))
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$agemage, design = design)) %>% round(digits = 3)
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
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$agemage, design = design)) %>% round(digits = 3)
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
      
      by_gender_plots(data = data, var = "agemage", limits = c(20,60), titles = c("At what age, approximately, would you say girls or women become middle aged?",
                                                                                  "At what age, approximately, would you say boys or men become middle aged?"))
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
                      titles = c('"At what age, approximately, would you say ____ reach middle age?" (WOMEN\'s responses)',
                                 '"At what age, approximately, would you say ____ reach middle age?" (MEN\'s responses)'))
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
                      titles = c('"At what age, approximately, would you say women reach middle age?"',
                                 '"At what age, approximately, would you say men reach middle age?"'))
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$ageoage, design = design)) %>% round(digits = 3)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$ageoage) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$ageoage, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$ageoage, design = design)) %>% round(digits = 3)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$ageoage) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      p1 <- ggplot(data %>% subset(ballot == 1),
                   mapping = aes(y = ageoage))+
        geom_boxplot(fill = "#F37E7E") +
        scale_y_continuous(limits = c(40,90),
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
        scale_y_continuous(limits = c(40,90),
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
      
      by_gender_plots(data = data, var = "ageoage", limits = c(40,90), titles = c("At what age, approximately, would you say women reach old age?",
                                                                                  "At what age, approximately, would you say men reach old age?"))
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
      
      by_year_plots(data = data, var = "ageoage", limits = c(40,90), 
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
      
      by_ballot_plots(data = data, var = "ageoage", limits = c(40,90),
                      titles = c('"At what age, approximately, would you say ____ reach old age?" (WOMEN\'s responses)',
                                 '"At what age, approximately, would you say ____ reach old age?" (MEN\'s responses)'))
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
      
      by_cohort_plots(data = data, var = "ageoage", limits =c(40,90),
                      titles = c('"At what age, approximately, would you say girls or women reach old age?"',
                                 '"At what age, approximately, would you say boys or men reach old age?"'))
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tygrtr, design = design)) %>% round(digits = 3)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tygrtr) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tygrtr, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tygrtr, design = design)) %>% round(digits = 3)
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$iagrtr, design = design)) %>% round(digits = 3)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$iagrtr) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$iagrtr, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$iagrtr, design = design)) %>% round(digits = 3)
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$towkht, design = design)) %>% round(digits = 3)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$towkht) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$towkht, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$towkht, design = design)) %>% round(digits = 3)
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
      
      by_year_plots(data = data, var = "towkht", limits = c(50,90), 
                    titles = c('"After what age would you say a woman is generally too old to be working 20 hours or more per week?"',
                               '"After what age would you say a man is generally too old to be working 20 hours or more per week?"'))
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tyglvp, design = design)) %>% round(digits = 3)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tyglvp) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tyglvp, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tyglvp, design = design)) %>% round(digits = 3)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$tyglvp) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      overview_plots(data = data, table_data1 = data_agg1, table_data2 = data_agg2, var = "tyglvp", limits = c(10,30),
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
      
      by_gender_plots(data = data, var = "tyglvp", limits = c(10,30), 
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
      
      by_year_plots(data = data, var = "tyglvp", limits = c(10,30), 
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
      
      by_ballot_plots(data = data, var = "tyglvp", limits = c(10,30),
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
      
      by_cohort_plots(data = data, var = "tyglvp", limits =c(10,30),
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$iaglptn, design = design)) %>% round(digits = 3)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$iaglptn) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$iaglptn, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$iaglptn, design = design)) %>% round(digits = 3)
        data_agg2$median[which(data_agg2$cntry == i)] <- median(subset(data, cntry == i & ballot == 2)$iaglptn) %>% round(digits = 2)
        data_agg2$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 2))
      }
      
      overview_plots(data = data, table_data1 = data_agg1, table_data2 = data_agg2, var = "iaglptn", limits = c(10,40),
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
      
      by_gender_plots(data = data, var = "iaglptn", limits = c(10,40), 
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
      
      by_year_plots(data = data, var = "iaglptn", limits = c(10,40), 
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
      
      by_ballot_plots(data = data, var = "iaglptn", limits = c(10,40),
                      titles = c('"In your opinion, what is the ideal age for a ____ to start living with a partner they are not married to?" (WOMENS\'s responses)',
                                 '"In your opinion, what is the ideal age for a ____ to start living with a partner they are not married to?" (MENS\'s responses)'))
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
      
      by_cohort_plots(data = data, var = "iaglptn", limits =c(10,40),
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
        data_agg1$se[which(data_agg1$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 1)$tolvpnt, design = design)) %>% round(digits = 3)
        data_agg1$median[which(data_agg1$cntry == i)] <- median(subset(data, cntry == i & ballot == 1)$tolvpnt) %>% round(digits = 2)
        data_agg1$total_N[which(data_agg1$cntry == i)] <- nrow(data_full %>% subset(cntry == i & ballot == 1))
      }
      
      for (i in data_agg2$cntry){
        design <- svydesign(ids = ~0, data = subset(data, cntry == i & ballot == 2), weights = subset(data, cntry == i & ballot == 2)$dweight)
        data_agg2$mean[which(data_agg2$cntry == i)] <- svymean(subset(data, cntry == i & ballot == 2)$tolvpnt, design = design)[1] %>% round(digits = 2)
        data_agg2$se[which(data_agg2$cntry == i)] <- SE(svymean(subset(data, cntry == i & ballot == 2)$tolvpnt, design = design)) %>% round(digits = 3)
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
  } ### PLOTS ###
  
  {
    output$map <- renderPlot({
      
      map_var1 <- paste(input$map_question, input$map_ballot, sep = "")
      map_var2 <- paste(input$map_question, input$map_ballot, sep = "")
      
      map <- {ggplot(data = world) +
          geom_sf(data = world_full, fill = "grey50")+
          geom_sf(aes(fill = world[[map_var1]])) +
          ggtitle(paste('',
                        recode(map_var1,
                               "tygpnt_f"~'"Before what age would you say a woman is generally too young to become a mother?"',
                               "tygpnt_m"~'"Before what age would you say a man is generally too young to become a father?"',
                               "iagpnt_f"~'"In your opinion, what age is ideal for a woman to become a mother?"',
                               "iagpnt_m"~'"In your opinion, what age is ideal for a man to become a father?"',
                               "tochld_f"~'"After what age would you say a woman is generally too old to consider having any more children?"',
                               "tochld_m"~'"After what age would you say a man is generally too old to consider having any more children?"',
                               "ageadlt_f"~"At what age, approximately, would you say girls or women become adults?",
                               "ageadlt_m"~"At what age, approximately, would you say boys or men become adults?",
                               "agemage_f"~"At what age, approximately, would you say women reach middle age?",
                               "agemage_m"~"At what age, approximately, would you say men reach middle age?",
                               "ageoage_f"~"At what age, approximately, would you say women reach old age?",
                               "ageoage_m"~"At what age, approximately, would you say men reach old age?",
                               "tygrtr_f"~"Before what age would you say a woman is generally too young to retire permanently?",
                               "tygrtr_m"~"Before what age would you say a man is generally too young to retire permanently?",
                               "iagrtr_f"~"In your opinion, what is the ideal age for a woman to retire permanently?",
                               "iagrtr_m"~"In your opinion, what is the ideal age for a man to retire permanently?",
                               "towkht_f"~"After what age would you say a woman is generally too old to be working 20 hours or more per week?",
                               "towkht_m"~"After what age would you say a man is generally too old to be working 20 hours or more per week?",
                               "tyglvp_f"~"Before what age would you say a woman is generally too young to start living with a partner she is not married to?",
                               "tyglvp_m"~"Before what age would you say a man is generally too young to start living with a partner he is not married to?",
                               "iaglptn_f"~"In your opinion, what is the ideal age for a girl or woman to start living with a partner she is not married to?",
                               "iaglptn_m"~"In your opinion, what is the ideal age for a boy or man to start living with a partner he is not married to?",
                               "tolvpnt_f"~"After what age would you say a woman is generally too old to still be living with her parents?",
                               "tolvpnt_m"~"After what age would you say a man is generally too old to still be living with his parents?"
                               ),
                        '')) +
          scale_x_continuous(limits = c(-20,50)) +
          scale_y_continuous(limits = c(35,70)) +
          theme(axis.text.y=element_blank(),
                axis.title.y = element_blank(),
                axis.ticks.y=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.x = element_blank(),
                legend.position = c(0.04, 0.74),
                panel.grid = element_blank(),)+
          facet_wrap(~year, nrow = 2)+
          scale_alpha_discrete(range = c(0.4,1)) +
          scale_fill_brewer(palette = "Dark"
                            , name = "year")+
          labs(fill = "Age") +
          scale_fill_continuous(limits = c(min(world[[map_var1]]), max(world[[map_var1]]))
                                #      if(map_var1 == "tygpnt_f") {c(18,20)}
                                # else if(map_var1 == "tygpnt_m") {c(20,23)}
                                # else if(map_var1 == "iagpnt_f") {c(22,28)}
                                # else if(map_var1 == "iagpnt_m") {c(25,30)}
                                # else if(map_var1 == "tochld_f") {c(40,45)}
                                # else if(map_var1 == "tochld_m") {c(45,50)}
                                # else {c(18,50)}
                                ,
                                high = "#132B43", low = "#56B1F7")}
      
      legend1 <- {ggplot(data = world 
                         #%>% subset(year == 2006) 
                         %>% na.omit(), 
                         aes(x = .data[[map_var1]], y = reorder(name, .data[[map_var1]]), fill=.data[[map_var1]])) +
          geom_bar(stat = "identity")+
          coord_cartesian(xlim = c(min(na.omit(world)[[map_var1]])-1, max(na.omit(world)[[map_var1]])+1)
                          #      if(map_var1 == "tygpnt_f") {c(17,21)}
                          # else if(map_var1 == "tygpnt_m") {c(18,24)}
                          # else if(map_var1 == "iagpnt_f") {c(21,29)}
                          # else if(map_var1 == "iagpnt_m") {c(24,31)}
                          # else if(map_var1 == "tochld_f") {c(39,46)}
                          # else if(map_var1 == "tochld_m") {c(44,51)}
                          # else {c(18,50)}
          )+
          scale_fill_gradient(low= "#56B1F7",high= "#132B43", space='Lab') +
          geom_flag(aes(country = code, y = reorder(name, .data[[map_var1]]), x = .data[[map_var1]]), size = 6)+
          ggtitle(" ")+
          theme(axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                #axis.ticks.x = element_blank(),
                legend.position = "none")+
          scale_y_discrete(breaks = NULL)+
          scale_x_continuous(breaks = if(map_var1 %in% c("agemage_f","agemage_m","ageoage_f","ageoage_m","tygrtr_f","tygrtr_m",
                                                         "iagrtr_f", "towkht_f","towkht_m","tolvpnt_f","tolvpnt_m")){seq(0,100, by = 2)}
                                      else{seq(0,100)})+
          theme(
            panel.grid.major.x = element_line(size = 1, colour = "#0091FF")
          )+
          facet_wrap(~ year, ncol = 1, scales = "free")}
      
      # legend2 <- ggplot(data = world %>% subset(year == 2018) %>% na.omit(), 
      #                   aes(x = .data[[map_var1]], y = reorder(name, .data[[map_var1]]), fill=.data[[map_var1]])) +
      #   geom_bar(stat = "identity")+
      #   coord_cartesian(xlim = c(min(na.omit(world)[[map_var1]])-1, max(na.omit(world)[[map_var1]])+1)
      #                   #      if(map_var1 == "tygpnt_f") {c(17,21)}
      #                   # else if(map_var1 == "tygpnt_m") {c(18,24)}
      #                   # else if(map_var1 == "iagpnt_f") {c(21,29)}
      #                   # else if(map_var1 == "iagpnt_m") {c(24,31)}
      #                   # else if(map_var1 == "tochld_f") {c(39,46)}
      #                   # else if(map_var1 == "tochld_m") {c(44,51)}
      #                   # else {c(18,50)}
      #                   )+
      #   scale_fill_gradient(low= "#56B1F7",high= "#132B43", space='Lab') +
      #   geom_flag(aes(country = code, y = reorder(name, .data[[map_var1]]), x = .data[[map_var1]]), size = 6)+
      #   ggtitle("2006")+
      #   theme(axis.title.x = element_blank(),
      #         axis.title.y = element_blank(),
      #         axis.ticks.x = element_blank(),
      #         legend.position = "none")+
      #   scale_y_discrete(breaks = NULL)+
      #   scale_x_continuous(breaks = seq(1,50))+
      #   theme(
      #     panel.grid.major.x = element_line(size = 1, colour = "#0091FF")
      #   )
      
      # legend <- grid.arrange(legend1, legend2, nrow = 2)
      
      ggarrange(map, legend1, ncol = 2, widths = c(12,3), heights = c(1,1), align = "v") %>% annotate_figure(top= " ")
      
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