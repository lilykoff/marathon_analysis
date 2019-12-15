library(shiny)
library(tidyverse)
library(reshape2)
library(shinythemes)

# read in data 
results_all <- read.csv("results_all.csv", stringsAsFactors = F)
# convert strings to factors 
results_all$qualifier <- as.factor(results_all$qualifier)
results_all$M.F <- as.factor(results_all$M.F)
results_all$agegroup <- as.factor(results_all$agegroup)
results_all$Country <- as.factor(results_all$Country)
results_all$Year <- as.factor(results_all$Year)
# create one more variable of interest: whether runner is a US citizen
results_all <- results_all %>% mutate(
  US_Citizen = case_when(
    Country=="USA" ~ "Yes",
    TRUE ~ "No"
  )
)
results_all$US_Citizen <- as.factor(results_all$US_Citizen)

################################################## UI 

ui <- fluidPage(theme = shinytheme("darkly"),
                tabsetPanel(type="tabs",
                            
                            # tab 1: allow user to compare finish times based on certain characteristics 
                            # user selects age and qualification status
                            # output is overlapping histogram of finish times by gender
                            tabPanel("Basic Demographics: Finish Times",
                                     pageWithSidebar(
                                       headerPanel('Compare Finish Time by Gender'),
                                       sidebarPanel(
                                         selectInput('age_dem1', 'Choose an Age Group', levels(results_all$agegroup)),
                                         selectInput('qual_dem1', 'Qualifier', levels(results_all$qualifier))),
                                       mainPanel(
                                         plotOutput('plot_dem1')
                                       )
                                     )),
                            
                            # tab 2: displays four different "summaries" of participants
                            # qualification status by gender
                            # qualification status by age
                            # gender by age 
                            # age by gender 
                            tabPanel("Basic Demographics: Participants",
                                     pageWithSidebar(
                                       headerPanel('Compare Participants'),
                                       sidebarPanel(
                                         radioButtons('comp', 'Choose Graph to Display', c("Qualification Status by Gender",
                                                                                           "Qualification Status by Age Group",
                                                                                           "Gender by Age Group",
                                                                                           "Age Group by Gender"))),
                                       mainPanel(
                                         plotOutput('plotbar')
                                       )
                                     )),
                            
                            # tab 3: visualizes finish times by year
                            # user selects age and qualification status
                            # output is a box plot of the distribution of each genders' finish times by year 
                            tabPanel("Finish Times by Year",
                                     pageWithSidebar(
                                       headerPanel('Compare Finish Times By Year'),
                                       sidebarPanel(
                                         selectInput('ageyear', 'Choose an Age Group', levels(results_all$agegroup)),
                                         selectInput('qyear', 'Choose Qualifying Status', levels(results_all$qualifier))),
                                       mainPanel(
                                         plotOutput('plotyear') #boxplot by year
                                       )
                                     )),
                            
                            # tab 4: visualize pacing strategy by gender 
                            # user selects age group and qualification status 
                            # output is box plot of percent change in each 5k segment from previous 5k segment comparing genders
                            tabPanel("Pacing Strategy by Gender",
                                     pageWithSidebar(
                                       headerPanel('Compare Genders within Age Group and Qualficiation Status'),
                                       sidebarPanel(
                                         selectInput('age1', 'Choose an Age Group', levels(results_all$agegroup)),
                                         selectInput('qual', 'Qualifier', levels(results_all$qualifier))),
                                       mainPanel(
                                         plotOutput('plot1')
                                       )
                                     )),
                            
                            # tab 5: visualize pacing by age 
                            # user selects gender and qualification status and chooses two age groups to compare 
                            # output is box plot of percent change in each 5k segment from previous 5k segment comparing age groups 
                            tabPanel("Pacing Strategy by Age",
                                     pageWithSidebar(
                                       headerPanel('Compare Age Groups within Gender and Qualification Status'),
                                       sidebarPanel(
                                         selectInput('gender', 'Choose a Gender', levels(results_all$M.F)),
                                         selectInput('age_a', 'Choose an Age Group', levels(results_all$agegroup)),
                                         selectInput('age_b', 'Choose a Second Age Group', levels(results_all$agegroup)),
                                         selectInput('qual2', 'Qualifier', levels(results_all$qualifier))),
                                       mainPanel(
                                         plotOutput('plot2')
                                       )
                                     )),
                            
                            # tab 6: visualize pacing by qualification status 
                            # user selects gender and age group
                            # output is box plot of percent change in each 5k segment from previous 5k segment comparing qualification status
                            tabPanel("Pacing Strategy by Qualification",
                                     pageWithSidebar(
                                       headerPanel('Compare Qualifiers within Gender and Age'),
                                       sidebarPanel(
                                         selectInput('gender2', 'Choose a Gender', levels(results_all$M.F)),
                                         selectInput('age', 'Choose an Age Group', levels(results_all$agegroup))),
                                       mainPanel(
                                         plotOutput('plot3')
                                       )
                                     )),
                            
                            # tab 7: runs a two sample t-test
                            # comparison is on percent change from first half marathon pace to second half marathon pacce 
                            # possible groups to compare: gender, age, qualification status
                            # user selects variable they want to compare by
                            # conditional logic then displays the other variables to fix and the variables to specify comparison for
                            # user must also specify equal variance and type of test 
                            # output is key summary statistics, test result, and histogram comparing the distributions of each group
                            tabPanel("Hypothesis Testing",
                                     pageWithSidebar(
                                       headerPanel('Perform a T-test'),
                                       sidebarPanel(
                                         selectInput('comparison', 'Choose Variable to Compare By', c("Age", "Gender", "Qualification Status")),
                                         conditionalPanel(
                                           condition = "input.comparison=='Age'",
                                           selectInput("genderfilter", "Stratify by Gender", levels(results_all$M.F)),
                                           selectInput("yearfilter", "Stratify by Year", levels(results_all$Year)),
                                           selectInput("qfilter", "Stratify by Qualification", levels(results_all$qualifier)),
                                           selectInput("age1c", "Age Group 1 to Compare", levels(results_all$agegroup)),
                                           selectInput("age2c", "Age Group 2 to Compare", levels(results_all$agegroup), selected=2)
                                         ),
                                         conditionalPanel(
                                           condition = "input.comparison=='Gender'",
                                           selectInput("agefilter", "Stratify by Age", levels(results_all$agegroup)),
                                           selectInput("yearfilter2", "Stratify by Year", levels(results_all$Year)),
                                           selectInput("qfilter2", "Stratify by Qualification", levels(results_all$qualifier))
                                         ),
                                         conditionalPanel(
                                           condition = "input.comparison=='Qualification Status'",
                                           selectInput("genderfilter2", "Stratify by Gender", levels(results_all$M.F)),
                                           selectInput("agefilter2", "Stratify by Age", levels(results_all$agegroup)),
                                           selectInput("yearfilter3", "Stratify by Year", levels(results_all$Year))
                                         ),
                                         selectInput('tail', 'Type of Test', choices = c("Equal" = "two.sided", 
                                                                                         "Less" = "less",
                                                                                         "Greater" = "greater")),
                                         radioButtons("varequal",
                                                      "Assume Samples Have Equal Variance?",
                                                      choices = c("Yes" = "y",
                                                                  "No" = "n"))
                                       ),
                                       mainPanel(fluidRow(column(10, offset = 1,
                                                                 plotOutput('graph'))),
                                                 fluidRow(column(8, offset = 1,
                                                                 h2("Null Hypothesis:"),
                                                                 p("There is no difference in the percent change in pace from the first to the second half of the marathon
                                                     between the two groups being compared among the chosen strata"),
                                                                 h2("Summary statistics"),
                                                                 tableOutput('parametric'),
                                                                 h2("Test Results"),
                                                                 tableOutput('results')))
                                       )
                                     )),
                            
                            # tab 8: linear regression
                            # user selects which variables they want to include in the regression
                            # output is regression output
                            # conditional logic then lets user input values and predict finish time based on regression
                            # adjusted r2 also included
                            tabPanel("Linear Regression",
                                     pageWithSidebar(
                                       headerPanel('Create a Linear Regression Model'),
                                       sidebarPanel(
                                         checkboxGroupInput('explanvars','Select the explanatory variables to include', choices = c("Age" = "Age",
                                                                                                                                    "Age Group" = "agegroup",
                                                                                                                                    "Gender" = "M.F",
                                                                                                                                    "Year" = "Year",
                                                                                                                                    "First 5K Pace" = "First5k",
                                                                                                                                    "Qualifier" = "qualifier",
                                                                                                                                    "US Citizen" = "US_Citizen")),
                                         tags$b("Choose Inputs to Predict Finish Time Based on Model:"),
                                         conditionalPanel(
                                           condition = "input.explanvars.includes('Age')",
                                           numericInput("agepred", "Age", value=25, min = min(results_all$Age), 
                                                        max = max(results_all$Age))
                                         ),
                                         conditionalPanel(
                                           condition = "input.explanvars.includes('agegroup')",
                                           selectInput("agegpred", "Age Group", choices=c(levels(results_all$agegroup)))
                                         ),
                                         conditionalPanel(
                                           condition = "input.explanvars.includes('M.F')",
                                           selectInput("genpred", "Gender", choices=c("Female" = "F", "Male" = "M"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.explanvars.includes('Year')",
                                           selectInput("ypred", "Year", choices=levels(results_all$Year))
                                         ),
                                         conditionalPanel(
                                           condition = "input.explanvars.includes('First5k')",
                                           sliderInput("timepred", "First 5K Pace", min = 4, max=18, value=8, step =0.5)
                                         ),
                                         conditionalPanel(
                                           condition = "input.explanvars.includes('qualifier')",
                                           selectInput("qpred", "Qualifier", choices=c("Qualifier", "Not Qualifier"))
                                         ),
                                         conditionalPanel(
                                           condition = "input.explanvars.includes('US_Citizen')",
                                           selectInput("cpred", "US Citizen", choices=c("Yes", "No"))
                                         ),
                                         p("Predicted Finish Time (Hours):"),
                                         verbatimTextOutput("pred")
                                         
                                       ),
                                       mainPanel(fluidRow(column(10, offset = 1,
                                                                 h2("Model Summary"),
                                                                 verbatimTextOutput("summary"),
                                                                 h3("Adjusted R Squared"),
                                                                 verbatimTextOutput("ar2")
                                       )
                                       
                                       
                                       ))))
                ))

###################################### SERVER 
server <- function(input, output) {
  # tab 1 output 
  output$plot_dem1 <- renderPlot({dat <- results_all %>% filter(agegroup==input$age_dem1&qualifier==input$qual_dem1)
  ggplot(dat, aes(x=Official.Time))+geom_histogram(data = subset(dat, M.F=="F"), aes(fill="red"), alpha=.4)+
    geom_histogram(data=subset(dat, M.F=="M"), aes(fill="blue"), alpha=.4)+
    labs(x = "Official Time (minutes)", y= "Count", title = "Finish Times by Gender")+theme_minimal()+
    scale_fill_manual("Gender", 
                      values =c('blue'='blue','red'='red'), labels = c("Male", "Female"))
  
  })
  
  # tab 2 output 
  output$plotbar <- renderPlot({
    if(input$comp=="Qualification Status by Gender"){
      ggplot(results_all, aes(x=qualifier,  group=M.F)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        geom_text(aes( label = scales::percent(..prop..),
                       y= ..prop.. ), stat= "count", vjust = -.5) +
        labs(title = "Qualification Status by Gender", y = "Percent of Participants", x = "Qualification Status")+
        facet_grid(~as.factor(M.F)) +
        scale_y_continuous(labels = scales::percent)+theme(legend.position = "none")}
    else if(input$comp=="Qualification Status by Age Group"){
      ggplot(results_all, aes(x=qualifier,  group=agegroup)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        labs(title = "Qualification Status by Age Group", y = "Percent of Participants", x = "Qualification Status")+
        facet_grid(~agegroup) +
        scale_y_continuous(labels = scales::percent)+theme(legend.position = "none")+
        theme(axis.text.x = element_text(angle=90))
    }
    else if(input$comp=="Gender by Age Group"){
      ggplot(results_all, aes(x=agegroup,  group=M.F)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        labs(title = "Gender by Age Group", y = "Percent of Participants", x ="Age Group")+
        facet_grid(~M.F) +
        scale_y_continuous(labels = scales::percent)+theme(legend.position = "none")+
        theme(axis.text.x = element_text(angle=45))
    }
    else{
      ggplot(results_all, aes(x=M.F,  group=agegroup)) + 
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
        labs(title = "Age Group by Gender", y = "Percent of Participants", x = "Gender")+
        facet_grid(~agegroup) +
        scale_y_continuous(labels = scales::percent)+theme(legend.position = "none")
    }
  })
  
  # tab 3 output 
  output$plotyear <- renderPlot({results_all %>% filter(qualifier==input$qyear&agegroup==input$ageyear) %>%
      ggplot(aes(x=Year, y=Official.Time))+geom_boxplot(aes(col=M.F))+labs(
        x="Year", y = "Official Time", title = "Distribution of Times by Year")+theme_minimal()+
      scale_color_manual("Gender", values = c("red", "blue"), labels = c("red" = "Female", "blue" = "Male"))
    
  })
  
  # tab 4 output 
  output$plot1 <- renderPlot({results_all %>% filter(agegroup==input$age1&qualifier==input$qual) %>%
      select(Name, agegroup, Change5to10, Change10to15, Change15to20, 
             Change20to25, Change25to30, Change30to35,Change35to40, M.F) %>% melt(id = c("Name", "agegroup", "M.F"))%>%
      filter(value>-25&value<25)%>%ggplot(aes(x=as.factor(variable), y = value, color=M.F))+geom_boxplot()+
      labs(x = "Kilometer", y = "Percent Change in Pace", title = "Pacing Strategy by Gender")+scale_color_manual(name = "Gender",  values=c("blue", "red"), labels = c("blue" = "Male", "red" = "Female"))+theme_minimal()
    
  })
  
  # tab 5 output 
  output$plot2 <- renderPlot({results_all %>% filter(M.F==input$gender&qualifier==input$qual2&agegroup==input$age_a|agegroup==input$age_b) %>%
      select(Name, agegroup, Change5to10, Change10to15, Change15to20, 
             Change20to25, Change25to30, Change30to35,Change35to40) %>% melt(id = c("Name", "agegroup"))%>%
      filter(value>-25&value<25)%>%ggplot(aes(x=as.factor(variable), y = value, color=agegroup))+geom_boxplot()+
      labs(x = "Kilometer", y = "Percent Change in Pace", title = "Pacing Strategy by Age Group")+scale_color_manual(name = "Age Group", values=c("red", "blue"))+theme_minimal()
    
  })
  
  # tab 6 output 
  output$plot3 <- renderPlot({results_all %>% filter(M.F==input$gender2&agegroup==input$age) %>%
      select(Name, agegroup, qualifier, Change5to10, Change10to15, Change15to20, 
             Change20to25, Change25to30, Change30to35,Change35to40) %>% melt(id = c("Name", "qualifier", "agegroup"))%>%
      filter(value>-100)%>%ggplot(aes(x=as.factor(variable), y = value, color=qualifier))+geom_boxplot()+
      labs(x = "Kilometer", y = "Percent Change in Pace", title = "Pacing Strategy by Qualification")+scale_color_manual(name = "Qualifier", values=c("red", "blue"))+theme_minimal()
    
  })
  
  # tab 7 output, overlapping histograms
  output$graph <- renderPlot({
    if(input$comparison=="Age"){
      dat <- results_all %>% filter(agegroup==input$age1c|agegroup==input$age2c) %>%
        filter(M.F==input$genderfilter&qualifier==input$qfilter&Year==input$yearfilter) %>%
        select(Name, agegroup, ChangeHalf) 
      ggplot(dat,aes(x=ChangeHalf))+geom_histogram(data = subset(dat, agegroup==input$age1c),aes(
        fill="blue"), alpha=.4)+geom_histogram(data = subset(dat, agegroup==input$age2c), aes(fill="red"), alpha=.4)+labs(
          x = "Percent Change in Pace From First to Second Half of Race", y = "Count",
          title = "Pace Change by Age Group"
        )+scale_fill_manual(name = "Age Group", values = c("blue", "red"), labels = c("blue" = paste(input$age1c), "red" = paste(input$age2c)))+theme_minimal()
    }
    else if(input$comparison=="Gender"){
      dat <- results_all %>% 
        filter(agegroup==input$agefilter&qualifier==input$qfilter2&Year==input$yearfilter2) %>%
        select(Name, M.F, ChangeHalf) 
      ggplot(dat, aes(x=ChangeHalf))+geom_histogram(data = subset(dat, M.F=="M"),aes(
        fill="blue"), alpha=.4)+geom_histogram(data=subset(dat, M.F=="F"),aes(fill="red"),alpha=.4)+labs(
          x = "Percent Change in Pace From First to Second Half of Race", y = "Count",
          title = "Pace Change by Gender"
        )+scale_fill_manual(name = "Gender", values = c("blue", "red"), labels = c("blue" = "Male", "red" = "Female"))+theme_minimal()
    }
    else{
      dat <- results_all %>% 
        filter(M.F==input$genderfilter2&agegroup==input$agefilter2&Year==input$yearfilter3) %>%
        select(Name, qualifier, ChangeHalf) 
      ggplot(dat, aes(x=ChangeHalf))+geom_histogram(data = subset(dat, qualifier=="Qualifier"),aes(
        fill="red"), alpha=.4)+geom_histogram(data = subset(dat, qualifier=="Not Qualifier"),aes(
          fill="blue"), alpha=.4)+labs(
            x = "Percent Change in Pace From First to Second Half of Race", y = "Count",
            title = "Pace Change by Qualification Status"
          )+scale_fill_manual(name = "Qualifier", values = c("red", "blue"),
                              labels = c("red" = "Qualifier","blue"= "Not Qualifier"))+theme_minimal()
    }
  })
  
  # tab 7 output: reactive function to get t-test results 
  ttestout <- reactive({
    if(input$comparison=="Age"){
      data <- results_all %>% filter(agegroup==input$age1c|agegroup==input$age2c) %>%
        filter(M.F==input$genderfilter&qualifier==input$qfilter&Year==input$yearfilter)
      var1 <- data$ChangeHalf[data$agegroup==input$age1c]
      var2 <- data$ChangeHalf[data$agegroup==input$age2c]}
    else if(input$comparison=="Gender"){
      data <- results_all %>% filter(agegroup==input$agefilter&qualifier==input$qfilter2&Year==input$yearfilter2)
      var1 <- data$ChangeHalf[data$M.F=="M"]
      var2 <- data$ChangeHalf[data$M.F=="F"]}
    else{
      data <- results_all %>% filter(M.F==input$genderfilter2&agegroup==input$agefilter2&Year==input$yearfilter3)
      var1 <- data$ChangeHalf[data$qualifier=="Qualifier"]
      var2 <- data$ChangeHalf[data$qualifier=="Not Qualifier"]
    }
    if (is.null(var1)){return(NULL)}
    if (is.null(var2)){return(NULL)}
    ve <- ifelse(input$varequal == 'y', TRUE, FALSE)
    t2 <- t.test(var1, var2, alternative = input$tail, var.equal = ve)
    return(t2)
  })
  
  # tab 7 output: results from t-test based on input 
  output$results <- renderTable({
    if(input$comparison=="Age"){
      data <- results_all %>% filter(agegroup==input$age1c|agegroup==input$age2c) %>%
        filter(M.F==input$genderfilter&qualifier==input$qfilter&Year==input$yearfilter)
      var1 <- data$ChangeHalf[data$agegroup==input$age1c]
      var2 <- data$ChangeHalf[data$agegroup==input$age2c]
      name1 <- input$age1c
      name2 <- input$age2c}
    else if(input$comparison=="Gender"){
      data <- results_all %>% filter(agegroup==input$agefilter&qualifier==input$qfilter2&Year==input$yearfilter2)
      var1 <- (data$ChangeHalf[data$M.F=="M"])
      var2 <- (data$ChangeHalf[data$M.F=="F"])
      name1 <- "Male"
      name2 <- "Female"}
    else{
      data <- results_all %>% filter(M.F==input$genderfilter2&agegroup==input$agefilter2&Year==input$yearfilter3)
      var1 <- (data$ChangeHalf[data$qualifier=="Qualifier"])
      var2 <- (data$ChangeHalf[data$qualifier=="Not Qualifier"])
      name1 <- "Qualifier"
      name2 <- "Not Qualifier"
    }
    mean1 <- mean(var1, na.rm=T)
    mean2 <- mean(var2, na.rm=T)
    diff <- mean1-mean2
    vals <- ttestout()
    if (is.null(vals)){return(NULL)}
    pval = vals$p.value
    stat = vals$statistic
    res <- data.frame("Difference" = diff, "Test Statistic" = stat, "P Value" = pval)
    return(res)
  })
  
  # tab 7 output: summary statistics table 
  output$parametric <- renderTable({
    if(input$comparison=="Age"){
      data <- results_all %>% filter(agegroup==input$age1c|agegroup==input$age2c) %>%
        filter(M.F==input$genderfilter&qualifier==input$qfilter&Year==input$yearfilter)
      var1 <- data$ChangeHalf[data$agegroup==input$age1c]
      var2 <- data$ChangeHalf[data$agegroup==input$age2c]
      name1 <- input$age1c
      name2 <- input$age2c}
    else if(input$comparison=="Gender"){
      data <- results_all %>% filter(agegroup==input$agefilter&qualifier==input$qfilter2&Year==input$yearfilter2)
      var1 <- (data$ChangeHalf[data$M.F=="M"])
      var2 <- (data$ChangeHalf[data$M.F=="F"])
      name1 <- "Male"
      name2 <- "Female"}
    else{
      data <- results_all %>% filter(M.F==input$genderfilter2&agegroup==input$agefilter2&Year==input$yearfilter3)
      var1 <- (data$ChangeHalf[data$qualifier=="Qualifier"])
      var2 <- (data$ChangeHalf[data$qualifier=="Not Qualifier"])
      name1 <- "Qualifier"
      name2 <- "Not Qualifier"
    }
    mean1 <- mean(var1, na.rm=T)
    mean2 <- mean(var2, na.rm=T)
    standard_deviation1 <- sd(var1, na.rm=T)
    standard_deviation2 <- sd(var2, na.rm=T)
    standard_error1 <- sd(var1, na.rm=T)/sqrt(length(var1))
    standard_error2 <- sd(var2, na.rm=T)/sqrt(length(var2))
    parametric1 <- data.frame(Variable = name1, Mean = mean1, 
                              SD =standard_deviation1, 
                              SE =standard_error1, n = length(var1))
    parametric2 <- data.frame(Variable = name2, Mean = mean2, 
                              SD =standard_deviation2, 
                              SE =standard_error2, n = length(var2))
    return(rbind(parametric1,parametric2))
  })

  # tab 8 output: reactive function to get linear regression output 
  lmout <- reactive({
    model <- lm(as.formula(paste("Official.Time","~",paste(input$explanvars, collapse="+"))), data=results_all)
  })
  
  # tab 8 output: reactive function to get linear model prediction output 
  lmpred <- reactive({
    new <- data.frame("Age" = input$agepred, "agegroup" = input$agegpred, "M.F" = input$genpred, "First5k" = input$timepred,
                      "qualifier" = input$qpred, "Year" = input$ypred, "US_Citizen" = input$cpred)
    return(predict(lmout(), newdata=new))
  })
  
  # tab 8 output: print predicted finish time 
  output$pred <- renderPrint({
    if(!is.null(input$explanvars)){
      as.numeric(round(lmpred()/60, 2))}
  })
  
  # tab 8 output: linear regression summary
  output$summary <- renderPrint({
    if(!is.null(input$explanvars)){
      summary(lmout())
    }
  })
  
  # tab 8 output: adjusted r^2
  output$ar2 <- renderPrint({
    if(!is.null(input$explanvars)){
      summary(lmout())$adj.r.squared}
  })
}
  
  


# Run the application 
shinyApp(ui = ui, server = server)
