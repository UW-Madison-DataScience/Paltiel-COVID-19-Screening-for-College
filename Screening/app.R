library(shiny)
library(shinydashboard)
library(scales)
library(tidyverse)
library(plotly)

#' TODO: list
#' - Numerical ranges for input parameters
#' - Have exogenous shock variables included only when exogenous shock = "Yes"
#' - Format some of the input variables (see here: https://stackoverflow.com/questions/51791983/how-to-format-r-shiny-numericinput)
#' 

# fix the styles, will over-ride later
# see https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors
input_element_color <- "primary" 
statistics_element_color <- "olive" 
# actual colors used:
# uw madison red #da004c
# darker red #8b0037

header <- dashboardHeader(
    title = "Paltiel COVID-19 Screening for College"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        id = "sidebar",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Source code", icon = icon("file-code-o"), 
                 href = "https://github.com/rstudio/shinydashboard/"),
        menuItem("Orignal Spreasheet", icon = icon("file-code-o"), 
                 href = "https://docs.google.com/spreadsheets/d/1otD4h-DpmAmh4dUAM4favTjbsly3t5z-OXOtFSbF1lY/edit#gid=1783644071")
    )
)

body <- dashboardBody(
    tags$style("@import url(https://use.fontawesome.com/releases/v5.14.0/css/all.css);"),
    # shinydashboard has fixed colors mapped to their CSS classes.  We over-ride those 
    # to produce a custom color palette.  Some details can be found here: https://stackoverflow.com/questions/36080529/r-shinydashboard-customize-box-status-color
    tags$style(HTML(
        "
        .box.box-solid.box-primary>.box-header {
            color:#fff;
            background:#727272
        }

        .box.box-solid.box-primary{
            border-bottom-color:#727272;
            border-left-color:#727272;
            border-right-color:#727272;
            border-top-color:#727272;
        }
        
        .bg-olive {
            background-color: #a1002f!important;
        }
        
        .skin-blue .main-header .navbar { background-color: #da004c!important; }

        .skin-blue .main-header .logo { background-color: #a1002f; }
        .skin-blue .main-header .logo:hover { background-color: #90002A; }
        .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
            border-left-color: #90002A; 
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover{ background-color:#a1002f; }
        .skin-blue .main-header .navbar .dropdown-menu li a:hover{ background:#90002A; }
        "
    )),

    
    ## INPUTS ------------------------------------------------------------------
    column(width = 2,
           ## Population
           box(title = "Population", width = NULL, solidHeader = TRUE, status = input_element_color,
               collapsible = TRUE, collapsed = FALSE,
               numericInput("initial_susceptible", "Initial susceptible", value = 1500),
               numericInput("initial_infected", "Initial infected", value = 10)
           ),
           ## Epidemiology
           box(title = "Epidemiology", width = NULL, solidHeader = TRUE, status = input_element_color,
               collapsible = TRUE, collapsed = FALSE,
               numericInput("R0", "R0", value = 1.5),
               radioButtons("exogenous_shocks", "Exogenous shocks?", choices = c("Yes", "No"), selected = "Yes"),
               numericInput("shocks_frequency", "Frequency of exogenous shocks (every x days)", value = 7),
               numericInput("new_infections_per_shock", "Number of new infections per shock", value = 10),
           ),
    ),
    column(width = 2,
           ## Clinical history
           box(title = "Clinical history", width = NULL, solidHeader = TRUE, status = input_element_color,
               collapsible = TRUE, collapsed = TRUE,
               numericInput("days_to_incubation", "Days to Incubation", value = 3),
               numericInput("time_to_recovery", "Time to recovery (days)", value = 14),
               numericInput("pct_advancing_to_symptoms", "% asymptomatics advancing to symptoms", value = 0.3),
               numericInput("symptom_case_fatality_ratio", "Symptom Case Fatality Ratio", value = 0.0005),
           ),
           ## Testing
           box(title = "Testing", width = NULL, solidHeader = TRUE, status = input_element_color,
               collapsible = TRUE, collapsed = FALSE,
               selectizeInput("freqency_of_screening", "Frequency of screening",
                              choices = c("Symptoms Only",
                                          "Every 4 weeks",
                                          "Every 3 weeks",
                                          "Every 2 weeks",
                                          "Weekly",
                                          "Every 3 days",
                                          "Every 2 days",
                                          "Daily"),
                              selected = "Weekly"),
               numericInput("test_sensitivity", "Test sensitivity", value = 0.8),
               numericInput("test_specificity", "Test specificity", value = 0.98),
               numericInput("test_cost", "Test cost ($)", value = 25),
               numericInput("isolation_return_time", "Time to return FPs from Isolation (days)", value = 3),
               numericInput("confirmatory_test_cost", "Confirmatory Test Cost", value = 100),
           ),
    ),
    
    ## OUTPUT: plot and metrics
    column(width = 8, 
           fluidRow(
               valueBoxOutput("testing_cost_box", width = 4), 
               valueBoxOutput("number_tested_box", width = 4), 
               valueBoxOutput("number_confirmatory_tests_box", width = 4), 
           ),
           fluidRow(
               valueBoxOutput("infections_box", width = 4), 
               valueBoxOutput("average_iu_census_box", width = 4), 
               # valueBoxOutput("average_pct_isolated_box", width = 4), 
               infoBoxOutput("average_pct_isolated_ibox", width = 4),
           ),
           # fluidRow(plotOutput("plot1")),
           plotlyOutput("plot1")
           
           
    )
    
)



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
    ## Reactive elements -------------------------------------------------------
    df <- reactive({
        
        num.exogenous.shocks <- case_when(
            input$exogenous_shocks == "Yes" ~ 1,
            input$exogenous_shocks == "No" ~ 0
        )
        cycles.per.day <- 3
        frequency.exogenous.shocks <- cycles.per.day*input$shocks_frequency
        cycles.per.test <- case_when(
            input$freqency_of_screening == "Daily" ~ 1*cycles.per.day,
            input$freqency_of_screening == "Every 2 Days" ~ 2*cycles.per.day,
            input$freqency_of_screening == "Every 3 Days" ~ 3*cycles.per.day,
            input$freqency_of_screening == "Weekly" ~ 7*cycles.per.day,
            input$freqency_of_screening == "Every 2 Weeks" ~ 14*cycles.per.day,
            input$freqency_of_screening == "Every 3 Weeks" ~ 21*cycles.per.day,
            input$freqency_of_screening == "Every 4 Weeks" ~ 28*cycles.per.day,
            input$freqency_of_screening == "Symptoms Only" ~ 99999999999
        )
        rho <- 1/(input$time_to_recovery*cycles.per.day)
        sigma <- rho*(input$pct_advancing_to_symptoms/(1-input$pct_advancing_to_symptoms))
        beta <- input$R0*(rho+sigma)
        delta <- (input$symptom_case_fatality_ratio/(1-input$symptom_case_fatality_ratio))*rho
        theta <- 1/(input$days_to_incubation*cycles.per.day)
        mu <- 1/(cycles.per.day*input$isolation_return_time)
        
        
        n.cycle <- 240
        
        mat <- matrix(c(0,input$initial_susceptible,0,0,input$initial_infected,0,0,0,0), nrow = 1)
        mat <- rbind(mat,
                     c(1,
                       max(0,mat[1,2]*(1-beta*(mat[1,5]/(mat[1,2]+mat[1,5]+mat[1,4])))+mat[1,3]*mu),
                       max(0,mat[1,3]*(1-mu)),
                       max(0,mat[1,4]*(1-theta)+ beta*(mat[1,2]*mat[1,5]/(mat[1,2]+mat[1,5]+mat[1,4]))),
                       max(0,mat[1,5]*(1-sigma-rho)+mat[1,4]*theta),
                       max(0,mat[1,6]*(1-delta-rho)+(mat[1,5]+mat[1,7])*sigma),
                       0,
                       max(0,mat[1,8]+(mat[1,5]+mat[1,6]+mat[1,7])*rho),
                       max(0,delta*mat[1,6]+mat[1,9]))
        )
        
        superspreader.event <- 0
        superspreader.event <- c(superspreader.event, 
                                 (1:n.cycle %% frequency.exogenous.shocks == 0)*num.exogenous.shocks)
        
        for(i in 2:n.cycle) {
            mat <- rbind(mat,
                         c(i,
                           max(0,mat[i,2]*(1-beta*(mat[i,5]/(mat[i,2]+mat[i,5]+mat[i,4])))+mat[i,3]*mu-mat[i-1,2]*(1-input$test_specificity)/cycles.per.test-superspreader.event[i+1]*input$new_infections_per_shock),
                           max(0,mat[i,3]*(1-mu)+mat[i-1,2]*(1-input$test_specificity)/cycles.per.test),
                           max(0,mat[i,4]*(1-theta)+ beta*(mat[i,2]*mat[i,5]/(mat[i,2]+mat[i,5]+mat[i,4]))+superspreader.event[i+1]*input$new_infections_per_shock),
                           max(0,mat[i,5]*(1-sigma-rho)+mat[i,4]*theta-mat[i-1,5]*input$test_sensitivity/cycles.per.test),
                           max(0,mat[i,6]*(1-delta-rho)+(mat[i,5]+mat[i,7])*sigma),
                           max(0,mat[i,7]*(1-sigma-rho)+mat[i-2,5]*input$test_sensitivity/cycles.per.test),
                           max(0,mat[i,8]+(mat[i,5]+mat[i,6]+mat[i,7])*rho),
                           max(0,delta*mat[i,6]+mat[i,9]))
            )
        }
        mat <- cbind(mat, superspreader.event)
        
        
        names.df <- c("Cycle","Susceptible","FP","Exposed","Asympt","Symptoms","TP","Recovered","Dead","Superspreader Event")
        df <- 
            mat %>% 
            as_tibble() %>% 
            rename_all(~names.df) %>% 
            mutate(`Persons Tested` = (lag(Susceptible,1,NA)+lag(Exposed,1,NA)+lag(Asympt,1,NA))/cycles.per.test,
                   `Total TPs` = lag(Asympt,2,NA)*input$test_sensitivity/cycles.per.test,
                   `Total FPs` = lag(Susceptible,2,NA)*(1-input$test_specificity)/cycles.per.test,
                   `Total TNs` = lag(Susceptible,2,NA)*input$test_specificity/cycles.per.test,
                   `Total FNs` = lag(Exposed,2,NA)+lag(Asympt,2,NA)*(1-input$test_sensitivity)/cycles.per.test) %>% 
            mutate(Day = Cycle/cycles.per.day,
                   `True Positive` = TP,
                   Symptoms = Symptoms,
                   `False Positive` = FP,
                   Total = TP+Symptoms+FP) %>% 
            mutate(`New Infections` = lag(Asympt,1,NA)*beta*lag(Susceptible,1,NA)/(lag(Susceptible,1,NA)+lag(Exposed,1,NA)+lag(Asympt,1,NA)),
                   `New Infections` = ifelse(Cycle>1,
                                             `New Infections`+pmin(`Superspreader Event`*input$new_infections_per_shock,lag(Susceptible,1,NA)),
                                             `New Infections`),
                   `New Infections` = ifelse(is.na(`New Infections`),0,`New Infections`),
                   `Cumulative Infections` = cumsum(`New Infections`),
                   `%Cumulative Infections` = `Cumulative Infections`/input$initial_susceptible)
        
    })
    
    sum.stat <- reactive({
        sum.stat <- 
            df() %>% 
            summarize(`Total Persons Tested in 80 days` = sum(`Persons Tested`, na.rm = TRUE),
                      `Total Confirmatory Tests Performed` = sum(`Total TPs`, na.rm = TRUE) + sum(`Total FPs`, na.rm = TRUE),
                      `Average Isolation Unit Census` = mean(`Total`, na.rm = TRUE),
                      `Average %TP in Isolation` = 1-(mean(`False Positive`, na.rm = TRUE)/mean(`Total`, na.rm = TRUE)),
                      `Total testing cost` = `Total Persons Tested in 80 days`*input$test_cost+`Total Confirmatory Tests Performed`*input$confirmatory_test_cost,
                      `Total Infections` = last(`Cumulative Infections`))
        
        sum.stat <- list(
            ## Expected outputs
            number_tested = sum.stat$`Total Persons Tested in 80 days`,
            number_confirmatory_tests = sum.stat$`Total Confirmatory Tests Performed`,
            average_iu_census = sum.stat$`Average Isolation Unit Census`,
            average_pct_isolated = sum.stat$`Average %TP in Isolation`,
            testing_cost = sum.stat$`Total testing cost`,
            infections = sum.stat$`Total Infections`
        )
        
    })

    ## OUTPUTS -----------------------------------------------------------------
    output$plot1 <- renderPlotly({
        df() %>% 
            select(Day, `True Positive`, Symptoms,`False Positive`) %>% 
            pivot_longer(`True Positive`:`False Positive`, names_to = "Group", values_to = "Value") %>% 
            mutate(Group = as.factor(Group),
                   Group = forcats::fct_relevel(Group, levels = c("True Positive", "Symptoms", "False Positive"))) %>% 
            group_by(Day) %>% 
            arrange(Group) %>% 
            mutate(`New Students` = sum(Value),
                   Students = cumsum(Value)) %>% 
            plot_ly(x = ~Day, 
                    y = ~Students, 
                    color = ~Group, 
                    colors = "RdYlBu",
                    alpha = 0.7,
                    type = "scatter",
                    mode = "lines",
                    fill = 'tonexty',
                    text = ~paste0("</br>", Group,": ", round(Value,3),
                                   "</br>Students: ", round(`New Students`,3),
                                   "</br>", Group," (Percentage of Students): ", 
                                   "</br>", scales::percent(Value/`New Students`, accuracy = 0.1)), 
                    hoverinfo = "text") %>% 
            layout(title = "Isolation Unit Occupancy") %>% 
            layout(yaxis = list(title = "Number of Students")) %>% 
            layout(autosize = TRUE, 
                   margin = list(l = 75,
                                 r = 75,
                                 b = 75,
                                 t = 75,
                                 pad = 10)) %>%
            config(displaylogo = FALSE)
    })
    
    # ## Expected outputs
    # number_tested <- 12388
    # number_confirmatory_tests <- 679
    # average_iu_census <- 42
    # average_pct_isolated <- 0.44
    # testing_cost <- 642272
    # infections <- 359
    
    ## Value Boxes 
    output$number_tested_box <- renderValueBox({
        valueBox(scales::comma(sum.stat()$number_tested), "Total Tests",
                 icon = icon("vial"),
                 color = statistics_element_color)
    })
    
    output$number_confirmatory_tests_box <- renderValueBox({
        valueBox(scales::comma(sum.stat()$number_confirmatory_tests), "Confirmatory Tests",
                 icon = icon("vials"), 
                 color = statistics_element_color)
    })
    
    output$average_iu_census_box <- renderValueBox({
        valueBox(scales::comma(sum.stat()$average_iu_census), "Isolation Unit Census (Avg.)",
                 color = statistics_element_color)
    })
    
    output$average_pct_isolated_box <- renderValueBox({
        valueBox(scales::percent(sum.stat()$average_pct_isolated), "Percentage in Isolation (Avg.)",
                 color = statistics_element_color)
    })
    
    output$testing_cost_box <- renderValueBox({
        valueBox(scales::dollar(sum.stat()$testing_cost), "Cost of Testing",
                 # icon = icon("money-bill-wave"),
                 icon = icon("dollar-sign"),
                 color = statistics_element_color)
    })
    
    output$infections_box <- renderValueBox({
        valueBox(scales::comma(sum.stat()$infections), "Total Infections",
                 icon = icon("viruses"),
                 color = statistics_element_color)
    })
    
    
    output$average_pct_isolated_ibox <- renderInfoBox({
        infoBox(NULL, scales::percent(sum.stat()$average_pct_isolated),
                subtitle = "Percentage in Isolation (Avg.)",
                color = statistics_element_color)
    })
    # output$approvalBox <- renderInfoBox({
    #     infoBox(
    #         "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
    #         color = "yellow"
    #     )
    # })
    # 

}

shinyApp(ui, server)





##Old code below -----------------------------------

# # input style 1 - within larger input box ------------------------------------------------------------------
# column(width = 4,
#        # fluidRow(box(width = NULL, background = "black", "Inputs")),
#        box(width = NULL, solidHeader = FALSE, status = input_element_color, title = "Inputs",
#            column(width = 6,
#                   ## Population
#                   box(title = "Population", width = NULL, #solidHeader = TRUE, status = input_element_color,
#                       background = "light-blue", 
#                       collapsible = TRUE, collapsed = TRUE,
#                       numericInput("initial_susceptible", "Initial susceptible", value = 1001),
#                       numericInput("initial_infected", "Initial infected", value = 10)
#                   ), 
#                   ## Epidemiology
#                   box(title = "Epidemiology", width = NULL, #solidHeader = TRUE, status = input_element_color,
#                       background = "light-blue", 
#                       collapsible = TRUE, collapsed = FALSE,
#                       numericInput("input$R0", "input$R0", value = 2.5),
#                       radioButtons("exogenous_shocks", "Exogenous shocks?", choices = c("Yes", "No"), selected = "Yes"),
#                       numericInput("shocks_frequency", "Frequency of exogenous shocks (every x days)", value = 7),
#                       numericInput("new_infections_per_shock", "Number of new infections per shock", value = 10)
#                   )
#            ),
#            column(width = 6,
#                   ## Clinical history
#                   box(title = "Clinical history", width = NULL, #solidHeader = TRUE, status = input_element_color,
#                       background = "light-blue", 
#                       collapsible = TRUE, collapsed = TRUE,
#                       numericInput("days_to_incubation", "Days to Incubation", value = 3),
#                       numericInput("time_to_recovery", "Time to recovery (days)", value = 14),
#                       numericInput("pct_advancing_to_symptoms", "% asymptomatics advancing to symptoms", value = 0.3),
#                       numericInput("symptom_case_fatality_ratio", "Symptom Case Fatality Ratio", value = 0.0005)
#                   ),
#                   ## Testing
#                   box(title = "Testing", width = NULL, #solidHeader = TRUE, status = input_element_color,
#                       background = "light-blue", 
#                       collapsible = TRUE, collapsed = FALSE,
#                       selectizeInput("freqency_of_screening", "Frequency of screening",
#                                      choices = c("Symptoms Only", 
#                                                  "Every 4 weeks",
#                                                  "Every 3 weeks",
#                                                  "Every 2 weeks",
#                                                  "Weekly",
#                                                  "Every 3 days",
#                                                  "Every 2 days",
#                                                  "Daily"),
#                                      selected = "Every 2 weeks"),
#                       numericInput("test_sensitivity", "Test sensitivity", value = 0.7),
#                       numericInput("test_specificity", "Test specificity", value = 0.98),
#                       numericInput("test_cost", "Test cost ($)", value = 25),
#                       numericInput("isolation_return_time", "Time to return FPs from Isolation (days)", value = 3),
#                       numericInput("confirmatory_test_cost", "Confirmatory Test Cost", value = 100)
#                   )
#            )
#        ),
# ),


# 
# # input style 2 - input header ------------------------------------------------------------------
# column(width = 4,
#        # fluidRow(box(width = NULL, background = "light-blue", "Inputs")),
#        box(width = NULL, background = "light-blue", "INPUTS"),
#        fluidRow(
#            column(width = 6,
#                   ## Population
#                   box(title = "Population", width = NULL, solidHeader = TRUE, status = input_element_color,
#                       collapsible = TRUE, collapsed = TRUE,
#                       numericInput("initial_susceptible", "Initial susceptible", value = 1001),
#                       numericInput("initial_infected", "Initial infected", value = 10)
#                   ),
#                   ## Epidemiology
#                   box(title = "Epidemiology", width = NULL, solidHeader = TRUE, status = input_element_color,
#                       collapsible = TRUE, collapsed = FALSE,
#                       numericInput("input$R0", "input$R0", value = 2.5),
#                       radioButtons("exogenous_shocks", "Exogenous shocks?", choices = c("Yes", "No"), selected = "Yes"),
#                       numericInput("shocks_frequency", "Frequency of exogenous shocks (every x days)", value = 7),
#                       numericInput("new_infections_per_shock", "Number of new infections per shock", value = 10)
#                   )
#            ),
#            column(width = 6,
#                   ## Clinical history
#                   box(title = "Clinical history", width = NULL, solidHeader = TRUE, status = input_element_color,
#                       collapsible = TRUE, collapsed = TRUE,
#                       numericInput("days_to_incubation", "Days to Incubation", value = 3),
#                       numericInput("time_to_recovery", "Time to recovery (days)", value = 14),
#                       numericInput("pct_advancing_to_symptoms", "% asymptomatics advancing to symptoms", value = 0.3),
#                       numericInput("symptom_case_fatality_ratio", "Symptom Case Fatality Ratio", value = 0.0005)
#                   ),
#                   ## Testing
#                   box(title = "Testing", width = NULL, solidHeader = TRUE, status = input_element_color,
#                       collapsible = TRUE, collapsed = FALSE,
#                       selectizeInput("freqency_of_screening", "Frequency of screening",
#                                      choices = c("Symptoms Only", 
#                                                  "Every 4 weeks",
#                                                  "Every 3 weeks",
#                                                  "Every 2 weeks",
#                                                  "Weekly",
#                                                  "Every 3 days",
#                                                  "Every 2 days",
#                                                  "Daily"),
#                                      selected = "Every 2 weeks"),
#                       numericInput("test_sensitivity", "Test sensitivity", value = 0.7),
#                       numericInput("test_specificity", "Test specificity", value = 0.98),
#                       numericInput("test_cost", "Test cost ($)", value = 25),
#                       numericInput("isolation_return_time", "Time to return FPs from Isolation (days)", value = 3),
#                       numericInput("confirmatory_test_cost", "Confirmatory Test Cost", value = 100)
#                   )
#            )  
#        )
# ),
