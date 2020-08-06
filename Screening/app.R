library(shiny)
library(shinydashboard)
library(scales)
library(tidyverse)
library(plotly)

#' TODO: list
#' - Have exogenous shock variables included only when exogenous shock = "Yes"
#' - Format some of the input variables (see here: https://stackoverflow.com/questions/51791983/how-to-format-r-shiny-numericinput)
#' - Make number of days an input parameter (85 for WI)

# fix the styles, will over-ride later
# see https://rstudio.github.io/shinydashboard/appearance.html#statuses-and-colors
input_element_color <- "primary" 
highlight_color <- "olive" 
regular_color <- "navy"
# actual colors used:
# uw madison red #da004c
# darker red #8b0037

header <- dashboardHeader(
  title = "Paltiel COVID-19 Screening for College"
)

sidebar <- dashboardSidebar(
  tags$style("@import url(https://use.fontawesome.com/releases/v5.14.0/css/all.css);"),
  sidebarMenu(
    id = "sidebar",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Source Code", icon = icon("file-code-o"), 
             href = "https://github.com/aravamu2/Paltiel-COVID-19-Screening-for-College/blob/master/Screening/app.R/"),
    menuItem("Original Spreadsheet", icon = icon("google-drive"), 
             href = "https://docs.google.com/spreadsheets/d/1otD4h-DpmAmh4dUAM4favTjbsly3t5z-OXOtFSbF1lY/edit#gid=1783644071"),
    menuItem("References", tabName = "references", icon = icon("book"))
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
    background:#646569
    }
    
    .box.box-solid.box-primary{
    border-bottom-color:#646569;
    border-left-color:#646569;
    border-right-color:#646569;
    border-top-color:#646569;
    }
    
    .bg-olive {
    background-color: #8F0000!important;
    }
    
    .bg-navy {
    background-color: #8F0000!important;
    }
    
    .skin-blue .main-header .navbar { background-color: #c5050c!important; }
    
    .skin-blue .main-header .logo { background-color: #9b0000; }
    .skin-blue .main-header .logo:hover { background-color: #8F0000; }
    .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
    border-left-color: #8F0000; 
    }
    .skin-blue .main-header .navbar .sidebar-toggle:hover{ background-color:#9b0000; }
    .skin-blue .main-header .navbar .dropdown-menu li a:hover{ background:#8F0000; }
    "
  )),
  tabItems(
    tabItem(
      # MAIN DASHBOARD ---------------------------------------------------
      tabName = "dashboard",
      ## INPUTS --------
      column(width = 2,
             ## Population
             box(title = "Population", width = NULL, solidHeader = TRUE, status = input_element_color,
                 collapsible = TRUE, collapsed = FALSE,
                 numericInput("initial_susceptible", "Initial susceptible", value = 4990,
                              min = 1000),
                 numericInput("initial_infected", "Initial infected", value = 10,
                              max = 500)
             ),
             ## Epidemiology
             box(title = "Epidemiology", width = NULL, solidHeader = TRUE, status = input_element_color,
                 collapsible = TRUE, collapsed = FALSE,
                 numericInput("R0", "R0", value = 2.5, min = 0.1, max = 5, step = 0.5),
                 radioButtons("exogenous_shocks", "Exogenous shocks?", choices = c("Yes", "No"), selected = "Yes"),
                 numericInput("frequency_exogenous_shocks", "Frequency of exogenous shocks (every x days)", value = 7, min = 0),
                 numericInput("new_infections_per_shock", "Number of new infections per shock", value = 10, min = 0, max = 200),
             ),
      ),
      column(width = 2,
             ## Clinical history
             box(title = "Clinical history", width = NULL, solidHeader = TRUE, status = input_element_color,
                 collapsible = TRUE, collapsed = TRUE,
                 numericInput("days_to_incubation", "Days to Incubation", value = 3, min = 1),
                 numericInput("time_to_recovery", "Time to recovery (days)", value = 14, min = 1),
                 numericInput("pct_advancing_to_symptoms", "% asymptomatics advancing to symptoms", value = 30,
                              min = 5, max = 95, step = 1),
                 numericInput("symptom_case_fatality_ratio", "Symptom case fatality risk", value = 0.0005,
                              min = 0, max = 0.01, step = 0.0001),
             ),
             ## Testing
             box(title = "Testing", width = NULL, solidHeader = TRUE, status = input_element_color,
                 collapsible = TRUE, collapsed = FALSE,
                 selectizeInput("frequency_of_screening", "Frequency of screening",
                                choices = c("Symptoms Only",
                                            "Every 4 weeks",
                                            "Every 3 weeks",
                                            "Every 2 weeks",
                                            "Weekly",
                                            "Every 3 days",
                                            "Every 2 days",
                                            "Daily"),
                                selected = "Weekly"),
                 numericInput("test_sensitivity", "Test sensitivity", value = 0.8,
                              min = 0.5, max = 1, step = 0.01),
                 numericInput("test_specificity", "Test specificity", value = 0.98,
                              min = 0.7, max = 1, step = 0.01),
                 numericInput("test_cost", "Test cost ($)", value = 25,
                              min = 0, max = 1000, step = 5),
                 numericInput("time_to_return_fps", "Time to return FPs from Isolation (days)", value = 1,
                              min = 0),
                 numericInput("confirmatory_test_cost", "Confirmatory Test Cost", value = 100,
                              min = 0, max = 1000, step = 5),
             ),
      ),
      ## OUTPUT: plot and metrics --------
      column(width = 8, 
             fluidRow(
               valueBoxOutput("testing_cost_box", width = 4),
               valueBoxOutput("number_tested_box", width = 4),
               valueBoxOutput("average_iu_census_box", width = 4),
             ),
             fluidRow(
               valueBoxOutput("infections_box", width = 4),
               valueBoxOutput("number_confirmatory_tests_box", width = 4),
               valueBoxOutput("average_pct_isolated_box", width = 4),
             ),
             box(plotlyOutput("plot1"), width = 400)
      )
    ),
    ## References ----------------------------------------------------------
    tabItem(
      tabName = "references",
      h2("Methodology"),
      p("The methodology for this application comes from the JAMA Network Open 
        article by ", 
        a("Paltiel, Zheng, and Walensky (2020).",
          href = "https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2768923"),
        " The authors also built a spreadsheet detailing their compartmental model calculations,
        which can be found ",
        a("online.", 
          href = "https://docs.google.com/spreadsheets/d/1otD4h-DpmAmh4dUAM4favTjbsly3t5z-OXOtFSbF1lY/edit#gid=1783644071"),
        " Our implementation currently reproduced this spreadsheet and may soon expand on it."),
      h2("Glossary"),
      p(strong("Sensitivity:"),"The ability of a test, case definition, or 
        surveillance system to identify true cases; the proportion of people 
        with a health condition (or the proportion of outbreaks) that are 
        identified by a screening test or case definition (or surveillance 
        system). See ", 
        a("CDC DSEPD Principles of Epidemiology in Public Health Practice.",
          href = "https://www.cdc.gov/csels/dsepd/ss1978/glossary.html")),
      p(strong("Specificity:"),"The ability or a test, case definition, or 
        surveillance system to exclude persons without the health condition of 
        interest; the proportion of persons without a health condition that is 
        correctly identified as such by a screening test, case definition, or 
        surveillance system. See ", 
        a("CDC DSEPD Principles of Epidemiology in Public Health Practice.",
          href = "https://www.cdc.gov/csels/dsepd/ss1978/glossary.html")),
      p(strong("Test cost:"),"The cost of a test to identify occurrence at the 
        individual level even if there is no reason to suspect infection - e.g., 
        there is no known exposure. This includes, but is not limited to, 
        screening of non-symptomatic individuals without known exposure with the 
        intent of making decisions based on the test results. Screening tests 
        are intended to identify infected individuals without, or prior to 
        the development of, symptoms who may be contagious so that measures can be 
        taken to prevent further transmission. See ", 
        a("CDC SARS-CoV-2 Diagnostic, Screening, and Surveillance Testing.",
          href = "https://www.cdc.gov/coronavirus/2019-ncov/lab/pooling-procedures.html")),
      p(strong("Confirmatory test cost:"),"The cost of a test to identify occurrence 
        at the individual level and is performed when there is a reason to 
        suspect that an individual may be infected, such as having symptoms or 
        suspected recent exposure or to determine the resolution of infection. See ", 
        a("CDC SARS-CoV-2 Diagnostic, Screening, and Surveillance Testing.",
          href = "https://www.cdc.gov/coronavirus/2019-ncov/lab/pooling-procedures.html")),
      p(strong("Initial susceptible:"),"Noninfected persons."),
      p(strong("Initial infected:"),"Infected, asymptomatic persons."),
      p(strong("R0:"),"The reproduction number is the average number of people that 
        one person with COVID-19 is likely to infect in a population without any 
        immunity (from a previous infection) or any interventions. R0 is an 
        estimate of how transmissible a pathogen is in a population. R0 
        estimates vary across populations and are a function of the duration of 
        contagiousness, the likelihood of infection per contact between a 
        susceptible person and an infectious person, and the contact rate. See ", 
        a("CDC COVID-19 Pandemic Planning Scenarios.",
          href = "https://www.cdc.gov/coronavirus/2019-ncov/hcp/planning-scenarios.html")),
      p(strong("Exogenous shocks:"),"Infections transmitted to students by 
        university employees or members of the surrounding community or during 
        superspreader events, such as parties."),
      p(strong("Days to Incubation:"),"The mean time from exposure to both 
        infectiousness and screening detectability."),
      p(strong("Time to recovery (days):"),"The mean time from confirmed 
        (true-positive) results would remain in the isolation dormitory to 
        ensure they were not infectious before proceeding to a recovered or 
        immune state."),
      p(strong("% asymptomatics advancing to symptoms:"),"The probability that 
        infection would eventually lead to observable COVID-19-defining symptoms 
        in the young cohort."),
      p(strong("Symptom case fatality risk:"),"The COVID-related mortality in 
        persons of college age."),
      p(strong("Time to return FPs from Isolation (days):"),"The time from 
        (false-positive) results would remain isolated, reflecting the 
        assumption that a highly specific confirmatory test could overturn the 
        original diagnosis, permitting them to return to the campus 
        population."),
      h2("Assumptions"),
      p(strong("Note:"),"An 80-day time horizon (during an abbreviated 80-day 
        semester, running from Labor Day through Thanksgiving) is used for the 
        analysis."),
      p(strong("Note:"),"A target population of younger than 30 years, nonimmune, 
        living students in a congregate setting essay at a medium-sized college 
        setting is assumed for the analysis."),
      p(strong("Note:"),"A lag of 8 hours after individuals receiving a positive 
        test result (true or false) is assumed. Those exhibiting COVID-19 
        symptoms would be moved from the general population to an isolation 
        dormitory, where their infection would be confirmed and receive 
        supportive care from which no further transmissions would occur. The lag 
        reflected both test turnaround delays and the time required to locate 
        and isolate identified cases."),
      h2("Contacts"),
      p("We encourage suggestions of new features and improvements to make the 
        visualizations more helpful. The developers can be contacted below."),
      tags$ul(tags$li("Srikanth Aravamuthan (",
                      a("aravamuthan@wisc.edu"),
                      ")"),
              tags$li("Sean Kent (",
                      a("spkent@wisc.edu"),
                      ")"),
              tags$li("Steve Goldstein (",
                      a("sgoldstein@wisc.edu"),
                      ")"),
              tags$li("Brian Yandell (",
                      a("brian.yandell@wisc.edu"),
                      ")")),
      h2("References"),
      p("Paltiel AD, Zheng A, Walensky RP. Assessment of SARS-CoV-2 Screening 
        Strategies to Permit the Safe Reopening of College Campuses in the 
        United States. JAMA Netw Open. 2020;3(7):e2016818. 
        doi:10.1001/jamanetworkopen.2020.16818"),
    )
  )
)



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  ## Check that inputs meet restrictions ---------------------------------------
  observe({
    # Don't throw an error if the field is left blank momentarily
    req(input$initial_susceptible,
        input$initial_infected,
        input$R0,
        input$new_infections_per_shock,
        input$days_to_incubation,
        input$time_to_recovery,
        input$pct_advancing_to_symptoms,
        input$symptom_case_fatality_ratio,
        input$test_sensitivity,
        input$test_specificity,
        input$test_cost,
        input$confirmatory_test_cost,
        cancelOutput = TRUE
    )
    
    showWarningIf <- function(condition, message) {
      if (condition) {
        showNotification(message, type = "warning")
      }
    }
    
    showWarningIf(input$initial_susceptible < 1000, "The value for initial susceptible you entered is below the recommended minimum of 1000.")
    showWarningIf(input$initial_infected > 500, "The value for initial infected you entered is above the recommended maximum of 500.")
    showWarningIf(input$R0 < 0.1, "The value for R0 you entered is below the recommended minimum of 0.1.")
    showWarningIf(input$R0 > 5, "The value for R0 you entered is above the recommended maximum of 5.")
    showWarningIf(input$new_infections_per_shock < 0, "The value for the number of new infections per shock you entered is below the recommended minimum of 0.")
    showWarningIf(input$new_infections_per_shock > 200, "The value the number of new infections per shock you entered is above the recommended maximum of 200.")
    showWarningIf(input$days_to_incubation < 1, "The value for days to incubation you entered is above the recommended maximum of 1.")
    showWarningIf(input$time_to_recovery < 1, "The value for time to recovery (days) you entered is above the recommended maximum of 1.")
    showWarningIf(input$pct_advancing_to_symptoms < 5, "The value for percent asymptomatic advancing to symptoms you entered is below the recommended minimum of 5.")
    showWarningIf(input$pct_advancing_to_symptoms > 95, "The value for percent asymptomatic advancing to symptoms you entered is above the recommended maximum of 95.")
    showWarningIf(input$symptom_case_fatality_ratio < 0, "The value for symptom case fatality risk you entered is below the recommended minimum of 0.")
    showWarningIf(input$symptom_case_fatality_ratio > 0.01, "The value for symptom case fatality risk you entered is above the recommended maximum of 0.01.")
    showWarningIf(input$test_sensitivity < 0.5, "The value for test sensitivity you entered is below the recommended minimum of 0.5.")
    showWarningIf(input$test_sensitivity > 1, "The value for test sensitivity you entered is above the recommended maximum of 1.")
    showWarningIf(input$test_specificity < 0.7, "The value for test specificity you entered is below the recommended minimum of 0.7.")
    showWarningIf(input$test_specificity > 1, "The value for test specificity you entered is above the recommended maximum of 1.")
    showWarningIf(input$test_cost < 0, "The value for test cost you entered is below the recommended minimum of 0.")
    showWarningIf(input$test_cost > 1000, "The value for test cost you entered is above the recommended maximum of 1000.")
    showWarningIf(input$confirmatory_test_cost < 0, "The value for confirmatory test cost you entered is below the recommended minimum of 0.")
    showWarningIf(input$confirmatory_test_cost > 1000, "The value for confirmatory test cost you entered is above the recommended maximum of 1000.")
  })
  
  ## Reactive elements -------------------------------------------------------
  df <- reactive({
    req(input$initial_susceptible,
        input$initial_infected,
        input$R0,
        input$new_infections_per_shock,
        input$days_to_incubation,
        input$time_to_recovery,
        input$pct_advancing_to_symptoms,
        input$symptom_case_fatality_ratio,
        input$test_sensitivity,
        input$test_specificity,
        input$test_cost,
        input$confirmatory_test_cost,
        cancelOutput = TRUE
    )
    
    num.exogenous.shocks <- case_when(
      input$exogenous_shocks == "Yes" ~ 1,
      input$exogenous_shocks == "No" ~ 0
    )
    cycles.per.day <- 3
    frequency.exogenous.shocks <- cycles.per.day*input$frequency_exogenous_shocks
    cycles.per.test <- case_when(
      input$frequency_of_screening == "Daily" ~ 1*cycles.per.day,
      input$frequency_of_screening == "Every 2 days" ~ 2*cycles.per.day,
      input$frequency_of_screening == "Every 3 days" ~ 3*cycles.per.day,
      input$frequency_of_screening == "Weekly" ~ 7*cycles.per.day,
      input$frequency_of_screening == "Every 2 weeks" ~ 14*cycles.per.day,
      input$frequency_of_screening == "Every 3 weeks" ~ 21*cycles.per.day,
      input$frequency_of_screening == "Every 4 weeks" ~ 28*cycles.per.day,
      input$frequency_of_screening == "Symptoms Only" ~ 99999999999
    )
    rho <- 1/(input$time_to_recovery*cycles.per.day)
    sigma <- rho*(input$pct_advancing_to_symptoms/100/(1-input$pct_advancing_to_symptoms/100))
    beta <- input$R0*(rho+sigma)
    delta <- (input$symptom_case_fatality_ratio/(1-input$symptom_case_fatality_ratio))*rho
    theta <- 1/(input$days_to_incubation*cycles.per.day)
    mu <- 1/(cycles.per.day*input$time_to_return_fps)
    
    n.cycle <- 240
    
    mat <- matrix(c(0,input$initial_susceptible,0,0,input$initial_infected,0,0,0,0), nrow = 1)
    mat <- 
      rbind(
        mat,
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
      mat <- 
        rbind(
          mat,
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
      slice(2:n()) %>% 
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
  
  ## OUTPUTS -------------------------------------------------------------------
  output$plot1 <- 
    renderPlotly({
      df() %>% 
        select(Day, `True Positive`, Symptoms, `False Positive`) %>% 
        pivot_longer(`True Positive`:`False Positive`, names_to = "Group", values_to = "Value") %>% 
        mutate(Group = as.factor(Group),
               Group = forcats::fct_relevel(Group, levels = c("True Positive", "Symptoms", "False Positive")),
               Group = forcats::fct_recode(Group,
                                           "Asymptomatic (TP)" = "True Positive",
                                           "Symptomatic" = "Symptoms",
                                           "Uninfected (FP)" = "False Positive")) %>% 
        group_by(Day) %>% 
        arrange(Group) %>% 
        mutate(`New Students` = sum(Value),
               Students = cumsum(Value)) %>% 
        plot_ly(x = ~Day, 
                y = ~Students, 
                color = ~Group, 
                colors = RColorBrewer::brewer.pal(9,"YlOrRd")[c(3,6,9)],
                alpha = 0.7,
                type = "scatter",
                mode = "lines",
                fill = 'tonexty',
                text = ~paste0("</br>", Group,": ", round(Value, 1), " students", 
                               " (", scales::percent(Value/`New Students`, accuracy = 0.1), ")",
                               "</br>Total students in isolation: ", round(`New Students`, 1),
                               "</br>Day: ", floor(Day)
                               # "</br>", Group," (Percentage of Students): ", 
                               # "</br>", scales::percent(Value/`New Students`, accuracy = 0.1)
                ), 
                hoverinfo = "text") %>% 
        layout(title = "Composition of Isolation Pool") %>% 
        layout(yaxis = list(title = "Number of Students")) %>% 
        layout(autosize = TRUE, 
               margin = list(l = 75,
                             r = 75,
                             b = 75,
                             t = 75,
                             pad = 10)) %>%
        config(displaylogo = FALSE)
    })
  
  ## Value Boxes 
  output$number_tested_box <- renderValueBox({
    valueBox(scales::comma(sum.stat()$number_tested), "Total Tests",
             icon = icon("vial"),
             color = regular_color)
  })
  
  output$number_confirmatory_tests_box <- renderValueBox({
    valueBox(scales::comma(sum.stat()$number_confirmatory_tests), "Confirmatory Tests",
             icon = icon("vials"), 
             color = regular_color)
  })
  
  output$average_iu_census_box <- renderValueBox({
    valueBox(scales::comma(sum.stat()$average_iu_census), "Isolation Pool Size (Avg.)",
             icon = icon("users"),
             color = regular_color)
  })
  
  output$average_pct_isolated_box <- renderValueBox({
    valueBox(scales::percent(sum.stat()$average_pct_isolated), "of Isolation Pool Infected (Avg.)",
             icon = icon("user-plus"),
             color = regular_color)
  })
  
  output$testing_cost_box <- renderValueBox({
    valueBox(scales::dollar(sum.stat()$testing_cost), "Cost of Testing",
             # icon = icon("money-bill-wave"),
             icon = icon("dollar-sign"),
             color = highlight_color)
  })
  
  output$infections_box <- renderValueBox({
    valueBox(scales::comma(sum.stat()$infections), "Total Infections",
             icon = icon("viruses"),
             color = highlight_color)
  })
  
}

shinyApp(ui, server)

## Old code below ---------------------------------------------------------------

# # input style 1 - within larger input box ----------------------------------------------------------------
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
#                       selectizeInput("frequency_of_screening", "Frequency of screening",
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
# # input style 2 - input header ---------------------------------------------------------------------------
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
#                       selectizeInput("frequency_of_screening", "Frequency of screening",
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
