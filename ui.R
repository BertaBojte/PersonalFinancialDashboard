library(shinydashboard)
library(shiny)
library(ggplot2)
library(data.table)
library(googlesheets)
library(lubridate)
library(readr)
library(RColorBrewer)
library(shinycssloaders)

#### HTML - CSS part for formattin ####
multicollabs <- list(tags$head(tags$style(
  HTML(
    "
    .shiny-options-group {
    height: 190px;
    -webkit-column-count: 2; /* Chrome, Safari, Opera */
    -moz-column-count: 2;    /* Firefox */
    column-count: 2;
    -webkit-column-fill: balance;
    -moz-column-fill: balance;
    column-fill: balance;
    margin-top: 0px;
    }
    
    .control-label {
    padding-bottom: 0px;
    }
    
    div.radio {
    margin-top: 0px;
    margin-bottom: 0px;
    padding-bottom: 3px;
    }
    
    div.shiny-spinner-output-container
    {
      height: 30px;
    }

  "
  )
  )))


# shiny-spinner-output-container

options(spinner.color = "green")
options(spinner.size = 0.3)

#### DasboardPage ####
dashboardPage(
#### Menu structure and style ####
  skin = 'green',
  dashboardHeader(title = "Financial Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("exchange")),
    menuItem("Actual Month Cost", tabName = "actual", icon = icon("credit-card")),
    menuItem("History Cost", tabName = "History",icon = icon("archive")),
    menuItem("Income", tabName = "Income", icon = icon("dollar")),
    menuItem("Contains dummy data", tabName = "", icon = icon("exclamation-triangle"))
  )),
#### Page structure ####
  dashboardBody(
    includeCSS('finance_css.css'),
    tabItems(
#### Overview page ####
    tabItem(tabName = 'overview',
            fluidRow(
              box(title = "Actual Month",
                width = 6, height = '100%',
                box("Act. Income", withSpinner(textOutput("actIncome")), width = 3),
                box("Act. Cost", withSpinner(textOutput("actCost")), width = 3),
                box("Act. savings", withSpinner(textOutput("actincome")), width = 3),
                box("Act. balance", withSpinner(textOutput("actBalance")), width = 3)),
              
              box(title = "Summary", width = 6, height = '100%',
                box("Sum. Income", withSpinner(textOutput("sumIncome")), width = 3),
                box("Sum. Cost", withSpinner(textOutput("SumCost")), width = 3),
                box("Sum. savings", withSpinner(textOutput("sum_saving")), width = 3),
                box("Sum. balance", withSpinner(textOutput("sumBalance")), width = 3))
              ),
            
            fluidRow(
              box( 
                  withSpinner(plotOutput('act_inc_cost', width = 'auto', height = 'auto'), size = 0.4),
                width = 6, height = 350),
              box( 
                sliderInput(
                  "ip_year_sum",
                  label = NULL,
                  min = 2018,
                  max = 2020,
                  value = c(2018, 2020),
                  # step = 1,
                  width = 'auto'),
                withSpinner(plotOutput('sum_inc_cost', width = 'auto', height = 'auto'), size = 0.4),
                width = 6, height = 350)
              )
            ),
#### Aktulis honlap oldal ####
    tabItem(tabName = 'actual',
            tabsetPanel(
              tabPanel(
                title = 'Dashboard',
                icon = icon('dashboard'),
                fluidRow(
                  box('All Cost', textOutput('totalAmount'), width = 4),
                  box('Cost', textOutput('costAmount'), width = 4),
                  box('Savings', textOutput('savingsAmount'), width = 4)
                ),
                fluidRow(box(
                  plotOutput('trend_costs', width = 'auto', height = 'auto'),
                  width = 12,
                  height = 180
                )),
                fluidRow(box(
                  plotOutput('op_bar_actual_Amount', width = 'auto', height = 'auto'),
                  width = 12,
                  height = 250
                ))
              ),
              tabPanel(
                title = 'Data',
                icon = icon('table'),
                dataTableOutput("op_costs")
              )
            )),
    
#### Archív oldal ####
    tabItem(tabName = 'History',
            tabsetPanel(
              tabPanel(
                title = 'Dashboard',
                icon = icon('dashboard'),
                fluidRow(
                  box('All Cost', textOutput('totalAmount_arc'), width = 4),
                  box('Cost', textOutput('costAmount_arc'), width =
                        4),
                  box('Savings', textOutput('incomeAmount_arc'), width = 4)
                ),
                fluidRow(
                  box(
                    sliderInput(
                      "ip_ev",
                      label = 'Year Selector',
                      min = 2018,
                      max = 2020,
                      value = c(2018, 2020),
                      step = 1
                    ),
                    # sliderInput(
                    #   "ip_honap",
                    #   label = NULL,
                    #   min = 1,
                    #   max = 12,
                    #   value = c(1, 12),
                    #   step = 1
                    # ),
                    radioButtons("ip_arc_n_h",
                                 label = "Daily/Monthly view",
                                 choices = list('Daily' = 'N','Monthly' = 'H'), inline = F,
                                 selected = "N"),
                    width = 3,
                    height = 210
                  ),
                  box(
                    plotOutput('trend_costs_acr'),
                    width = 9,
                    height = 210
                  )
                ),
                fluidRow(
                  box(
                    multicollabs,
                    radioButtons(
                      "ip_Category",
                      label = h3('Category'),
                      choices = list(
                        'Drinks' = 'Drinks',
                        'Food' = 'Food',
                        'Shopping' = 'Shopping',
                        'Transport' = 'Transport',
                        'Healthcare' = 'Healthcare',
                        'Sport' = 'Sport',
                        'Fun' = 'Fun',
                        'Travel' = 'Travel',
                        'Work' = 'Work',
                        'Bank' = 'Bank',
                        'Savings' = 'Savings',
                        'Other' = 'Other',
                        'Education' = 'Education',
                        'Bills' = 'Bills'
                      ),
                      selected = 'Food'
                    ),
                    width = 3,
                    height = 300
                  ),
                  box(
                    plotOutput('op_bar_monthly_Amount'),
                    width = 9,
                    height = 300
                  )
                ),
                fluidRow(
                  box(plotOutput(
                    'op_pie_ForWho', width = 'auto', height = 110
                  ), width = 4),
                  box(plotOutput(
                    'op_pie_How', width = 'auto', height = 110
                  ), width = 4),
                  box(plotOutput(
                    'op_pie_freq', width = 'auto', height = 110
                  ), width = 4)
                )
              ),
              tabPanel(
                title = 'Data',
                icon = icon('table'),
                dataTableOutput("op_costs_arc")
              )
            )),
    
#### Income oldal ####
    tabItem(tabName = 'Income',
            tabsetPanel(
              tabPanel(
                title = 'Dashboard',
                icon = icon('dashboard'),
                fluidRow(
                  box('All Income', textOutput('totalIncome'), width = 4),
                  box('Compensation', textOutput('IncomeCompensation'), width = 4),
                  box('Other', textOutput('IncomeOther'), width = 4)
                )
                ,
                fluidRow(
                  box(
                    sliderInput(
                      "ip_ev_inc",
                      label = 'Year Selector',
                      min = 2018,
                      max = 2020,
                      value = c(2018, 2020),
                      step = 1
                    ),
                    # sliderInput(
                    #   "ip_honap_inc",
                    #   label = NULL,
                    #   min = 1,
                    #   max = 12,
                    #   value = c(1, 12),
                    #   step = 1
                    # ),
                    radioButtons("ip_inc_n_h",
                                 label = 'Daily/Monthly view',
                                 choices = list('Daily' = 'N','Monthly' = 'H'), inline = F,
                                 selected = "N"),
                    width = 3,
                    height = 210
                  ),
                  box(
                    plotOutput('trend_Income'),
                    width = 9,
                    height = 210
                  )
                ),
                fluidRow(
                  box(
                    multicollabs,
                    radioButtons(
                      "ip_Category_inc",
                      label = h3('Category'),
                      choices = list(
                        'Compensation' = 'Compensation',
                        'Family' = 'Family',
                        'Scholarship' = 'Scholarship',
                        'Bank' = 'Bank',
                        'Other' = 'Other'
                      ),
                      selected = 'Compensation'
                    ),
                    width = 3,
                    height = 250
                  ),
                  box(
                    plotOutput('op_bar_monthly_Amount_Income'),
                    width = 9,
                    height = 250
                  )
                )
              ),
              tabPanel(
                title = 'Data',
                icon = icon('table'),
                dataTableOutput("op_Income")
              )
            ))
  ))
)
