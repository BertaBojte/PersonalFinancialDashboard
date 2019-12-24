library(shiny)
library(ggplot2)
library(data.table)
library(googlesheets)
library(lubridate)
library(readr)
library(RColorBrewer)

options(scipen  =  999)

#### Helper variabes #### 
days  =  data.table(days  =  seq(1, 31, by  =  1),
                   V1  =  seq(1, 31, by  =  1))

actDate  =  Sys.Date()

colors  <-  c('#969696','#9ECAE1','#005A32','#2171B5',
          '#2a334f','#238B45','#252525','#084594',
          '#CC4C02','#FEC44F','#99000D','#854442',
          '#946b2d','#737373','#993404','#bb1515',
          '#F16913','#dc6900','#8C2D04','#eeba30')

color   <-   c("#7DB359", "#B38F59", "#B359B3", "#59B37D", "#7D59B3", 
          "#B35959", "#598FB3", "#59B3A1", "#B37D59", "#59B38F", 
          "#A159B3", "#596BB3", "#59B3B3", "#8FB359", "#B359A1", 
          "#6BB359", "#A1B359", "#B3596B", "#597DB3", "#6B59B3", 
          "#8F59B3", "#59B36B", "#B3B359", "#59A1B3", "#59B359", 
          "#5959B3", "#B3598F", "#B3A159", "#B3597D", "#B36B59")

#### Authentication Gsheet ####
auth  <-  function() {
  # authentication
  gs_ls()
  
  # workbook read-in
  finance_sheet  <-   gs_title("Personal_Finances_Public")
}

finance_sheet  <-  auth()

#### Data download ####
actDataDownload  <-  function() {
  costs  <-  na.omit(data.table(
    gs_read(
      literal  =  F ,
      ss  =  finance_sheet,
      ws  =  "Actual_Month",
      range  =  cell_cols(1:9),
      col_names  =  T
    )
  ))
  
  costs[, Date :=  as_date(Date, origin  =  '1899-12-30')]
  setnames(costs, 'Amount', 'Amount')
  # setnames(costs,'Hónap','YearMonth')
  setnames(costs, 'Product', 'Product')
  setnames(costs, 'Category', 'Category')
  setnames(costs, 'SubCategory', 'SubCategory')
  setnames(costs, 'Frequency', 'Frequency')
  
  costs  <- 
    costs[, .(`#`,
                 Product,
                 Amount,
                 Category,
                 SubCategory,
                 ForWho,
                 Date,
                 Frequency,
                 How)]
  costs[, days :=  day(Date)]
  
}

arcDataDownload  <-  function() {
  costs_arc  <-  na.omit(data.table(
    gs_read(
      literal  =  F ,
      ss  =  finance_sheet,
      ws  =  "Costs",
      range  =  cell_cols(1:10),
      col_names  =  T,
      col_types  =  cols(YearMonth  =  'c')
    )
  ))

  
  costs_arc[, Date :=  as_date(Date, origin  =  '1899-12-30')]
  setnames(costs_arc, 'Amount', 'Amount')
  setnames(costs_arc,'YearMonth','YearMonth')
  setnames(costs_arc, 'Product', 'Product')
  setnames(costs_arc, 'Category', 'Category')
  setnames(costs_arc, 'SubCategory', 'SubCategory')
  setnames(costs_arc, 'Frequency', 'Frequency')
  
  costs_arc  <- 
    costs_arc[, .(`#`,
                     Product,
                     Amount,
                     Category,
                     SubCategory,
                     ForWho,
                     Date,
                     Frequency,
                     How,
                     YearMonth)]
}

incDataDownload  <-  function() {
  Income  <-  na.omit(data.table(
    gs_read(
      literal  =  F ,
      ss  =  finance_sheet,
      ws  =  "Income",
      range  =  cell_cols(1:7),
      col_names  =  T,
      col_types  =  cols(YearMonth  =  'c')
    )
  ))
  
  Income[, Date :=  as_date(Date, origin  =  '1899-12-30')]
  setnames(Income, 'Amount', 'Amount')
  setnames(Income, 'YearMonth', 'YearMonth')
  setnames(Income, 'Product', 'Product')
  setnames(Income, 'Category', 'Category')
  setnames(Income, 'SubCategory', 'SubCategory')
  
  
  Income  <- 
    Income[, .(`#`,
                  Product,
                  Amount,
                  Category,
                  SubCategory,
                  Date,
                  YearMonth)]
}


costs  <-  actDataDownload()
costs[,YearMonth := paste(year(Date), month(Date), sep  =  '.')]
costs$YearMonth1  <-  factor(costs$YearMonth, as.character(unique(costs$YearMonth)))

costs_arc  <-  arcDataDownload()
costs_arc$YearMonth1  <-  factor(costs_arc$YearMonth, as.character(unique(costs_arc$YearMonth)))

Income  <-  incDataDownload()
Income$YearMonth1  <-  factor(Income$YearMonth, as.character(unique(Income$YearMonth)))

setkey(costs, days)
setkey(days, V1)
costs_days  <-  costs[days]


#### Shiny Server #### 
shinyServer(function(input, output) {
  # -------------Overview Site-------------------
  # Overview Site
  # -----------------1st Row-----------------
  # Actual Month
  output$actIncome  <- 
    renderText(format(Income[paste(year(Date), month(Date), sep  =  '.')  == 
                                  paste(year(actDate), month(actDate), sep  =  '.'),
                                sum(Amount)], big.mark  =  ' '))
  
  output$actCost  <- 
    renderText(format(costs[Category !=  'Savings', sum(Amount)],
                      big.mark  =  ' '))
  
  output$actincome  <- 
    renderText(format(costs[Category  ==  'Savings', sum(Amount)],
                      big.mark  =  ' '))
  
  output$actBalance  <- 
    renderText(format(Income[paste(year(Date), month(Date), sep  = '.')  == 
                                  paste(year(actDate), month(actDate), sep  = '.'),
                                sum(Amount)] - costs[, sum(Amount)]
                      , big.mark  =  ' '))
  
  # Summary
  output$sumIncome  <- 
    renderText(format(Income[, sum(Amount)], big.mark  =  ' '))
  
  output$SumCost  <- 
    renderText(format(costs_arc[Category !=  'Savings', sum(Amount)],
                      big.mark  =  ' '))
  
  output$sum_saving  <- 
    renderText(format(costs_arc[Category  ==  'Savings', sum(Amount)],
                      big.mark  =  ' '))
  
  output$sumBalance  <- 
    renderText(format(Income[paste(year(Date), month(Date), sep  =  '.') != 
                                  paste(year(actDate), month(actDate), sep  =  '.'),
                                sum(Amount)] -
                        (costs_arc[, sum(Amount)])
                      , big.mark  =  ' '))
  
  # -----------------2nd Row-----------------
  
  # Actual Month
  
  act_cost_monthly <- costs[paste(year(Date), month(Date), sep  =  '.')  == 
                            paste(year(actDate), month(actDate), sep  =  '.')
                          & Category !=  'Savings', sum(Amount), by = YearMonth]
  setnames(act_cost_monthly, 'V1', 'value')
  act_cost_monthly$tip1 <- "cost"
  
  act_income_monthly <- costs[paste(year(Date), month(Date), sep  =  '.')  == 
                              paste(year(actDate), month(actDate), sep  =  '.')
                            & Category  ==  'Savings', sum(Amount), by = YearMonth]
  setnames(act_income_monthly, 'V1', 'value')
  act_income_monthly$tip1 <- "income"
  
  act_cost_income <- rbind(act_cost_monthly, act_income_monthly)
  act_cost_income$tip <- "cost"
  
  act_inc_monthly <- Income[paste(year(Date), month(Date), sep  =  '.')  == 
                            paste(year(actDate), month(actDate), sep  =  '.'), 
                          sum(Amount), by = YearMonth]
  setnames(act_inc_monthly, 'V1', 'value')
  act_inc_monthly$tip1 <- "inc"
  
  inc_helper <- data.table(YearMonth = act_inc_monthly$YearMonth,
                        value = 0, tip1 = "cost")
  act_inc_monthly <- rbind(act_inc_monthly, inc_helper)
  act_inc_monthly$tip <- "inc"
  
  act <- rbind(act_cost_income, act_inc_monthly, fill = T)
  
  act$h_t <- paste(act$YearMonth,act$tip, sep = "_")
  act <- act[order(YearMonth,tip)]
  act$rnum <- seq(1,nrow(act),by = 1)
  act$ht1 <- factor(act$h_t, levels =  as.character(unique(act$h_t)))
  act[,tot := sum(value), by = list(YearMonth,tip)]
  act[act == 0] <- NA
  
output$act_inc_cost <- renderPlot(
  ggplot(act) +
    geom_col(aes(x = act$ht1, y = value, group = tip1, fill = tip1),
             alpha = 1, position  =  position_stack(reverse  =  TRUE)) +
    geom_text(aes(x = act$ht1, y = value, group = tip1, label = format(value, big.mark = " ")),
              size = 5, position = position_stack(vjust = 0.5,reverse = T)) +
    geom_text(aes(x = act$ht1, y = tot, group = tip, label = format(tot, big.mark = " ")),
              size = 5, nudge_y = 25000) +
    xlab(NULL) +
    ylab(NULL) +
    scale_x_discrete(breaks = act$ht1, labels = act$tip) +
    theme_bw() +
    theme(
      axis.text.x  =  element_text(angle  =  80, size  =  15, hjust  =  1),
      legend.position  =  "none",
      plot.margin  =  unit(c(1, 1, 1, 1), "lines")) +
    scale_fill_manual(values  =  c(colors[11],colors[6],
                                   colors[14], colors[11]))
  ,
  height = 330, width  =  'auto' )
  
  # Összegző
  sum_cost_monthly <- costs_arc[paste(year(Date), month(Date), sep  =  '.') != 
                                 paste(year(actDate), month(actDate), sep  =  '.')
                               & Category !=  'Savings', 
                               sum(Amount), by = YearMonth]
  setnames(sum_cost_monthly, 'V1', 'value')
  sum_cost_monthly$tip1 <- "cost"
  
  sum_income_monthly <- costs_arc[paste(year(Date), month(Date), sep  =  '.') != 
                                   paste(year(actDate), month(actDate), sep  =  '.')
                                 & Category  ==  'Savings', 
                                 sum(Amount), by = YearMonth]
  setnames(sum_income_monthly, 'V1', 'value')
  sum_income_monthly$tip1 <- "income"
  
  sum_cost_income <- rbind(sum_cost_monthly, sum_income_monthly)
  sum_cost_income$tip <- "cost"
  
  
  sum_inc_monthly <- Income[paste(year(Date), month(Date), sep  =  '.') != 
                             paste(year(actDate), month(actDate), sep  =  '.'), 
                           sum(Amount), by = YearMonth]
  setnames(sum_inc_monthly, 'V1', 'value')
  sum_inc_monthly$tip1 <- "inc."
  
  inc_helper <- data.table(YearMonth = sum_inc_monthly$YearMonth,
                        value = 0, tip1 = "cost")
  sum_inc_monthly <- rbind(sum_inc_monthly, inc_helper)
  
  sum_inc_monthly$tip <- "inc"
  
  summ <- rbind(sum_cost_income, sum_inc_monthly, fill = T)
  
  summ[,Ev := as.double(substr(YearMonth,1,4))]
  summ[,H := as.numeric(substr(YearMonth,6,8))]
  
  summ <- summ[order(Ev,H,tip)]
  summ$YearMonth1 <- factor(summ$YearMonth, as.character(unique(summ$YearMonth)))
  summ$h_t <- paste(summ$YearMonth1,summ$tip, sep = "_")
  summ$rnum <- seq(1,nrow(summ),by = 1)
  summ$ht1 <- factor(summ$h_t, levels =  as.character(unique(summ$h_t)))
  summ <- summ[order(Ev,H,-tip,tip1)]
  sum_helper <- data.table(x = unique(summ$ht1))
  sum_helper[,Ev := as.double(substr(x,1,4))]
  rownames(sum_helper) <- NULL
  
  filt_sum  <-  reactive({
    sum_helper[Ev >=  input$ip_year_sum[1] &
                 Ev <=  input$ip_year_sum[2]
               ]})
  
  filtered_Amountzo1  <-  reactive({
    summ[Ev >=  input$ip_year_sum[1] &
                         Ev <=  input$ip_year_sum[2] 
              ][order(ht1)]
  })
# filtered_Amountzo1() - summ
  # filt_sum() - sum_helper
  output$sum_inc_cost <- renderPlot(
    ggplot(filt_sum()) +
      geom_bar(aes(x = filtered_Amountzo1()$ht1, y = value, group = tip, fill = tip), data = filtered_Amountzo1(),
               stat = 'identity', position  =  'stack', alpha = 1) +
      geom_col(aes(x = filtered_Amountzo1()$ht1, y = value, group = tip1, fill = tip1), data = filtered_Amountzo1(),
               alpha = 1, position  =  position_stack(reverse  =  TRUE)) +
      xlab(NULL) +
      ylab(NULL) +
      scale_x_discrete(breaks = filtered_Amountzo1()$ht1, labels = filtered_Amountzo1()$YearMonth1) +
      theme_bw() +
       theme(
         axis.text.x  =  element_text(angle  =  80, size  =  10, hjust  =  1),
         legend.position  =  "none",
         plot.margin  =  unit(c(1, 1, 1, 1), "lines")) +
      scale_fill_manual(values  =  c(colors[11], colors[11],colors[6],colors[14]))
    ,
    height = 260, width  =  'auto'
  )
  
  
  # ---------Actual Site-----------------------
  # Actual Site
  
  # -----------------0 Row-----------------
  output$op_costs  <-  renderDataTable(costs)
  
  # -----------------1st Row-----------------
  output$totalAmount  <- 
    renderText(format(costs[, sum(Amount)], big.mark  =  ' '))
  
  output$costAmount  <- 
    renderText(format(costs[Category !=  'Savings',
                               sum(Amount)],
                      big.mark  =  ' '))
  
  output$savingsAmount  <- 
    renderText(format(costs[Category  ==  'Savings',
                               sum(Amount)], big.mark  =  ' '))
  
  # -----------------2nd Row-----------------
  # Trend chart - reactive table
  # Trend chart
  output$trend_costs  <-  renderPlot(
    ggplot(costs_days[Category !=  'Savings'])
    + geom_line(aes(x  =  days, y  =  Amount), stat  =  'identity', size  =  1.2)
    + xlab(NULL)
    + ylab(NULL)
    + theme_bw()
    + theme(# legend.position  =  "bottom",
      plot.margin  =  margin(5, 5, 5, 5))
    + scale_fill_manual(values  =  colors),
    height  =  155,
    width  =  'auto'
  )
  
  # -----------------3rd Row-----------------
  # costs by subcategory
  output$op_bar_actual_Amount  <-  renderPlot(
    ggplot(costs[Category !=  'Savings']) +
      geom_bar(aes(
        x  =  Category, y  =  Amount, fill  =  SubCategory
      ), stat  =  'identity')
    + xlab(NULL)
    + ylab(NULL)
    + theme_bw()
    + theme(
      plot.margin  =  margin(5, 5, 5, 5),
      legend.text  =  element_text(size  =  10),
      legend.key.size  =  unit(10, 'pt'),
      legend.title  =  element_blank()
    ) +
      scale_fill_manual(values  =  color)
    ,
    height  =  230,
    width  =  'auto'
  )
  
  
  # ---------History Site---------------
  # History Site
  
  # -----------------0 Row-----------------
  #Data Site - History costs table
  output$op_costs_arc  <-  renderDataTable(costs_arc)
  # -----------------1st Row-----------------
  output$totalAmount_arc  <- 
    renderText(format(costs_arc[, sum(Amount)], big.mark  =  ' '))
  
  output$costAmount_arc  <- 
    renderText(format(costs_arc[Category !=  'Savings',
                                   sum(Amount)],
                      big.mark  =  ' '))
  
  output$incomeAmount_arc  <- 
    renderText(format(costs_arc[Category  ==  'Savings',
                                   sum(Amount)], big.mark  =  ' '))
  
  # -----------------2nd Row-----------------
  # Trend chart - reactive table
  filtered_trend_arc  <-  reactive({
    costs_arc[year(Date) >=  input$ip_ev[1] &
                   year(Date) <=  input$ip_ev[2]
                 &
                   Category !=  'Savings']})
  
  # Trend chart
  plotType_arc  <-  function(type) {
    if (type  ==  'N') {
      trend  <-  ggplot(filtered_trend_arc()) +
        geom_line(aes(x  =  Date, y  =  Amount),
                  stat  =  'identity',
                  size  =  1.2) + xlab(NULL) + ylab(NULL) +
        theme_bw() + theme(legend.position  =  "bottom",
                           plot.margin = margin(5,5,5,5)) 
    }
    else {
      trend  <-  ggplot(filtered_trend_arc()) +
        geom_bar(aes(x  =  YearMonth1, y  =  Amount, fill = Category),
                 stat  =  'identity',
                 size  =  1.2) + xlab(NULL) + ylab(NULL) +
        theme_bw() + theme(legend.position  =  "bottom",
                           plot.margin = margin(5,5,5,5)) +
        guides(fill  =  guide_legend(nrow  =  1)) +
        scale_fill_manual(values  =  c(
          'Drinks'  =  colors[8],
          'Food'  =  colors[11],
          'Shopping'  =  colors[9],
          'Transportation'  =  colors[3],
          'Healthcare'  =  colors[6],
          'Sport'  =  colors[4],
          'Fun'  =  colors[17],
          'Travel'  =  colors[7],
          'Work'  =  colors[19],
          'Bank'  =  colors[1],
          'Savings'  =  colors[15],
          'Other'  =  colors[14],
          'Education'  =  colors[2],
          'Bills' =  colors[10]
        ))
    }
    return(trend)
  }
  
  output$trend_costs_acr  <-  renderPlot(plotType_arc(input$ip_arc_n_h),
                                          height  =  190)
  
  # -----------------3rd Row-----------------

  bills_col = c(
    "#fee6ce",
    "#fdae6b",
    "#fd8d3c",
    "#f16913",
    "#d94801",
    "#a63603",
    "#7f2704",
    "#fee391",
    "#fec44f",
    "#fe9929",
    "#ec7014",
    "#cc4c02",
    "#993404",
    "#662506",
    "#fdbb84",
    "#fc8d59",
    "#ef6548",
    "#d7301f",
    "#a50f15"
  )
  
  subcat_color <- function(subcat){
    if (subcat == 'Drinks') {
      return(rev(brewer.pal(n  =  8, name = 'Blues' )))
    }
    else if (subcat == 'Food') {
      return(rev(brewer.pal(n  =  8, name = 'Reds' )))
    }
    else if (subcat == 'Shopping') {
      return(rev(brewer.pal(n  =  9, name = 'YlOrBr' )))
    }
    else if (subcat == 'Transport') {
      return(rev(brewer.pal(n  =  8, name = 'Greens' )))
    }
    else if (subcat == 'Healthcare') {
      return(rev(brewer.pal(n  =  8, name = 'Greens' )))
    }
    else if (subcat == 'Sport') {
      return(rev(brewer.pal(n  =  8, name = 'Blues' )))
    }
    else if (subcat == 'Fun') {
      return(rev(brewer.pal(n  =  8, name = 'Oranges' )))
    }
    else if (subcat == 'Travel') {
      return(rev(brewer.pal(n  =  8, name = 'Greys' )))
    }
    else if (subcat == 'Work') {
      return(rev(brewer.pal(n  =  8, name = 'Oranges' )))
    }
    else if (subcat == 'Bank') {
      return(rev(brewer.pal(n  =  8, name = 'Greys' )))
    }
    else if (subcat == 'Savings') {
      return(rev(brewer.pal(n  =  8, name = 'Reds' )))
    }
    else if (subcat == 'Other') {
      return(rev(brewer.pal(n  =  8, name = 'Greys' )))
    }
    else if (subcat == 'Education') {
      return(rev(brewer.pal(n  =  8, name = 'Blues' )))
    }
    else if (subcat == 'Bills') {
      return(rev(bills_col))
    }
    else {
      return(brewer.pal(n  =  8, name = 'RdYlGn' ))
    }

  }
  
  # monthly costs by subcategory - reactive table
  filtered_costs_arc  <-  reactive({
    costs_arc[Category %in% input$ip_Category  ==  T &
                   year(Date) >=  input$ip_ev[1] &
                   year(Date) <=  input$ip_ev[2]]
  })
  
  # monthly costs by subcategory 
  output$op_bar_monthly_Amount  <-  renderPlot(
    ggplot(filtered_costs_arc()) +
      geom_bar(aes(
        x  =  YearMonth1, y  =  Amount, fill  =  SubCategory
      ), stat  =  'identity')
    + xlab(NULL) + ylab(NULL) + theme_bw() +
      theme(plot.margin = margin(5,5,5,5)) +
    scale_fill_manual(values  =  subcat_color(input$ip_Category)),
    height  =  280
  )
  
  # -----------------4th Row-----------------
  
  costs_arc_no_income  <- 
    costs_arc[Category !=  'Savings']
  
  # ForWho chart
  output$op_pie_ForWho  <-  renderPlot(
    ggplot(costs_arc_no_income) +
      geom_bar(aes(
        x  =  '', y  =  Amount, fill  =  ForWho
      ), stat  =  'identity') +
      coord_flip() + xlab(NULL) + ylab(NULL) +
      theme_bw() + theme(
        plot.margin  =  margin(5, 5, 5, 5),
        legend.position  =  "bottom",
        legend.text  =  element_text(size  =  10),
        legend.key.size  =  unit(10, 'pt'),
        legend.title  =  element_blank()
      ) +
      scale_fill_manual(values  =  colors)
      ,
    height  =  100
  )
  
  # How chart
  output$op_pie_How  <-  renderPlot(
    ggplot(costs_arc_no_income) +
      geom_bar(aes(
        x  =  '', y  =  Amount, fill  =  How
      ), stat  =  'identity') +
      coord_flip() + xlab(NULL) + ylab(NULL) +
      theme_bw() + theme(
        plot.margin  =  margin(5, 5, 5, 5),
        legend.position  =  "bottom",
        legend.text  =  element_text(size  =  10),
        legend.key.size  =  unit(10, 'pt'),
        legend.title  =  element_blank()
      ) +
      scale_fill_manual(values  =  colors)
      ,
    height  =  100
  )
  
  # Frequency chart
  output$op_pie_freq  <-  renderPlot(
    ggplot(costs_arc_no_income) +
      geom_bar(aes(
        x  =  '', y  =  Amount, fill  =  Frequency
      ), stat  =  'identity') +
      coord_flip() +
      xlab(NULL) +
      ylab(NULL) +
      theme_bw() +
      theme(
        plot.margin  =  margin(5, 5, 5, 5),
        legend.position  =  "bottom",
        legend.text  =  element_text(size  =  10),
        legend.key.size  =  unit(10, 'pt'),
        legend.title  =  element_blank()
      ) +
      scale_fill_manual(values  =  colors) +
      guides(col  =  guide_legend(nrow  =  5, byrow  =  T))
    ,
    height  =  100
  )
  
  
  #------Income Site-------------------
  # Income Site
  
  # -----------------0 Row-----------------
  #Data Site - History costs table
  output$op_Income  <-  renderDataTable(Income)
  
  # -----------------1st Row-----------------
  output$totalIncome  <- 
    renderText(format(Income[, sum(Amount)], big.mark  =  ' '))
  
  output$IncomeCompensation  <- 
    renderText(format(Income[Category  ==  'Compensation',
                                sum(Amount)],
                      big.mark  =  ' '))
  
  output$IncomeOther  <- 
    renderText(format(Income[Category !=  'Compensation',
                                sum(Amount)], big.mark  =  ' '))
  
  # -----------------2nd Row-----------------
  filtered_trend_inc  <-  reactive({
    Income[year(Date) >=  input$ip_ev_inc[1] &
                year(Date) <=  input$ip_ev_inc[2]]
  })
  
  # Trend chart
  plotType_inc  <-  function(type) {
    if (type  ==  'N') {
      trend  <-  ggplot(filtered_trend_inc()) +
        geom_line(aes(x  =  Date, y  =  Amount), stat  =  'identity', size  =  1.2) + xlab(NULL) + 
        ylab(NULL) +
        theme_bw() + theme(legend.position  =  "bottom",
                           plot.margin = margin(5,5,5,5)) +
        scale_fill_manual(values  =  colors)
    }
    else {
      trend  <-  ggplot(filtered_trend_inc()) +
        geom_bar(aes(x  =  YearMonth1, y  =  Amount, fill = Category),
                 stat  =  'identity',
                 size  =  1.2) + xlab(NULL) + ylab(NULL) +
        theme_bw() + theme(legend.position  =  "bottom",
                           plot.margin = margin(5,5,5,5)) +
        scale_fill_manual(values  =  c('Compensation' = colors[6],
                                     'Family' = colors[14],
                                     'Scholarship' = colors[10],
                                     'Bank' = colors[15],
                                     'Other' = colors[2]))
    }
    return(trend)
  }
  
  output$trend_Income  <-  renderPlot(plotType_inc(input$ip_inc_n_h),
                                          height  =  190)
  
  
  # -----------------3rd Row-----------------
  # monthly Income by subcategory reactive table
  filtered_inc  <-  reactive({
    Income[Category %in% input$ip_Category_inc  ==  T & 
                year(Date) >=  input$ip_ev_inc[1] &
        year(Date) <=  input$ip_ev_inc[2] ]
  })
  
  # Monthly costs by subcategory
  output$op_bar_monthly_Amount_Income  <-  renderPlot(
    ggplot(filtered_inc()) +
      geom_bar(aes(
        x  =  YearMonth1, y  =  Amount, fill  =  SubCategory
      ), stat  =  'identity') +
      xlab(NULL) + ylab(NULL) + theme_bw() +
      theme(plot.margin = margin(5,5,5,5)) +
      scale_fill_manual(values  = c(
        'Bank_Interest' = colors[15],
        'Bank_Lending' = colors[18],
        'Bank_Other' = colors[19],
        'Family_Parental Support' = colors[14],
        'Family_Other' = colors[8],
        'Family_Rent' = colors[6],
        'Other_Other' = colors[17],
        'Other_Lending' = colors[19],
        'Compensation_Employer1' = colors[6],
        'Compensation_Employer2' = colors[7],
        'Compensation_Cafeteria' = colors[8],
        'Compensation_Other' = colors[1],
        'Scholarship_University' = colors[10],
        'Scholarship_Other' = colors[13]
      )) 
    ,
    height  =  230
  )
})
