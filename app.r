library(shiny)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(shinyWidgets)
library(shinyMatrix)
library(magrittr)
library(grid)
library(gridExtra)


  theme_set(theme_bw())

  #dat_pth <- "C:/GitRep/CEVT-expl/Data/"
  
  #dir_pth <- "C:/GitRep/CEVT_TO/"

  #summary_pth <- "C:/Albacore_Catch_Modelling/Full_process/"
  
  #current_day <- yday(Sys.Date())
  current_day <- 200   # Hard code this for now as current date doesn't work well, needs thought
  current_year  <- 2024 #year(Sys.Date())
  
  # Set the years for the app to cover, including current year
  yr_rng <- 2016:current_year
  
  load("./Output/CEVT_Results_Saved.RData", verbose = TRUE)
  
  ctrl_hist <- read.csv(file = "./Output/CEVT_control_file.csv", header = TRUE)
  
  #ctrl_Latest <- read.csv(file = paste0(summary_pth, "Output/TO/CEVT_control_file_Latest.csv"), header = TRUE)
  

#____________________________________________________________________________________________________________
# User interface

ui <- navbarPage(
  title = "Tonga CEVT",
  tabPanel("In-season",
           
           tags$head(
             tags$style(HTML("hr {border-top: 1px solid #000000;}"))
           ),
           
           titlePanel(title = div(img(height = 177, width = 1181, src = "HeaderBar2.png")), windowTitle = "Tongan CEVT"),
           
           setBackgroundColor(
             color = c("white", "white"),
             gradient = "linear",
             direction = c("bottom","left")
           ),
           
           sidebarLayout(
             sidebarPanel(width = 2,
                          # selectInput("year", "Select the year of fishing:",
                          #             c("Current year" = "yr_cur", "Historical" = "yr_hist")),
                          # br(),
                          # conditionalPanel(
                          #   "input.year == 'yr_hist'",
                          #   sliderInput("slideryr", "Year slider", min = yr_rng[1], max = yr_rng[length(yr_rng) - 1], value = yr_rng[length(yr_rng) - 1],
                          #               width = "400px", ticks = TRUE, sep = "", step = 1)
                          # ),
                          sliderInput("slideryr", "Choose year",  min = min(yr_rng), max = max(yr_rng), value = max(yr_rng),
                                      width = "400px", ticks = TRUE, sep = "", step = 1),
                          sliderInput("sliderday", "Choose day of year",  min = 10, max = 360, value = current_day,
                                      width = "400px", ticks = TRUE, sep = "", step = 10),
                          br(),
                          checkboxInput("add_forest", "Add 2nd model", value = FALSE),
                          checkboxInput("add_TAC", "Add TAC to plot", value = TRUE),
                          numericInput("tac", "Total allowable catch (mt):", 1600, min = 0, max = 2000, step = 100, width = "200px")
             ),
             mainPanel(
               br(),
               plotOutput("Pred_plot"),
               br(),
               br(),
               br(),
               br(),
               br(),
               plotOutput("Vesbar"),
             )
           )
  ),
  tabPanel("Annual totals",
           mainPanel(
             br()
           )
  )
  
)


#____________________________________________________________________________________________________________
# The server


server <- function(input, output) {
  
  
  sum_dat <- reactive({

      fcl_yr <- input$slideryr
      
      fcl_day <- input$sliderday
      
      cnt_index <- which(ctrl_hist$x == fcl_yr & ctrl_hist$y == fcl_day)
      
      dat <- CEVT_output[[cnt_index]]

  })
  
  
  output$Pred_plot <- renderPlot({
    
    dat_pl <- sum_dat()
    
    focal_yr <- input$slideryr
    
    focal_day <- input$sliderday
    
    max_y <- 1.35*max(dat_pl$pl_ref$Cum_alb/1000) #max(1.35*max(pl_ref$Cum_alb/1000), dat_all_future$Cum_alb/1000*1.1)
    
    if(input$add_TAC) max_y <- max(1.35*max(dat_pl$pl_ref$Cum_alb/1000), input$tac)
    
    pl <- ggplot()
    
    if(input$slideryr != max(yr_rng)) pl <- pl + geom_line(data = dat_pl$pl_ref, aes(x = set_day, y = Cum_alb/1000), linewidth = 1.2)
    if(input$add_TAC) pl <- pl + geom_hline(yintercept = input$tac, colour = alpha("magenta3", .5), linetype = 2) 
    
    dat_pl$dat_all_future$cum_UL95 <- ifelse(dat_pl$dat_all_future$cum_UL95 > max_y*1000, max_y*1000, dat_pl$dat_all_future$cum_UL95)
    
    pl <- pl + geom_bar(data = dat_pl$pl_predict_glm, aes(x = set_day, y = Cum_alb/1000, fill = Type), stat = "identity", width = 1) +
                geom_vline(xintercept = focal_day, colour = alpha("dodgerblue", .5)) +
                geom_hline(yintercept = dat_pl$pl_current$Cum_alb/1000, colour = alpha("dodgerblue", .8), linetype = 2) +
                geom_ribbon(data = dat_pl$boot_all_sht, aes(x = set_day, ymin = cum_LL95/1000, ymax = cum_UL95/1000), fill = alpha("red", .15), colour = "grey30") + #, linetype = 2) +
                geom_line(data = dat_pl$pl_tot_glm, aes(x = set_day, y = Cum_alb/1000), linewidth = 1.2, colour = alpha("red", .9)) +
                geom_point(data = dat_pl$pl_current, aes(x = set_day, y = Cum_alb/1000), colour = alpha("red", .99), size = 5) +
                geom_hline(yintercept = last(dat_pl$dat_all_future$Cum_alb/1000), colour = alpha("green2", .85), linetype = 2) +
                geom_line(data = dat_pl$dat_all_future, aes(x = Day, y = Cum_alb/1000), linewidth = .9, colour = alpha("deeppink", .3)) +
                geom_ribbon(data = dat_pl$dat_all_future, aes(x = Day, ymin = cum_LL95/1000, ymax = cum_UL95/1000), fill = alpha("yellow", .15)) + #, linetype = 2) +
                geom_point(data = dat_pl$pl_current, aes(x = set_day, y = Cum_alb/1000), colour = alpha("red", .99), size = 5) +
                annotate("text", x = 0, y = 1.05*dat_pl$pl_current$Cum_alb/1000, label = paste(round(dat_pl$pl_current$Cum_alb/1000), "mt"), colour = "dodgerblue", fontface = 2, size = 5) +
                annotate("text", x = 0, y = 1.05*last(dat_pl$dat_all_future$Cum_alb/1000), label = paste(round(last(dat_pl$dat_all_future$Cum_alb/1000)), "mt"), colour = "green2", fontface = 2, size = 5) +
                annotate("text", x = 40, y = .88*max_y, label = paste(day(dat_pl$boot_all$day_date[focal_day]), month.abb[month(dat_pl$boot_all$day_date[focal_day])], focal_yr), size = 7, colour = "grey30", fontface = 2) +
                annotate("text", x = 40, y = .95*max_y, label = paste("Day", focal_day), size = 7, colour = "grey30") +
                xlab("") + ylab("Cumulative catch of albacore (mt)") +   # xlab("Day of year")
                scale_fill_manual(values = c(alpha("navy", .2), alpha("navy", .7))) + theme_minimal() + # Theme clean looks nicer but has border
                scale_x_continuous(breaks = seq(1, 365, 40),
                                   labels = paste(day(dat_pl$boot_all$day_date[seq(1, 365, 40)]), month.abb[month(dat_pl$boot_all$day_date[seq(1, 365, 40)])])) +
                scale_y_continuous(limits = c(NA, max_y), breaks = seq(0, max_y, 250)) +
                theme(axis.title = element_text(size = 16), axis.text.x = element_text(size = 14, angle = 45, vjust = .5, hjust = .5),
                      axis.text.y = element_text(size = 14), legend.position = "top", legend.text = element_text(size = 13),
                      legend.background = element_blank(), legend.title = element_blank())
    
    if(input$add_forest) pl <- pl + geom_line(data = dat_pl$pl_tot, aes(x = set_day, y = Cum_alb/1000), linewidth = 1, colour = alpha("green", .7))
    
    pl
    
  }, height = 500, width = 800)
  
  
  output$Vesbar <- renderPlot({

    dat_pl <- sum_dat()

    pl = ggplot() + geom_bar(data = dat_pl$pl_ves, aes(x = Vessel, y = alb_catch_ref/1000, fill = Type), stat = "identity", width = .8) +#, colour = "black") +
                scale_fill_manual(values = c(alpha("navy", .1), alpha("navy", .5))) +
                #geom_bar(data = dat_pl$cum_ref_ves, aes(x = Vessel, y = alb_catch_ref/1000), stat = "identity", width = .8, fill = alpha("white",.01), colour = alpha("black", .3)) +
                #coord_flip() + xlab("Vessel") + ylab("Cumulative catch of albacore (mt)") + theme_clean() +
                xlab("") + ylab("Cumulative catch of albacore (mt)") + theme_minimal() +
                theme(axis.title = element_text(size = 16), axis.text.y = element_text(size = 14),
                      axis.text.x = element_text(size = 14, angle = 45, vjust = .5, hjust = .5),
                      legend.position = "none",
                      legend.background = element_blank(), legend.title = element_blank())
    pl

  }, height = 400, width = 800)
  
  
  # windows(1500,3000)
  # ggplot(ves_tile_data, aes(x = vesselname, y = set_day, fill = Type)) + geom_tile() +
  #   scale_x_discrete(position = "top") + scale_y_reverse(limits = c(366,0)) +
  #   ylab("Day of year") +
  #   theme(axis.text = element_text(angle = 90, vjust = 1, hjust = 0),
  #         axis.title.x = element_blank(),
  #         legend.position = "bottom", legend.title = element_blank())
  
  
  
}

shinyApp(ui = ui, server = server)


