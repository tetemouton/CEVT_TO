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

  dat_pth <- "C:/GitRep/CEVT-expl/Data/"
  
  dir_pth <- "C:/GitRep/CEVT_TO/"

  summary_pth <- "C:/Albacore_Catch_Modelling/Full_process/"
  
  
  
  #latest.yr <- 2024
  
  current_day <- yday(Sys.Date())
  
  # Set the years for the app to cover, including current year
  yr_rng <- 2016:2024
  
  
  
  load(paste0(summary_pth, "Output/TO/CEVT_Results_Saved.RData"), verbose = TRUE)
  
  load(paste0(summary_pth, "Output/TO/CEVT_Results_Saved_Latest.RData"), verbose = TRUE)
  
  ctrl_hist <- read.csv(file = paste0(summary_pth, "Output/TO/CEVT_control_file.csv"), header = TRUE)
  
  ctrl_Latest <- read.csv(file = paste0(summary_pth, "Output/TO/CEVT_control_file_Latest.csv"), header = TRUE)
  
  
  focal_yr = 2019
  focal_day = 50
  
  current_index <- which(ctrl_hist$x == focal_yr & ctrl_hist$y == focal_day)
  
  
  CEVT_output[[1]][[13]]
  CEVT_output[[1]]
  
  
  
  max_y <- 1.35*max(pl_ref$Cum_alb/1000) #max(1.35*max(pl_ref$Cum_alb/1000), dat_all_future$Cum_alb/1000*1.1)
  
  #dat_all_future$cum_UL95 <- ifelse(dat_all_future$cum_UL95 > max_y*1000, max_y*1000, dat_all_future$cum_UL95)          
  
  windows(3000,2000)
  pl = ggplot() + geom_line(data = pl_ref, aes(x = set_day, y = Cum_alb/1000), linewidth = 1.2) +
    geom_bar(data = pl_predict_glm, aes(x = set_day, y = Cum_alb/1000, fill = Type), stat = "identity", width = 1) +
    geom_vline(xintercept = focal_day, colour = alpha("dodgerblue", .5)) +
    geom_hline(yintercept = pl_current$Cum_alb/1000, colour = alpha("dodgerblue", .5), linetype = 2) +
    geom_ribbon(data = boot_all_sht, aes(x = set_day, ymin = cum_LL95/1000, ymax = cum_UL95/1000), fill = alpha("red", .15), colour = "grey30") + #, linetype = 2) +
    geom_line(data = pl_tot, aes(x = set_day, y = Cum_alb/1000), linewidth = 1, colour = alpha("green", .7)) +
    geom_line(data = pl_tot_glm, aes(x = set_day, y = Cum_alb/1000), linewidth = 1.2, colour = alpha("red", .9)) +
    geom_point(data = pl_current, aes(x = set_day, y = Cum_alb/1000), colour = alpha("red", .99), size = 5) +
    geom_line(data = dat_all_future, aes(x = Day, y = Cum_alb/1000), linewidth = .9, colour = alpha("deeppink", .3)) +
    geom_ribbon(data = dat_all_future, aes(x = Day, ymin = cum_LL95/1000, ymax = cum_UL95/1000), fill = alpha("yellow", .15)) + #, linetype = 2) +
    geom_point(data = pl_current, aes(x = set_day, y = Cum_alb/1000), colour = alpha("red", .99), size = 5) +
    annotate("text", x = 0, y = 1.05*pl_current$Cum_alb/1000, label = paste(round(pl_current$Cum_alb/1000), "mt"), colour = "dodgerblue", fontface = 2, size = 5) +
    annotate("text", x = 40, y = .95*max_y, label = boot_all$day_date[focal_day], size = 7, colour = "grey30") +
    annotate("text", x = 40, y = .88*max_y, label = paste("Day", focal_day), size = 7, colour = "grey30") +
    xlab("Day of year") + ylab("Cumulative catch of albacore (mt)") +
    scale_fill_manual(values = c(alpha("navy", .2), alpha("navy", .7))) + theme_clean() +
    scale_x_continuous(breaks = seq(1, 365, 40), labels = boot_all$day_date[seq(1, 365, 40)]) +
    scale_y_continuous(limits = c(NA, max_y), breaks = seq(0, max_y, 250)) +
    theme(axis.title = element_text(size = 16), axis.text.x = element_text(size = 14, angle = 45, vjust = 0.5, hjust = .5),
          axis.text.y = element_text(size = 14), legend.position = "top",
          legend.background = element_blank(), legend.title = element_blank())
  print(pl)
  savePlot(filename = paste0(save_pth, "Plots/TO/CEVT2_", focal_yr, "_", focal_day, ".png"), type = "png")
  dev.off()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  # Load data files generated in the data_preparation_script.r located in same folder as this script
  # Logsheets of fishing activity
  save_ls <- read.csv(paste0(dat_pth, "logsheet_set.csv"))
  # VMS days data for fishing activity
  save_vms <- read.csv(paste0(dat_pth, "vms_set.csv"))
  
  #fished_kg <- read.csv(paste0(dat_pth, "fished_kg.csv"))
  
  # These are the tables of mean catch per day, by year and fleet, for using to predict cumulative catches in-season
  catch_values <- read.csv(paste0(dat_pth, "catch_values.csv"))
  # These are the tables of the scalar of logsheets to VMS days to adjust predictions of cumulative catches in-season
  set_comp <- read.csv(paste0(dat_pth, "set_scalars.csv"))
  # Trajectories of relative catch over the calendar year for previous years
  seas_dat <- read.csv(paste0(dat_pth, "historical_daily_increments.csv"))
  
  
#____________________________________________________________________________________________________________
# User interface

ui <- navbarPage(
  title = "CEVT",
  tabPanel("In-season",
           
           tags$head(
             tags$style(HTML("hr {border-top: 1px solid #000000;}"))
           ),
           
           titlePanel(title = div(img(height = 130, width = 1300, src = "HeaderBar1.png")), windowTitle = "At ease on the high seas"),
           
           setBackgroundColor(
             color = c("white", "white"),
             gradient = "linear",
             direction = c("bottom","left")
           ),
           
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("year", "Select the year of fishing:",
                                      c("Current year" = "yr_cur", "Historical" = "yr_hist")),
                          br(),
                          conditionalPanel(
                            "input.year == 'yr_hist'",
                            sliderInput("slideryr", "Year slider", min = yr_rng[1], max = yr_rng[length(yr_rng) - 1], value = yr_rng[length(yr_rng) - 1],
                                        width = "400px", ticks = TRUE, sep = "", step = 1)
                          ),
                          selectInput("datatypes", "Which data to use:",
                                      c("Just VMS" = "vms_pred", "VMS and LS" = "ls_pred", "VMS and LS integrated" = "ls_integ")),
                          br(),
                          # checkboxGroupInput("prediction", "Type of prediction:",
                          #                    c("Just VMS" = "vms_pred", "VMS and scalar" = "ls_pred"),
                          #                    selected = c("Just VMS","VMS and LS")),
                          checkboxInput("prediction", "Add VMS scalar", value = TRUE),
                          br(),
                          sliderInput("sliderday", "Choose day of year",  min = 1, max = 366, value = 366,
                                      width = "400px", ticks = TRUE, sep = ""),
                          br(),
                          numericInput("tac", "Total allowable catch (mt):", 1600, min = 0, max = 2000, step = 100, width = "200px")
             ),
             mainPanel(
               br(),
               br(),
               plotOutput("Catbar"),
               plotOutput("Vesbar"),
               #tableOutput("Cattbil")
               #plotlyOutput("PlotlyPlot")
             )
           )
  ),
  tabPanel("Previous years",
           mainPanel(
             br()
           )
  ),
  tabPanel("Catch converter",
           mainPanel(
             br()
           )
  )
  
)


#____________________________________________________________________________________________________________
# The server


server <- function(input, output) {
  
  
  ls_dat <- reactive({
    
    if(input$year == "yr_cur"){
      
      dat_ls <- save_ls %>% filter(year == yr_rng[length(yr_rng)])
      
    } else{
        
      dat_ls <- save_ls %>% filter(year == input$slideryr)
        
    }

  })
  
  
  vms_dat <- reactive({
    
    if(input$year == "yr_cur"){
      
      dat_vms <- save_vms %>% filter(year == yr_rng[length(yr_rng)])
      
    } else{
      
      dat_vms <- save_vms %>% filter(year == input$slideryr)
      
    }
    
  })
  
  
  increment_dat <- reactive({
    
    if(input$year == "yr_cur"){
      
      dat_increment <- seas_dat %>% filter(year != yr_rng[length(yr_rng)])
      
    } else{
      
      dat_increment <- seas_dat %>% filter(year != input$slideryr)
      #dat_increment <- seas_dat %>% filter(year != 2022)
      
    }
    
  })
  
  
  ent_dat <- reactive({
    
    ls_yr <- ls_dat()
    
  })
  
  
  comb_dat <- reactive({
  
    ls_yr <- ls_dat()
    #ls_yr <- save_ls %>% filter(year == 2022) # To have a look outside of app
    
    vms_yr <- vms_dat()
    #vms_yr <- save_vms %>% filter(year == 2022) # To have a look outside of app
    
    ent_yr <- ent_dat()
    #ent_yr <- save_ls %>% filter(year == 2022) # To have a look outside of app
    
    vms_yr %<>% mutate(flag_cat = ifelse(flag_id == "TO", "TO", "OT"))
    
    par_match <- match(paste(vms_yr$year - 1, vms_yr$flag_cat), paste(catch_values$year, catch_values$flag_cat))
    scalar_match <- match(vms_yr$year - 1, set_comp$year)
    
    vms_yr$pred_alb <- catch_values$alb_mt_est[par_match]
    vms_yr$pred_alb_cor <- vms_yr$pred_alb*set_comp$set_scalar[scalar_match]
    vms_yr$nset_cor <- set_comp$set_scalar[scalar_match]
    
    ls_cat <- ls_yr %>% group_by(set_day) %>% summarise(ls_sets = n(), Cat_alb_ls = sum(alb_kg_est)/1000)
    vms_cat <- vms_yr %>% group_by(set_day) %>% summarise(vms_sets = sum(nset_cor), Cat_alb_vms = sum(pred_alb_cor)) # If didn't want vms correction scalar - vms_sets = n(),
    ent_cat <- ent_yr %>% group_by(ent_day) %>% summarise(ent_sets = n(), Cat_alb_ent = sum(alb_kg_est)/1000) %>% rename(set_day = ent_day)
    
    full_dat <- data.frame(set_day = 1:366)
    
    full_dat <- left_join(full_dat, ls_cat, by = "set_day")
    full_dat <- left_join(full_dat, vms_cat, by = "set_day")
    full_dat <- left_join(full_dat, ent_cat, by = "set_day")
    full_dat[is.na(full_dat)] <- 0
    
    full_dat %<>% arrange(set_day) %>%
             mutate(Cum_sets_ls = cumsum(ls_sets), Cum_sets_vms = cumsum(vms_sets), Cum_sets_ent = cumsum(ent_sets),
                    Cum_ls = cumsum(Cat_alb_ls), Cum_vms = cumsum(Cat_alb_vms), Cum_ent = cumsum(Cat_alb_ent))
    
    full_dat %<>% mutate(Cum_sets_unent = Cum_sets_vms - Cum_sets_ent,
                         Cum_vms_unent = Cum_sets_unent*(Cum_vms/Cum_sets_vms),
                         Cum_tot = Cum_vms_unent + Cum_ent,
                         ent_ratio = ifelse(Cum_sets_ent > 10, (Cum_vms/Cum_sets_vms)/(Cum_ent/Cum_sets_ent), 1),
                         Cum_complete = Cum_ent + Cum_vms_unent/ent_ratio)
    
  })
  
  
  output$Catbar <- renderPlot({
    
    dat_pl <- comb_dat()
    #dat_pl <- full_dat
    
    
    # selectInput("datatypes", "Which data to use:",
    #             c("Just VMS" = "vms_pred", "VMS and LS" = "ls_pred", "VMS and LS integrated" = "ls_integ"))
    
    if(input$datatypes == "vms_pred") dat_pl$resp <- dat_pl$Cum_vms
    
    if(input$datatypes == "ls_pred") dat_pl$resp <- dat_pl$Cum_tot
    
    if(input$datatypes == "ls_integ") dat_pl$resp <- dat_pl$Cum_complete
    
    dat_pl_cut <- dat_pl %>% filter(set_day <= input$sliderday)
    
    if(input$year == "yr_cur"){
      
      dat_pl %<>% filter(set_day <= current_day)      
      
      dat_pl_cut %<>% filter(set_day <= current_day)
      
    }
    
    incr_pl <- increment_dat()
    #incr_pl <- dat_increment
    
    if(input$year == "yr_cur"){
    
      cut_day <- min(input$sliderday, current_day)
    
    } else{
      
      cut_day <- input$sliderday
                     
    }
    
    
    incr_pl %<>% filter(set_day >= cut_day)

    pred_tracks <- incr_pl

    pred_tracks$incrmt <- ifelse(pred_tracks$set_day == cut_day, 1, pred_tracks$incrmt)
    pred_tracks$pred_cat <- NA
    
    for(i in 1:length(pred_tracks$set_day)){
      
      pred_tracks$pred_cat[i] <- ifelse(pred_tracks$set_day[i] == cut_day,
                                        dat_pl_cut$resp[dat_pl_cut$set_day == cut_day],
                                        pred_tracks$incrmt[i]*pred_tracks$pred_cat[i-1])
      
    }
    
    mean_track <- pred_tracks %>% group_by(set_day) %>% summarise(med_cat = median(pred_cat))
    
    
    pl <- ggplot() + xlab("") + ylab("Catch (1,000's mt)") + geom_line(data = pred_tracks, aes(x = set_day, y = pred_cat, colour = factor(year))) +
                 geom_bar(data = dat_pl_cut, aes(x = set_day, y = resp), fill = "steelblue", alpha = 0.6,  stat = "identity", width = 1) +
                 geom_bar(data = mean_track, aes(x = set_day, y = med_cat), fill = "steelblue", alpha = 0.2,  stat = "identity", width = 1) +
                 geom_line(data = dat_pl, aes(x = set_day, y = Cum_ls), alpha = 0.8,  stat = "identity", linewidth = 1.2) +
                 scale_colour_manual(values = rep("grey", 10)) +
                 geom_hline(yintercept = input$tac, colour = alpha("red", .3), linetype = "longdash", linewidth = .8) +
                 scale_x_continuous(limits = c(0,366)) + scale_y_continuous(limits = c(0, 2500)) + theme_minimal() +
                 theme(panel.border = element_blank(), axis.text = element_text(size = 14), axis.title = element_text(size = 14),
                       legend.position = "none")
    pl
    
  }, height = 350, width = 1000)
  
  
  output$Vesbar <- renderPlot({

    dat_pl <- ls_dat()
    
    dat_pl_cut <- dat_pl %>% filter(set_day <= input$sliderday)

    dat_ves <- dat_pl_cut %>% group_by(vesselname, flag_id) %>% summarise(Catch = sum(alb_kg_est)/1000) %>% arrange(desc(Catch))
    dat_ves$vesselname <- factor(dat_ves$vesselname, levels = unique(dat_ves$vesselname))
    #dat_ves$flag_id <- as.factor(as.numeric(dat_ves$flag_id))

    # %>% filter(between(yy, input$sliderrng[1], input$sliderrng[2]))

    pl <- ggplot() + xlab("") + ylab("Catch (1,000's mt)") +
                 geom_bar(data = dat_ves, aes(x = vesselname, y = Catch, fill = flag_id), alpha = 0.6,  stat = "identity", width = .6, colour = "black", linewidth = .3) +
                 #scale_x_continuous(limits = c(0,366)) + scale_y_continuous(limits = c(0, 1500)) +
                 scale_fill_manual(values = alpha(c("seagreen","darkorchid","dodgerblue","firebrick"), .5)) + theme_minimal() +
                 theme(panel.border = element_blank(), axis.text = element_text(size = 14), axis.title = element_text(size = 14),
                       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.title = element_blank(),
                       legend.position = "top")
                       #legend.position = "none")
    pl

  }, height = 400, width = 800)
  
  
}

shinyApp(ui = ui, server = server)













































