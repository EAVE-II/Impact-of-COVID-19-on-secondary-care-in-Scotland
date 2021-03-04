######################################################################
## Title: Impact of COVID-19 on accident and emergency attendances
##          and emergency and planned hospital admissions in
##          Scotland: an interrupted time-series analysis
## Short title: Impact of COVID-19 on secondary care in Scotland
## DOI: 10.1177/0141076820962447
## Code author: Rachel H. Mulholland <Rachel.Mulholland@ed.ac.uk>
## Description: Functions for analysis
######################################################################


#### Baseline model function ####
# Baseline model = the number of days*change-point
# Function to extract ANOVA results [1], estimates (intercept and slope) [2], fitted lines [3] and model diagnostics [4] (Plots)

baseline_model_fn <- function(outcome){
  
  # Subset to data outcome
  scotland_data_subset <- subset(scotland_data, Outcome==outcome)
  
  # Fit the model
  baseline_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown, data=scotland_data_subset)
  
  # [1] - ANOVA results 
  baseline_model_ANOVA <- anova(baseline_model)
  
  # [2] - Estimates
  # Create skeleton dataset to put in estimates
  baseline_model_estimates <- data.frame(matrix(ncol=6, nrow=3*2))
  colnames(baseline_model_estimates) <- c("Outcome","BA_Pandemic_Lockdown", "Coeff_type", "est", "lwr", "upr")
  baseline_model_estimates$Outcome <- rep(outcome, times=6)
  baseline_model_estimates$BA_Pandemic_Lockdown <- rep(c("Before", "Between", "After"), times=2)
  baseline_model_estimates$Coeff_type <- rep(c("Intercept", "Slope"), each=3)
  
  # Define the three time-periods
  time_periods <- c("Before", "Between", "After")
  
  # Fit model each time for each time-period to get estimates for slopes and intercepts
  for(i in 1:3){
    # Redefine baseline level for each loop (once for each time period)
    scotland_data_subset <- within(scotland_data_subset, BA_Pandemic_Lockdown <- relevel(BA_Pandemic_Lockdown, ref= time_periods[i]))
    
    # Refit baseline model
    baseline_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown, data=scotland_data_subset)
    
    # Capture estimates and their 95% CI
    baseline_coefs <- baseline_model$coefficients
    baseline_coefs_cis <- confint(baseline_model)
    
    # Populate the estimate table
    baseline_model_estimates$est[which(baseline_model_estimates$BA_Pandemic_Lockdown==time_periods[i])] <- round(baseline_coefs[1:2],3)
    
    baseline_model_estimates$lwr[which(baseline_model_estimates$BA_Pandemic_Lockdown==time_periods[i])] <- round(baseline_coefs_cis[1:2,1],3)
    
    baseline_model_estimates$upr[which(baseline_model_estimates$BA_Pandemic_Lockdown==time_periods[i])] <- round(baseline_coefs_cis[1:2,2],3)
    
    
    
  }
  
  
  # [3] - Fitted lines and 95% CI
  # To create segmented fitted lines we must fit the model to all days in time-periods and stop at each of the change-points
  
  # Create a vector for each segmented time-period by day 
  change_pts <- as.Date(c("2020-01-05", "2020-03-11", "2020-03-11","2020-03-23","2020-03-23","2020-06-28"))
  # Before - Jan 05 to Mar 11
  before_days <- as.Date(change_pts[1]:change_pts[2], origin = "1970-01-01")
  # Between - Mar 11 to Mar 23
  between_days <- as.Date(change_pts[3]:change_pts[4], origin = "1970-01-01")
  # After - Mar 23 to 28 Jun
  after_days <- as.Date(change_pts[5]:change_pts[6], origin = "1970-01-01")
  
  # Calculate the number of possible time points 
  n <- length(before_days)+length(between_days)+length(after_days)
  
  # Next create a dataset with all time points in time-periods
  baseline_model_prediction <- data.frame(matrix(ncol=3, nrow=n))
  colnames(baseline_model_prediction) <- c("BA_Pandemic_Lockdown", "No_days", "Outcome")
  
  baseline_model_prediction$Outcome <- rep(outcome, times=n)
  baseline_model_prediction$Date <- c(before_days, between_days, after_days)
  baseline_model_prediction$No_days <- as.numeric(c(c(before_days, between_days, after_days)-as.Date("2020-01-05")))
  baseline_model_prediction$BA_Pandemic_Lockdown <- c(rep("Before", times=length(before_days)),
                                                      rep("Between", times=length(between_days)),
                                                      rep("After", times=length(after_days)))
  baseline_model_prediction$Time_period_no <- c(rep(1, times=length(before_days)),
                                                rep(2, times=length(between_days)),
                                                rep(3, times=length(after_days)))
  
  # Link variation raw datato prediction dataset
  scotland_data_subset <- scotland_data_subset %>%
    mutate(Time_period_no=recode(BA_Pandemic_Lockdown, "Before"=1, "Between"=2, "After"=3))
  
  
  scotland_data_subset$Date_BA_Outcome <- paste(scotland_data_subset$Week_ending, scotland_data_subset$Time_period_no, scotland_data_subset$Outcome)
  baseline_model_prediction$Date_BA_Outcome <- paste(baseline_model_prediction$Date, baseline_model_prediction$Time_period_no, baseline_model_prediction$Outcome)
  
  baseline_model_prediction <- merge(x=baseline_model_prediction, y=scotland_data_subset[, c("Date_BA_Outcome", "Variation")], by="Date_BA_Outcome", all.x=T)
  
  
  # Use the model to predict the outcome with 95% CI
  predictions <- predict(baseline_model, newdata = baseline_model_prediction, interval = "confidence")
  baseline_model_prediction$Predict <- predictions[,1]
  baseline_model_prediction$Lwr <- predictions[,2]
  baseline_model_prediction$Upr <- predictions[,3]
  
  
  # [4] - Model diagnostics
  # Residuals
  baseline_residuals <- baseline_model$residuals
  names(baseline_residuals) <- scotland_data_subset$BA_Pandemic_Lockdown
  
  #Fitted values
  baseline_fitted_values <- baseline_model$fitted.values
  names(baseline_fitted_values) <- scotland_data_subset$BA_Pandemic_Lockdown
  
  par(mfrow=c(1,3))
  # Histograms
  hist(baseline_residuals, breaks=50, main=" ", xlab="Residuals")
  
  # QQ Plot
  qqnorm(baseline_residuals, main=" ")
  qqline(baseline_residuals)
  
  # Residuals vs fitted
  plot(baseline_fitted_values, baseline_residuals, xlab="Fitted values", ylab="Residuals")
  abline(h=0,lty=2)
  
  
  par(mfrow=c(1,2))
  # ACF
  acf(baseline_residuals, main=" ")
  # PACF
  pacf(baseline_residuals, main=" ")
  
  # Returning tables for function
  return(list(baseline_model_ANOVA, baseline_model_estimates, baseline_model_prediction))
}



#### Demographic variation plots ####
# Plots variations between 2020 and 2018-2019 average by demographic, split by each outcome

demographic_variation_plot_fn <- function(demographic){
  
  if(demographic=="Sex"){
    demographic_data <- scotland_data_sex
  } else {if(demographic=="Age"){
    demographic_data <- scotland_data_age
  } else {if(demographic=="SIMD"){
    demographic_data <- scotland_data_simd
  }}}
  
  
  # A&E 
  demographic_data_ae <- subset(demographic_data, Outcome=="A&E Attendances")
  
  p_ae <- ggplot(demographic_data_ae, aes(x=Week_ending, y=Variation, color=Category, linetype=Category))+
    geom_line(size=1)+
    theme_classic()+
    labs(x = "Week ending (2020)", y=" ",
         title = "A)")+
    geom_hline(yintercept = 0, linetype=2)+
    annotate("text", x=as.Date("2020-06-28"), y=4, label="2018-2019 average", hjust=1, size=3)+
    geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-03-27"), y=-17, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0)+
    geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1)+
    scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
    scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left")
  
  # Emerg
  demographic_data_emerg <- subset(demographic_data, Outcome=="Emergency Hospital Admissions")
  
  p_emerg <- ggplot(demographic_data_emerg, aes(x=Week_ending, y=Variation, color=Category, linetype=Category))+
    geom_line(size=1)+
    theme_classic()+
    labs(x = "Week ending (2020)", y ="% change between 2020 and 2018-2019 average",
         title = "B)")+
    geom_hline(yintercept = 0, linetype=2)+   
    # annotate("text", x=as.Date("2020-06-28"), y=2, label="2018-2019 average", hjust=1, size=3)+
    geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
    geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
    scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
    scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left")
  
  # Planned
  demographic_data_planned <- subset(demographic_data, Outcome=="Planned Hospital Admissions")
  
  p_planned <- ggplot(demographic_data_planned, aes(x=Week_ending, y=Variation, color=Category, linetype=Category))+
    geom_line(size=1)+
    theme_classic()+
    labs(x = "Week ending (2020)", y=" ",
         title = "C)")+
    geom_hline(yintercept = 0, linetype=2)+   
    #annotate("text", x=as.Date("2020-06-28"), y=2, label="2018-2019 average", hjust=1, size=3)+
    geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
    geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
    scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
    scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left")
  
  # Plot together
  
  g_ae <- ggplotGrob(p_ae)
  g_emerg <- ggplotGrob(p_emerg)
  g_planned <- ggplotGrob(p_planned)
  
  plot(rbind(g_ae,g_emerg,g_planned))
  
  
  
}

#### Demographic count plots ####
# Plots 2020 counts by demographic, split by each outcome

demographic_counts_plot_fn <- function(demographic){
  
  if(demographic=="Sex"){
    demographic_data <- scotland_data_sex
  } else {if(demographic=="Age"){
    demographic_data <- scotland_data_age
  } else {if(demographic=="SIMD"){
    demographic_data <- scotland_data_simd
  }}}
  
  
  # A&E 
  demographic_data_ae <- subset(demographic_data, Outcome=="A&E Attendances")
  
  p_ae <- ggplot(demographic_data_ae, aes(x=Week_ending, y=Count, color=Category, linetype=Category))+
    geom_line(size=1)+
    theme_classic()+
    labs(x = "Week ending (2020)", y=" ",
         title = "A)")+
    geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-03-27"), y=11000, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0)+
    geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-03-07"), y=11000, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1)+
    scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
    scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left")
  
  # Emerg
  demographic_data_emerg <- subset(demographic_data, Outcome=="Emergency Hospital Admissions")
  
  p_emerg <- ggplot(demographic_data_emerg, aes(x=Week_ending, y=Count, color=Category, linetype=Category))+
    geom_line(size=1)+
    theme_classic()+
    labs(x = "Week ending (2020)", y ="Count",
         title = "B)")+
    geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
    geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
    scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
    scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left")
  
  # Planned
  demographic_data_planned <- subset(demographic_data, Outcome=="Planned Hospital Admissions")
  
  p_planned <- ggplot(demographic_data_planned, aes(x=Week_ending, y=Count, color=Category, linetype=Category))+
    geom_line(size=1)+
    theme_classic()+
    labs(x = "Week ending (2020)", y=" ",
         title = "C)")+
    geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
    geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
    scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
    scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left")
  
  # Plot together
  
  g_ae <- ggplotGrob(p_ae)
  g_emerg <- ggplotGrob(p_emerg)
  g_planned <- ggplotGrob(p_planned)
  
  plot(rbind(g_ae,g_emerg,g_planned))
  
  
  
}


##### Demographic model function ####
# Fits all models for demographics by outcome

demographic_models_fn <- function(demographic, outcome){
  
  if(demographic=="Sex"){
    demographic_data <- scotland_data_sex
  } else {if(demographic=="Age"){
    demographic_data <- scotland_data_age
  } else {if(demographic=="SIMD"){
    demographic_data <- scotland_data_simd
  }}}
  
  demographic_data_outcome <- subset(demographic_data, Outcome==outcome)
  
  # Results of three-way interaction
  three_way_interaction_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown*Category, data=demographic_data_outcome)
  
  three_way_interaction_model_anova <- anova(three_way_interaction_model)
  
  # Alternative models
  alternative_model_0 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown, data=demographic_data_outcome)
  alternative_model_1 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category, data=demographic_data_outcome)
  alternative_model_2 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category*No_days, data=demographic_data_outcome)
  alternative_model_3 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category*BA_Pandemic_Lockdown, data=demographic_data_outcome)
  alternative_model_4 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category*No_days+Category*BA_Pandemic_Lockdown, data=demographic_data_outcome)
  
  alternative_models_aic <- AIC(alternative_model_0,alternative_model_1, alternative_model_2, alternative_model_3, alternative_model_4)
  alternative_models_bic <- BIC(alternative_model_0,alternative_model_1, alternative_model_2, alternative_model_3, alternative_model_4)
  
  return(list(three_way_interaction_model_anova, alternative_models_aic, alternative_models_bic))
}



###### Demographic alternative model function ####
# Extract key statistics from chosen demographic model by outcome

demographic_alternative_model_fn <- function(demographic, outcome, model){
  
  # Getting data
  if(demographic=="Sex"){
    demographic_data <- scotland_data_sex
    categories <- sex
  } else {if(demographic=="Age"){
    demographic_data <- scotland_data_age
    categories <- age
  } else {if(demographic=="SIMD"){
    demographic_data <- scotland_data_simd
    categories <- simd
  }}}
  
  # Subset
  demographic_data_outcome <- subset(demographic_data, Outcome==outcome)
  
  # [1] ANOVA
  if(model==1){
    alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category, data=demographic_data_outcome)
  } else {
    if(model==2){
      alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category*No_days, data=demographic_data_outcome)
    } else {
      if(model==3){
        alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category*BA_Pandemic_Lockdown, data=demographic_data_outcome)
      } else {
        if(model==4){
          alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category*No_days+Category*BA_Pandemic_Lockdown, data=demographic_data_outcome)
          
        }
      }
    }
  }
  
  alternative_model_anova <- anova(alternative_model)
  
  
  # [2] Estimates
  # Find number of categories
  n <- length(categories)
  
  # Create skeleton dataset
  alternative_model_estimates <- data.frame(matrix(ncol=7, nrow=3*2*n))
  colnames(alternative_model_estimates) <- c("Outcome","Category","BA_Pandemic_Lockdown", "Coeff_type", "est", "lwr", "upr")
  alternative_model_estimates$Outcome <- rep(outcome, times=6*n)
  alternative_model_estimates$Category <- rep(categories, each=6)
  alternative_model_estimates$BA_Pandemic_Lockdown <- rep(rep(c("Before", "Between", "After"), each=2), times=n)
  alternative_model_estimates$Coeff_type <- rep(c("Intercept", "Slope"), times=3*n)
  
  time_periods <- c("Before", "Between", "After")
  # Carry out with different baseline change-points
  for(i in 1:3){
    demographic_data_outcome <- within(demographic_data_outcome, BA_Pandemic_Lockdown <- relevel(BA_Pandemic_Lockdown, ref= time_periods[i]))
    
    for(j in 1:n){
      
      demographic_data_outcome <- within(demographic_data_outcome, Category <- relevel(Category, ref= categories[j]))
      
      
      # Chosen model
      if(model==1){
        alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category, data=demographic_data_outcome)
      } else {
        if(model==2){
          alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category*No_days, data=demographic_data_outcome)
        } else {
          if(model==3){
            alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category*BA_Pandemic_Lockdown, data=demographic_data_outcome)
          } else {
            if(model==4){
              alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Category*No_days+Category*BA_Pandemic_Lockdown, data=demographic_data_outcome)
              
            }
          }
        }
      }
      
      
      
      
      
      alternative_coefs <- alternative_model$coefficients
      alternative_coefs_cis <- confint(alternative_model)
      
      alternative_model_estimates$est[which(alternative_model_estimates$BA_Pandemic_Lockdown==time_periods[i]&
                                              alternative_model_estimates$Category==categories[j])] <- round(alternative_coefs[1:2],3)
      
      alternative_model_estimates$lwr[which(alternative_model_estimates$BA_Pandemic_Lockdown==time_periods[i]&
                                              alternative_model_estimates$Category==categories[j])] <- round(alternative_coefs_cis[1:2,1],3)
      
      alternative_model_estimates$upr[which(alternative_model_estimates$BA_Pandemic_Lockdown==time_periods[i]&
                                              alternative_model_estimates$Category==categories[j])] <- round(alternative_coefs_cis[1:2,2],3)
      
      
      
      
      
    }
    
    
    
  }
  
  # [3] Fitted lines and 95% CI
  
  # To create segmented fitted lines we must fit the model to all days in time-periods and stop at each of the change-points
  
  # Create a vector for each segmented time-period by day 
  change_pts <- as.Date(c("2020-01-05", "2020-03-11", "2020-03-11","2020-03-23","2020-03-23","2020-06-28"))
  # Before - Jan 05 to Mar 11
  before_days <- as.Date(change_pts[1]:change_pts[2], origin = "1970-01-01")
  # Between - Mar 11 to Mar 23
  between_days <- as.Date(change_pts[3]:change_pts[4], origin = "1970-01-01")
  # After - Mar 23 to 28 Jun
  after_days <- as.Date(change_pts[5]:change_pts[6], origin = "1970-01-01")
  
  # Vector of all days and timeperiods
  all_dates <- c(before_days, between_days, after_days)
  all_no_days <- as.numeric(c(c(before_days, between_days, after_days)-as.Date("2020-01-05")))
  all_time_periods <- c(rep("Before", times=length(before_days)),
                        rep("Between", times=length(between_days)),
                        rep("After", times=length(after_days)))
  all_time_periods_no <- c(rep(1, times=length(before_days)),
                           rep(2, times=length(between_days)),
                           rep(3, times=length(after_days)))
  
  # Calculate the number of possible time points 
  m <- length(before_days)+length(between_days)+length(after_days)
  
  # Next create a dataset with all time points in time-periods
  alternative_model_prediction <- data.frame(matrix(ncol=4, nrow=m*n))
  colnames(alternative_model_prediction) <- c("BA_Pandemic_Lockdown", "No_days", "Outcome", "Category")
  
  alternative_model_prediction$Outcome <- rep(outcome, times=m*n)
  alternative_model_prediction$Dates <- rep(all_dates, times=n)
  alternative_model_prediction$No_days <- rep(all_no_days, times=n)
  alternative_model_prediction$BA_Pandemic_Lockdown <- rep(all_time_periods, times=n)
  alternative_model_prediction$Time_period_no <- rep(all_time_periods_no, times=n)
  alternative_model_prediction$Category <- rep(categories, each=m)
  
  alternative_model_prediction <- transform(alternative_model_prediction, Category = factor(Category, levels= categories))
  
  # Link variation raw datato prediction dataset
  demographic_data_outcome <- demographic_data_outcome %>%
    mutate(Time_period_no=recode(BA_Pandemic_Lockdown, "Before"=1, "Between"=2, "After"=3))
  
  demographic_data_outcome$Date_BA_Outcome_Category <- paste(demographic_data_outcome$Week_ending, demographic_data_outcome$Time_period_no, 
                                                             demographic_data_outcome$Outcome, demographic_data_outcome$Category)
  alternative_model_prediction$Date_BA_Outcome_Category <- paste(alternative_model_prediction$Date, alternative_model_prediction$Time_period_no, 
                                                                 alternative_model_prediction$Outcome, alternative_model_prediction$Category)
  
  alternative_model_prediction <- merge(x=alternative_model_prediction, y=demographic_data_outcome[, c("Date_BA_Outcome_Category", "Variation")], by="Date_BA_Outcome_Category", all.x=T)
  
  
  # Use the model to predict the outcome with 95% CI
  predictions <- predict(alternative_model, newdata = alternative_model_prediction, interval = "confidence")
  alternative_model_prediction$Predict <- predictions[,1]
  alternative_model_prediction$Lwr <- predictions[,2]
  alternative_model_prediction$Upr <- predictions[,3]
  
  
  
  
  # [4] Model diagnostics
  # Residuals
  alternative_residuals <- alternative_model$residuals
  names(alternative_residuals) <- demographic_data_outcome$Category
  
  #Fitted values
  alternative_fitted_values <- alternative_model$fitted.values
  names(alternative_fitted_values) <- demographic_data_outcome$BA_Pandemic_Lockdown
  
  par(mfrow=c(1,3))
  # Histograms
  hist(alternative_residuals, breaks=50, main=" ", xlab="Residuals")
  
  # QQ Plot
  qqnorm(alternative_residuals, main=" ")
  qqline(alternative_residuals)
  
  # Residuals vs fitted
  plot(alternative_fitted_values, alternative_residuals, xlab="Fitted values", ylab="Residuals")
  abline(h=0,lty=2)
  
  # ACF
  if(demographic=="Sex"){
    par(mfrow=c(1,2))
  } else {if(demographic=="Age"){
    par(mfrow=c(2,4))
  } else {if(demographic=="SIMD"){
    par(mfrow=c(1,5))
  }}}
  
  for(k in 1:length(categories)){
    alternative_residuals_category <- alternative_residuals[which(names(alternative_residuals)==categories[k])]
    
    # ACF
    acf(alternative_residuals_category, main=categories[k])
  }
  
  
  # PACF
  if(demographic=="Sex"){
    par(mfrow=c(1,2))
  } else {if(demographic=="Age"){
    par(mfrow=c(2,4))
  } else {if(demographic=="SIMD"){
    par(mfrow=c(1,5))
  }}}
  
  for(k in 1:length(categories)){
    alternative_residuals_category <- alternative_residuals[which(names(alternative_residuals)==categories[k])]
    
    # PACF
    pacf(alternative_residuals_category, main=categories[k])
  }
  
  
  
  # Output
  return(list(alternative_model_anova, alternative_model_estimates, alternative_model_prediction))
  
  
  
}




###### Speciality models ####
# Fit all models for specialities by outcome

speciality_models_fn <- function(outcome){
  
  specialty_data_outcome <- subset(scotland_data_specialty, Outcome==outcome)
  
  # Results of three-way interaction
  three_way_interaction_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown*Specialty, data=specialty_data_outcome)
  
  three_way_interaction_model_anova <- anova(three_way_interaction_model)
  
  # Alternative models
  alternative_model_0 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown, data=specialty_data_outcome)
  alternative_model_1 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty, data=specialty_data_outcome)
  alternative_model_2 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty*No_days, data=specialty_data_outcome)
  alternative_model_3 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty*BA_Pandemic_Lockdown, data=specialty_data_outcome)
  alternative_model_4 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty*No_days+Specialty*BA_Pandemic_Lockdown, data=specialty_data_outcome)
  
  alternative_models_aic <- AIC(alternative_model_0,alternative_model_1, alternative_model_2, alternative_model_3, alternative_model_4)
  alternative_models_bic <- BIC(alternative_model_0,alternative_model_1, alternative_model_2, alternative_model_3, alternative_model_4)
  
  return(list(three_way_interaction_model_anova, alternative_models_aic, alternative_models_bic))
}


##### Speciality alternative model function ####
# Extracts key statistics from chosen speciality model by each outcome
# Function to extract ANOVA results [1], estimates (intercept and slope) [2], fitted lines [3] and model diagnostics [4] (Plots)

specialty_alternative_model_fn <- function(outcome, model){
  
  
  # Subset
  specialty_data_outcome <- subset(scotland_data_specialty, Outcome==outcome)
  
  # [1] ANOVA
  if(model==1){
    alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty, data=specialty_data_outcome)
  } else {
    if(model==2){
      alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty*No_days, data=specialty_data_outcome)
    } else {
      if(model==3){
        alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty*BA_Pandemic_Lockdown, data=specialty_data_outcome)
      } else {
        if(model==4){
          alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty*No_days+Specialty*BA_Pandemic_Lockdown, data=specialty_data_outcome)
          
        }
      }
    }
  }
  
  alternative_model_anova <- anova(alternative_model)
  
  
  # [2] Estimates
  # Find number of categories
  categories <- as.character(unique(specialty_data_outcome$Specialty))
  n <- length(categories)
  
  # Create skeleton dataset
  
  alternative_model_estimates <- data.frame(matrix(ncol=7, nrow=3*2*n))
  colnames(alternative_model_estimates) <- c("Outcome","Specialty","BA_Pandemic_Lockdown", "Coeff_type", "est", "lwr", "upr")
  alternative_model_estimates$Outcome <- rep(outcome, times=6*n)
  alternative_model_estimates$Specialty <- rep(categories, each=6)
  alternative_model_estimates$BA_Pandemic_Lockdown <- rep(rep(c("Before", "Between", "After"), each=2), times=n)
  alternative_model_estimates$Coeff_type <- rep(c("Intercept", "Slope"), times=3*n)
  #alternative_model_estimates <- transform(alternative_model_estimates, Specialty = factor(Specialty, levels= categories))
  
  time_periods <- c("Before", "Between", "After")
  specialty_data_outcome <- transform(specialty_data_outcome, BA_Pandemic_Lockdown = factor(BA_Pandemic_Lockdown, levels= time_periods))
  # Carry out with different baseline change-points
  for(i in 1:3){
    specialty_data_outcome <- within(specialty_data_outcome, BA_Pandemic_Lockdown <- relevel(BA_Pandemic_Lockdown, ref= time_periods[i]))
    
    for(j in 1:n){
      
      specialty_data_outcome <- within(specialty_data_outcome, Specialty <- relevel(Specialty, ref= categories[j]))
      
      
      # Chosen model
      if(model==1){
        alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty, data=specialty_data_outcome)
      } else {
        if(model==2){
          alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty*No_days, data=specialty_data_outcome)
        } else {
          if(model==3){
            alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty*BA_Pandemic_Lockdown, data=specialty_data_outcome)
          } else {
            if(model==4){
              alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Specialty*No_days+Specialty*BA_Pandemic_Lockdown, data=specialty_data_outcome)
              
            }
          }
        }
      }
      
      
      
      
      
      alternative_coefs <- alternative_model$coefficients
      alternative_coefs_cis <- confint(alternative_model)
      
      alternative_model_estimates$est[which(alternative_model_estimates$BA_Pandemic_Lockdown==time_periods[i]&
                                              alternative_model_estimates$Specialty==categories[j])] <- round(alternative_coefs[1:2],3)
      
      alternative_model_estimates$lwr[which(alternative_model_estimates$BA_Pandemic_Lockdown==time_periods[i]&
                                              alternative_model_estimates$Specialty==categories[j])] <- round(alternative_coefs_cis[1:2,1],3)
      
      alternative_model_estimates$upr[which(alternative_model_estimates$BA_Pandemic_Lockdown==time_periods[i]&
                                              alternative_model_estimates$Specialty==categories[j])] <- round(alternative_coefs_cis[1:2,2],3)
      
      
      
      
      
    }
    
    
    
  }
  # [3] Fitted lines and 95% CI
  
  # To create segmented fitted lines we must fit the model to all days in time-periods and stop at each of the change-points
  
  # Create a vector for each segmented time-period by day 
  change_pts <- as.Date(c("2020-01-05", "2020-03-11", "2020-03-11","2020-03-23","2020-03-23","2020-06-28"))
  # Before - Jan 05 to Mar 11
  before_days <- as.Date(change_pts[1]:change_pts[2], origin = "1970-01-01")
  # Between - Mar 11 to Mar 23
  between_days <- as.Date(change_pts[3]:change_pts[4], origin = "1970-01-01")
  # After - Mar 23 to 28 Jun
  after_days <- as.Date(change_pts[5]:change_pts[6], origin = "1970-01-01")
  
  # Vector of all days and timeperiods
  all_dates <- c(before_days, between_days, after_days)
  all_no_days <- as.numeric(c(c(before_days, between_days, after_days)-as.Date("2020-01-05")))
  all_time_periods <- c(rep("Before", times=length(before_days)),
                        rep("Between", times=length(between_days)),
                        rep("After", times=length(after_days)))
  all_time_periods_no <- c(rep(1, times=length(before_days)),
                           rep(2, times=length(between_days)),
                           rep(3, times=length(after_days)))
  
  # Calculate the number of possible time points 
  m <- length(before_days)+length(between_days)+length(after_days)
  
  # Next create a dataset with all time points in time-periods
  alternative_model_prediction <- data.frame(matrix(ncol=4, nrow=m*n))
  colnames(alternative_model_prediction) <- c("BA_Pandemic_Lockdown", "No_days", "Outcome", "Specialty")
  
  alternative_model_prediction$Outcome <- rep(outcome, times=m*n)
  alternative_model_prediction$Dates <- rep(all_dates, times=n)
  alternative_model_prediction$No_days <- rep(all_no_days, times=n)
  alternative_model_prediction$BA_Pandemic_Lockdown <- rep(all_time_periods, times=n)
  alternative_model_prediction$Time_period_no <- rep(all_time_periods_no, times=n)
  alternative_model_prediction$Specialty <- rep(categories, each=m)
  
  alternative_model_prediction <- transform(alternative_model_prediction, Specialty = factor(Specialty, levels= categories))
  
  
  # Link variation raw datato prediction dataset
  specialty_data_outcome <- specialty_data_outcome %>%
    mutate(Time_period_no=recode(BA_Pandemic_Lockdown, "Before"=1, "Between"=2, "After"=3))
  
  specialty_data_outcome$Date_BA_Outcome_Speciality <- paste(specialty_data_outcome$Week_ending, specialty_data_outcome$Time_period_no, 
                                                             specialty_data_outcome$Outcome, specialty_data_outcome$Specialty)
  alternative_model_prediction$Date_BA_Outcome_Speciality <- paste(alternative_model_prediction$Date, alternative_model_prediction$Time_period_no, 
                                                                   alternative_model_prediction$Outcome, alternative_model_prediction$Specialty)
  
  alternative_model_prediction <- merge(x=alternative_model_prediction, y=specialty_data_outcome[, c("Date_BA_Outcome_Speciality", "Variation")], by="Date_BA_Outcome_Speciality", all.x=T)
  
  
  # Use the model to predict the outcome with 95% CI
  predictions <- predict(alternative_model, newdata = alternative_model_prediction, interval = "confidence")
  alternative_model_prediction$Predict <- predictions[,1]
  alternative_model_prediction$Lwr <- predictions[,2]
  alternative_model_prediction$Upr <- predictions[,3]
  
  
  
  
  # [4] Model diagnostics
  # Residuals
  alternative_residuals <- alternative_model$residuals
  names(alternative_residuals) <- specialty_data_outcome$Specialty
  
  #Fitted values
  alternative_fitted_values <- alternative_model$fitted.values
  names(alternative_fitted_values) <- specialty_data_outcome$BA_Pandemic_Lockdown
  
  par(mfrow=c(1,3))
  # Histograms
  hist(alternative_residuals, breaks=50, main=" ", xlab="Residuals")
  
  # QQ Plot
  qqnorm(alternative_residuals, main=" ")
  qqline(alternative_residuals)
  
  # Residuals vs fitted
  plot(alternative_fitted_values, alternative_residuals, xlab="Fitted values", ylab="Residuals")
  abline(h=0,lty=2)
  
  # ACF
  par(mfrow=c(2,5))
  for(k in 1:length(categories)){
    alternative_residuals_Specialty <- alternative_residuals[which(names(alternative_residuals)==categories[k])]
    
    # ACF
    acf(alternative_residuals_Specialty, main=categories[k])
  }
  
  
  # PACF
  par(mfrow=c(2,5))
  for(k in 1:length(categories)){
    alternative_residuals_Specialty <- alternative_residuals[which(names(alternative_residuals)==categories[k])]
    
    # PACF
    pacf(alternative_residuals_Specialty, main=categories[k])
  }
  
  
  
  # Output
  return(list(alternative_model_anova, alternative_model_estimates, alternative_model_prediction))
  
  
  
}



##### NHS Health Board mean differences ####
# Extract differences before and after change-points by each NHS HB for an outcome

hb_mean_diff_fn <- function(outcome){
  
  scotland_data_hbs_outcome <- subset(scotland_data_hbs, Outcome == outcome)
  difference_tbl_hbs <- data.frame(matrix(ncol=4, nrow=length(unique(scotland_data_hbs_outcome$Area_name))))
  colnames(difference_tbl_hbs) <- c("Area_name", "Before", "After", "Difference")
  difference_tbl_hbs$Area_name <- sort(as.character(unique(scotland_data_hbs_outcome$Area_name)))
  
  for(i in 1:nrow(difference_tbl_hbs)){
    before_grp <- (scotland_data_hbs_outcome$Variation[scotland_data_hbs_outcome$BA_Pandemic_Lockdown=="Before" &
                                                         scotland_data_hbs_outcome$Area_name==difference_tbl_hbs$Area_name[i]])
    
    after_grp <- (scotland_data_hbs_outcome$Variation[scotland_data_hbs_outcome$BA_Pandemic_Lockdown=="After" &
                                                        scotland_data_hbs_outcome$Area_name==difference_tbl_hbs$Area_name[i]])
    # Record means and difference
    difference_tbl_hbs$Before[i] <- round(mean(before_grp), 2)
    difference_tbl_hbs$After[i] <- round(mean(after_grp), 2)
    difference_tbl_hbs$Difference[i] <- round(mean(after_grp)-mean(before_grp), 2)
    
    
  }
  difference_tbl_hbs
  
}



##### NHS HB models #####
# Fit all types of models for NHS Health Boards by outcome

hbs_models_fn <- function(outcome){
  
  hb_data_outcome <- subset(scotland_data_hbs, Outcome==outcome)
  
  # Results of three-way interaction
  three_way_interaction_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown*Area_name, data=hb_data_outcome)
  
  three_way_interaction_model_anova <- anova(three_way_interaction_model)
  
  # Alternative models
  alternative_model_0 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown, data=hb_data_outcome)
  alternative_model_1 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name, data=hb_data_outcome)
  alternative_model_2 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name*No_days, data=hb_data_outcome)
  alternative_model_3 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name*BA_Pandemic_Lockdown, data=hb_data_outcome)
  alternative_model_4 <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name*No_days+Area_name*BA_Pandemic_Lockdown, data=hb_data_outcome)
  
  alternative_models_aic <- AIC(alternative_model_0,alternative_model_1, alternative_model_2, alternative_model_3, alternative_model_4)
  alternative_models_bic <- BIC(alternative_model_0,alternative_model_1, alternative_model_2, alternative_model_3, alternative_model_4)
  
  return(list(three_way_interaction_model_anova, alternative_models_aic, alternative_models_bic))
}



##### NHS HB Alternative models #####
# Extracts key statistics for chosen NHS Health Board models by outcomes
# Function to extract ANOVA results [1], estimates (intercept and slope) [2], fitted lines [3] and model diagnostics [4] (Plots)

hbs_alternative_model_fn <- function(outcome, model){
  
  
  # Subset
  hbs_data_outcome <- subset(scotland_data_hbs, Outcome==outcome)
  
  # [1] ANOVA
  if(model==1){
    alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name, data=hbs_data_outcome)
  } else {
    if(model==2){
      alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name*No_days, data=hbs_data_outcome)
    } else {
      if(model==3){
        alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name*BA_Pandemic_Lockdown, data=hbs_data_outcome)
      } else {
        if(model==4){
          alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name*No_days+Area_name*BA_Pandemic_Lockdown, data=hbs_data_outcome)
        } else {
          if(model==5){
            alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown*Area_name, data=hbs_data_outcome)
          }
        }
      }
    }
  }
  
  alternative_model_anova <- anova(alternative_model)
  
  
  # [2] Estimates
  # Find number of categories
  categories <- as.character(unique(hbs_data_outcome$Area_name))
  n <- length(categories)
  
  # Create skeleton dataset
  alternative_model_estimates <- data.frame(matrix(ncol=7, nrow=3*2*n))
  colnames(alternative_model_estimates) <- c("Outcome","Area_name","BA_Pandemic_Lockdown", "Coeff_type", "est", "lwr", "upr")
  alternative_model_estimates$Outcome <- rep(outcome, times=6*n)
  alternative_model_estimates$Area_name <- rep(categories, each=6)
  alternative_model_estimates$BA_Pandemic_Lockdown <- rep(rep(c("Before", "Between", "After"), each=2), times=n)
  alternative_model_estimates$Coeff_type <- rep(c("Intercept", "Slope"), times=3*n)
  
  time_periods <- c("Before", "Between", "After")
  hbs_data_outcome <- transform(hbs_data_outcome, BA_Pandemic_Lockdown = factor(BA_Pandemic_Lockdown, levels= time_periods))
  hbs_data_outcome <- transform(hbs_data_outcome, Area_name = factor(Area_name, levels= categories))
  # Carry out with different baseline change-points
  for(i in 1:3){
    hbs_data_outcome <- within(hbs_data_outcome, BA_Pandemic_Lockdown <- relevel(BA_Pandemic_Lockdown, ref= time_periods[i]))
    
    for(j in 1:n){
      
      hbs_data_outcome <- within(hbs_data_outcome, Area_name <- relevel(Area_name, ref= categories[j]))
      
      
      # Chosen model
      if(model==1){
        alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name, data=hbs_data_outcome)
      } else {
        if(model==2){
          alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name*No_days, data=hbs_data_outcome)
        } else {
          if(model==3){
            alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name*BA_Pandemic_Lockdown, data=hbs_data_outcome)
          } else {
            if(model==4){
              alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown+Area_name*No_days+Area_name*BA_Pandemic_Lockdown, data=hbs_data_outcome)
            } else {
              if(model==5){
                alternative_model <- lm(Variation ~ No_days*BA_Pandemic_Lockdown*Area_name, data=hbs_data_outcome)
              }
            }
          }
        }
      }
      
      
      
      
      
      alternative_coefs <- alternative_model$coefficients
      alternative_coefs_cis <- confint(alternative_model)
      
      alternative_model_estimates$est[which(alternative_model_estimates$BA_Pandemic_Lockdown==time_periods[i]&
                                              alternative_model_estimates$Area_name==categories[j])] <- round(alternative_coefs[1:2],3)
      
      alternative_model_estimates$lwr[which(alternative_model_estimates$BA_Pandemic_Lockdown==time_periods[i]&
                                              alternative_model_estimates$Area_name==categories[j])] <- round(alternative_coefs_cis[1:2,1],3)
      
      alternative_model_estimates$upr[which(alternative_model_estimates$BA_Pandemic_Lockdown==time_periods[i]&
                                              alternative_model_estimates$Area_name==categories[j])] <- round(alternative_coefs_cis[1:2,2],3)
      
      
      
      
      
    }
    
    
    
  }
  
  # [3] Fitted lines and 95% CI
  
  # To create segmented fitted lines we must fit the model to all days in time-periods and stop at each of the change-points
  
  # Create a vector for each segmented time-period by day 
  change_pts <- as.Date(c("2020-01-05", "2020-03-11", "2020-03-11","2020-03-23","2020-03-23","2020-06-28"))
  # Before - Jan 05 to Mar 11
  before_days <- as.Date(change_pts[1]:change_pts[2], origin = "1970-01-01")
  # Between - Mar 11 to Mar 23
  between_days <- as.Date(change_pts[3]:change_pts[4], origin = "1970-01-01")
  # After - Mar 23 to 28 Jun
  after_days <- as.Date(change_pts[5]:change_pts[6], origin = "1970-01-01")
  
  # Vector of all days and timeperiods
  all_no_days <- as.numeric(c(c(before_days, between_days, after_days)-as.Date("2020-01-05")))
  all_time_periods <- c(rep("Before", times=length(before_days)),
                        rep("Between", times=length(between_days)),
                        rep("After", times=length(after_days)))
  
  # Calculate the number of possible time points 
  m <- length(before_days)+length(between_days)+length(after_days)
  
  # Next create a dataset with all time points in time-periods
  
  
  alternative_model_prediction <- data.frame(matrix(ncol=4, nrow=m*n))
  colnames(alternative_model_prediction) <- c("BA_Pandemic_Lockdown", "No_days", "Outcome", "Specialty")
  
  alternative_model_prediction$Outcome <- rep(outcome, times=m*n)
  alternative_model_prediction$No_days <- rep(all_no_days, times=n)
  alternative_model_prediction$BA_Pandemic_Lockdown <- rep(all_time_periods, times=n)
  alternative_model_prediction$Area_name <- rep(categories, each=m)
  
  alternative_model_prediction <- transform(alternative_model_prediction, Area_name = factor(Area_name, levels= categories))
  # Link variation raw datato prediction dataset
  hbs_data_outcome$Nodays_BA_Outcome_Area_name <- paste(hbs_data_outcome$No_days, hbs_data_outcome$BA_Pandemic_Lockdown, 
                                                        hbs_data_outcome$Outcome, hbs_data_outcome$Area_name)
  alternative_model_prediction$Nodays_BA_Outcome_Area_name <- paste(alternative_model_prediction$No_days, alternative_model_prediction$BA_Pandemic_Lockdown, 
                                                                    alternative_model_prediction$Outcome, alternative_model_prediction$Area_name)
  
  alternative_model_prediction <- merge(x=alternative_model_prediction, y=hbs_data_outcome[, c("Nodays_BA_Outcome_Area_name", "Variation")], by="Nodays_BA_Outcome_Area_name", all.x=T)
  
  
  # Use the model to predict the outcome with 95% CI
  predictions <- predict(alternative_model, newdata = alternative_model_prediction, interval = "confidence")
  alternative_model_prediction$Predict <- predictions[,1]
  alternative_model_prediction$Lwr <- predictions[,2]
  alternative_model_prediction$Upr <- predictions[,3]
  
  
  
  
  # [4] Model diagnostics
  # Residuals
  alternative_residuals <- alternative_model$residuals
  names(alternative_residuals) <- hbs_data_outcome$Area_name
  
  #Fitted values
  alternative_fitted_values <- alternative_model$fitted.values
  names(alternative_fitted_values) <- hbs_data_outcome$Area_name
  
  par(mfrow=c(1,3))
  # Histograms
  hist(alternative_residuals, breaks=50, main=" ", xlab="Residuals")
  
  # QQ Plot
  qqnorm(alternative_residuals, main=" ")
  qqline(alternative_residuals)
  
  # Residuals vs fitted
  plot(alternative_fitted_values, alternative_residuals, xlab="Fitted values", ylab="Residuals")
  abline(h=0,lty=2)
  
  # ACF
  par(mfrow=c(3,5))
  for(k in 1:length(categories)){
    alternative_residuals_Specialty <- alternative_residuals[which(names(alternative_residuals)==categories[k])]
    
    # ACF
    acf(alternative_residuals_Specialty, main=categories[k])
  }
  
  
  # PACF
  par(mfrow=c(3,5))
  for(k in 1:length(categories)){
    alternative_residuals_Specialty <- alternative_residuals[which(names(alternative_residuals)==categories[k])]
    
    # PACF
    pacf(alternative_residuals_Specialty, main=categories[k])
  }
  
  
  
  # Output
  return(list(alternative_model_anova, alternative_model_estimates, alternative_model_prediction))
  
  
  
}
