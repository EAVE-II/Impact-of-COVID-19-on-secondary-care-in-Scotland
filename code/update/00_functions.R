######################################################################
## Title: 
## Short title: Impact of COVID-19 on secondary care in Scotland (updated)
## DOI: 
## Code author: Rachel H. Mulholland <Rachel.Mulholland@ed.ac.uk>
## Description: 00_functions: Updated functions for analysis
######################################################################

#### Baseline model function ####
# Baseline model = the number of days*change-point
# Function to extract ANOVA results [1], estimates (intercept and slope) [2], fitted lines [3] and model diagnostics [4] (Plots)
# Input:
# - outcome: Outcome of interest ("Planned Hospital Admissions","Emergency Hospital Admissions" or "A&E Attendances")
# - changepoint: The change-point of interest (a date)
# - postld: Whether or not data should be subsetted to post lockdown only (T/F)
# - weighted: Whether or not the model should be weighted to the Count (T/F)
# - diag_plots: Whether or not diagnostic plots should be outputted (T/F)



baseline_model_fn <- function(data, outcome, postld, changepoint, weighted, diag_plots){
  
  ## Post lockdown or not
  # Includes changepoint input for defining before and after periods
  if(length(changepoint)==1){
  # Includes outcome subset
  if(postld == T){
    data_input <- data %>%
      filter(BA_Pandemic_Lockdown== "After") %>%
      mutate(BA = factor(case_when(Week_ending < as.Date(changepoint) ~ "Before",
                                      TRUE ~ "After"),
                            levels = c("Before", "After"))) %>%
      mutate(No_days = as.numeric(Week_ending - Week_ending[1])/7)%>%
      filter(Outcome == outcome)
    
  } else {
    data_input <- data %>%
      mutate(BA = factor(case_when(Week_ending < as.Date(changepoint) ~ "Before",
                                      TRUE ~ "After"),
                            levels = c("Before", "After"))) %>%
      mutate(No_days = as.numeric(Week_ending - Week_ending[1])/7) %>%
      filter(Outcome == outcome)
  }
  
  } else {
    
    if(postld == T){
      data_input <- data %>%
        filter(BA_Pandemic_Lockdown== "After") %>%
        mutate(BA = factor(case_when(Week_ending < as.Date(changepoint[1]) ~ "Before",
                                     Week_ending > as.Date(changepoint[2]) ~ "After",
                                     TRUE ~ "Between"),
                           levels = c("Before", "Between","After"))) %>%
        mutate(No_days = as.numeric(Week_ending - Week_ending[1])/7)%>%
        filter(Outcome == outcome)
      
    } else {
      data_input <- data %>%
        mutate(BA = factor(case_when(Week_ending < as.Date(changepoint[1]) ~ "Before",
                                     Week_ending > as.Date(changepoint[2]) ~ "After",
                                     TRUE ~ "Between"),
                           levels = c("Before", "Between","After"))) %>%
        mutate(No_days = as.numeric(Week_ending - Week_ending[1])/7) %>%
        filter(Outcome == outcome)
    }
    
    
    
  }
    
  ## If weighted then fit ITSA model with weights
  # ITSA model: Interaction between No. days from lockdown and changepoint
  if(weighted == T){
    baseline_model <- lm(Variation ~ No_days*BA, data=data_input, weights = Count)
    
  } else {
    baseline_model <- lm(Variation ~ No_days*BA, data=data_input)
    
  }
  
  ## Difference estimates
  baseline_diff_ests <- bind_cols(coef_name = names(baseline_model$coefficients),
                                  est=baseline_model$coefficients,
                                  confint(baseline_model)) %>%
    rename("lwr"=3, "upr"=4) %>%
    mutate(Outcome = outcome) %>%
    filter(coef_name %in% c("BAAfter", "No_days:BAAfter")) %>%
    mutate(coef_name = ifelse(coef_name=="BAAfter", "Step change", "Slope change"))
  
  
  ## Model goodness of fit
  baseline_mod_good <- tibble(AIC = AIC(baseline_model),
                              BIC = BIC(baseline_model),
                              Rsq_adj = summary(baseline_model)$adj.r.squared)
  
  ## Model diagnostics
  
  # Residuals and fitted values
  baseline_mod_diag <- tibble(residuals = baseline_model$residuals,
                              fitted.values = baseline_model$fitted.values)
    
  # Plots:
  # Histogram
  p1 <- ggplot(aes(x=residuals), data=baseline_mod_diag)+
    geom_histogram() +
    theme_light() +
    labs(x="Residuals")
  
  # QQ Plot
  p2 <- ggplot(aes(sample = residuals), data=baseline_mod_diag) +
    stat_qq() + stat_qq_line() +
    theme_light()
  
  
  # Residuals vs fitted
  p3 <- ggplot(aes(x=fitted.values, y=residuals), data=baseline_mod_diag) +
    geom_point()+
    geom_hline(yintercept = 0) +
    theme_light()
  
  
  # ACF and PACF - fix to ggplot later
  #acf(baseline_residuals, main=" ")
  # PACF
  #pacf(baseline_residuals, main=" ")
  
  
  ## Predictions
  # Start
  z_strt <- lubridate::floor_date(as.Date(min(data_input$Week_ending)), 
                                  unit="weeks", week_start=1)
  # End
  z_end <- as.Date(max(data_input$Week_ending))
  
  # String of start and end points split by change-point
  if(length(changepoint)==1){
  change_pts <- as.Date(c(z_strt,changepoint,changepoint ,z_end))
  
  # Before days
  before_days <- as.Date(change_pts[1]:change_pts[2], origin = "1970-01-01")
  
  # After days
  after_days <- as.Date(change_pts[3]:change_pts[4], origin = "1970-01-01")
  
  # Calculate the number of possible time points 
  n <- length(before_days)+length(after_days)
  
  # Next create a dataset with all time points in time-periods
  baseline_model_prediction <- data.frame(matrix(ncol=3, nrow=n))
  colnames(baseline_model_prediction) <- c("BA", "No_days", "Outcome")
  
  baseline_model_prediction$Outcome <- rep(outcome, times=n)
  baseline_model_prediction$Date <- c(before_days, after_days)
  baseline_model_prediction$No_days <- as.numeric(c(c(before_days, after_days)-min(all_data$Week_ending)))/7
  baseline_model_prediction$BA <- c(rep("Before", times=length(before_days)),
                                    rep("After", times=length(after_days)))
  
  } else {
    
    # Create a vector for each segmented time-period by day 
    change_pts <- as.Date(c(z_strt,changepoint[1],changepoint[1],changepoint[2] ,changepoint[2] ,z_end))
    
    # Before
    before_days <- as.Date(change_pts[1]:change_pts[2], origin = "1970-01-01")
    # Between
    between_days <- as.Date(change_pts[3]:change_pts[4], origin = "1970-01-01")
    # After
    after_days <- as.Date(change_pts[5]:change_pts[6], origin = "1970-01-01")
    
    # Calculate the number of possible time points 
    n <- length(before_days)+length(between_days)+length(after_days)
    
    # Next create a dataset with all time points in time-periods
    baseline_model_prediction <- data.frame(matrix(ncol=3, nrow=n))
    colnames(baseline_model_prediction) <- c("BA", "No_days", "Outcome")
    
    baseline_model_prediction$Outcome <- rep(outcome, times=n)
    baseline_model_prediction$Date <- c(before_days, between_days, after_days)
    baseline_model_prediction$No_days <- as.numeric(c(c(before_days, between_days, after_days)-min(all_data$Week_ending)))/7
    baseline_model_prediction$BA <- c(rep("Before", times=length(before_days)),
                                                        rep("Between", times=length(between_days)),
                                                        rep("After", times=length(after_days)))
    baseline_model_prediction$Time_period_no <- c(rep(1, times=length(before_days)),
                                                  rep(2, times=length(between_days)),
                                                  rep(3, times=length(after_days)))
    
  }
  

  
  data_input <- data_input %>%
    mutate(Date_BA_Outcome = paste0(Week_ending, BA, Outcome))
  
  baseline_model_prediction <- baseline_model_prediction %>%
    mutate(Date_BA_Outcome = paste0(Date, BA, Outcome))
  
  
  baseline_model_prediction <- baseline_model_prediction %>%
    left_join(data_input %>%
                select(Date_BA_Outcome, Variation, Count))
  
  
  # Use the model to predict the outcome with 95% CI
  if(weighted == T){
    predictions <- predict(baseline_model, newdata = baseline_model_prediction, 
                           interval = "confidence", weights = baseline_model_prediction$Count)
    
  } else {
      predictions <- predict(baseline_model, newdata = baseline_model_prediction, interval = "confidence")
      
    }
  
  # Extract predictions
  baseline_model_prediction$Predict <- predictions[,1]
  baseline_model_prediction$Lwr <- predictions[,2]
  baseline_model_prediction$Upr <- predictions[,3]
  
  baseline_model_prediction
  
  

  
  ## Estimates
  time_periods = unique(baseline_model_prediction$BA)
  estimate_grps = c("Intercept", "Slope")
  
  baseline_model_estimates <- expand.grid("BA" = time_periods, "Coeff_type" = estimate_grps) %>%
    mutate(Outcome = outcome) %>%
    mutate(est = NA) %>%
    mutate(lwr = NA) %>%
    mutate(upr = NA)
  

  for(i in 1:length(time_periods)){
    # Redefine baseline level for each loop (once for each time period)
    data_input <- within(data_input, BA <- relevel(BA, ref= time_periods[i]))
    
    # Refit baseline model
    if(weighted == T){
      baseline_model <- lm(Variation ~ No_days*BA, data=data_input, weights = Count)
      
    } else {
      baseline_model <- lm(Variation ~ No_days*BA, data=data_input)
      
    }    
    # Capture estimates and their 95% CI
    baseline_coefs <- baseline_model$coefficients
    baseline_coefs_cis <- confint(baseline_model)
    
    # Populate the estimate table
    baseline_model_estimates$est[which(baseline_model_estimates$BA==time_periods[i])] <- round(baseline_coefs[1:2],3)
    
    baseline_model_estimates$lwr[which(baseline_model_estimates$BA==time_periods[i])] <- round(baseline_coefs_cis[1:2,1],3)
    
    baseline_model_estimates$upr[which(baseline_model_estimates$BA==time_periods[i])] <- round(baseline_coefs_cis[1:2,2],3)
    
    
    
  }
  
  baseline_model_estimates
  
  
  
  ## Outputs
  
  if(diag_plots == T){
    return(list(gridExtra::grid.arrange(p1, p2, p3, ncol=3), 
                baseline_model_prediction, baseline_diff_ests, baseline_model_estimates, baseline_mod_good))
  } else{
    return(list(baseline_model_prediction, baseline_diff_ests, baseline_model_estimates, baseline_mod_good))
    
  }
}




#### Demographics plot function #####
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
  
  p_ae <- ggplot(demographic_data_ae) +
    # Labels
    labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="A&E") +
    # 2018-2019 average
    geom_hline(yintercept = 0, linetype=2)+
    annotate("text", x=as.Date("2021-02-14"), y=2, label="2018-2019 average", hjust=0, size=3) +
    geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
    annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
    # Lines
    geom_line(aes(x=Week_ending, y=Variation, color=Category), size=1)+
    # Points 
    geom_point(aes(x=Week_ending, y=Variation, color=Category, shape=Category), size=2)+
    theme_classic()+
    # Colours of lines and points
    scale_color_manual(demographic,values=col_scheme)+
    scale_shape_manual(demographic, values=shape_scheme)+
    #scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left",
          legend.position = c(0.01,0.95),
          legend.direction =  "horizontal") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
    scale_y_continuous(labels = function(x) paste0(x, "%"))
  
  p_ae
  
  # Emerg
  demographic_data_emerg <- subset(demographic_data, Outcome=="Emergency Hospital Admissions")
  
  p_emerg <- ggplot(demographic_data_emerg) +
    # Labels
    labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Emergency Hospital Admissions") +
    # 2018-2019 average
    geom_hline(yintercept = 0, linetype=2)+
    geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
    # Lines
    geom_line(aes(x=Week_ending, y=Variation, color=Category), size=1)+
    # Points 
    geom_point(aes(x=Week_ending, y=Variation, color=Category, shape=Category), size=2)+
    theme_classic()+
    # Colours of lines and points
    scale_color_manual(demographic,values=col_scheme)+
    scale_shape_manual(demographic, values=shape_scheme)+
    theme(legend.position = "none") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
    scale_y_continuous(labels = function(x) paste0(x, "%"))
  
  p_emerg
  
  # Planned
  demographic_data_planned <- subset(demographic_data, Outcome=="Planned Hospital Admissions")
  
  p_planned <- ggplot(demographic_data_planned) +
    # Labels
    labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Planned Hospital Admissions") +
    # 2018-2019 average
    geom_hline(yintercept = 0, linetype=2)+
    geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
    # Lines
    geom_line(aes(x=Week_ending, y=Variation, color=Category), size=1)+
    # Points 
    geom_point(aes(x=Week_ending, y=Variation, color=Category, shape=Category), size=2)+
    theme_classic()+
    # Colours of lines and points
    scale_color_manual(demographic,values=col_scheme)+
    scale_shape_manual(demographic, values=shape_scheme)+
    theme(legend.position = "none") +
    scale_x_date(date_breaks = "months" , date_labels = "%b") +
    geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
    scale_y_continuous(labels = function(x) paste0(x, "%"))
  
  # Plot together
  plot_grid(p_ae, p_emerg, p_planned, align = "v", ncol=1)
  
  
  
}



##### Demographic model function ####
# Fits all models for demographics by outcome
# Input:
# - demographic: Demographic of interest (Age, Sex, SIMD)
# - outcome: Outcome of interest ("Planned Hospital Admissions","Emergency Hospital Admissions" or "A&E Attendances")
# - changepoint: The change-point of interest (a date)
# - postld: Whether or not data should be subsetted to post lockdown only (T/F)
# - weighted: Whether or not the model should be weighted to the Count (T/F)

demographic_models_fn <- function(demographic, outcome, changepoint, postld, weighted){
  
  if(demographic=="Sex"){
    demographic_data <- scotland_data_sex
  } else {if(demographic=="Age"){
    demographic_data <- scotland_data_age
  } else {if(demographic=="SIMD"){
    demographic_data <- scotland_data_simd
  }}}
  
  
  if(postld == T){
    demographic_data_outcome <- demographic_data %>%
      filter(BA_Pandemic_Lockdown== "After") %>%
      mutate(BA = factor(case_when(Week_ending < as.Date(changepoint) ~ "Before",
                                   TRUE ~ "After"),
                         levels = c("Before", "After"))) %>%
      mutate(No_days = as.numeric(Week_ending - Week_ending[1]))%>%
      filter(Outcome == outcome)
    
    
  } else {
    demographic_data_outcome <- demographic_data %>%
      mutate(BA = factor(case_when(Week_ending < as.Date(changepoint) ~ "Before",
                                   TRUE ~ "After"),
                         levels = c("Before", "After"))) %>%
      mutate(No_days = as.numeric(Week_ending - Week_ending[1]))%>%
      filter(Outcome == outcome)
    
  }
  
  

  # Results of three-way interaction
  if(weighted == T){
    three_way_interaction_model <- lm(Variation ~ No_days*BA*Category, data=demographic_data_outcome)
    # Alternative models
    alternative_model_0 <- lm(Variation ~ No_days*BA, data=demographic_data_outcome, weights = Count)
    alternative_model_1 <- lm(Variation ~ No_days*BA+Category, data=demographic_data_outcome, weights = Count)
    alternative_model_2 <- lm(Variation ~ No_days*BA+Category*No_days, data=demographic_data_outcome, weights = Count)
    alternative_model_3 <- lm(Variation ~ No_days*BA+Category*BA, data=demographic_data_outcome, weights = Count)
    alternative_model_4 <- lm(Variation ~ No_days*BA+Category*No_days+Category*BA, data=demographic_data_outcome, weights = Count)
    
  } else {
    three_way_interaction_model <- lm(Variation ~ No_days*BA*Category, data=demographic_data_outcome)
    # Alternative models
    alternative_model_0 <- lm(Variation ~ No_days*BA, data=demographic_data_outcome)
    alternative_model_1 <- lm(Variation ~ No_days*BA+Category, data=demographic_data_outcome)
    alternative_model_2 <- lm(Variation ~ No_days*BA+Category*No_days, data=demographic_data_outcome)
    alternative_model_3 <- lm(Variation ~ No_days*BA+Category*BA, data=demographic_data_outcome)
    alternative_model_4 <- lm(Variation ~ No_days*BA+Category*No_days+Category*BA, data=demographic_data_outcome)
    
  }
  three_way_interaction_model_anova <- anova(three_way_interaction_model)
  
  
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
    alternative_model <- lm(Variation ~ No_days*BA+Category, data=demographic_data_outcome)
  } else {
    if(model==2){
      alternative_model <- lm(Variation ~ No_days*BA+Category*No_days, data=demographic_data_outcome)
    } else {
      if(model==3){
        alternative_model <- lm(Variation ~ No_days*BA+Category*BA, data=demographic_data_outcome)
      } else {
        if(model==4){
          alternative_model <- lm(Variation ~ No_days*BA+Category*No_days+Category*BA, data=demographic_data_outcome)
          
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
  colnames(alternative_model_estimates) <- c("Outcome","Category","BA", "Coeff_type", "est", "lwr", "upr")
  alternative_model_estimates$Outcome <- rep(outcome, times=6*n)
  alternative_model_estimates$Category <- rep(categories, each=6)
  alternative_model_estimates$BA <- rep(rep(c("Before", "Between", "After"), each=2), times=n)
  alternative_model_estimates$Coeff_type <- rep(c("Intercept", "Slope"), times=3*n)
  
  time_periods <- c("Before", "Between", "After")
  # Carry out with different baseline change-points
  for(i in 1:3){
    demographic_data_outcome <- within(demographic_data_outcome, BA <- relevel(BA, ref= time_periods[i]))
    
    for(j in 1:n){
      
      demographic_data_outcome <- within(demographic_data_outcome, Category <- relevel(Category, ref= categories[j]))
      
      
      # Chosen model
      if(model==1){
        alternative_model <- lm(Variation ~ No_days*BA+Category, data=demographic_data_outcome)
      } else {
        if(model==2){
          alternative_model <- lm(Variation ~ No_days*BA+Category*No_days, data=demographic_data_outcome)
        } else {
          if(model==3){
            alternative_model <- lm(Variation ~ No_days*BA+Category*BA, data=demographic_data_outcome)
          } else {
            if(model==4){
              alternative_model <- lm(Variation ~ No_days*BA+Category*No_days+Category*BA, data=demographic_data_outcome)
              
            }
          }
        }
      }
      
      
      
      
      
      alternative_coefs <- alternative_model$coefficients
      alternative_coefs_cis <- confint(alternative_model)
      
      alternative_model_estimates$est[which(alternative_model_estimates$BA==time_periods[i]&
                                              alternative_model_estimates$Category==categories[j])] <- round(alternative_coefs[1:2],3)
      
      alternative_model_estimates$lwr[which(alternative_model_estimates$BA==time_periods[i]&
                                              alternative_model_estimates$Category==categories[j])] <- round(alternative_coefs_cis[1:2,1],3)
      
      alternative_model_estimates$upr[which(alternative_model_estimates$BA==time_periods[i]&
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
  colnames(alternative_model_prediction) <- c("BA", "No_days", "Outcome", "Category")
  
  alternative_model_prediction$Outcome <- rep(outcome, times=m*n)
  alternative_model_prediction$Dates <- rep(all_dates, times=n)
  alternative_model_prediction$No_days <- rep(all_no_days, times=n)
  alternative_model_prediction$BA <- rep(all_time_periods, times=n)
  alternative_model_prediction$Time_period_no <- rep(all_time_periods_no, times=n)
  alternative_model_prediction$Category <- rep(categories, each=m)
  
  alternative_model_prediction <- transform(alternative_model_prediction, Category = factor(Category, levels= categories))
  
  # Link variation raw datato prediction dataset
  demographic_data_outcome <- demographic_data_outcome %>%
    mutate(Time_period_no=recode(BA, "Before"=1, "Between"=2, "After"=3))
  
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
  names(alternative_fitted_values) <- demographic_data_outcome$BA
  
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


