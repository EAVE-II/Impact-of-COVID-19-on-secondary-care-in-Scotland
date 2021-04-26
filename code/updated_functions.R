######################################################################
## Title: Impact of COVID-19 on accident and emergency attendances
##          and emergency and planned hospital admissions in
##          Scotland: an interrupted time-series analysis
## Short title: Impact of COVID-19 on secondary care in Scotland
## DOI: 10.1177/0141076820962447
## Code author: Rachel H. Mulholland <Rachel.Mulholland@ed.ac.uk>
## Description: Updated functions for analysis
######################################################################

#### Baseline model function ####
# Baseline model = the number of days*change-point
# Function to extract ANOVA results [1], estimates (intercept and slope) [2], fitted lines [3] and model diagnostics [4] (Plots)
# Input:
# - outcome: Outcome of interest ("Planned Hospital Admissions","Emergency Hospital Admissions" or "A&E Attendances")
# - changepoint: The change-point of interest (a date)
# - postld: Whether or not data should be subsetted to post lockdown only (T/F)
# - weighted: Whether or not the model should be weighted to the Count (T/F)

data <- scotland_data
outcome <- "Planned Hospital Admissions"
postld <- T
changepoint <- "2020-09-22"
weighted <- T

baseline_model_fn <- function(data, outcome, postld, changepoint, weighted){
  
  ## Post lockdown or not
  # Includes changepoint input for defining before and after periods
  # Includes outcome subset
  if(postld == T){
    data_input <- data %>%
      filter(BA_Pandemic_Lockdown == "After") %>%
      mutate(BA = factor(case_when(Week_ending < as.Date(changepoint) ~ "Before",
                                      TRUE ~ "After"),
                            levels = c("Before", "After"))) %>%
      mutate(No_days = as.numeric(Week_ending - Week_ending[1]))%>%
      filter(Outcome == outcome)
    
  } else {
    data_input <- data %>%
      mutate(BA = factor(case_when(Week_ending < as.Date(changepoint) ~ "Before",
                                      TRUE ~ "After"),
                            levels = c("Before", "After"))) %>%
      mutate(No_days = as.numeric(Week_ending - Week_ending[1])) %>%
      filter(Outcome == outcome)
  }
  
  
  ## If weighted then fit ITSA model with weights
  # ITSA model: Interaction between No. days from lockdown and changepoint
  if(weighted == T){
    baseline_model <- lm(Variation ~ No_days*BA, data=data_input, weights = Count)
    
  } else {
    baseline_model <- lm(Variation ~ No_days*BA, data=data_input)
    
  }
  
  ## Preparing for function outputs:
  output_list <- list()
  
  
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
  p3
  
  
  output_list[1] <- gridExtra::grid.arrange(p1, p2, p3, ncol=3)
  
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
  baseline_model_prediction$No_days <- as.numeric(c(c(before_days, after_days)-as.Date("2020-03-23")))
  baseline_model_prediction$BA <- c(rep("Before", times=length(before_days)),
                                          rep("After", times=length(after_days)))
  
  
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
  
  output_list[2] <- baseline_model_prediction
  
  

  
  ## Estimates
  #Fit model each time for each time-period to get estimates for slopes and intercepts
  time_periods <- c("Before", "After")
  
  for(i in 1:2){
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
    baseline_model_estimates$est[which(baseline_model_estimates$BA_EO2HO==time_periods[i])] <- round(baseline_coefs[1:2],3)
    
    baseline_model_estimates$lwr[which(baseline_model_estimates$BA_EO2HO==time_periods[i])] <- round(baseline_coefs_cis[1:2,1],3)
    
    baseline_model_estimates$upr[which(baseline_model_estimates$BA_EO2HO==time_periods[i])] <- round(baseline_coefs_cis[1:2,2],3)
    
    
    
  }
  
  output_list[3] <- baseline_model_estimates
  
  
}
