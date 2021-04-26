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

baseline_model_fn <- function(outcome, model){
  
  # Subset 
  scotland_data_postld_subset <- subset(scotland_data_postld, Outcome==outcome)
  
  # Model 1 - Interaction between No. days from lockdown and before/after eat out to help out
  model1 <- lm(Variation ~ No_days*BA_EO2HO, data=scotland_data_postld_subset)
  #summary(model1)
  #anova(model1, test="LRT")
  
  # Model 2 - Model 1 but weighted to the count
  model2 <- lm(Variation ~ No_days*BA_EO2HO, data=scotland_data_postld_subset, weights = Count)
  #summary(model2)
  #anova(model2, test="LRT")
  
  # Baseline model
  if(model == "model1"){
    baseline_model <- model1
    
  } else 
    if(model == "model2"){
      baseline_model <- model2
    }
  
  
  
  # Residuals
  baseline_residuals <- baseline_model$residuals
  names(baseline_residuals) <- scotland_data_postld_subset$BA_Pandemic_Lockdown
  
  #Fitted values
  baseline_fitted_values <- baseline_model$fitted.values
  names(baseline_fitted_values) <- scotland_data_postld_subset$BA_Pandemic_Lockdown
  
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
  
  
  # Predictions
  change_pts <- as.Date(c("2020-03-23","2020-08-03","2020-08-03","2021-01-24"))
  # Before - Mar 23 to Aug 3
  before_days <- as.Date(change_pts[1]:change_pts[2], origin = "1970-01-01")
  # After - Aug 3 to Jan 23
  after_days <- as.Date(change_pts[3]:change_pts[4], origin = "1970-01-01")
  
  # Calculate the number of possible time points 
  n <- length(before_days)+length(after_days)
  
  # Next create a dataset with all time points in time-periods
  baseline_model_prediction <- data.frame(matrix(ncol=3, nrow=n))
  colnames(baseline_model_prediction) <- c("BA_EO2HO", "No_days", "Outcome")
  
  baseline_model_prediction$Outcome <- rep(outcome, times=n)
  baseline_model_prediction$Date <- c(before_days, after_days)
  baseline_model_prediction$No_days <- as.numeric(c(c(before_days, after_days)-as.Date("2020-03-23")))
  baseline_model_prediction$BA_EO2HO <- c(rep("Before", times=length(before_days)),
                                          rep("After", times=length(after_days)))
  
  
  scotland_data_postld_subset <- scotland_data_postld_subset %>%
    mutate(Date_BA_Outcome = paste0(Week_ending, BA_EO2HO, Outcome))
  
  baseline_model_prediction <- baseline_model_prediction %>%
    mutate(Date_BA_Outcome = paste0(Date, BA_EO2HO, Outcome))
  
  
  baseline_model_prediction <- baseline_model_prediction %>%
    left_join(scotland_data_postld_subset %>%
                select(Date_BA_Outcome, Variation, Count))
  
  
  # Use the model to predict the outcome with 95% CI
  if(model == "model1"){
    predictions <- predict(baseline_model, newdata = baseline_model_prediction, interval = "confidence")
    
  } else 
    if(model == "model2"){
      predictions <- predict(baseline_model, newdata = baseline_model_prediction, interval = "confidence", weights = baseline_model_prediction$Count)
      
    }
  
  predictions <- predict(baseline_model, newdata = baseline_model_prediction, interval = "confidence", weights = baseline_model_prediction$Count)
  baseline_model_prediction$Predict <- predictions[,1]
  baseline_model_prediction$Lwr <- predictions[,2]
  baseline_model_prediction$Upr <- predictions[,3]
  
  baseline_model_prediction
}