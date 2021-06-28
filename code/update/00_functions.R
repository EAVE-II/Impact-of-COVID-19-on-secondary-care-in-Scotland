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
    mutate(coef_name = ifelse(coef_name=="BAAfter", "Step change", "Slope change")) %>%
    mutate(coef_name = factor(coef_name, levels=c("Step change", "Slope change")))
  
  
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
  plot_grid(p_ae, p_emerg, p_planned, align = "v", ncol=1, labels = "AUTO")
  
  
  
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
  } else {if(demographic=="Specialty"){
    demographic_data <- scotland_data_specialty %>%
      select(-Category) %>%
      rename(Category = Specialty) %>%
      mutate(Type = "Specialty")
  } else {if(demographic=="NHS Health Board"){
    demographic_data <- scotland_data_hbs %>%
      select(-Category) %>%
      rename(Category = Area_name) %>%
      mutate(Type = "NHS Health Board")
  }}}}}
  
  
  
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
    three_way_interaction_model <- lm(Variation ~ No_days*BA*Category, data=demographic_data_outcome, weights = Count)
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
    alternative_model_5 <- lm(Variation ~ No_days*BA+Category*No_days+Category*BA, data=demographic_data_outcome, weights = Count)
    
  }
  three_way_interaction_model_anova <- anova(three_way_interaction_model)
  
  
  alternative_models_aic <- AIC(alternative_model_0,alternative_model_1, alternative_model_2, alternative_model_3, alternative_model_4, three_way_interaction_model)
  alternative_models_bic <- BIC(alternative_model_0,alternative_model_1, alternative_model_2, alternative_model_3, alternative_model_4, three_way_interaction_model)
  
  return(list(three_way_interaction_model_anova, alternative_models_aic, alternative_models_bic))
}



###### Demographic alternative model function ####
# Extract key statistics from chosen demographic model by outcome

demographic_alternative_model_fn <- function(demographic, outcome, model, changepoint, postld, weighted, diag_plots){
  
  # Subset to demographics
  if(demographic=="Sex"){
    demographic_data <- scotland_data_sex
  } else {if(demographic=="Age"){
    demographic_data <- scotland_data_age
  } else {if(demographic=="SIMD"){
    demographic_data <- scotland_data_simd
  } else {if(demographic=="Specialty"){
    demographic_data <- scotland_data_specialty %>%
      select(-Category) %>%
      rename(Category = Specialty) %>%
      mutate(Type = "Specialty") %>%
      mutate(Category = factor(Category))
  } else {if(demographic=="NHS Health Board"){
    demographic_data <- scotland_data_hbs %>%
      select(-Category) %>%
      rename(Category = Area_name) %>%
      mutate(Type = "NHS Health Board")%>%
      mutate(Category = factor(Category))
  }}}}}
  
  demographic_data <- demographic_data %>%
    mutate(Category = factor(Category))
  
  # Subset to post lockdown only and outcome 
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
  
  
  
  # Get model based on weights or not
  if(weighted == T){
    if(model==1){
      alternative_model <- lm(Variation ~ No_days*BA+Category, data=demographic_data_outcome, weights = Count)
    } else {
      if(model==2){
        alternative_model <- lm(Variation ~ No_days*BA+Category*No_days, data=demographic_data_outcome, weights = Count)
      } else {
        if(model==3){
          alternative_model <- lm(Variation ~ No_days*BA+Category*BA, data=demographic_data_outcome, weights = Count)
        } else {
          if(model==4){
            alternative_model <- lm(Variation ~ No_days*BA+Category*No_days+Category*BA, data=demographic_data_outcome, weights = Count)
            
          } else {
            if(model==5){
              alternative_model <- lm(Variation ~ No_days*BA*Category, data=demographic_data_outcome, weights = Count)
            }
          }
        }
      }
      
    }
    
  } else {
    
    
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
            
          } else {
            if(model==5){
              alternative_model <- lm(Variation ~ No_days*BA*Category, data=demographic_data_outcome)
            }
          }
        }
      }
      
    }
    
  }
  
  
  
  ### [1] & [2] Estimates
  # [1] = Intercept and slope estimates for each line
  # [2] = Differences in intercept and slope before and after change-point for step change and slope change
  
  categories <- sort(unique(demographic_data_outcome$Category))
  time_periods <- c("Before", "After")
  coef_name <- c("Intercept", "Slope")
  
  ## [1]
  # Create skeleton dataset
  alternative_model_estimates <- expand_grid(outcome, categories, time_periods, coef_name)%>%
    add_column(est = NA, lwr =NA, upr=NA)
  
  
  ## [2]
  alternative_model_diff_estimates <- expand_grid(outcome, categories, coef_name)%>%
    add_column(est = NA, lwr =NA, upr=NA)
  
  
  # Carry out with different baseline change-points
  
  for(i in 1:length(categories)){
    demographic_data_outcome <- within(demographic_data_outcome, Category <- relevel(Category, ref= as.character(categories[i])))
    demographic_data_outcome <- within(demographic_data_outcome, BA <- relevel(BA, ref= "Before"))   
    
    # Fit model for difference before and after
    if(model==1){
      alternative_model <- lm(Variation ~ No_days*BA+Category, data=demographic_data_outcome, weights = Count)
    } else {
      if(model==2){
        alternative_model <- lm(Variation ~ No_days*BA+Category*No_days, data=demographic_data_outcome, weights = Count)
      } else {
        if(model==3){
          alternative_model <- lm(Variation ~ No_days*BA+Category*BA, data=demographic_data_outcome, weights = Count)
        } else {
          if(model==4){
            alternative_model <- lm(Variation ~ No_days*BA+Category*No_days+Category*BA, data=demographic_data_outcome, weights = Count)
            
          } else {
            if(model==5){
              alternative_model <- lm(Variation ~ No_days*BA*Category, data=demographic_data_outcome, weights = Count)
            }
          }
        }
      }
      
    }
    
    # Extract coefficients
    alternative_coefs <- alternative_model$coefficients
    alternative_coefs_cis <- as.data.frame(confint(alternative_model)) %>%
      mutate(est = alternative_coefs) %>%
      mutate(coef_name = names(alternative_coefs)) %>%
      rename("lwr"=1, "upr"=2) %>%
      relocate(coef_name, est)
    
    # Get step change row
    step_change <- which(alternative_coefs_cis$coef_name == "BAAfter")
    # Get slope change row
    slope_change <- which(alternative_coefs_cis$coef_name == "No_days:BAAfter")
    
    diff_ests <- rbind(alternative_coefs_cis[step_change,], alternative_coefs_cis[slope_change,])
    
    # Put in alternative_model_diff_estimates
    n <- which(alternative_model_diff_estimates$categories == as.character(categories[i]))
    alternative_model_diff_estimates$est[n] <- diff_ests$est
    alternative_model_diff_estimates$lwr[n] <- diff_ests$lwr
    alternative_model_diff_estimates$upr[n] <- diff_ests$upr
    
    
    for(j in 1:length(time_periods)){
      demographic_data_outcome <- within(demographic_data_outcome, BA <- relevel(BA, ref= time_periods[j]))    
      
      # Chosen model
      if(model==1){
        alternative_model <- lm(Variation ~ No_days*BA+Category, data=demographic_data_outcome, weights = Count)
      } else {
        if(model==2){
          alternative_model <- lm(Variation ~ No_days*BA+Category*No_days, data=demographic_data_outcome, weights = Count)
        } else {
          if(model==3){
            alternative_model <- lm(Variation ~ No_days*BA+Category*BA, data=demographic_data_outcome, weights = Count)
          } else {
            if(model==4){
              alternative_model <- lm(Variation ~ No_days*BA+Category*No_days+Category*BA, data=demographic_data_outcome, weights = Count)
              
            } else {
              if(model==5){
                alternative_model <- lm(Variation ~ No_days*BA*Category, data=demographic_data_outcome, weights = Count)
              }
            }
          }
        }
        
      }
      
      
      
      # Extract coefficients
      alternative_coefs <- alternative_model$coefficients
      alternative_coefs_cis <- as.data.frame(confint(alternative_model)) %>%
        mutate(est = alternative_coefs) %>%
        mutate(coef_name = names(alternative_coefs)) %>%
        rename("lwr"=1, "upr"=2) %>%
        relocate(coef_name, est)
      
      # Get estimates
      intercept <- which(alternative_coefs_cis$coef_name == "(Intercept)")
      slope <- which(alternative_coefs_cis$coef_name == "No_days")
      
      ests <- rbind(alternative_coefs_cis[intercept,], alternative_coefs_cis[slope,])
      
      # Put in alternative_model_diff_estimates
      n <- which(alternative_model_estimates$categories == as.character(categories[i]) &
                   alternative_model_estimates$time_periods == time_periods[j])
      alternative_model_estimates$est[n] <- ests$est
      alternative_model_estimates$lwr[n] <- ests$lwr
      alternative_model_estimates$upr[n] <- ests$upr
      
      
      
      
    }
    
    
    
  }
  
  # Final adjustments for diff estimates
  alternative_model_diff_estimates <- alternative_model_diff_estimates %>%
    mutate(coef_name =ifelse(coef_name == "Intercept", "Step change", "Slope change")) %>%
    mutate(coef_name = factor(coef_name, levels= c("Step change", "Slope change"))) %>%
    mutate(Label = paste0(round(est,1), " (95% CI: ",
                                round(lwr,1), " to ",
                                round(upr,1), ")"))
  
  # Final adjustments for estimates
  alternative_model_estimates <- alternative_model_estimates %>%
      mutate(Label = paste0(round(est,1), " (95% CI: ",
                          round(lwr,1), " to ",
                          round(upr,1), ")"))
  
  # [3] Fitted lines and 95% CI
  
  # To create segmented fitted lines we must fit the model to all days in time-periods and stop at each of the change-points
  
  # Create a vector for each segmented time-period by day 
  # Start
  z_strt <- lubridate::floor_date(as.Date(min(demographic_data_outcome$Week_ending)), 
                                  unit="weeks", week_start=1)
  # End
  z_end <- as.Date(max(demographic_data_outcome$Week_ending))
  
  # Change-point periods
  change_pts <- as.Date(c(z_strt,changepoint,changepoint ,z_end))
  
  # Before
  before_days <- as.Date(change_pts[1]:change_pts[2], origin = "1970-01-01")
  
  # After - Mar 23 to 28 Jun
  after_days <- as.Date(change_pts[3]:change_pts[4], origin = "1970-01-01")
  
  # Vector of all days and timeperiods
  all_dates <- c(before_days, after_days)
  all_no_days <- as.numeric(c(c(before_days, after_days)-z_strt))
  all_time_periods <- c(rep("Before", times=length(before_days)),
                        rep("After", times=length(after_days)))
  
  dates_df <- bind_cols(all_dates, all_no_days, all_time_periods) %>%
    rename(Date = 1, No_days =2, BA =3)
  
  alternative_model_prediction <- expand_grid(dates_df, categories) %>%
    mutate(Outcome = outcome) %>%
    relocate(Outcome) %>%
    rename(Category = categories) %>%
    mutate(Category = factor(Category, levels= categories))
  
  
  # Link variation raw datato prediction dataset
  
  demographic_data_outcome$Date_BA_Outcome_Category <- paste(demographic_data_outcome$Week_ending, demographic_data_outcome$BA, 
                                                             demographic_data_outcome$Outcome, demographic_data_outcome$Category)
  alternative_model_prediction$Date_BA_Outcome_Category <- paste(alternative_model_prediction$Date, alternative_model_prediction$BA, 
                                                                 alternative_model_prediction$Outcome, alternative_model_prediction$Category)
  
  alternative_model_prediction <- merge(x=alternative_model_prediction, y=demographic_data_outcome[, c("Date_BA_Outcome_Category", "Variation")], by="Date_BA_Outcome_Category", all.x=T)
  
  
  # Use the model to predict the outcome with 95% CI
  predictions <- predict(alternative_model, newdata = alternative_model_prediction, interval = "confidence")
  alternative_model_prediction$Predict <- predictions[,1]
  alternative_model_prediction$Lwr <- predictions[,2]
  alternative_model_prediction$Upr <- predictions[,3]
  
  # Update order so plot is smooth
  alternative_model_prediction <- alternative_model_prediction %>%
    arrange(Category, Date, desc(BA))  
  
  
  ## Outputs
  
  if(diag_plots == T){
    # [3] Model diagnostics
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
    
    
    
    ## Outputs
    
    return(list(summary(alternative_model), anova(alternative_model), alternative_model_estimates,alternative_model_diff_estimates, alternative_model_prediction))
    
  } else {
    return(list(summary(alternative_model), anova(alternative_model), alternative_model_estimates, alternative_model_diff_estimates, alternative_model_prediction))
    
  }
  
  
}





##### Mean differences for 4 weeks periods #####
# 4 weeks before change-point 2020-09-06 to 2020-09-27
# 4 weeks after change-point 2020-10-04 to 2020-11-01
# 4 weeks before end date: 2021-02-28 to 2021-03-28


mean_diff_tbl <- function(outcome, changepoint, explanatory){
  
  changepoint_wk <- ceiling_date(changepoint, unit = "week", week_start = 7)
  
  mean_diff_list <- list()
  
  j <- 1
  for(j in 1:length(explanatory)){
    
    explanatory_j <- explanatory[j]
    
    
    if(explanatory_j == "Total"){
      data_input = scotland_data %>%
        mutate(Category = "Total")
    } else {if(explanatory_j=="Sex"){
      data_input <- scotland_data_sex
    } else {if(explanatory_j=="Age"){
      data_input <- scotland_data_age
    } else {if(explanatory_j=="SIMD"){
      data_input <- scotland_data_simd
    } else {if(explanatory_j=="Specialty"){
      data_input <- scotland_data_specialty %>%
        select(-Category) %>%
        rename(Category = Specialty) %>%
        mutate(Type = "Specialty") %>%
        mutate(Category = factor(Category))
    } else {if(explanatory_j=="NHS Health Board"){
      data_input <- scotland_data_hbs %>%
        select(-Category) %>%
        rename(Category = Area_name) %>%
        mutate(Type = "NHS Health Board")%>%
        mutate(Category = factor(Category))
    }}}}}}
    
    
    data_input_outcome <- data_input %>%
      filter(BA_Pandemic_Lockdown== "After") %>%
      mutate(BA = factor(case_when(Week_ending < as.Date(changepoint) ~ "Before",
                                   TRUE ~ "After"),
                         levels = c("Before", "After"))) %>%
      mutate(No_days = as.numeric(Week_ending - Week_ending[1]))%>%
      filter(Outcome == outcome) %>%
      mutate(BA_4wk_tp = ifelse(Week_ending %in% seq(as.Date(changepoint_wk)-21, as.Date(changepoint_wk), by="week"), "4 weeks before change-point",
                                ifelse(Week_ending %in% seq(as.Date(changepoint_wk)+7, as.Date(changepoint_wk)+28, by="week"), "4 weeks after change-point",
                                       ifelse(Week_ending %in% seq(as.Date(max(Week_ending))-21, as.Date(max(Week_ending)), by="week"), "4 weeks before end date",NA
                                       ))))
    
    # Unique levels
    BA_4wk_tp_unique <- unique(scotland_data_outcome$BA_4wk_tp)
    BA_4wk_tp_unique <- BA_4wk_tp_unique[!is.na(BA_4wk_tp_unique)]
    
    category_unique <- unique(data_input_outcome$Category)
    
    # Skeleton table
    mean_diff_tbl <- expand_grid(category_unique, BA_4wk_tp_unique) %>%
      mutate(Average_2018_2019 = NA,
             Count_2020_2021 = NA,
             Diff = NA,
             Sig_diff = NA) %>%
      rename(Category=1, Period=2)
    
    
    
    i <- 1
    for(i in 1:nrow(mean_diff_tbl)){
      data_input_outcome_i <- data_input_outcome %>%
        filter(BA_4wk_tp == mean_diff_tbl$Period[i] & 
                 Category == mean_diff_tbl$Category[i])
      
      t_test_i <- t.test(data_input_outcome_i$Count, data_input_outcome_i$Average_2018_2019)
      t_test_i
      
      mean_diff_tbl$Average_2018_2019[i] <- round(mean(data_input_outcome_i$Average_2018_2019),1)
      mean_diff_tbl$Count_2020_2021[i] <- round(mean(data_input_outcome_i$Count),1)
      
      diff_i <- round(diff(t_test_i$estimate)*(-1),1)
      ci_lwr_i <- round(t_test_i$conf.int[1],1)
      ci_upr_i <- round(t_test_i$conf.int[2],1)
      p_i <- ifelse(t_test_i$p.value<0.001, 0.001, round(t_test_i$p.value,3))
      
      input_i <- paste0(diff_i, " (", ci_lwr_i, ", ", ci_upr_i, "; p=", p_i, ")")
      
      mean_diff_tbl$Diff[i] <- input_i
      
      mean_diff_tbl$Sig_diff[i] <- ifelse(t_test_i$p.value < 0.05, T, F)
      
      
      
    }
    
    mean_diff_list[[j]] <- mean_diff_tbl
    
    
    
  }
  mean_diff <- mean_diff_list %>%
    reduce(full_join)
  
  mean_diff
  
}








