
######################################################################
## Title: Impact of COVID-19 on accident and emergency attendances
##          and emergency and planned hospital admissions in
##          Scotland: an interrupted time series analysis
## Short title: Impact of COVID-19 on secondary care in Scotland
## DOI: 
## Script of: Analysis
## Date: 06/08/2020
## Code author: Rachel H. Mulholland <Rachel.Mulholland@ed.ac.uk>
######################################################################

##### 1 - Setting up and loading in the data #####

#### 1.1 - Load in libraries ####
library("RColorBrewer")
library("rgdal")
library("tmap")
library("ggplot2")
library("gtable")
library("rcompanion")


#### 1.2 - Create colour scheme #### 
phs_main <- rgb(67,53,139, maxColorValue = 255)
phs_purple<- rgb(150,64,145, maxColorValue = 255)
phs_blue <- rgb(0,149,212, maxColorValue = 255)
phs_green <- rgb(134,188,37, maxColorValue = 255)
phs_trendcol1 <- rgb(74,126,187, maxColorValue = 255)
phs_trendcol2 <- rgb(151,171,204, maxColorValue = 255)
phs_red <- rgb(201,12,12, maxColorValue = 255) 
phs_gold <- rgb(254,160,5, maxColorValue = 255) 
phs_grey <- rgb(190,190,190, maxColorValue = 255) 
phs_purple2 <- rgb(208,145,205, maxColorValue = 255)
phs_orange <- rgb(246,156,80, maxColorValue = 255)
phs_spec <- colorRampPalette(c(phs_purple2, phs_main))
phs_spec2 <- colorRampPalette(c(phs_main, phs_purple2))

#### 1.3 - Load in data #### 

# Load in A&E attendance and hospital admission data
ae_attend <- read.csv("A&E_Attendances.csv")
hosp_admissions <- read.csv("Hospital_Admission.csv")

# Add columns to 'ae_attend' to match columns in 'hosp_admissions'
ae_attend$Admission_type <- "A&E Attendances"
ae_attend$Specialty <- "All"

# Merge datasets together
all_data <- rbind(hosp_admissions, ae_attend)

# Change 'Week_ending' to date
all_data$Week_ending <- as.Date(all_data$Week_ending, "%d-%b-%y")

# Rename columns
colnames(all_data)[8] <- "Variation"
colnames(all_data)[1] <- "Outcome"

# Change outcome to character and add on 'Hospital Admissions to 'Emergency' and 'Planned'
all_data$Outcome <- as.character(all_data$Outcome)
all_data$Outcome[which(all_data$Outcome=="Emergency")] <- "Emergency Hospital Admissions"
all_data$Outcome[which(all_data$Outcome=="Planned")] <- "Planned Hospital Admissions"

# Find weeks before pandemic (change-point 1) and after lockdown (change-point 2)
before_pandemic <- which(all_data$Week_ending < as.Date("2020-03-11"))
after_lockdown <- which(all_data$Week_ending > as.Date("2020-03-23"))
# Assign weeks to relevant time periods
all_data$BA_Pandemic_Lockdown <- "Between"
all_data$BA_Pandemic_Lockdown[before_pandemic] <- "Before"
all_data$BA_Pandemic_Lockdown[after_lockdown] <- "After"

# Make 'BA_Pandemic_Lockdown' a factor
all_data <- transform(all_data, BA_Pandemic_Lockdown = factor(BA_Pandemic_Lockdown, levels= c("Before", "Between", "After")))

# Calculate number of days variable
all_data$No_days <- as.numeric(all_data$Week_ending-all_data$Week_ending[1])


#### 1.4 - Subset to Scotland level data #### 
scotland_data <- subset(all_data, Area_name=="Scotland" & Outcome!="All" & Specialty=="All" & Category=="All")


#### 1.5 - Subset to demographic data #### 

# Extract the category levels for each demographic variable
sex <- c("Male", "Female")
age <- c("Aged under 5", "Aged 5 to 14", "Aged 15 to 44", "Aged 45 to 64", "Aged 65 to 74", "Aged 75 to 84", "Aged 85 and over")
simd <- c("Quintile 1 - most deprived", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 - least deprived")

# Create individual datasets for each demographic variable
scotland_data_sex <- subset(all_data, Area_name=="Scotland" & Outcome!="All" &
                              Specialty=="All" & Category %in% sex)

scotland_data_age <- subset(all_data, Area_name=="Scotland" & Outcome!="All" &
                              Specialty=="All" & Category %in% age)
scotland_data_age <- transform(scotland_data_age, Category = factor(Category, levels= age))


scotland_data_simd <- subset(all_data, Area_name=="Scotland" & Outcome!="All" &
                               Specialty=="All" & Category %in% simd)


#### 1.6 - Subset to speciality data (hospital admissions only) #### 
scotland_data_specialty <- subset(all_data, Area_name=="Scotland" & Outcome!="All" &
                                    Specialty!="All" & Category=="All")

## Finding suppressed counts of counts < 5
  # Create a unique identifier for each row
  specialties <- unique(scotland_data_specialty$Specialty)
  outcomes_specialties <- unique(scotland_data_specialty$Outcome)
  dates <- unique(scotland_data_specialty$Week_ending)
  # Getting all possible combinations
  specialty_allcombos <- expand.grid(specialties, outcomes_specialties, dates)
  specialty_allcombos$Specialty_combo <- paste(specialty_allcombos$Var1, specialty_allcombos$Var2, specialty_allcombos$Var3)

  scotland_data_specialty$Specialty_combo <- paste(scotland_data_specialty$Specialty, scotland_data_specialty$Outcome,
                                                 scotland_data_specialty$Week_ending)

  # Counting the number of suppressed values
  not_incl <- which(!(specialty_allcombos$Specialty_combo %in% scotland_data_specialty$Specialty_combo))
  specialty_exclude <- specialty_allcombos[not_incl,]
  table(specialty_exclude$Var1, specialty_exclude$Var2)


  # Taking out the specialities that have at least one suppressed value
  scotland_data_specialty <- scotland_data_specialty[-(which(scotland_data_specialty$Specialty=="Accident & Emergency" & 
                                                             scotland_data_specialty$Outcome=="Planned Hospital Admissions")),]

  # Taking out the specialties that are doublers
  scotland_data_specialty <- scotland_data_specialty[-(which(scotland_data_specialty$Specialty%in% c("Medical (incl. Cardiology & Cancer)",
                                                                                                   "Paediatrics (medical & surgical)"))),]
  # Replacing specialities vector with new unique values
  specialties <- unique(scotland_data_specialty$Specialty)

  

#### 1.7 - Subset to NHS health board data #### 
scotland_data_hbs <- subset(all_data, Area_type=="Health board" & Outcome!="All" &
                              Specialty=="All" & Category=="All")

  ## Finding suppressed counts of counts < 5
  # Create a unique identifier for each row
  hbs <- unique(scotland_data_hbs$Area_name)
  outcomes_hbs <- unique(scotland_data_hbs$Outcome)
  dates <- unique(scotland_data_hbs$Week_ending)

  hbs_allcombos <- expand.grid(hbs, outcomes_hbs, dates)
  hbs_allcombos$HB_combo <- paste(hbs_allcombos$Var1, hbs_allcombos$Var2, hbs_allcombos$Var3)
  
  scotland_data_hbs$HB_combo <- paste(scotland_data_hbs$Area_name, scotland_data_hbs$Outcome,
                                    scotland_data_hbs$Week_ending)
  
  # Counting the number of suppressed values
  not_incl <- which(!(hbs_allcombos$HB_combo %in% scotland_data_hbs$HB_combo))
  hbs_exclude <- hbs_allcombos[not_incl,]
  table(hbs_exclude$Var1, hbs_exclude$Var2)

  # Taking away suppressed counts of counts
  scotland_data_hbs <- scotland_data_hbs[-(which(scotland_data_hbs$Area_name %in% c("NHS Orkney", "NHS Shetland", "NHS Western Isles") & 
                                                   scotland_data_hbs$Outcome=="Planned Hospital Admissions")),]
  # Taking away Forth Valley
  scotland_data_hbs <- scotland_data_hbs[-(which(scotland_data_hbs$Area_name == c("NHS Forth Valley") & 
                                                   scotland_data_hbs$Outcome %in% c("Planned Hospital Admissions", "Emergency Hospital Admissions"))),]



#### 1.8 - Load in shapefile of Scottish NHS Health Boards #### 
# File found: https://data.gov.uk/dataset/27d0fe5f-79bb-4116-aec9-a8e565ff756a/nhs-health-boards

# Insert directory to shapefile below (note that \ must be replaced by / in directory path)
hb_map <- readOGR("directory to shapefile", "SG_NHS_HealthBoards_2019")

# Renaming 'Area_name' in 'scotland_data_hbs' to the same format as in the shapefile (for linkage purposes)
scotland_data_hbs$Area_name <- substring(scotland_data_hbs$Area_name, 5)
scotland_data_hbs$Area_name <- gsub("&", "and", scotland_data_hbs$Area_name)

# Check the names match (order doesn't matter at this point)
hb_map$HBName %in% scotland_data_hbs$Area_name


##### 2 - Initial analyses on outcomes #####

#### 2.1 - Initial plots of percentage change and counts for each outcome ####

# S2 Appendix: Figure 1. Temporal trends of percentage change for A&E attendances and hospital admissions across Scotland
ggplot(scotland_data, aes(x=Week_ending, y=Variation, color=Outcome, linetype=Outcome))+
  geom_line(size=1)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="% change between 2020 and 2018-2019 average")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2020-06-14"), y=2, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1)+
  geom_vline(xintercept=as.Date("2020-03-23"), color=phs_purple, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-03-27"), y=-17, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0)+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  scale_linetype_manual("Outcome", values=2:4)

# S3 Appendix: Figure 1. Temporal trends of counts in A&E attendances and hospital admissions across Scotland
ggplot(scotland_data, aes(x=Week_ending, y=Count, color=Outcome, linetype=Outcome))+
  geom_line(size=1)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="Count")+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-03-07"), y=20000, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1)+
  geom_vline(xintercept=as.Date("2020-03-23"), color=phs_purple, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-03-27"), y=20000, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0)+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  scale_linetype_manual("Outcome", values=2:4)


#### 2.2 - Plots of means with 95% CI #####
# Calculating means of the percentage change within each time-period for all three outcomes with 95% CIs
scotland_outcome_means <- groupwiseMean(Variation ~ Outcome + BA_Pandemic_Lockdown,
                                        data=scotland_data,
                                        conf = 0.95)
# Plot
p_mean <- ggplot(scotland_outcome_means, aes(x=BA_Pandemic_Lockdown, y=Mean, col=Outcome))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper), width=0.25)+
  facet_wrap(~Outcome)+
  theme_bw()+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  theme(legend.position = "none")+
  labs(x = "Time period", y ="Mean (%)")+
  coord_flip()

# Create data frame for labels of 'mean (95% CI: lqr, upr)'
scotland_outcome_means_labels <- data.frame(Variation=rep(1:3, times=3), 
                                            BA_Pandemic_Lockdown=scotland_outcome_means$BA_Pandemic_Lockdown, 
                                            Outcome=scotland_outcome_means$Outcome,
                                            Mean=scotland_outcome_means$Mean) 

scotland_outcome_means_labels$Label <- paste(round(scotland_outcome_means$Mean,1), " (95% CI:",
                                             round(scotland_outcome_means$Trad.lower,1), " to ",
                                             round(scotland_outcome_means$Trad.upper,1), ")", sep="")

# Plot altogether
# S4 Appendix: Figure 1. Mean percentage change of 2020 to 2018-2019 average for A&E attendances and hospital admissions
p_mean + geom_text(data=scotland_outcome_means_labels, label=scotland_outcome_means_labels$Label, size=4,fontface = "bold", position = position_nudge(x = -0.25))


#### 2.3 - Fitting baseline model for each outcome #####

#### 2.3.1 - Fit model and extract estimates and model diagnostics ####

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
    baseline_model_prediction$No_days <- as.numeric(c(c(before_days, between_days, after_days)-as.Date("2020-01-05")))
    baseline_model_prediction$BA_Pandemic_Lockdown <- c(rep("Before", times=length(before_days)),
                                                        rep("Between", times=length(between_days)),
                                                        rep("After", times=length(after_days)))
    
    # Link variation raw datato prediction dataset
    scotland_data_subset$Nodays_BA_Outcome <- paste(scotland_data_subset$No_days, scotland_data_subset$BA_Pandemic_Lockdown, scotland_data_subset$Outcome)
    baseline_model_prediction$Nodays_BA_Outcome <- paste(baseline_model_prediction$No_days, baseline_model_prediction$BA_Pandemic_Lockdown, baseline_model_prediction$Outcome)
    
    baseline_model_prediction <- merge(x=baseline_model_prediction, y=scotland_data_subset[, c("Nodays_BA_Outcome", "Variation")], by="Nodays_BA_Outcome", all.x=T)
    
    
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


baseline_model_fn("A&E Attendances")
baseline_model_fn("Emergency Hospital Admissions")
baseline_model_fn("Planned Hospital Admissions")


#### 2.3.2 - Plot fitted models ####

# Extract the predictions for each model
outcome_predictions <- rbind(data.frame(baseline_model_fn("A&E Attendances")[3]),
                             data.frame(baseline_model_fn("Emergency Hospital Admissions")[3]),
                             data.frame(baseline_model_fn("Planned Hospital Admissions")[3]))

# Transform the pandemic and lockdown dates and labels and average label into number of days
pandemic_day <- as.Date("2020-03-11")-as.Date("2020-01-05")
pandemic_label <- as.Date("2020-03-07")-as.Date("2020-01-05")
lockdown_day <- as.Date("2020-03-23")-as.Date("2020-01-05")
lockdown_label <- as.Date("2020-03-27")-as.Date("2020-01-05")
average_label <- as.Date("2020-06-14")-as.Date("2020-01-05")

# Plot the data:
# Figure 2. Fitted lines of the baseline model for A&E attendances and hospital admissions across Scotland
ggplot(outcome_predictions, aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Outcome, color = NULL), alpha = .15)+
  geom_point(aes(shape=Outcome, col=Outcome), size=1.5)+
  geom_line(aes(x=No_days, y=Predict, linetype=Outcome, col=Outcome), size=1)+
  theme_classic()+
  labs(x = "Number of days from 5th Jan 2020", y ="% change between 2020 and 2018-2019 average")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=average_label, y=2, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  annotate("text", x=lockdown_label, y=-17, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  annotate("text", x=pandemic_label, y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1)+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  scale_linetype_manual("Outcome", values=2:4)+
  scale_fill_manual("Outcome", values=c(phs_trendcol1, phs_trendcol2, phs_green))


#### 2.3.3 - Plot estimates of fitted models ####

outcome_estimates <- rbind(data.frame(baseline_model_fn("A&E Attendances")[2]),
                           data.frame(baseline_model_fn("Emergency Hospital Admissions")[2]),
                           data.frame(baseline_model_fn("Planned Hospital Admissions")[2]))

outcome_estimates <- transform(outcome_estimates, BA_Pandemic_Lockdown = factor(BA_Pandemic_Lockdown, levels= c("After", "Between", "Before")))

# Getting labels - estimate (95% CI: lwr, upr)
outcome_estimates_labels <- outcome_estimates
outcome_estimates_labels$Label <- paste(round(outcome_estimates$est,1), " (95% CI:",
                                        round(outcome_estimates$lwr,1), " to ",
                                        round(outcome_estimates$upr,1), ")", sep="")

# Intercept only
outcome_estimates_int <- subset(outcome_estimates, Coeff_type=="Intercept")
outcome_estimates_labels_int <- subset(outcome_estimates_labels, Coeff_type=="Intercept")
p_int <- ggplot(data=outcome_estimates_int, aes(x=BA_Pandemic_Lockdown, y=est, col=Outcome))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(~Outcome)+
  theme_bw()+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated intercept", title="A)")+
  coord_flip()+
  ylim(c(-300, 450))
p_int + geom_text(data=outcome_estimates_labels_int, label=outcome_estimates_labels_int$Label, size=4,fontface = "bold", position = position_nudge(x = -0.25))


outcome_estimates_slopes <- subset(outcome_estimates, Coeff_type=="Slope")
outcome_estimates_labels_slopes <- subset(outcome_estimates_labels, Coeff_type=="Slope")
p_slope <- ggplot(data=outcome_estimates_slopes, aes(x=BA_Pandemic_Lockdown, y=est, col=Outcome))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.25)+
  facet_grid(~Outcome)+
  theme_bw()+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated slopes", title="B)")+
  coord_flip()+
  ylim(c(-7, 3))
p_slope + geom_text(data=outcome_estimates_labels_slopes, label=outcome_estimates_labels_slopes$Label, size=4, fontface = "bold", position = position_nudge(x = -0.25))





##### 3 - Demographic analyses #####

#### 3.1 - Plots of percentage change and counts for each demographic variable and outcome ####

### 3.1.1 - Variation (S2 Appendix)

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
# S2 Appendix: Figure 2
demographic_variation_plot_fn("Sex")
# S2 Appendix: Figure 3
demographic_variation_plot_fn("Age")
# S2 Appendix: Figure 4
demographic_variation_plot_fn("SIMD")

### 3.1.2 - Counts (S3 Appendix)

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
# S3 Appendix: Figure 2
demographic_counts_plot_fn("Sex")
# S3 Appendix: Figure 3
demographic_counts_plot_fn("Age")
# S3 Appendix: Figure 4
demographic_counts_plot_fn("SIMD")


#### 3.2 - Fitting 3-way interactions and alternative models ####

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

# Testing all three-way interactions and alternative models for each demographic variable and outcome
demographic_models_fn("Sex", "A&E Attendances")
demographic_models_fn("Sex", "Emergency Hospital Admissions")
demographic_models_fn("Sex", "Planned Hospital Admissions")

demographic_models_fn("Age", "A&E Attendances")
demographic_models_fn("Age", "Emergency Hospital Admissions")
demographic_models_fn("Age", "Planned Hospital Admissions")

demographic_models_fn("SIMD", "A&E Attendances")
demographic_models_fn("SIMD", "Emergency Hospital Admissions")
demographic_models_fn("SIMD", "Planned Hospital Admissions")


#### 3.3 - Fit alternative model and extract info ####

# Function to extract ANOVA results [1], estimates (intercept and slope) [2], fitted lines [3] and model diagnostics [4] (Plots)

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
    all_no_days <- as.numeric(c(c(before_days, between_days, after_days)-as.Date("2020-01-05")))
    all_time_periods <- c(rep("Before", times=length(before_days)),
                          rep("Between", times=length(between_days)),
                          rep("After", times=length(after_days)))
    
    # Calculate the number of possible time points 
    m <- length(before_days)+length(between_days)+length(after_days)
    
    # Next create a dataset with all time points in time-periods
    
    
    alternative_model_prediction <- data.frame(matrix(ncol=4, nrow=m*n))
    colnames(alternative_model_prediction) <- c("BA_Pandemic_Lockdown", "No_days", "Outcome", "Category")
    
    alternative_model_prediction$Outcome <- rep(outcome, times=m*n)
    alternative_model_prediction$No_days <- rep(all_no_days, times=n)
    alternative_model_prediction$BA_Pandemic_Lockdown <- rep(all_time_periods, times=n)
    alternative_model_prediction$Category <- rep(categories, each=m)
    
    alternative_model_prediction <- transform(alternative_model_prediction, Category = factor(Category, levels= categories))
    # Link variation raw datato prediction dataset
    demographic_data_outcome$Nodays_BA_Outcome_Category <- paste(demographic_data_outcome$No_days, demographic_data_outcome$BA_Pandemic_Lockdown, 
                                                             demographic_data_outcome$Outcome, demographic_data_outcome$Category)
    alternative_model_prediction$Nodays_BA_Outcome_Category <- paste(alternative_model_prediction$No_days, alternative_model_prediction$BA_Pandemic_Lockdown, 
                                                                     alternative_model_prediction$Outcome, alternative_model_prediction$Category)
    
    alternative_model_prediction <- merge(x=alternative_model_prediction, y=demographic_data_outcome[, c("Nodays_BA_Outcome_Category", "Variation")], by="Nodays_BA_Outcome_Category", all.x=T)
    
    
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
# Age model for A&E Attendances (Model 3)
age_ae_model <- demographic_alternative_model_fn("Age", "A&E Attendances", 3)
# Age model for Emergency Hospital Admissions (Model 3)
age_emerg_model <- demographic_alternative_model_fn("Age", "Emergency Hospital Admissions", 3)
# Age model for Planned Hospital Admissions (Model 3)
age_planned_model <- demographic_alternative_model_fn("Age", "Planned Hospital Admissions", 3)

# SIMD model for Emergency Hospital Admissions (Model 1)
simd_emerg_model <- demographic_alternative_model_fn("SIMD", "Emergency Hospital Admissions", 1)


#### 3.3.1 - Plot fitted models ####

#### 3.3.1.1 - Age plots ####
# Figure 4. Fitted lines of speciality models for hospital admissions

# Transform the pandemic and lockdown dates and labels and average label into number of days
pandemic_day <- as.Date("2020-03-11")-as.Date("2020-01-05")
pandemic_label <- as.Date("2020-03-07")-as.Date("2020-01-05")
lockdown_day <- as.Date("2020-03-23")-as.Date("2020-01-05")
lockdown_label <- as.Date("2020-03-27")-as.Date("2020-01-05")
average_label <- as.Date("2020-06-14")-as.Date("2020-01-05")

# Plot the data:
# Figure 3. Fitted lines of age models for A&E attendances and hospital admissions
# S6 Appendix: Figure 1. Fitted lines of SIMD model for emergency hospital admissions
p_ae_age <- ggplot(data.frame(age_ae_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_point(aes(shape=Category, col=Category), size=1.5)+
  geom_line(aes(x=No_days, y=Predict, linetype=Category, col=Category), size=1)+
  theme_classic()+
  labs(x = " ", y =" ",
       title="A)")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=average_label, y=4, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  annotate("text", x=lockdown_label, y=-15, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  annotate("text", x=pandemic_label, y=-40, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1)+
  scale_color_manual("Age",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  scale_linetype_manual("Age", values=c(1,2,3,4,1,2,3))+
  scale_shape_manual("Age", values=c(15,19,17,23,0,1,2))+
  scale_fill_manual("Age",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))

p_emerg_age <- ggplot(data.frame(age_emerg_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_point(aes(shape=Category, col=Category), size=1.5)+
  geom_line(aes(x=No_days, y=Predict, linetype=Category, col=Category), size=1)+
  theme_classic()+
  labs(x = " ", y ="% change between 2020 and 2018-2019 average",
       title="B)")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  scale_color_manual("Age",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  scale_linetype_manual("Age", values=c(1,2,3,4,1,2,3))+
  scale_shape_manual("Age", values=c(15,19,17,23,0,1,2))+
  scale_fill_manual("Age",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))

p_planned_age <- ggplot(data.frame(age_planned_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_point(aes(shape=Category, col=Category), size=1.5)+
  geom_line(aes(x=No_days, y=Predict, linetype=Category, col=Category), size=1)+
  theme_classic()+
  labs(x = "Number of days from 5th Jan 2020", y =" ",
       title="C)")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  scale_color_manual("Age",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  scale_linetype_manual("Age", values=c(1,2,3,4,1,2,3))+
  scale_shape_manual("Age", values=c(15,19,17,23,0,1,2))+
  scale_fill_manual("Age",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))


# Plot together
g_ae_age <- ggplotGrob(p_ae_age)
g_emerg_age <- ggplotGrob(p_emerg_age)
g_planned_age <- ggplotGrob(p_planned_age)

# Figure 3
plot(rbind(g_ae_age,g_emerg_age,g_planned_age))

#### 3.3.1.2 - SIMD plots ####
# S6 Appendix: Figure 1
ggplot(data.frame(simd_emerg_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_point(aes(shape=Category, col=Category), size=1.5)+
  geom_line(aes(x=No_days, y=Predict, linetype=Category, col=Category), size=1)+
  theme_classic()+
  labs(x = "Number of days from 5th Jan 2020", y ="% change between 2020 and 2018-2019 average",
       title=" ")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  scale_color_manual("SIMD",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  scale_linetype_manual("SIMD", values=c(1,2,3,4,1,2,3))+
  scale_shape_manual("SIMD", values=c(15,19,17,23,0,1,2))+
  scale_fill_manual("SIMD",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))


#### 3.3.2 - Plot estimates of models ####

#### 3.3.2.1 - Age estimates ####

outcome_estimates_age <- rbind(data.frame(age_ae_model[2]),
                               data.frame(age_emerg_model[2]),
                               data.frame(age_planned_model[2]))

outcome_estimates_age <- transform(outcome_estimates_age, BA_Pandemic_Lockdown = factor(BA_Pandemic_Lockdown, levels= c("After", "Between", "Before")))
outcome_estimates_age$Category <- as.factor(outcome_estimates_age$Category)
outcome_estimates_age <- transform(outcome_estimates_age, Category = factor(Category, levels= age))


outcome_estimates_age_int <- subset(outcome_estimates_age, Coeff_type=="Intercept")

ggplot(data=outcome_estimates_age_int, aes(x=BA_Pandemic_Lockdown, y=est, col=Category))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(Category~Outcome)+
  theme_bw()+
  scale_color_manual("Age",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated intercept", title="A)")+
  coord_flip()

outcome_estimates_age_slopes <- subset(outcome_estimates_age, Coeff_type=="Slope")
ggplot(data=outcome_estimates_age_slopes, aes(x=BA_Pandemic_Lockdown, y=est, col=Outcome))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(~Outcome)+
  theme_bw()+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated slope", title="B)")+
  coord_flip()


#### 3.3.2.2 - SIMD estimates ####


outcome_estimates_simd <- data.frame(simd_emerg_model[2])
outcome_estimates_simd <- transform(outcome_estimates_simd, BA_Pandemic_Lockdown = factor(BA_Pandemic_Lockdown, levels= c("After", "Between", "Before")))
outcome_estimates_simd$Category <- as.factor(outcome_estimates_simd$Category)
outcome_estimates_simd <- transform(outcome_estimates_simd, Category = factor(Category, levels= simd))



outcome_estimates_simd_int <- subset(outcome_estimates_simd, Coeff_type=="Intercept")

ggplot(data=outcome_estimates_simd_int, aes(x=BA_Pandemic_Lockdown, y=est, col=Category))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(Category~Outcome)+
  theme_bw()+
  scale_color_manual("simd",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated intercept", title="A)")+
  coord_flip()

outcome_estimates_simd_slopes <- subset(outcome_estimates_simd, Coeff_type=="Slope")

ggplot(data=outcome_estimates_simd_slopes, aes(x=BA_Pandemic_Lockdown, y=est), col="black")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(~Outcome)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated slope", title="B)")+
  coord_flip()



##### 4 - Speciality analyses (hospital admissions only) #####

#### 4.1 - Plots of percentage change and counts for speciality and outcome ####

### 4.1.1 - Variation (S2 Appendix)

# Emerg
scotland_data_specialty_emerg <- subset(scotland_data_specialty, Outcome=="Emergency Hospital Admissions")

p_emerg <- ggplot(scotland_data_specialty_emerg, aes(x=Week_ending, y=Variation))+
  geom_line(size=1, color=phs_trendcol2)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="% change between 2020 and 2018-2019 average",
       title = "A)")+
  geom_hline(yintercept = 0, linetype=2)+   
  #annotate("text", x=as.Date("2020-06-28"), y=2, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  facet_wrap(~Specialty, ncol=4)


# Planned
scotland_data_specialty_planned <- subset(scotland_data_specialty, Outcome=="Planned Hospital Admissions")

p_planned <- ggplot(scotland_data_specialty_planned, aes(x=Week_ending, y=Variation))+
  geom_line(size=1, color=phs_green)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="% change between 2020 and 2018-2019 average",
       title = "B)")+
  geom_hline(yintercept = 0, linetype=2)+   
  #annotate("text", x=as.Date("2020-06-28"), y=2, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  facet_wrap(~Specialty, ncol=4)

# Plot together

g_emerg <- ggplotGrob(p_emerg)
g_planned <- ggplotGrob(p_planned)

# S2 Appendix: Figure 5
plot(rbind(g_emerg,g_planned))


### 4.1.2 - Count (S3 Appendix)

# Emerg

p_emerg <- ggplot(scotland_data_specialty_emerg, aes(x=Week_ending, y=Count))+
  geom_line(size=1, color=phs_trendcol2)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="Count",
       title = "A)")+
  geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  facet_wrap(~Specialty, ncol=4)


# Planned
p_planned <- ggplot(scotland_data_specialty_planned, aes(x=Week_ending, y=Count))+
  geom_line(size=1, color=phs_green)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="Count",
       title = "B)")+
  geom_vline(xintercept=as.Date("2020-03-23"), colour= phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  facet_wrap(~Specialty, ncol=4)

# Plot together

g_emerg <- ggplotGrob(p_emerg)
g_planned <- ggplotGrob(p_planned)

# S3 Appendix: Figure 5
plot(rbind(g_emerg,g_planned))


#### 4.2 - Fitting 3-way interactions and alternative models ####

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

# Finding optimum specialty model for hospital admissions
speciality_models_fn("Emergency Hospital Admissions")
speciality_models_fn("Planned Hospital Admissions")


#### 4.3 - Fit alternative model and extract info ####

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
  alternative_model_prediction$Specialty <- rep(categories, each=m)
  
  alternative_model_prediction <- transform(alternative_model_prediction, Specialty = factor(Specialty, levels= categories))
  # Link variation raw datato prediction dataset
  specialty_data_outcome$Nodays_BA_Outcome_Specialty <- paste(specialty_data_outcome$No_days, specialty_data_outcome$BA_Pandemic_Lockdown, 
                                                               specialty_data_outcome$Outcome, specialty_data_outcome$Specialty)
  alternative_model_prediction$Nodays_BA_Outcome_Specialty <- paste(alternative_model_prediction$No_days, alternative_model_prediction$BA_Pandemic_Lockdown, 
                                                                   alternative_model_prediction$Outcome, alternative_model_prediction$Specialty)
  
  alternative_model_prediction <- merge(x=alternative_model_prediction, y=specialty_data_outcome[, c("Nodays_BA_Outcome_Specialty", "Variation")], by="Nodays_BA_Outcome_Specialty", all.x=T)
  
  
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

# Speciality models for both hospital admissions (Model 3)
specialty_emerg_model <- specialty_alternative_model_fn("Emergency Hospital Admissions", 3)
specialty_planned_model <- specialty_alternative_model_fn("Planned Hospital Admissions", 3)


#### 4.3.1 - Plot fitted models ####

# Emergency
p_specialty_emerg <- ggplot(data.frame(specialty_emerg_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Specialty, color = NULL), alpha = .15)+
  geom_point(aes(shape=Specialty, col=Specialty), size=1.5)+
  geom_line(aes(x=No_days, y=Predict, linetype=Specialty, col=Specialty), size=1)+
  theme_classic()+
  labs(x = " ", y ="% change between 2020 and 2018-2019 average",
       title="A)")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=average_label, y=4, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  annotate("text", x=lockdown_label, y=30, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  annotate("text", x=pandemic_label, y=-50, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1)+
  scale_color_manual("Specialty",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red, phs_grey, phs_orange))+
  scale_linetype_manual("Specialty", values=1:9)+
  scale_shape_manual("Specialty", values=c(15,19,17,23,0,1,2, 3, 4))+
  scale_fill_manual("Specialty",values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red, phs_grey, phs_orange))



# Planned
p_specialty_planned <- ggplot(data.frame(specialty_planned_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Specialty, color = NULL), alpha = .15)+
  geom_point(aes(shape=Specialty, col=Specialty), size=1.5)+
  geom_line(aes(x=No_days, y=Predict, linetype=Specialty, col=Specialty), size=1)+
  theme_classic()+
  labs(x = "Number of days from 5th Jan 2020", y ="% change between 2020 and 2018-2019 average",
       title="B)")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  scale_color_manual("Specialty",values=c(phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red, phs_grey, phs_orange))+
  scale_linetype_manual("Specialty", values=2:9)+
  scale_shape_manual("Specialty", values=c(19,17,23,0,1,2, 3, 4))+
  scale_fill_manual("Specialty",values=c(phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red, phs_grey, phs_orange))

# Plot together
g_emerg_specialty <- ggplotGrob(p_specialty_emerg)
g_planned_specialty <- ggplotGrob(p_specialty_planned)

# Figure 4
plot(rbind(g_emerg_specialty,g_planned_specialty))



#### 4.3.2 - Plot estimates ####
outcome_estimates_specialty <- rbind(data.frame(specialty_emerg_model[2]),
                                     data.frame(specialty_planned_model[2]))

# PLOT TOGETHER
outcome_estimates_specialty <- transform(outcome_estimates_specialty, BA_Pandemic_Lockdown = factor(BA_Pandemic_Lockdown, levels= c("After", "Between", "Before")))


outcome_estimates_specialty_int <- subset(outcome_estimates_specialty, Coeff_type=="Intercept")

ggplot(data=outcome_estimates_specialty_int, aes(x=BA_Pandemic_Lockdown, y=est))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(Outcome~Specialty)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated intercept", title="A)")+
  coord_flip()

outcome_estimates_specialty_slopes <- subset(outcome_estimates_specialty, Coeff_type=="Slope")

ggplot(data=outcome_estimates_specialty_slopes, aes(x=BA_Pandemic_Lockdown, y=est))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(~Outcome)+
  theme_bw()+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated slope", title="B)")+
  coord_flip()



##### 5 - NHS Health Board analysis ######

#### 5.1 - Plots of percentage change and counts for speciality and outcome ####
scotland_data_hbs_ae <- subset(scotland_data_hbs, Outcome == "A&E Attendances")
scotland_data_hbs_emerg <- subset(scotland_data_hbs, Outcome == "Emergency Hospital Admissions")
scotland_data_hbs_planned <- subset(scotland_data_hbs, Outcome == "Planned Hospital Admissions")

### 5.1.1 - Variation 
# S2 Appendix: Figure 6
ggplot(scotland_data_hbs_ae, aes(x=Week_ending, y=Variation))+
  geom_line(size=1, col=phs_trendcol1)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="% change between 2020 and 2018-2019 average",
       title="A)")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=as.Date("2020-03-23"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=35, label="UK Lockdown\n(23rd March)", color=phs_purple, hjust=0, size=3)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=35, label="WHO announces pandemic\n(11th March)", color=phs_blue, hjust=1, size=3)+
  theme(legend.position = "none")+
  facet_wrap(~Area_name, ncol=5)


ggplot(scotland_data_hbs_emerg, aes(x=Week_ending, y=Variation))+
  geom_line(size=1, col=phs_trendcol2)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="% change between 2020 and 2018-2019 average",
       #title="Impact of COVID-19 on NHS Health Boards",
       title="B)")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=as.Date("2020-03-23"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=35, label="UK Lockdown\n(23rd March)", color=phs_purple, hjust=0, size=3)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=35, label="WHO announces pandemic\n(11th March)", color=phs_blue, hjust=1, size=3)+
  theme(legend.position = "none")+
  facet_wrap(~Area_name, ncol=5)

ggplot(scotland_data_hbs_planned, aes(x=Week_ending, y=Variation))+
  geom_line(size=1, col=phs_green)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="% change between 2020 and 2018-2019 average",
       #title="Impact of COVID-19 on NHS Health Boards",
       title="C)")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=as.Date("2020-03-23"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=35, label="UK Lockdown\n(23rd March)", color=phs_purple, hjust=0, size=3)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=35, label="WHO announces pandemic\n(11th March)", color=phs_blue, hjust=1, size=3)+
  theme(legend.position = "none")+
  facet_wrap(~Area_name, ncol=5)


### 5.1.1 - Count
# S3 Appendix: Figure 6
ggplot(scotland_data_hbs_ae, aes(x=Week_ending, y=Count))+
  geom_line(size=1, col=phs_trendcol1)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="Count",
       title="A)")+
  geom_vline(xintercept=as.Date("2020-03-23"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=35, label="UK Lockdown\n(23rd March)", color=phs_purple, hjust=0, size=3)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=35, label="WHO announces pandemic\n(11th March)", color=phs_blue, hjust=1, size=3)+
  theme(legend.position = "none")+
  facet_wrap(~Area_name, ncol=5)


ggplot(scotland_data_hbs_emerg, aes(x=Week_ending, y=Count))+
  geom_line(size=1, col=phs_trendcol2)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="Count",
       #title="Impact of COVID-19 on NHS Health Boards",
       title="B)")+
  geom_vline(xintercept=as.Date("2020-03-23"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=35, label="UK Lockdown\n(23rd March)", color=phs_purple, hjust=0, size=3)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=35, label="WHO announces pandemic\n(11th March)", color=phs_blue, hjust=1, size=3)+
  theme(legend.position = "none")+
  facet_wrap(~Area_name, ncol=5)

ggplot(scotland_data_hbs_planned, aes(x=Week_ending, y=Count))+
  geom_line(size=1, col=phs_green)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="Count",
       #title="Impact of COVID-19 on NHS Health Boards",
       title="C)")+
  geom_vline(xintercept=as.Date("2020-03-23"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=35, label="UK Lockdown\n(23rd March)", color=phs_purple, hjust=0, size=3)+
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=35, label="WHO announces pandemic\n(11th March)", color=phs_blue, hjust=1, size=3)+
  theme(legend.position = "none")+
  facet_wrap(~Area_name, ncol=5)


#### 5.2 - Spatial plots of differences ####

# Get differences
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

diff_ae <- hb_mean_diff_fn("A&E Attendances")
diff_emerg <- hb_mean_diff_fn("Emergency Hospital Admissions")
diff_planned <- hb_mean_diff_fn("Planned Hospital Admissions")

# Adding to shape file 
hb_map$Diff_ae <- NA
hb_map$Diff_emerg <- NA
hb_map$Diff_planned <- NA


for(i in 1:length(hb_map$HBName)){
  selected_hb <- (hb_map$HBName)[i]
  
  # A&E
  n <- which(diff_ae$Area_name==selected_hb)
  
  if(length(n)==1){
    hb_map$Diff_ae[i] <- diff_ae$Difference[n]
    
  } else {hb_map$Diff_ae[i] <- NA}
  
  # Emergency
  n <- which(diff_emerg$Area_name==selected_hb)
  
  if(length(n)==1){
    hb_map$Diff_emerg[i] <- diff_emerg$Difference[n]
    
  } else {hb_map$Diff_emerg[i] <- NA }
  
  # Planned
  n <- which(diff_planned$Area_name==selected_hb)
  
  if(length(n)==1){
    hb_map$Diff_planned[i] <- diff_planned$Difference[n]
    
  } else {hb_map$Diff_planned[i] <- NA }
  
  
  
  
  
}

hb_map$Diff_ae
hb_map$Diff_emerg
hb_map$Diff_planned



# Plot

# A&E
map1 <- tm_shape(hb_map)+
  tm_fill(col="Diff_ae", palette = phs_spec2(7), title = "Difference (%)")+
  tm_layout(frame = FALSE, main.title = "A)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

# Emergency
map2 <-tm_shape(hb_map)+
  tm_fill(col="Diff_emerg", palette = phs_spec(7), title = "Difference (%)")+
  tm_layout(frame = FALSE, main.title = "B)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

# Planned
map3 <- tm_shape(hb_map)+
  tm_fill(col="Diff_planned", palette = phs_spec2(7), title = "Difference (%)")+
  tm_layout(frame = FALSE, main.title = "C)",
            main.title.size = 1, main.title.position="left",legend.position=c("left","top"))

# Figure 5
tmap_arrange(map1, map2, map3)


#### 5.3 - Fitting 3-way interactions and alternative models ####


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

# Finding optimum models
hbs_models_fn("A&E Attendances")
hbs_models_fn("Emergency Hospital Admissions")
hbs_models_fn("Planned Hospital Admissions")




#### 5.4 - Fit alternative model and extract info ####

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

# Health Board model for A&E (Model 2)
hbs_ae_model <- hbs_alternative_model_fn("A&E Attendances", 2)
# Health Board model for Emergency (Model 2)
hbs_emerg_model <- hbs_alternative_model_fn("Emergency Hospital Admissions", 2)
# Health Board model for Planned (Three way interaction - Model 5 in function)
hbs_planned_model <- hbs_alternative_model_fn("Planned Hospital Admissions", 5)



#### 5.5 - Plot fitted models ####

# S8 Appendix
# A&E
ggplot(data.frame(hbs_ae_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr), alpha = .15)+
  geom_point(size=1.5)+
  geom_line(aes(x=No_days, y=Predict), size=1)+
  theme_classic()+
  labs(x = "Number of days from 5th Jan 2020", y =" ",
       title="A)")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  facet_wrap(~Area_name, ncol=5)

# Emergency
ggplot(data.frame(hbs_emerg_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr), alpha = .15)+
  geom_point(size=1.5)+
  geom_line(aes(x=No_days, y=Predict), size=1)+
  theme_classic()+
  labs(x = "Number of days from 5th Jan 2020", y =" ",
       title="A)")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  facet_wrap(~Area_name, ncol=5)

# Planned
ggplot(data.frame(hbs_planned_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr), alpha = .15)+
  geom_point(size=1.5)+
  geom_line(aes(x=No_days, y=Predict), size=1)+
  theme_classic()+
  labs(x = "Number of days from 5th Jan 2020", y =" ",
       title="A)")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=1, size=1)+
  facet_wrap(~Area_name, ncol=5)



#### 5.6 - Plot estimates ####

outcome_estimates_hbs <- rbind(data.frame(hbs_ae_model[2]),
                               data.frame(hbs_emerg_model[2]),
                               data.frame(hbs_planned_model[2]))
outcome_estimates_hbs <- transform(outcome_estimates_hbs, BA_Pandemic_Lockdown = factor(BA_Pandemic_Lockdown, levels= c("After", "Between", "Before")))


outcome_estimates_hbs_int <- subset(outcome_estimates_hbs, Coeff_type=="Intercept")

ggplot(data=outcome_estimates_hbs_int, aes(x=BA_Pandemic_Lockdown, y=est))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(Outcome~Area_name)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated intercept", title="A)")+
  coord_flip()

outcome_estimates_hbs_slopes <- subset(outcome_estimates_hbs, Coeff_type=="Slope")

ggplot(data=outcome_estimates_hbs_slopes, aes(x=BA_Pandemic_Lockdown, y=est), col="black")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(Outcome~Area_name)+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated slope", title="B)")+
  coord_flip()

