
######################################################################
## Title: Impact of COVID-19 on accident and emergency attendances
##          and emergency and planned hospital admissions in
##          Scotland: an interrupted time-series analysis
## Short title: Impact of COVID-19 on secondary care in Scotland
## DOI: 10.1177/0141076820962447
## Code author: Rachel H. Mulholland <Rachel.Mulholland@ed.ac.uk>
## Description: Analysis code
######################################################################

##### 1 - Setting up and loading in the data #####

#### 1.1 - Load in libraries and functions ####
# Libraries
library("RColorBrewer")
library("rgdal")
library("tmap")
library("ggplot2")
library("gtable")
library("rcompanion")
library("tidyverse")

# Analysis functions
source("./code/functions.R")

#### 1.2 - Create colour scheme #### 
phs_main <- rgb(67,53,139, maxColorValue = 255)
phs_purple<- rgb(150,64,145, maxColorValue = 255)
phs_blue <- rgb(0,149,212, maxColorValue = 255)
phs_green <- rgb(134,188,37, maxColorValue = 255)
phs_trendcol1 <- rgb(74,126,187, maxColorValue = 255)
phs_trendcol2 <- rgb(151,171,204, maxColorValue = 255)
phs_red <- rgb(201,12,12, maxColorValue = 255)
phs_gold <- rgb(254,160,5, maxColorValue = 255)
phs_orange <- rgb(254,160,5, maxColorValue = 255) 
phs_purple2 <- rgb(208,145,205, maxColorValue = 255)
phs_teal <- rgb(38,164,117, maxColorValue = 255)
phs_spec <- colorRampPalette(c(phs_purple2, phs_main))
phs_spec2 <- colorRampPalette(c(phs_main, phs_purple2))

col_scheme <- c(phs_trendcol1, phs_main, phs_green, phs_purple2, phs_orange, phs_red, phs_teal, "gold2", "gray40")
shape_scheme <- c(19,17,15,23,0,1,2, 3, 4)

#### 1.3 - Load in data #### 

# Load in A&E attendance and add columns to 'ae_attend' to match columns in 'hosp_admissions'
ae_attend <- read.csv("./data/original/A&E_Attendances.csv") %>%
  mutate(Admission_type = "A&E Attendances",
         Specialty = "All")

# Load in hospital admission data
hosp_admissions <- read.csv("./data/original/Hospital_Admissions.csv") %>%
  filter(Admission_type != "All")


# Merge datasets together
all_data <- bind_rows(hosp_admissions, ae_attend) %>%
  mutate(Week_ending = as.Date(Week_ending, "%d-%b-%y")) %>%# Change 'Week_ending' to date
  rename(Variation = 8) %>% # Rename Variation... to 'Variation'
  rename(Outcome = 1) %>% # Rename Admission_type to 'Outcome'
  mutate(Outcome = recode(Outcome, "Emergency" = "Emergency Hospital Admissions", 
         "Planned" = "Planned Hospital Admissions")) %>% # Change outcome to character and add on 'Hospital Admissions to 'Emergency' and 'Planned'
  mutate(BA_Pandemic_Lockdown = factor(case_when(Week_ending < as.Date("2020-03-11") ~ "Before",
                                          Week_ending > as.Date("2020-03-23") ~ "After",
                                          TRUE ~ "Between"),
                                       levels = c("Before", "Between", "After"))) %>% # Assign weeks to relevant time periods of before pandemic (change-point 1) and after lockdown (change-point 2)
  mutate(No_days = as.numeric(Week_ending - Week_ending[1]))


#### 1.4 - Subset to Scotland level data #### 
scotland_data <- all_data %>%
  filter(Area_name == "Scotland", Specialty=="All", Category=="All")




#### 1.5 - Subset to demographic data #### 

# Extract the category levels for each demographic variable
sex <- c("Male", "Female")
age <- c("Aged under 5", "Aged 5 to 14", "Aged 15 to 44", "Aged 45 to 64", "Aged 65 to 74", "Aged 75 to 84", "Aged 85 and over")
simd <- c("Quintile 1 - most deprived", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5 - least deprived")

# Create individual datasets for each demographic variable
scotland_data_sex <- all_data %>%
  filter(Area_name == "Scotland", Specialty=="All", Category %in% sex)

scotland_data_age <- all_data %>%
  filter(Area_name == "Scotland", Specialty=="All", Category %in% age) %>%
  mutate(Category = factor(Category, levels= age)) # Make factor


scotland_data_simd <- all_data %>%
  filter(Area_name == "Scotland", Specialty=="All", Category %in% simd)


#### 1.6 - Subset to speciality data (hospital admissions only) #### 
scotland_data_specialty <- all_data %>%
  filter(Area_name == "Scotland", Specialty!="All", Category=="All")
  

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
scotland_data_hbs <- all_data %>%
    filter(Area_type == "Health board", Specialty=="All", Category=="All")


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


baseline_model_fn("A&E Attendances")
baseline_model_fn("Emergency Hospital Admissions")
baseline_model_fn("Planned Hospital Admissions")


#### 2.3.2 - Plot fitted models ####

# Extract the predictions for each model
outcome_predictions <- rbind(data.frame(baseline_model_fn("A&E Attendances")[3]),
                             data.frame(baseline_model_fn("Emergency Hospital Admissions")[3]),
                             data.frame(baseline_model_fn("Planned Hospital Admissions")[3]))

# Transform the pandemic and lockdown dates and labels and average label into number of days
pandemic_day <- as.Date("2020-03-11")
pandemic_label <- as.Date("2020-03-07")
lockdown_day <- as.Date("2020-03-23")
lockdown_label <- as.Date("2020-03-27")
average_label <- as.Date("2020-06-14")

# Plot the data:
# Figure 2. Fitted lines of the baseline model for A&E attendances and hospital admissions across Scotland
ggplot(outcome_predictions, aes(x=Date, y=Variation))+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=average_label, y=2, label="2018-2019 average", hjust=1, size=3)+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Outcome, color = NULL), alpha = .15)+
  geom_line(aes(x=Date, y=Predict, col=Outcome), size=0.75, linetype=1)+
  geom_point(aes(shape=Outcome, col=Outcome), size=2)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="% change between 2020 and 2018-2019 average")+
  geom_vline(xintercept=lockdown_day, color="white", linetype=1, size=0.75)+
  geom_vline(xintercept=pandemic_day, color="white", linetype=1, size=0.75)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=2, size=0.75)+
  annotate("text", x=lockdown_label, y=-17, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0, size=3.5)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=2, size=0.75)+
  annotate("text", x=pandemic_label, y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3.5)+
  scale_color_manual("Outcome",values=col_scheme)+
  scale_fill_manual("Outcome", values=col_scheme)+
  scale_shape_manual("Outcome", values=shape_scheme)+
  #theme(legend.position = "none") +
  scale_x_date(date_breaks = "months" , date_labels = "%b")

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

# S2 Appendix: Figure 2
demographic_variation_plot_fn("Sex")
# S2 Appendix: Figure 3
demographic_variation_plot_fn("Age")
# S2 Appendix: Figure 4
demographic_variation_plot_fn("SIMD")

### 3.1.2 - Counts (S3 Appendix)

# S3 Appendix: Figure 2
demographic_counts_plot_fn("Sex")
# S3 Appendix: Figure 3
demographic_counts_plot_fn("Age")
# S3 Appendix: Figure 4
demographic_counts_plot_fn("SIMD")


#### 3.2 - Fitting 3-way interactions and alternative models ####


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
pandemic_day <- as.Date("2020-03-11")
pandemic_label <- as.Date("2020-03-07")
lockdown_day <- as.Date("2020-03-23")
lockdown_label <- as.Date("2020-03-27")
average_label <- as.Date("2020-06-14")

# Plot the data:
# Figure 3. Fitted lines of age models for A&E attendances and hospital admissions
# S6 Appendix: Figure 1. Fitted lines of SIMD model for emergency hospital admissions
p_ae_age <- ggplot(data.frame(age_ae_model[3]), aes(x=Dates, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Dates, y=Predict, col=Category), size=0.75)+
  geom_vline(xintercept=lockdown_day, color="white", linetype=1, size=0.75)+
  geom_vline(xintercept=pandemic_day, color="white", linetype=1, size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = " ", y =" ",
       title="A)")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=average_label, y=4, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=2, size=0.75)+
  annotate("text", x=pandemic_label, y=-40, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3.5)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=2, size=0.75)+
  annotate("text", x=lockdown_label, y=-15, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0, size=3.5)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b")
#theme(legend.position = "none")


p_emerg_age <- ggplot(data.frame(age_emerg_model[3]), aes(x=Dates, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Dates, y=Predict, col=Category), size=0.75)+
  geom_vline(xintercept=lockdown_day, color="white", linetype=1, size=0.75)+
  geom_vline(xintercept=pandemic_day, color="white", linetype=1, size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = " ", y ="% change between 2020 and 2018-2019 average",
       title="B)")+
  geom_hline(yintercept = 0, linetype=2)+
  #annotate("text", x=average_label, y=4, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=2, size=0.75)+
  #annotate("text", x=lockdown_label, y=40, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0, size=3.5)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=2, size=0.75)+
  #annotate("text", x=pandemic_label, y=-15, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3.5)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b")
#theme(legend.position = "none")


p_planned_age <- ggplot(data.frame(age_planned_model[3]), aes(x=Dates, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Dates, y=Predict, col=Category), size=0.75)+
  geom_vline(xintercept=lockdown_day, color="white", linetype=1, size=0.75)+
  geom_vline(xintercept=pandemic_day, color="white", linetype=1, size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = "Week ending (2020)", y =" ",
       title="C)")+
  geom_hline(yintercept = 0, linetype=2)+
  #annotate("text", x=average_label, y=4, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=2, size=0.75)+
  #annotate("text", x=lockdown_label, y=40, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0, size=3.5)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=2, size=0.75)+
  #annotate("text", x=pandemic_label, y=-15, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3.5)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b")
#theme(legend.position = "none")



# Plot together
g_ae_age <- ggplotGrob(p_ae_age)
g_emerg_age <- ggplotGrob(p_emerg_age)
g_planned_age <- ggplotGrob(p_planned_age)

# Figure 3
plot(rbind(g_ae_age,g_emerg_age,g_planned_age))

#### 3.3.1.2 - SIMD plots ####
# S6 Appendix: Figure 1 (Colour scheme slightly different)
ggplot(data.frame(simd_emerg_model[3]), aes(x=No_days, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_point(aes(shape=Category, col=Category), size=1.5)+
  geom_line(aes(x=No_days, y=Predict, linetype=Category, col=Category), size=1)+
  theme_classic()+
  labs(x = "Number of days from 5th Jan 2020", y ="% change between 2020 and 2018-2019 average",
       title=" ")+
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept=c(lockdown_day-as.Date("2020-01-05")), color=phs_purple, linetype=1, size=1)+
  geom_vline(xintercept = c(pandemic_day-as.Date("2020-01-05")), colour=phs_blue, linetype=1, size=1)+
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

# Finding optimum specialty model for hospital admissions
speciality_models_fn("Emergency Hospital Admissions")
speciality_models_fn("Planned Hospital Admissions")


#### 4.3 - Fit alternative model and extract info ####

# Speciality models for both hospital admissions (Model 3)
specialty_emerg_model <- specialty_alternative_model_fn("Emergency Hospital Admissions", 3)
specialty_planned_model <- specialty_alternative_model_fn("Planned Hospital Admissions", 3)


#### 4.3.1 - Plot fitted models ####

# Emergency
p_specialty_emerg <- ggplot(data.frame(specialty_emerg_model[3]), aes(x=Dates, y=Variation))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, color = NULL), fill = phs_main, alpha = .15)+
  geom_line(aes(x=Dates, y=Predict),col=phs_main, size=0.5)+
  geom_vline(xintercept=lockdown_day, color="white", linetype=1, size=0.5)+
  geom_vline(xintercept=pandemic_day, color="white", linetype=1, size=0.5)+
  geom_point(col=phs_main, shape=17, size=1)+
  theme_classic()+
  labs(x = " ", y ="% change between 2020 and 2018-2019 average",
       title="A)")+
  #annotate("text", x=average_label, y=4, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=2, size=0.5)+
  #annotate("text", x=pandemic_label, y=-40, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3.5)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=2, size=0.5)+
  #annotate("text", x=lockdown_label, y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0, size=3.5)+
  facet_wrap(~Specialty)+
  theme(legend.position = "none")

# Planned
p_specialty_planned <- ggplot(data.frame(specialty_planned_model[3]), aes(x=Dates, y=Variation))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, color = NULL), fill = phs_green, alpha = .15)+
  geom_line(aes(x=Dates, y=Predict),col=phs_green, size=0.5)+
  geom_vline(xintercept=lockdown_day, color="white", linetype=1, size=0.5)+
  geom_vline(xintercept=pandemic_day, color="white", linetype=1, size=0.5)+
  geom_point(col=phs_green, shape=15, size=1)+
  theme_classic()+
  labs(x = "Week ending (2020)", y ="% change between 2020 and 2018-2019 average",
       title="B)")+
  #annotate("text", x=average_label, y=4, label="2018-2019 average", hjust=1, size=3)+
  geom_vline(xintercept = pandemic_day, colour=phs_blue, linetype=2, size=0.5)+
  #annotate("text", x=pandemic_label, y=-40, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3.5)+
  geom_vline(xintercept=lockdown_day, color=phs_purple, linetype=2, size=0.5)+
  #annotate("text", x=lockdown_label, y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_purple, hjust=0, size=3.5)+
  facet_wrap(~Specialty)+
  theme(legend.position = "none")
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


# Finding optimum models
hbs_models_fn("A&E Attendances")
hbs_models_fn("Emergency Hospital Admissions")
hbs_models_fn("Planned Hospital Admissions")




#### 5.4 - Fit alternative model and extract info ####


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

