
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
source("./code/updated_functions.R")

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
ae_attend <- read.csv("./data/update/A&E_Attendances.csv") %>%
  mutate(Admission_type = "A&E Attendances",
         Specialty = "All")

# Load in hospital admission data
hosp_admissions <- read.csv("./data/update/Hospital_Admissions.csv") %>%
  filter(Admission_type != "All")


# Merge datasets together
all_data <- bind_rows(hosp_admissions, ae_attend) %>%
  mutate(Week_ending = as.Date(Week_ending, "%d %b %y")) %>%# Change 'Week_ending' to date
  rename(Variation = 8) %>% # Rename Variation... to 'Variation'
  rename(Outcome = 1) %>% # Rename Admission_type to 'Outcome'
  mutate(Outcome = recode(Outcome, "Emergency" = "Emergency Hospital Admissions", 
                          "Planned" = "Planned Hospital Admissions")) %>% # Change outcome to character and add on 'Hospital Admissions to 'Emergency' and 'Planned'
  mutate(BA = factor(case_when(Week_ending < as.Date("2020-03-11") ~ "Before",
                                                 Week_ending > as.Date("2020-03-23") ~ "After",
                                                 TRUE ~ "Between"),
                                       levels = c("Before", "Between", "After"))) %>% # Assign weeks to relevant time periods of before pandemic (change-point 1) and after lockdown (change-point 2)
  mutate(No_days = as.numeric(Week_ending - Week_ending[1]))


# Min date
min(all_data$Week_ending)

# Max date
max(all_data$Week_ending)



#### 1.4 - Subset to Scotland level data #### 
scotland_data <- all_data %>%
  filter(Area_name == "Scotland", Specialty=="All", Category=="All")




#### 2 - Time series plots ####

#### 2.1 - Total outcomes ####

## Counts
p_count <- bind_rows(scotland_data %>%
            select(Outcome, Count, Week_ending) %>%
            mutate(Group = "2020-21"),
          scotland_data %>%
            select(Outcome, Average_2018_2019, Week_ending) %>%
            rename(Count = Average_2018_2019) %>%
            mutate(Group = "2018-19 average")) %>%
  ggplot() +
  # UK Lockdown - 23 Mar 2020
  annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
           ymin=0, ymax=30000,fill=phs_red, alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-03-27"), y=31000, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Phase 1 Introduced - 29 May 2020
  annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
           ymin=0, ymax=30000, fill="orange", alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-06-01"), y=31000, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
  # Phase 2 introduced - 19 Jun 2020
  annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
           ymin=0, ymax=30000, fill="gold", alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-06-21"), y=33000, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
  # Phase 3 introduced - 9th July
  annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
           ymin=0, ymax=30000, fill=phs_green, alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-07-11"), y=35000, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
  # Phase 3 restrictions announced - 22 Sept
  annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
           ymin=0, ymax=30000, fill="gold", alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=31000, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
  # Local authority levels allocated - 2 Nov
  annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
           ymin=0, ymax=30000, fill=phs_purple2, alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-11-05"), y=33000, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
  # Boxing day lockdown - 26 Dec
  annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
           ymin=0, ymax=30000,fill=phs_red, alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-12-29"), y=35000, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Lines
  geom_line(aes(x=Week_ending, y=Count, color=Outcome, linetype=Group), size=1)+
  # Points
  geom_point(aes(x=Week_ending, y=Count, color=Outcome, shape=Outcome), size=2)+
  theme_classic()+
  # Labels
  labs(x = "Week ending", y ="Count") +
  # WHO announcement
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-03-07"), y=31000, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
  # Eat out to help out
  #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-08-07"), y=33000, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
  # Colours of lines and points
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  #scale_linetype_manual("Outcome", values=2:4) +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "months" , date_labels = "%b")


p_count


## Differences

p_diff <- ggplot(scotland_data)+
  # UK Lockdown - 23 Mar 2020
  annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
                ymin=-80, ymax=10,fill=phs_red, alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-03-27"), y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Phase 1 Introduced - 29 May 2020
  annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
                ymin=-80, ymax=10, fill="orange", alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-06-01"), y=15, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
  # Phase 2 introduced - 19 Jun 2020
  annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
           ymin=-80, ymax=10, fill="gold", alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-06-21"), y=20, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
  # Phase 3 introduced - 9th July
  annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
           ymin=-80, ymax=10, fill=phs_green, alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-07-11"), y=25, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
  # Phase 3 restrictions announced - 22 Sept
  annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
           ymin=-80, ymax=10, fill="gold", alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=15, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
  # Local authority levels allocated - 2 Nov
  annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
           ymin=-80, ymax=10, fill=phs_purple2, alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-11-05"), y=20, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
  # Boxing day lockdown - 26 Dec
  annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
           ymin=-80, ymax=10,fill=phs_red, alpha=0.3)+
  geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Lines
  geom_line(aes(x=Week_ending, y=Variation, color=Outcome), size=1)+
  # Points
  geom_point(aes(x=Week_ending, y=Variation, color=Outcome, shape=Outcome), size=2)+
  theme_classic()+
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average") +
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  # WHO announcement
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
  # Eat out to help out
  #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-08-07"), y=-50, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
  # Colours of lines and points
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_trendcol2, phs_green))+
  #scale_linetype_manual("Outcome", values=2:4) +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "months" , date_labels = "%b")
  

p_diff
  
# Plot together
library(cowplot)
plot_grid(p_count, p_diff, labels = "AUTO", ncol=1, align = "v")






##### 3 - Modelling ####
# Using start of 'Phase 3 restrictions' as changepoint
# Date: 22nd September 2020
# Look at post lockdown only

##### 3.1 - Total outcomes ####

# Use 'baseline_model_fn' from 'updated_functions.R' to get information on all outcomes
# Obtain predictions and estimates
z_1 <- baseline_model_fn(data = scotland_data, outcome = "A&E Attendances",
                  postld = T, changepoint = "2020-09-22", weighted = T, diag_plots = F)

z_2 <- baseline_model_fn(data = scotland_data, outcome = "Emergency Hospital Admissions",
                  postld = T, changepoint = "2020-09-22", weighted = T, diag_plots = F)

z_3 <- baseline_model_fn(data = scotland_data, outcome = "Planned Hospital Admissions",
                  postld = T, changepoint = "2020-09-22", weighted = T, diag_plots = F)


## All predictions
outcome_predictions <- bind_rows(z_1[[1]], z_2[[1]], z_3[[1]])
  
# Plot
ggplot(outcome_predictions)+
  # Points
  geom_point(aes(x=Date, y=Variation, color=Outcome, shape=Outcome), size=2)+
  # Fitted lines and 95% CI
  geom_line(aes(x=Date, y=Predict, color=Outcome), size=0.75)+
  geom_ribbon(aes(x=Date, ymin =Lwr, ymax =Upr, fill = Outcome, color = NULL), alpha = .15)+
  theme_classic()+
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average") +
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  # Eat out to help out
  geom_vline(xintercept=as.Date("2020-09-22"), color=phs_purple, linetype=1, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=-50, label="Phase 3 restrictions announced\n22nd Sep 2020", color=phs_purple, hjust=0, size=3, fontface =2)+
  # Colours of lines and points
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_main, phs_green))+
  scale_fill_manual("Outcome",values=c(phs_trendcol1, phs_main, phs_green))+
  #scale_linetype_manual("Outcome", values=2:4) +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "months" , date_labels = "%b")


## Estimates
outcome_estimates <- bind_rows(z_1[[2]], z_2[[2]], z_3[[2]])

# Getting labels - estimate (95% CI: lwr, upr)
outcome_estimates_labels <- outcome_estimates
outcome_estimates_labels$Label <- paste(round(outcome_estimates$est,1), " (95% CI: ",
                                        round(outcome_estimates$lwr,1), " to ",
                                        round(outcome_estimates$upr,1), ")", sep="")

# Plot estimates with 95% CIs
# Intercept only
outcome_estimates_int <- subset(outcome_estimates, Coeff_type=="Intercept")
outcome_estimates_labels_int <- subset(outcome_estimates_labels, Coeff_type=="Intercept")
p_int <- ggplot(data=outcome_estimates_int, aes(x=BA, y=est, col=Outcome))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.5)+
  facet_grid(~Outcome)+
  theme_bw()+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_main, phs_green))+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated intercept", title="A)")+
  coord_flip()+
  ylim(c(min(outcome_estimates_int$est)-25, max(outcome_estimates_int$est)+25)) +
  geom_text(data=outcome_estimates_labels_int, label=outcome_estimates_labels_int$Label, 
            size=3,fontface = "bold", position = position_nudge(x = -0.25))


outcome_estimates_slopes <- subset(outcome_estimates, Coeff_type=="Slope")
outcome_estimates_labels_slopes <- subset(outcome_estimates_labels, Coeff_type=="Slope")
p_slope <- ggplot(data=outcome_estimates_slopes, aes(x=BA, y=est, col=Outcome))+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.25)+
  facet_grid(~Outcome)+
  theme_bw()+
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_main, phs_green))+
  theme(legend.position = "none")+
  labs(x="Time period", y="Estimated slopes", title="B)")+
  coord_flip()+
  ylim(c(min(outcome_estimates_slopes$est)-0.2, max(outcome_estimates_slopes$est)+0.2)) +
  geom_text(data=outcome_estimates_labels_slopes, label=outcome_estimates_labels_slopes$Label,
            size=3, fontface = "bold", position = position_nudge(x = -0.25))


gridExtra::grid.arrange(p_int, p_slope)


#### 3.2 - Total outcomes - 3 way interaction ####
# This compares across the three outcomes
changepoint <- "2020-09-22"

scotland_data_postld <- scotland_data %>%
  filter(BA_Pandemic_Lockdown == "After") %>%
  mutate(BA = factor(case_when(Week_ending < as.Date(changepoint) ~ "Before",
                               TRUE ~ "After"),
                     levels = c("Before", "After"))) %>%
  mutate(No_days = as.numeric(Week_ending - Week_ending[1]))


baseline_model3 <- lm(Variation ~ Outcome*No_days*BA, data=scotland_data_postld, weights = Count)
summary(baseline_model3)

baseline_model3 <- lm(Variation ~ Outcome*No_days*BA_EO2HO, data=scotland_data_postld)
summary(baseline_model3)

anova(baseline_model3)




##### Demographics ######
demographic <- "Sex"


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
    # UK Lockdown - 23 Mar 2020
    annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
             ymin=-80, ymax=10,fill=phs_red, alpha=0.3)+
    geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-03-27"), y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
    # Phase 1 Introduced - 29 May 2020
    annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
             ymin=-80, ymax=10, fill="orange", alpha=0.3)+
    geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-06-01"), y=15, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
    # Phase 2 introduced - 19 Jun 2020
    annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
             ymin=-80, ymax=10, fill="gold", alpha=0.3)+
    geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-06-21"), y=20, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
    # Phase 3 introduced - 9th July
    annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
             ymin=-80, ymax=10, fill=phs_green, alpha=0.3)+
    geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-07-11"), y=25, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
    # Phase 3 restrictions announced - 22 Sept
    annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
             ymin=-80, ymax=10, fill="gold", alpha=0.3)+
    geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-09-25"), y=15, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
    # Local authority levels allocated - 2 Nov
    annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
             ymin=-80, ymax=10, fill=phs_purple2, alpha=0.3)+
    geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-11-05"), y=20, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
    # Boxing day lockdown - 26 Dec
    annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
             ymin=-80, ymax=10,fill=phs_red, alpha=0.3)+
    geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
    # Labels
    labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="A&E") +
    # 2018-2019 average
    geom_hline(yintercept = 0, linetype=2)+
    annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
    # WHO announcement
    geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
    # Eat out to help out
    #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-08-07"), y=-50, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
    # Lines
    geom_line(aes(x=Week_ending, y=Variation, color=Category), size=1)+
    # Points
    geom_point(aes(x=Week_ending, y=Variation, color=Category, shape=Category), size=2)+
    theme_classic()+
    # Colours of lines and points
    scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
    scale_shape_manual(demographic, values=shape_scheme)+
    #scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left") +
    scale_x_date(date_breaks = "months" , date_labels = "%b")
    
    
    
  # Emerg
  demographic_data_emerg <- subset(demographic_data, Outcome=="Emergency Hospital Admissions")
  
  p_emerg <- ggplot(demographic_data_emerg) +
    # UK Lockdown - 23 Mar 2020
    annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
             ymin=-80, ymax=10,fill=phs_red, alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-03-27"), y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
    # Phase 1 Introduced - 29 May 2020
    annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
             ymin=-80, ymax=10, fill="orange", alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-06-01"), y=15, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
    # Phase 2 introduced - 19 Jun 2020
    annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
             ymin=-80, ymax=10, fill="gold", alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-06-21"), y=20, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
    # Phase 3 introduced - 9th July
    annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
             ymin=-80, ymax=10, fill=phs_green, alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-07-11"), y=25, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
    # Phase 3 restrictions announced - 22 Sept
    annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
             ymin=-80, ymax=10, fill="gold", alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-09-25"), y=15, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
    # Local authority levels allocated - 2 Nov
    annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
             ymin=-80, ymax=10, fill=phs_purple2, alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-11-05"), y=20, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
    # Boxing day lockdown - 26 Dec
    annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
             ymin=-80, ymax=10,fill=phs_red, alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
    # Labels
    labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Emergency") +
    # 2018-2019 average
    geom_hline(yintercept = 0, linetype=2)+
    annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
    # WHO announcement
    geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
    # Eat out to help out
    #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-08-07"), y=-50, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
    # Lines
    geom_line(aes(x=Week_ending, y=Variation, color=Category), size=1)+
    # Points
    geom_point(aes(x=Week_ending, y=Variation, color=Category, shape=Category), size=2)+
    theme_classic()+
    # Colours of lines and points
    scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
    scale_shape_manual(demographic, values=shape_scheme)+
    #scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left") +
    scale_x_date(date_breaks = "months" , date_labels = "%b")
  
  # Planned
  demographic_data_planned <- subset(demographic_data, Outcome=="Planned Hospital Admissions")
  
  p_planned <- ggplot(demographic_data_planned) +
    # UK Lockdown - 23 Mar 2020
    annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
             ymin=-80, ymax=10,fill=phs_red, alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-03-27"), y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
    # Phase 1 Introduced - 29 May 2020
    annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
             ymin=-80, ymax=10, fill="orange", alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-06-01"), y=15, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
    # Phase 2 introduced - 19 Jun 2020
    annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
             ymin=-80, ymax=10, fill="gold", alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-06-21"), y=20, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
    # Phase 3 introduced - 9th July
    annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
             ymin=-80, ymax=10, fill=phs_green, alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-07-11"), y=25, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
    # Phase 3 restrictions announced - 22 Sept
    annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
             ymin=-80, ymax=10, fill="gold", alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-09-25"), y=15, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
    # Local authority levels allocated - 2 Nov
    annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
             ymin=-80, ymax=10, fill=phs_purple2, alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-11-05"), y=20, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
    # Boxing day lockdown - 26 Dec
    annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
             ymin=-80, ymax=10,fill=phs_red, alpha=0.3)+
    #geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
    # Labels
    labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Planned") +
    # 2018-2019 average
    geom_hline(yintercept = 0, linetype=2)+
    annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
    # WHO announcement
    geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
    annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
    # Eat out to help out
    #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
    #annotate("text", x=as.Date("2020-08-07"), y=-50, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
    # Lines
    geom_line(aes(x=Week_ending, y=Variation, color=Category), size=1)+
    # Points
    geom_point(aes(x=Week_ending, y=Variation, color=Category, shape=Category), size=2)+
    theme_classic()+
    # Colours of lines and points
    scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
    scale_shape_manual(demographic, values=shape_scheme)+
    #scale_linetype_manual(demographic, values=2:8)+
    theme(legend.title=element_text(size=9), 
          legend.text=element_text(size=8),
          legend.key.size = unit(0.25, "cm"),
          legend.justification="left") +
    scale_x_date(date_breaks = "months" , date_labels = "%b")
  
  # Plot together
plot_grid(p_ae, p_emerg, p_planned, align = "v", ncol=1)
  
  
  
}

demographic_variation_plot_fn("Sex")
demographic_variation_plot_fn("Age")
demographic_variation_plot_fn("SIMD")


##### Specialties #####


# Emerg
scotland_data_specialty_emerg <- subset(scotland_data_specialty, Outcome=="Emergency Hospital Admissions")

p_emerg <- ggplot(scotland_data_specialty_emerg) +
  # UK Lockdown - 23 Mar 2020
  annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Phase 1 Introduced - 29 May 2020
  annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
           ymin=-80, ymax=100, fill="orange", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-01"), y=15, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
  # Phase 2 introduced - 19 Jun 2020
  annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-21"), y=20, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
  # Phase 3 introduced - 9th July
  annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
           ymin=-80, ymax=100, fill=phs_green, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-07-11"), y=25, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
  # Phase 3 restrictions announced - 22 Sept
  annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-09-25"), y=15, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
  # Local authority levels allocated - 2 Nov
  annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
           ymin=-80, ymax=100, fill=phs_purple2, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-11-05"), y=20, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
  # Boxing day lockdown - 26 Dec
  annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Emergency") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=phs_trendcol2, size=1)+
  # Points
  geom_point(aes(x=Week_ending, y=Variation),color=phs_trendcol2, size=2)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  # WHO announcement
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
  # Eat out to help out
  #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-08-07"), y=-50, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
  theme_classic()+
  # Colours of lines and points
  #scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  #scale_shape_manual(demographic, values=shape_scheme)+
  #scale_linetype_manual(demographic, values=2:8)+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_wrap(~Specialty, ncol=4)


# Emerg
scotland_data_specialty_planned <- subset(scotland_data_specialty, Outcome=="Planned Hospital Admissions")

p_planned <- ggplot(scotland_data_specialty_planned) +
  # UK Lockdown - 23 Mar 2020
  annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Phase 1 Introduced - 29 May 2020
  annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
           ymin=-80, ymax=100, fill="orange", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-01"), y=15, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
  # Phase 2 introduced - 19 Jun 2020
  annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-21"), y=20, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
  # Phase 3 introduced - 9th July
  annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
           ymin=-80, ymax=100, fill=phs_green, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-07-11"), y=25, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
  # Phase 3 restrictions announced - 22 Sept
  annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-09-25"), y=15, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
  # Local authority levels allocated - 2 Nov
  annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
           ymin=-80, ymax=100, fill=phs_purple2, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-11-05"), y=20, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
  # Boxing day lockdown - 26 Dec
  annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Planned") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=phs_green, size=1)+
  # Points
  geom_point(aes(x=Week_ending, y=Variation),color=phs_green, size=2)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  # WHO announcement
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
  # Eat out to help out
  #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-08-07"), y=-50, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
  theme_classic()+
  # Colours of lines and points
  #scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  #scale_shape_manual(demographic, values=shape_scheme)+
  #scale_linetype_manual(demographic, values=2:8)+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_wrap(~Specialty, ncol=4)

p_emerg
p_planned
plot_grid(p_emerg, p_planned, align="v")




### NHS Health Boards ####

# A&E
scotland_data_hb_ae <- subset(scotland_data_hbs, Outcome=="A&E Attendances")

ggplot(scotland_data_hb_ae) +
  # UK Lockdown - 23 Mar 2020
  annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Phase 1 Introduced - 29 May 2020
  annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
           ymin=-80, ymax=100, fill="orange", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-01"), y=15, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
  # Phase 2 introduced - 19 Jun 2020
  annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-21"), y=20, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
  # Phase 3 introduced - 9th July
  annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
           ymin=-80, ymax=100, fill=phs_green, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-07-11"), y=25, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
  # Phase 3 restrictions announced - 22 Sept
  annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-09-25"), y=15, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
  # Local authority levels allocated - 2 Nov
  annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
           ymin=-80, ymax=100, fill=phs_purple2, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-11-05"), y=20, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
  # Boxing day lockdown - 26 Dec
  annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="A&E") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=phs_trendcol1, size=1)+
  # Points
  geom_point(aes(x=Week_ending, y=Variation),color=phs_trendcol1, size=2)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  # WHO announcement
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
  # Eat out to help out
  #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-08-07"), y=-50, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
  theme_classic()+
  # Colours of lines and points
  #scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  #scale_shape_manual(demographic, values=shape_scheme)+
  #scale_linetype_manual(demographic, values=2:8)+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_wrap(~Area_name, ncol=4)


# Emerg
scotland_data_hb_emerg <- subset(scotland_data_hbs, Outcome=="Emergency Hospital Admissions")

ggplot(scotland_data_hb_emerg) +
  # UK Lockdown - 23 Mar 2020
  annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Phase 1 Introduced - 29 May 2020
  annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
           ymin=-80, ymax=100, fill="orange", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-01"), y=15, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
  # Phase 2 introduced - 19 Jun 2020
  annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-21"), y=20, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
  # Phase 3 introduced - 9th July
  annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
           ymin=-80, ymax=100, fill=phs_green, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-07-11"), y=25, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
  # Phase 3 restrictions announced - 22 Sept
  annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-09-25"), y=15, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
  # Local authority levels allocated - 2 Nov
  annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
           ymin=-80, ymax=100, fill=phs_purple2, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-11-05"), y=20, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
  # Boxing day lockdown - 26 Dec
  annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Emergency") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=phs_trendcol2, size=1)+
  # Points
  geom_point(aes(x=Week_ending, y=Variation),color=phs_trendcol2, size=2)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  # WHO announcement
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
  # Eat out to help out
  #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-08-07"), y=-50, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
  theme_classic()+
  # Colours of lines and points
  #scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  #scale_shape_manual(demographic, values=shape_scheme)+
  #scale_linetype_manual(demographic, values=2:8)+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_wrap(~Area_name, ncol=4)


# Planned
scotland_data_hb_planned <- subset(scotland_data_hbs, Outcome=="Planned Hospital Admissions")

ggplot(scotland_data_hb_planned) +
  # UK Lockdown - 23 Mar 2020
  annotate("rect",xmin=as.Date("2020-03-23"), xmax=as.Date("2020-05-29"),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-03-23"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-27"), y=15, label="UK lockdown\n(23 Mar 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Phase 1 Introduced - 29 May 2020
  annotate("rect",xmin=as.Date("2020-05-29"), xmax=as.Date("2020-06-19"),
           ymin=-80, ymax=100, fill="orange", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-05-29"), colour="orange", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-01"), y=15, label="Phase 1 announced\n(29 May 2020)", color="orange", hjust=0, size=3, fontface =2)+
  # Phase 2 introduced - 19 Jun 2020
  annotate("rect",xmin=as.Date("2020-06-19"), xmax=as.Date("2020-07-09"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-06-19"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-06-21"), y=20, label="Phase 2 announced\n(19 Jun 2020)", color="gold2", hjust=0, size=3, fontface =2)+
  # Phase 3 introduced - 9th July
  annotate("rect",xmin=as.Date("2020-07-09"), xmax=as.Date("2020-09-22"),
           ymin=-80, ymax=100, fill=phs_green, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-07-09"), colour=phs_green, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-07-11"), y=25, label="Phase 3 announced\n(09 Jul 2020)", color=phs_green, hjust=0, size=3, fontface =2)+
  # Phase 3 restrictions announced - 22 Sept
  annotate("rect",xmin=as.Date("2020-09-22"), xmax=as.Date("2020-11-02"),
           ymin=-80, ymax=100, fill="gold", alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-09-22"), colour="gold", alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-09-25"), y=15, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="gold", hjust=0, size=3, fontface =2)+
  # Local authority levels allocated - 2 Nov
  annotate("rect",xmin=as.Date("2020-11-02"), xmax=as.Date("2020-12-26"),
           ymin=-80, ymax=100, fill=phs_purple2, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-11-02"), colour=phs_purple2, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-11-05"), y=20, label="Local authority levels allocated\n(2 Nov 2020)", color=phs_purple2, hjust=0, size=3, fontface =2)+
  # Boxing day lockdown - 26 Dec
  annotate("rect",xmin=as.Date("2020-12-26"), xmax=max(scotland_data$Week_ending),
           ymin=-80, ymax=100,fill=phs_red, alpha=0.3)+
  #geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, alpha=0.3, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Planned") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=phs_green, size=1)+
  # Points
  geom_point(aes(x=Week_ending, y=Variation),color=phs_green, size=2)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  # WHO announcement
  geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=-17, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
  # Eat out to help out
  #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-08-07"), y=-50, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
  theme_classic()+
  # Colours of lines and points
  #scale_color_manual(demographic,values=c(phs_trendcol1, phs_trendcol2, phs_green, phs_main, phs_purple2, phs_gold, phs_red))+
  #scale_shape_manual(demographic, values=shape_scheme)+
  #scale_linetype_manual(demographic, values=2:8)+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_wrap(~Area_name, ncol=4)




