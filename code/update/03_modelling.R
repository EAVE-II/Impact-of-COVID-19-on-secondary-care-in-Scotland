######################################################################
## Title: 
## Short title: Impact of COVID-19 on secondary care in Scotland (updated)
## DOI: 
## Code author: Rachel H. Mulholland <Rachel.Mulholland@ed.ac.uk>
## Description: 03_modelling: ITSA modelling
######################################################################

#### 1 - Load in data and functions ####

# Functions
source("./code/update/00_functions.R")
# Data
source("./code/update/00_data_setup.R")

# Using start of 'Phase 3 restrictions' as changepoint
# Date: 22nd September 2020
# Look at post lockdown only

##### 3.1 - Total outcomes ####

# Use 'baseline_model_fn' from 'updated_functions.R' to get information on all outcomes
## 2 change-point
# Obtain predictions and estimates
z_1 <- baseline_model_fn(data = scotland_data, outcome = "A&E Attendances",
                         postld = T, changepoint = c("2020-09-22", "2020-12-26"), weighted = T, diag_plots = F)
z_1[[2]]

z_2 <- baseline_model_fn(data = scotland_data, outcome = "Emergency Hospital Admissions",
                         postld = T, changepoint = c("2020-09-22", "2020-12-26"), weighted = T, diag_plots = F)
z_2[[2]]

z_3 <- baseline_model_fn(data = scotland_data, outcome = "Planned Hospital Admissions",
                         postld = T, changepoint = c("2020-09-22", "2020-12-26"), weighted = T, diag_plots = F)
z_3[[2]]

## 1 change-point
# Obtain predictions and estimates
z_1 <- baseline_model_fn(data = scotland_data, outcome = "A&E Attendances",
                         postld = T, changepoint = c("2020-09-22"), weighted = T, diag_plots = F)
z_1[[2]]

z_2 <- baseline_model_fn(data = scotland_data, outcome = "Emergency Hospital Admissions",
                         postld = T, changepoint = c("2020-09-22"), weighted = T, diag_plots = F)
z_2[[2]]

z_3 <- baseline_model_fn(data = scotland_data, outcome = "Planned Hospital Admissions",
                         postld = T, changepoint = c("2020-09-22"), weighted = T, diag_plots = F)
z_3[[2]]

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
  annotate("text", x=as.Date("2021-03-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour=phs_purple, linetype=2, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=10, label="Phase 3 restrictions announced\n(22 Sep 2020)", color=phs_purple, hjust=0, size=3.5, fontface =2)+
  # Boxing day lockdown - 26 Dec
  #geom_vline(xintercept = as.Date("2020-12-26"), colour=phs_red, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-12-29"), y=25, label="Nationwide lockdown\n(26 Dec 2020)", color=phs_red, hjust=0, size=3, fontface =2)+
  # Colours of lines and points
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_main, phs_green))+
  scale_fill_manual("Outcome",values=c(phs_trendcol1, phs_main, phs_green))+
  #scale_linetype_manual("Outcome", values=2:4) +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "months" , date_labels = "%b")


## Estimates
outcome_estimates <- bind_rows(z_1[[3]], z_2[[3]], z_3[[3]])

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
  ylim(c(min(outcome_estimates_int$lwr)-25, max(outcome_estimates_int$upr)+25)) +
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
  ylim(c(min(outcome_estimates_slopes$lwr)-0.2, max(outcome_estimates_slopes$upr)+0.2)) +
  geom_text(data=outcome_estimates_labels_slopes, label=outcome_estimates_labels_slopes$Label,
            size=3, fontface = "bold", position = position_nudge(x = -0.25))


gridExtra::grid.arrange(p_int, p_slope)


## Goodness of fit
z_1[[4]]
z_2[[4]]
z_3[[4]]


#### 3.2 - Total outcomes - 3 way interaction ####
# This compares across the three outcomes
changepoint <- "2020-09-22"


scotland_data_postld <- scotland_data %>%
  filter(BA_Pandemic_Lockdown == "After") %>%
  mutate(BA = factor(case_when(Week_ending < as.Date(changepoint) ~ "Before",
                               TRUE ~ "After"),
                     levels = c("Before", "After")))


baseline_model3 <- lm(Variation ~ Outcome*No_days*BA, data=scotland_data_postld, weights = Count)
summary(baseline_model3)

anova(baseline_model3)


#### 3.3 - Demographic modelling ####

# demographic_models_fn from updated_functions
# Input:
# - demographic: Demographic of interest (Age, Sex, SIMD)
# - outcome: Outcome of interest ("Planned Hospital Admissions","Emergency Hospital Admissions" or "A&E Attendances")
# - changepoint: The change-point of interest (a date)
# - postld: Whether or not data should be subsetted to post lockdown only (T/F)
# - weighted: Whether or not the model should be weighted to the Count (T/F)

# Output:
# 1- Summary of three way interaction (i.e. No_days*BA*Category)
# 2 - AIC of all model combinations
# 3 - BIC of all model combinations#

# Combinations:
# 0 - No_days*BA
# 1 - No_days*BA+Category
# 2 - No_days*BA+Category*No_days
# 3 - No_days*BA+Category*BA
# 4 - No_days*BA+Category*No_days+Category*BA

demographic_models_fn("Sex", "A&E Attendances", "2020-09-22", T, T)
demographic_models_fn("Sex", "Emergency Hospital Admissions", "2020-09-22", T, T)
demographic_models_fn("Sex", "Planned Hospital Admissions", "2020-09-22", T, T)

demographic_models_fn("Age", "A&E Attendances")
demographic_models_fn("Age", "Emergency Hospital Admissions")
demographic_models_fn("Age", "Planned Hospital Admissions")

demographic_models_fn("SIMD", "A&E Attendances")
demographic_models_fn("SIMD", "Emergency Hospital Admissions")
demographic_models_fn("SIMD", "Planned Hospital Admissions")

