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
p_itsa <- ggplot(outcome_predictions)+
  # Fitted lines and 95% CI
  geom_line(aes(x=Date, y=Predict), size=1, col=eave_blue)+
  geom_ribbon(aes(x=Date, ymin =Lwr, ymax =Upr, fill = Outcome), alpha = .15, fill=eave_blue)+
  theme_classic()+
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average") +
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  # Points
  geom_point(aes(x=Date, y=Variation), size=2, col=eave_blue)+
  theme(legend.position = "bottom") +
  #theme(panel.border=element_blank(), axis.line=element_line())
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_grid(~Outcome) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

p_itsa


# Save
png(width=1100, height=400,filename = "./outputs/ITSA.png")
p_itsa

dev.off()



## Estimates
outcome_estimates <- bind_rows(z_1[[3]], z_2[[3]], z_3[[3]])

# Getting labels - estimate (95% CI: lwr, upr)
outcome_estimates_labels <- outcome_estimates
outcome_estimates_labels$Label <- paste(round(outcome_estimates$est,1), " (95% CI: ",
                                        round(outcome_estimates$lwr,1), " to ",
                                        round(outcome_estimates$upr,1), ")", sep="")

# Plot estimates with 95% CIs
p_ests <- ggplot(outcome_estimates_labels) +
  geom_point(aes(x=est, y= Outcome, col= BA, shape=BA), size=2.5) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= Outcome, col= BA), width=0, size=0.75)+
  geom_text(aes(x=est, y=Outcome, label=Label, col=BA), vjust=-1, fontface=2, size=3) +
  facet_grid(.~Coeff_type, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "bottom")+
  scale_color_manual("BA",values=c(eave_blue2, eave_orange)) +
  labs(x="Estimate (95% CI)")

p_ests

png(width=700, height=300,filename = "./outputs/ITSA_ests.png")
p_ests

dev.off()


# Difference estimates
outcome_diff_estimates <- bind_rows(z_1[[2]], z_2[[2]], z_3[[2]])

outcome_diff_estimates$Label <- paste(round(outcome_diff_estimates$est,1), " (95% CI: ",
                                        round(outcome_diff_estimates$lwr,1), " to ",
                                        round(outcome_diff_estimates$upr,1), ")", sep="")

p_diff_ests <- ggplot(outcome_diff_estimates) +
  geom_point(aes(x=est, y= Outcome), size=2.5, col=eave_blue2) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= Outcome), width=0, size=0.75, col=eave_blue2)+
  geom_text(aes(x=est, y=Outcome, label=Label), col=eave_blue2, vjust=-1, fontface=2, size=3) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "bottom")+
  scale_color_manual("BA",values=c(eave_blue2, eave_orange)) +
  labs(x="Estimate (95% CI)")

p_diff_ests

png(width=700, height=300,filename = "./outputs/ITSA_diff_ests.png")
p_diff_ests

dev.off()

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

