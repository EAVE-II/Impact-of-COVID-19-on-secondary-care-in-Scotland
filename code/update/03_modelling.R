######################################################################
## Title: 
## Short title: Impact of COVID-19 on secondary care in Scotland (updated)
## DOI: 
## Code author: Rachel H. Mulholland <Rachel.Mulholland@ed.ac.uk>
## Description: 03_modelling: ITSA modelling
######################################################################

#### 0 - Load in data and functions ####

# Functions
source("./code/update/00_functions.R")
# Data
source("./code/update/00_data_setup.R")

# Using start of 'Phase 3 restrictions' as changepoint
# Date: 22nd September 2020
# Look at post lockdown only

##### 1 - Total outcomes ####
# Looking at each outcome overall

#### 1.1 - Baseline models per outcome ####

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
  geom_point(aes(x=est, y= Outcome, col=coef_name), size=2.5) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= Outcome, col=coef_name), width=0, size=0.75)+
  geom_text(aes(x=est, y=Outcome, label=Label, col=coef_name), vjust=-1, fontface=2, size=3) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual("BA",values=c(eave_green, eave_orange)) +
  labs(x="Estimate (95% CI)")

p_diff_ests

png(width=700, height=300,filename = "./outputs/ITSA_diff_ests.png")
p_diff_ests

dev.off()

## Goodness of fit
z_1[[4]]
z_2[[4]]
z_3[[4]]


## Plot ITSA with estimates
png(width=1100, height=800,filename = "./outputs/ITSA_plot_ests.png")
plot_grid(p_itsa, p_diff_ests, align = "v", ncol=1, labels = "AUTO", rel_heights = c(1,1))

dev.off()



#### 1.2 - Total outcomes - 3 way interaction ####
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


#### 2 - Demographic modelling ####

#### 2.1 - Finding model for each demographic and outcome ####
# demographic_models_fn from 00_functions.R

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

## Sex
demographic_models_fn("Sex", "A&E Attendances", "2020-09-22", T, T) # Baseline model has lowest AIC and BIC 
demographic_models_fn("Sex", "Emergency Hospital Admissions", "2020-09-22", T, T) # Baseline model has lowest AIC and BIC 
demographic_models_fn("Sex", "Planned Hospital Admissions", "2020-09-22", T, T) # Baseline model has lowest AIC and BIC 
# No significant differences between sex

## Age
demographic_models_fn("Age", "A&E Attendances", "2020-09-22", T, T) # Model 1 has lowest AIC and BIc
demographic_models_fn("Age", "Emergency Hospital Admissions", "2020-09-22", T, T) # Three way interaction has lowest AIC and significant F-test pvalue
demographic_models_fn("Age", "Planned Hospital Admissions", "2020-09-22", T, T) # Model 3 has lowest AIC and BIC
# Some differences between age group - requires further investigation

## SIMD
demographic_models_fn("SIMD", "A&E Attendances", "2020-09-22", T, T) # Baseline model has lowest AIC and BIC 
demographic_models_fn("SIMD", "Emergency Hospital Admissions", "2020-09-22", T, T) # Model 1 has lowest AIC and BIC
demographic_models_fn("SIMD", "Planned Hospital Admissions", "2020-09-22", T, T) # Baseline model has lowest AIC and BIC 
# No significant differences between SIMDs, possibly something in emergency hosp admissions that needs investigated further


#### 2.2 - Fitting optimum model per demographic and outcome ####
# demographic_alternative_model_fn from 00_functions.R 

### Age

# Age model for A&E Attendances (Model 3)
age_ae_model <- demographic_alternative_model_fn("Age", "A&E Attendances", 1, as.Date("2020-09-22"), T, T, F)
# Age model for Emergency Hospital Admissions (Model 3)
age_emerg_model <- demographic_alternative_model_fn("Age", "Emergency Hospital Admissions", 3, as.Date("2020-09-22"), T, T, F)
# Age model for Planned Hospital Admissions (Model 3)
age_planned_model <- demographic_alternative_model_fn("Age", "Planned Hospital Admissions", 3, as.Date("2020-09-22"), T, T, F)



## Plots for A&E 
# ITSA predictions
p_age_ae_itsa <- ggplot(data.frame(age_ae_model[5]), aes(x=Date, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Date, y=Predict, col=Category), size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = " ", y =" ")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_grid(~Outcome) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.95),
        legend.direction =  "horizontal")


# Estimates
p_age_ae_ests <- ggplot(age_ae_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual("BA",values=c(eave_green, eave_orange)) +
  labs(x="Estimate (95% CI)")

# Plot together
p_age_ae <- plot_grid(p_age_ae_itsa, p_age_ae_ests, ncol=1)
p_age_ae



## Plots for Emerg 
# ITSA predictions
p_age_emerg_itsa <- ggplot(data.frame(age_emerg_model[5]), aes(x=Date, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Date, y=Predict, col=Category), size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = " ", y =" ")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_grid(~Outcome) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.95),
        legend.direction =  "horizontal")


# Estimates
p_age_emerg_ests <- ggplot(age_emerg_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual("BA",values=c(eave_green, eave_orange)) +
  labs(x="Estimate (95% CI)")

# Plot together
p_age_emerg <- plot_grid(p_age_emerg_itsa, p_age_emerg_ests, ncol=1)
p_age_emerg


## Plots for Planned 
# ITSA predictions
p_age_planned_itsa <- ggplot(data.frame(age_planned_model[5]), aes(x=Date, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Date, y=Predict, col=Category), size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = " ", y =" ")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_grid(~Outcome) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.95),
        legend.direction =  "horizontal")


# Estimates
p_age_planned_ests <- ggplot(age_planned_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual("BA",values=c(eave_green, eave_orange)) +
  labs(x="Estimate (95% CI)")

# Plot together
p_age_planned <- plot_grid(p_age_planned_itsa, p_age_planned_ests, ncol=1)
p_age_planned



#### 3 - Specialty modelling ####

#### 3.1 - Finding model for each Specialty and outcome ####
# demographic_models_fn from 00_functions.R

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

## Find which model fits best
demographic_models_fn("Specialty", "Emergency Hospital Admissions", "2020-09-22", T, T) # Baseline model has lowest AIC and BIC 
demographic_models_fn("Specialty", "Planned Hospital Admissions", "2020-09-22", T, T) # Baseline model has lowest AIC and BIC 


#### 3.2 - Fitting optimum model per Specialty and outcome ####
# demographic_alternative_model_fn from 00_functions.R 

# Specialty model for Emergency Hospital Admissions (Model 3)
specialty_emerg_model <- demographic_alternative_model_fn("Specialty", "Emergency Hospital Admissions", 3, as.Date("2020-09-22"), T, T, F)
# Specialty model for Planned Hospital Admissions (Model 3)
specialty_planned_model <- demographic_alternative_model_fn("Specialty", "Planned Hospital Admissions", 3, as.Date("2020-09-22"), T, T, F)



## Plots for Emerg 
# ITSA predictions
p_spec_emerg_itsa <- ggplot(data.frame(specialty_emerg_model[5]), aes(x=Date, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Date, y=Predict, col=Category), size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = " ", y =" ")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_grid(~Outcome) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.95),
        legend.direction =  "horizontal")


# Estimates
p_spec_emerg_ests <- ggplot(specialty_emerg_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual("BA",values=c(eave_green, eave_orange)) +
  labs(x="Estimate (95% CI)")

# Plot together
p_spec_emerg <- plot_grid(p_spec_emerg_itsa, p_spec_emerg_ests, ncol=1)
p_spec_emerg


## Plots for Planned 
# ITSA predictions
p_spec_planned_itsa <- ggplot(data.frame(specialty_planned_model[5]), aes(x=Date, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Date, y=Predict, col=Category), size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = " ", y =" ")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_grid(~Outcome) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.95),
        legend.direction =  "horizontal")


# Estimates
p_spec_planned_ests <- ggplot(specialty_planned_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual("BA",values=c(eave_green, eave_orange)) +
  labs(x="Estimate (95% CI)")

# Plot together
p_spec_planned <- plot_grid(p_spec_planned_itsa, p_spec_planned_ests, ncol=1)
p_spec_planned
