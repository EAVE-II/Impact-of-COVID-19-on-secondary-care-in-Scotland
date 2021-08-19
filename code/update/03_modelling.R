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
## 1 change-point
# Obtain predictions and estimates
z_1 <- baseline_model_fn(data = scotland_data, outcome = "A&E Attendances",
                         postld = T, changepoint = c("2020-09-22"), weighted = F, diag_plots = T)
z_1[[2]]

z_2 <- baseline_model_fn(data = scotland_data, outcome = "Emergency Hospital Admissions",
                         postld = T, changepoint = c("2020-09-22"), weighted = F, diag_plots = T)
z_2[[2]]

z_3 <- baseline_model_fn(data = scotland_data, outcome = "Planned Hospital Admissions",
                         postld = T, changepoint = c("2020-09-22"), weighted = F, diag_plots = T)
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
  #annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  geom_text(data = data.frame(Week_ending = as.Date("2020-09-26"),
                              text = "Restrictions announced\n(22 Sep 2020)",
                              Predict = c(5),
                              Outcome = "A&E Attendances"),
            aes(x=Week_ending, y=Predict, label = text), hjust=0, size=3, col="firebrick1", fontface=2) +
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
outcome_estimates_labels$Label <- paste(outcome_estimates$est, " (95% CI: ",
                                        outcome_estimates$lwr, " to ",
                                        outcome_estimates$upr, ")", sep="")
outcome_estimates_labels$vjust_tp <- ifelse(outcome_estimates_labels$BA=="Before", -1, 2)


# Plot estimates with 95% CIs
p_ests <- ggplot(outcome_estimates_labels) +
  geom_point(aes(x=est, y= Outcome, col= BA, shape=BA), size=2.5) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= Outcome, col= BA), width=0, size=0.75)+
  geom_text(aes(x=est, y=Outcome, label=Label, col=BA, vjust = vjust_tp),fontface=2, size=3) +
  facet_grid(.~Coeff_type, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.97),
        legend.direction =  "horizontal") + 
  scale_color_manual(name = "Time period",values=c(eave_green, "firebrick1")) +
  scale_shape_manual(name = "Time period",values=c(19,15)) +
  labs(x="Estimate (95% CI)", y="Age group") 

p_ests

png(width=1300, height=500,filename = "./outputs/ITSA_ests.png")
p_ests

dev.off()


# Difference estimates
outcome_diff_estimates <- bind_rows(z_1[[2]], z_2[[2]], z_3[[2]])

outcome_diff_estimates$Label <- paste(signif(outcome_diff_estimates$est,3), " (95% CI: ",
                                      signif(outcome_diff_estimates$lwr,3), " to ",
                                      signif(outcome_diff_estimates$upr,3), ")", sep="")

p_diff_ests <- ggplot(outcome_diff_estimates) +
  geom_point(aes(x=est, y= Outcome), size=2.5, col=eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= Outcome, col=coef_name), width=0, size=0.75, col=eave_blue)+
  geom_text(aes(x=est, y=Outcome, label=Label, col=coef_name), vjust=-1, fontface=2, size=3, col=eave_blue) +
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

## Diagnostic plots
png(width=900, height=500,filename = "./outputs/ITSA_ae_diags.png")
z_1[[5]]
dev.off()


png(width=900, height=500,filename = "./outputs/ITSA_emerg_diags.png")
z_2[[5]]
dev.off()

png(width=900, height=500,filename = "./outputs/ITSA_planned_diags.png")
z_3[[5]]
dev.off()

## Plot ITSA with estimates
png(width=1100, height=800,filename = "./outputs/ITSA_plot_ests.png")
plot_grid(p_itsa, p_diff_ests, align = "v", ncol=1, labels = "AUTO", rel_heights = c(1,1))

dev.off()

png(width=1100, height=800,filename = "./outputs/ITSA_ests.png")
plot_grid(p_diff_ests, p_ests, align = "v", ncol=1, labels = "AUTO", rel_heights = c(1,1))

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
demographic_models_fn("Sex", "A&E Attendances", "2020-09-22", T, F) # Baseline model has lowest AIC and BIC 
demographic_models_fn("Sex", "Emergency Hospital Admissions", "2020-09-22", T, F) # Baseline model has lowest AIC and BIC 
demographic_models_fn("Sex", "Planned Hospital Admissions", "2020-09-22", T, F) # Baseline model has lowest AIC and BIC 
# No significant differences between sex

## Age
demographic_models_fn("Age", "A&E Attendances", "2020-09-22", T, F) # Three way interaction
demographic_models_fn("Age", "Emergency Hospital Admissions", "2020-09-22", T, F) # Three way interaction has lowest AIC and significant F-test pvalue
demographic_models_fn("Age", "Planned Hospital Admissions", "2020-09-22", T, F) # Three way interaction has lowest AIC and significant F-test pvalue
# Some differences between age group - requires further investigation

## SIMD
demographic_models_fn("SIMD", "A&E Attendances", "2020-09-22", T, F) # Baseline model has lowest AIC and BIC 
demographic_models_fn("SIMD", "Emergency Hospital Admissions", "2020-09-22", T, F) # Model 1 has lowest AIC and BIC
demographic_models_fn("SIMD", "Planned Hospital Admissions", "2020-09-22", T, F) # Baseline model has lowest AIC and BIC 
# No significant differences between SIMDs, possibly something in emergency hosp admissions that needs investigated further


#### 2.2 - Age models ####
# demographic_alternative_model_fn from 00_functions.R 

# Age model for A&E Attendances (Model 3)
age_ae_model <- demographic_alternative_model_fn("Age", "A&E Attendances", 5, as.Date("2020-09-22"), T, F, T)
# Age model for Emergency Hospital Admissions (Model 3)
age_emerg_model <- demographic_alternative_model_fn("Age", "Emergency Hospital Admissions", 5, as.Date("2020-09-22"), T, F, T)
# Age model for Planned Hospital Admissions (Model 3)
age_planned_model <- demographic_alternative_model_fn("Age", "Planned Hospital Admissions", 5, as.Date("2020-09-22"), T, F, T)



## Plots for A&E 
# ITSA predictions
p_age_ae_itsa <- ggplot(data.frame(age_ae_model[5]), aes(x=Date, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Date, y=Predict, col=Category), size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = " ", y ="% change from 2018-2019 average")+
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


# Difference before and after estimates
write.csv(age_ae_model[[4]], file = "./outputs/ae_diff_ests.csv", row.names = F)

p_age_ae_diff_ests <- ggplot(age_ae_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Estimate (95% CI)", y="Age group")


p_age_ae_diff_ests

# Estimates
p_age_ae_ests <- age_ae_model[[3]] %>%
  mutate(time_periods = factor(time_periods, levels=c("Before", "After"))) %>%
  mutate(vjust_tp = ifelse(time_periods=="Before", -1, 2)) %>%
  ggplot() +
  geom_point(aes(x=est, y= categories, shape = time_periods, col = time_periods), size=2.5) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories, col = time_periods), width=0, size=0.75)+
  geom_text(aes(x=est, y=categories, label=Label, col = time_periods, vjust = vjust_tp), fontface=2, size=3) +
  facet_grid(~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.97),
        legend.direction =  "horizontal") + 
  scale_color_manual(name = "Time period",values=c(eave_green, "firebrick1")) +
  scale_shape_manual(name = "Time period",values=c(19,15)) +
  labs(x="Estimate (95% CI)", y="Age group") 
p_age_ae_ests


# Plot estimates together
p_age_ae <- plot_grid(p_age_ae_diff_ests, p_age_ae_ests, ncol=1, labels="AUTO")
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
  #annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  #annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  #annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_grid(~Outcome) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.position = "none")



# Difference before and after estimates
write.csv(age_emerg_model[[4]], file = "./outputs/emerg_diff_ests.csv", row.names = F)

p_age_emerg_diff_ests <- ggplot(age_emerg_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Estimate (95% CI)", y="Age group")


p_age_emerg_diff_ests

# Estimates
p_age_emerg_ests <- age_emerg_model[[3]] %>%
  mutate(time_periods = factor(time_periods, levels=c("Before", "After"))) %>%
  mutate(vjust_tp = ifelse(time_periods=="Before", -1, 2)) %>%
  ggplot() +
  geom_point(aes(x=est, y= categories, shape = time_periods, col = time_periods), size=2.5) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories, col = time_periods), width=0, size=0.75)+
  geom_text(aes(x=est, y=categories, label=Label, col = time_periods, vjust = vjust_tp), fontface=2, size=3) +
  facet_grid(~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.97),
        legend.direction =  "horizontal") + 
  scale_color_manual(name = "Time period",values=c(eave_green, "firebrick1")) +
  scale_shape_manual(name = "Time period",values=c(19,15)) +
  labs(x="Estimate (95% CI)", y="Age group") 
p_age_emerg_ests


# Plot estimates together
p_age_emerg <- plot_grid(p_age_emerg_diff_ests, p_age_emerg_ests, ncol=1, labels="AUTO")
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
  #annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  #annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  #annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  scale_color_manual("Age",values=col_scheme)+
  scale_shape_manual("Age", values=shape_scheme)+
  scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_grid(~Outcome) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.position = "none")

# Difference before and after estimates
write.csv(age_planned_model[[4]], file = "./outputs/planned_diff_ests.csv", row.names = F)

p_age_planned_diff_ests <- ggplot(age_planned_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Estimate (95% CI)", y="Age group")


p_age_planned_diff_ests

# Estimates
p_age_planned_ests <- age_planned_model[[3]] %>%
  mutate(time_periods = factor(time_periods, levels=c("Before", "After"))) %>%
  mutate(vjust_tp = ifelse(time_periods=="Before", -1, 2)) %>%
  ggplot() +
  geom_point(aes(x=est, y= categories, shape = time_periods, col = time_periods), size=2.5) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories, col = time_periods), width=0, size=0.75)+
  geom_text(aes(x=est, y=categories, label=Label, col = time_periods, vjust = vjust_tp), fontface=2, size=3) +
  facet_grid(~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.97),
        legend.direction =  "horizontal") + 
  scale_color_manual(name = "Time period",values=c(eave_green, "firebrick1")) +
  scale_shape_manual(name = "Time period",values=c(19,15)) +
  labs(x="Estimate (95% CI)", y="Age group") 
p_age_planned_ests


# Plot estimates together
p_age_planned <- plot_grid(p_age_planned_diff_ests, p_age_planned_ests, ncol=1, labels="AUTO")
p_age_planned



## Age plots side by side
png(width=800, height=1000,filename = "./outputs/ITSA_age.png")
plot_grid(p_age_ae_itsa, p_age_emerg_itsa, p_age_planned_itsa, labels = "AUTO", ncol=1)
dev.off()

png(width=600, height=1000,filename = "./outputs/ITSA_age_ae_estimates.png")
p_age_ae
dev.off()

png(width=1600, height=1000,filename = "./outputs/ITSA_age_estimates.png")
plot_grid(p_age_ae_diff_ests, p_age_emerg_diff_ests, p_age_planned_diff_ests,
          p_age_ae_ests,p_age_emerg_ests, p_age_planned_ests,
          labels = "AUTO", ncol=3)

dev.off()


## Diagnostic plots
png(width=900, height=1000,filename = "./outputs/ITSA_age_ae_diags.png")
age_ae_model[[6]]
dev.off()


png(width=900, height=1000,filename = "./outputs/ITSA_age_emerg_diags.png")
age_emerg_model[[6]]
dev.off()

png(width=900, height=1000,filename = "./outputs/ITSA_age_planned_diags.png")
age_planned_model[[6]]
dev.off()




#### 2.3 - SIMD model ####
# demographic_alternative_model_fn from 00_functions.R 

simd_emerg_model <- demographic_alternative_model_fn("SIMD", "Emergency Hospital Admissions", 1, as.Date("2020-09-22"), T, T, F)

## Plots for Emerg 
# ITSA predictions
p_simd_emerg_itsa <- ggplot(data.frame(simd_emerg_model[5]), aes(x=Date, y=Variation))+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr, fill = Category, color = NULL), alpha = .15)+
  geom_line(aes(x=Date, y=Predict, col=Category), size=0.75)+
  geom_point(aes(shape=Category, col=Category), size=2)+
  theme_classic()+
  labs(x = " ", y ="% change from 2018-2019 average")+
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  scale_color_manual("Deprivation quintile",values=col_scheme)+
  scale_shape_manual("Deprivation quintile", values=shape_scheme)+
  scale_fill_manual("Deprivation quintile",values=col_scheme)+
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



# Difference before and after estimates
p_simd_emerg_diff_ests <- ggplot(simd_emerg_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  labs(x="Estimate (95% CI)", y="Deprivation quintile")


p_simd_emerg_diff_ests

# Estimates
p_simd_emerg_ests <- simd_emerg_model[[3]] %>%
  mutate(time_periods = factor(time_periods, levels=c("Before", "After"))) %>%
  mutate(vjust_tp = ifelse(time_periods=="Before", -1, 2)) %>%
  ggplot() +
  geom_point(aes(x=est, y= categories, shape = time_periods, col = time_periods), size=2.5) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories, col = time_periods), width=0, size=0.75)+
  geom_text(aes(x=est, y=categories, label=Label, col = time_periods, vjust = vjust_tp), fontface=2, size=3) +
  facet_grid(~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.97),
        legend.direction =  "horizontal") + 
  scale_color_manual(name = "Time period",values=c(eave_green, "firebrick1")) +
  scale_shape_manual(name = "Time period",values=c(19,15)) +
  labs(x="Estimate (95% CI)", y="Deprivation quintile") 
p_simd_emerg_ests


# Plot estimates together
p_simd_emerg <- plot_grid(p_simd_emerg_diff_ests, p_simd_emerg_ests, ncol=1, labels="AUTO")
p_simd_emerg

# Plot altogether for supplement
png(width=600, height=1200,filename = "./outputs/ITSA_simd.png")
plot_grid(p_simd_emerg_itsa, p_simd_emerg_diff_ests, p_simd_emerg_ests,
          labels = "AUTO", ncol=1)

dev.off()




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
demographic_models_fn("Specialty", "Emergency Hospital Admissions", "2020-09-22", T, F) # 3 way interaction has lowest AIC 
demographic_models_fn("Specialty", "Planned Hospital Admissions", "2020-09-22", T, F) # 3 way interaction has lowest AIC 


#### 3.2 - Fitting optimum model per Specialty and outcome ####
# demographic_alternative_model_fn from 00_functions.R 

# Specialty model for Emergency Hospital Admissions (Model 3)
specialty_emerg_model <- demographic_alternative_model_fn("Specialty", "Emergency Hospital Admissions", 5, as.Date("2020-09-22"), T, F, T)
# Specialty model for Planned Hospital Admissions (Model 3)
specialty_planned_model <- demographic_alternative_model_fn("Specialty", "Planned Hospital Admissions", 5, as.Date("2020-09-22"), T, F, T)



## Plots for Emerg 
# ITSA predictions
p_spec_emerg_itsa <- ggplot(data.frame(specialty_emerg_model[5]), aes(x=Date, y=Variation))+
  geom_point(size=1.5, col=eave_blue)+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr), alpha = .25, fill = eave_blue)+
  geom_line(aes(x=Date, y=Predict), size=0.75, col=eave_blue)+
  theme_classic()+
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average")+
  geom_hline(yintercept = 0, linetype=2)+
  #annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  #annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  #annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  #scale_color_manual("Age",values=col_scheme)+
  #scale_shape_manual("Age", values=shape_scheme)+
  #scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_wrap(~Category, ncol=4) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme(axis.text=element_text(size=8, angle = 45))

p_spec_emerg_itsa

# Estimates
p_spec_emerg_diff_ests <- ggplot(specialty_emerg_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual("BA",values=c(eave_green, eave_orange)) +
  labs(x="Estimate (95% CI)", y="Specialty")


# Estimates
p_spec_emerg_ests <- specialty_emerg_model[[3]] %>%
  mutate(time_periods = factor(time_periods, levels=c("Before", "After"))) %>%
  mutate(vjust_tp = ifelse(time_periods=="Before", -1, 2)) %>%
  ggplot() +
  geom_point(aes(x=est, y= categories, shape = time_periods, col = time_periods), size=2.5) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories, col = time_periods), width=0, size=0.75)+
  geom_text(aes(x=est, y=categories, label=Label, col = time_periods, vjust = vjust_tp), fontface=2, size=3) +
  facet_grid(~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.97),
        legend.direction =  "horizontal") + 
  scale_color_manual(name = "Time period",values=c(eave_green, "firebrick1")) +
  scale_shape_manual(name = "Time period",values=c(19,15)) +
  labs(x="Estimate (95% CI)", y="Specialty") 
p_spec_emerg_ests



## Plots for Planned 
# ITSA predictions
p_spec_planned_itsa <- ggplot(data.frame(specialty_planned_model[5]), aes(x=Date, y=Variation))+
  geom_point(size=1.5, col=eave_green)+
  geom_ribbon( aes(ymin =Lwr, ymax =Upr), alpha = .25, fill = eave_green)+
  geom_line(aes(x=Date, y=Predict), size=0.75, col=eave_green)+
  theme_classic()+
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average")+
  geom_hline(yintercept = 0, linetype=2)+
  #annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  #annotate("text", x=as.Date("2021-01-04"), y=2, label="2021", hjust=0, size=3) +
  # White background for change-point
  geom_vline(xintercept = as.Date("2020-09-22"), colour="white", linetype=1, size=1)+
  # Change-point 1
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  #annotate("text", x=as.Date("2020-09-25"), y=5, label="Phase 3 restrictions announced\n(22 Sep 2020)", color="firebrick1", hjust=0, size=3.5, fontface =2)+
  #scale_color_manual("Age",values=col_scheme)+
  #scale_shape_manual("Age", values=shape_scheme)+
  #scale_fill_manual("Age",values=col_scheme)+
  #facet_wrap(~Category) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_wrap(~Category, ncol=4) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(axis.text=element_text(size=8, angle = 45))

p_spec_planned_itsa

# Estimates
p_spec_planned_diff_ests <- ggplot(specialty_planned_model[[4]]) +
  geom_point(aes(x=est, y= categories), size=2.5, col = eave_blue) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories), width=0, size=0.75, col = eave_blue)+
  geom_text(aes(x=est, y=categories, label=Label), vjust=-1, fontface=2, size=3, col = eave_blue) +
  facet_grid(.~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.position = "none")+
  scale_color_manual("BA",values=c(eave_green, eave_orange)) +
  labs(x="Estimate (95% CI)", y="Specialty")

# Estimates
p_spec_planned_ests <- specialty_planned_model[[3]] %>%
  mutate(time_periods = factor(time_periods, levels=c("Before", "After"))) %>%
  mutate(vjust_tp = ifelse(time_periods=="Before", -1, 2)) %>%
  ggplot() +
  geom_point(aes(x=est, y= categories, shape = time_periods, col = time_periods), size=2.5) +
  geom_errorbar(aes(xmin=lwr, xmax=upr, y= categories, col = time_periods), width=0, size=0.75)+
  geom_text(aes(x=est, y=categories, label=Label, col = time_periods, vjust = vjust_tp), fontface=2, size=3) +
  facet_grid(~coef_name, scales = "free") +
  geom_vline(xintercept = 0, linetype=2) +
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left",
        legend.position = c(0.01,0.97),
        legend.direction =  "horizontal") + 
  scale_color_manual(name = "Time period",values=c(eave_green, "firebrick1")) +
  scale_shape_manual(name = "Time period",values=c(19,15)) +
  labs(x="Estimate (95% CI)", y="Specialty") 


# Plot together
p_spec_planned <- plot_grid(p_spec_planned_itsa, p_spec_planned_ests, ncol=1)
p_spec_planned


## Output
png(width=800, height=700,filename = "./outputs/ITSA_spec.png")
plot_grid(p_spec_emerg_itsa, p_spec_planned_itsa, labels = "AUTO", ncol=1)
dev.off()


png(width=1200, height=1000,filename = "./outputs/ITSA_spec_estimates.png")
plot_grid(p_spec_emerg_diff_ests, p_spec_planned_diff_ests, p_spec_emerg_ests,
          p_spec_planned_ests,
          labels = "AUTO", ncol=2)

dev.off()



## Diagnostic plots
png(width=900, height=1200,filename = "./outputs/ITSA_spec_emerg_diags.png")
specialty_emerg_model[[6]]
dev.off()


png(width=900, height=1000,filename = "./outputs/ITSA_spec_planned_diags.png")
specialty_planned_model[[6]]
dev.off()



