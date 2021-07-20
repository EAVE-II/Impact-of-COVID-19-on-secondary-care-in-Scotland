######################################################################
## Title: 
## Short title: Impact of COVID-19 on secondary care in Scotland (updated)
## DOI: 
## Code author: Rachel H. Mulholland <Rachel.Mulholland@ed.ac.uk>
## Description: 02_descriptive_analysis: Descriptive analyses
######################################################################

#### 1 - Load in data and functions ####

# Functions
source("./code/update/00_functions.R")
# Data
source("./code/update/00_data_setup.R")


#### 2 - Total outcome plots ####

## Counts
p_count <- bind_rows(scotland_data %>%
                       select(Outcome, Count, Week_ending) %>%
                       mutate(Group = "2020-21"),
                     scotland_data %>%
                       select(Outcome, Average_2018_2019, Week_ending) %>%
                       rename(Count = Average_2018_2019) %>%
                       mutate(Group = "2018-19 average")) %>%
  ggplot() +
  # Lines
  geom_line(aes(x=Week_ending, y=Count, linetype=Group ,col=Group), size=0.5)+
  # Labels
  labs(x = "Week ending", y ="Count") +
  # Colours of lines and points
  scale_color_manual("Group",values=c(eave_blue2, eave_green))+
  scale_linetype_manual("Group", values = c(2,1)) +
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  geom_text(data = data.frame(Week_ending = rep(as.Date("2021-01-04"), 3),
                              text = rep("2021", times=3),
                              Count = c(30000, 13000, 4500),
                              Outcome = sort(unique(scotland_data$Outcome))),
            aes(x=Week_ending, y=Count, label = text), hjust=0, size=3) +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  geom_text(data = data.frame(Week_ending = as.Date("2020-09-26"),
                              text = "Restrictions announced\n(22 Sep 2020)",
                              Count = c(32000),
                              Outcome = "A&E Attendances"),
            aes(x=Week_ending, y=Count, label = text), hjust=0, size=3, col="firebrick1", fontface=2) +
  facet_wrap(~Outcome, scales = "free") +
  theme_classic() +
  theme(legend.position = "bottom")

p_count


## Differences

p_diff <- scotland_data %>%
  #mutate(Variation = Variation/100) %>%
  ggplot()+
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), size=0.5, col=eave_blue)+
  # Points
  # geom_point(aes(x=Week_ending, y=Variation, color=Outcome, shape=Outcome), size=2)+
  theme_classic()+
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average") +
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2020-04-01"), y=2, label="2018-2019 average", hjust=0, size=3) +
  geom_vline(xintercept = as.Date("2020-09-22"), colour="firebrick1", linetype=2, size=1)+
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "months" , date_labels = "%b")+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  facet_rep_grid(~Outcome) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))



p_diff

# Plot together
png(width=900, height=600,filename = "./outputs/overall_trends.png")
plot_grid(p_count, p_diff, labels = "AUTO", ncol=1, align = "v")

dev.off()



### 3 - Demographic plots ####
# demographic_variation_plot_fn from updated_functions.R
demographic_variation_plot_fn("Sex")
demographic_variation_plot_fn("Age")
demographic_variation_plot_fn("SIMD")

png(width=600, height=900,filename = "./outputs/sex_trends.png")
demographic_variation_plot_fn("Sex")
dev.off()

png(width=600, height=900, filename = "./outputs/age_trends.png")
demographic_variation_plot_fn("Age")
dev.off()

png(width=600, height=900,filename = "./outputs/simd_trends.png")
demographic_variation_plot_fn("SIMD")
dev.off()

##### 4 - Specialties plots #####

# Emerg
scotland_data_specialty_emerg <- subset(scotland_data_specialty, Outcome=="Emergency Hospital Admissions")

p_emerg <- ggplot(scotland_data_specialty_emerg) +
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Emergency") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=eave_blue, size=1)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  facet_rep_wrap(~Specialty, ncol=4)

p_emerg

# Planned
scotland_data_specialty_planned <- subset(scotland_data_specialty, Outcome=="Planned Hospital Admissions")

p_planned <- ggplot(scotland_data_specialty_planned) +
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Planned") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=eave_green, size=1)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  facet_rep_wrap(~Specialty, ncol=4)


p_planned

plot_grid(p_emerg, p_planned, align="v", ncol = 1, labels = "AUTO")

png(width=900, height=900,filename = "./outputs/specialty_trends.png")
plot_grid(p_emerg, p_planned, align="v", ncol = 1, labels = "AUTO",
          rel_heights = c(3/5,2/5))

dev.off()

### 5 - NHS Health Boards plots ####

# A&E
scotland_data_hb_ae <- subset(scotland_data_hbs, Outcome=="A&E Attendances")

p_ae <- ggplot(scotland_data_hb_ae) +
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="A&E Attendances") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=eave_blue, size=1)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_wrap(~Area_name, ncol=4)


# Emerg
scotland_data_hb_emerg <- subset(scotland_data_hbs, Outcome=="Emergency Hospital Admissions")

p_emerg <- ggplot(scotland_data_hb_emerg) +
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Emergency Hospital Admissions") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=eave_green, size=1)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_wrap(~Area_name, ncol=4)


# Planned
scotland_data_hb_planned <- subset(scotland_data_hbs, Outcome=="Planned Hospital Admissions")

p_planned <- ggplot(scotland_data_hb_planned) +
  # Labels
  labs(x = "Week ending (2020-2021)", y ="% change from 2018-2019 average", title="Planned Hospital Admissions") +
  # Lines
  geom_line(aes(x=Week_ending, y=Variation), color=eave_blue2, size=1)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  theme_classic()+
  theme(legend.title=element_text(size=9), 
        legend.text=element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.justification="left") +
  scale_x_date(date_breaks = "months" , date_labels = "%b") +
  facet_rep_wrap(~Area_name, ncol=4)


plot_grid(p_ae, p_emerg, p_planned, align="v", ncol = 1, labels = "AUTO")


png(width=900, height=1300,filename = "./outputs/nhs_trends.png")
plot_grid(p_ae, p_emerg, p_planned, align="v", ncol = 1, labels = "AUTO")

dev.off()




#### 6 - Mean differences ####
# Differences between 2018-2019 average and 2020-2021 counts for:
#   4 weeks before change-point 2020-09-06 to 2020-09-27
#   4 weeks after change-point 2020-10-04 to 2020-11-01
#   4 weeks before end date: 2021-02-28 to 2021-03-28

ae_mean_diff <- mean_diff_tbl("A&E Attendances", as.Date("2020-09-22"), c("Total", "Age", "Sex", "SIMD", "NHS Health Board"))
emerg_mean_diff <- mean_diff_tbl("Emergency Hospital Admissions", as.Date("2020-09-22"), c("Total", "Age", "Sex", "SIMD","Specialty", "NHS Health Board"))
planned_mean_diff <- mean_diff_tbl("Planned Hospital Admissions", as.Date("2020-09-22"), c("Total", "Age", "Sex", "SIMD","Specialty", "NHS Health Board"))


write.csv(ae_mean_diff, file = "./outputs/ae_mean_diff.csv", row.names = F)
write.csv(emerg_mean_diff, file = "./outputs/emerg_mean_diff.csv", row.names = F)
write.csv(planned_mean_diff, file = "./outputs/planned_mean_diff.csv", row.names = F)





