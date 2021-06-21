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
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "months" , date_labels = "%b")+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  facet_wrap(~Outcome, scales = "free") +
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



##### 4 - Specialties plots #####
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
  geom_line(aes(x=Week_ending, y=Variation), color=phs_main, size=1)+
  # Points
  geom_point(aes(x=Week_ending, y=Variation),color=phs_main, size=2)+
  # 2018-2019 average
  geom_hline(yintercept = 0, linetype=2)+
  annotate("text", x=as.Date("2021-01-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  # WHO announcement
  #geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
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
  #geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
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
plot_grid(p_emerg, p_planned, align="v", ncol = 1, labels = "AUTO")




### 5 - NHS Health Boards plots ####

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
