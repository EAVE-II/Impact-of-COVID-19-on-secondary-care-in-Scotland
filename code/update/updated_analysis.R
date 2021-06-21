
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
library("cowplot")
library("scales")

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
  rename(Variation = "Variation....") %>% # Rename Variation... to 'Variation'
  rename(Outcome = 1) %>% # Rename Admission_type to 'Outcome'
  mutate(Outcome = recode(Outcome, "Emergency" = "Emergency Hospital Admissions", 
                          "Planned" = "Planned Hospital Admissions")) %>% # Change outcome to character and add on 'Hospital Admissions to 'Emergency' and 'Planned'
  mutate(BA_Pandemic_Lockdown  = factor(case_when(Week_ending < as.Date("2020-03-11") ~ "Before",
                                                 Week_ending > as.Date("2020-03-23") ~ "After",
                                                 TRUE ~ "Between"),
                                       levels = c("Before", "Between", "After"))) %>% # Assign weeks to relevant time periods of before pandemic (change-point 1) and after lockdown (change-point 2)
  filter(Week_ending > as.Date("2020-03-23")) %>%
  filter(Week_ending < as.Date("2021-04-04")) %>%
  mutate(No_days = as.numeric(Week_ending - Week_ending[1])/7)


# Min date
min(all_data$Week_ending)

# Max date
max(all_data$Week_ending)

# No. weeks
difftime(min(all_data$Week_ending),
         max(all_data$Week_ending),units="weeks")


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
  mutate(Category = if_else(Category %in% c("Aged 65 to 74", "Aged 75 to 84"), "Aged 65 to 84", Category)) %>%
  mutate(Category = factor(Category, levels= c("Aged under 5", "Aged 5 to 14", "Aged 15 to 44", "Aged 45 to 64", "Aged 65 to 84", "Aged 85 and over"))) # Make factor


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




#### 2 - Time series plots ####

#### 2.1 - Total outcome plots ####

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
  #geom_vline(xintercept = as.Date("2020-03-11"), colour=phs_blue, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-03-07"), y=31000, label="WHO announces pandemic\n(11 Mar 2020)", color=phs_blue, hjust=1, size=3, fontface =2)+
  # Eat out to help out
  #geom_vline(xintercept=as.Date("2020-08-03"), color=phs_purple, linetype=1, size=1)+
  #annotate("text", x=as.Date("2020-08-07"), y=33000, label="Eat out to help out introduced\n(3 Aug 2020)", color=phs_purple, hjust=0, size=3, fontface =2)+
  # Colours of lines and points
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_main, phs_green))+
  #scale_linetype_manual("Outcome", values=2:4) +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "months" , date_labels = "%b")


p_count


## Differences

p_diff <- scotland_data %>%
  #mutate(Variation = Variation/100) %>%
  ggplot()+
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
  annotate("text", x=as.Date("2021-03-01"), y=2, label="2018-2019 average", hjust=1, size=3) +
  scale_color_manual("Outcome",values=c(phs_trendcol1, phs_main, phs_green))+
  #scale_linetype_manual("Outcome", values=2:4) +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "months" , date_labels = "%b")+
  scale_y_continuous(labels = function(x) paste0(x, "%")) #+
  #geom_vline(xintercept = as.Date("2021-01-01"), linetype=2)+
  #annotate("text", x=as.Date("2021-01-03"), y=15, label="2021", hjust=0, size=4)
  

p_diff
  
# Plot together
png(width=600, height=950,filename = "./outputs/overall_trends.png")
plot_grid(p_count, p_diff, labels = "AUTO", ncol=1, align = "v")

dev.off()



### 2.2 - Demographic plots ####
# demographic_variation_plot_fn from updated_functions.R
demographic_variation_plot_fn("Sex")
demographic_variation_plot_fn("Age")
demographic_variation_plot_fn("SIMD")



##### 2.3 - Specialties plots #####
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




### 2.4 - NHS Health Boards plots ####

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



##### 3 - Modelling ####
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








