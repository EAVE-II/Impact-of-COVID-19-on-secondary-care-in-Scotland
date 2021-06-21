######################################################################
## Title: 
## Short title: Impact of COVID-19 on secondary care in Scotland (updated)
## DOI: 
## Code author: Rachel H. Mulholland <Rachel.Mulholland@ed.ac.uk>
## Description: 01_data_setup: Prepares data into format required for analyses
######################################################################

##### 0 - Setup #####

#### 0.1 - Load in libraries and functions ####
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

#### 0.2 - Create colour scheme #### 
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

#### 1 - Load in data #### 

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

