# SSA_project_code


library(ggplot2)
install.packages("vegan")
library(vegan)
library(tidyr)
install.packages("RColorBrewer") 
library(RColorBrewer)            
install.packages("readr")
library(readr)
library(dplyr)

setwd("/Users/victoriamoix/Desktop/SSA Project/R")


ssa_data <- read_csv("09_29_23_SSA_R_OND.csv")

View(ssa_data)

required_colors <- c("red", "coral", "pink", "hotpink", "orange", "tan", "yellow", "green", "limegreen", "cyan", "blue", "purple", "violet", "#4B0082")

# excluded columns
columns_to_exclude <- c("Name", "Affiliation", "Title", "Species_Common_Name", "What is the year of the most recent source of information?")

# specified columns dataset
species_qa_dataset <- ssa_data %>%
  select(-one_of(columns_to_exclude))



ssa_numerical_dataset <- ssa_data %>%
  select("Species_Taxa", "Life_History_Profile",	"Species_Resource_Needs",	"Circumstances_Population_Resilience","Demographics_Influencing_Population_Resilience",	"Evolutionary_History_Across_Range","Quality_of_Map",
         "Monitoring_Surveys",	"Current_Population_Structure",	"Current_Overall_Distribution",	"Current_Overall_Abundance",	"Overall_Growth_Rate",	"Current_Overall_Genetics",	"Use_of_Habitat",	"Boundaries_Species_Range",	"Use_Species_Range",	"Info_on_Resources",	"Changes_Historical_to_Current",	"The explanation of the causes and effects that resulted in the current species’ condition with respect to the life history and habitat needs is...",	
         "Are the implications of any missing or diminished resources that affect the number of populations within the species’ ecological settings...",	"Are the implications of any missing or diminished resources that affect the distribution of populations within the species’ ecological settings…", "Are the implications of any missing or diminished resources that affect the connectivity of populations within the species’ ecological settings…",	"The overall implications of any missing/diminished resources on the demographic parameters (ie population size, mortality rate, etc)  of the species is…",
         "Overall, the information used to calculate the resiliency of the species is...", "Overall, the 4 used to calculate the redundancy of the species is...", "The anticipated future of the most likely future of the species is…", "Are the results of the analytical methods for the future conditions of the species…", "Are the levels of certainty for the analysis of future conditions…", "If there is a conservation strategy for the species, it is…", 
         "Overall, the information used to calculate the predicted resiliency of the species is...", "Overall, the information used to calculate the predicted redundancy of the species is...", "Overall, the information used to calculate the predicted representation of the species is...",
         "Major stressors/threats overall are…",	"Based on the information presented in the SSA, the overall immediacy of the stressors, meaning how close in time/extent  is…",	"Based on the information presented in the SSA, the overall sources of the stressors are…",
         "Based on the information presented in the SSA, overall minor stressors for the species are…" )


# replace NA values with 0
ssa_numerical_dataset[ , 2:36] <- lapply(ssa_numerical_dataset[ , 2:36], function(x) ifelse(is.na(x), 0, x))

# long format
long_data_numerical <- pivot_longer(ssa_numerical_dataset, cols = -1, names_to = "Question", values_to = "Response")

# choices 1, 2, 3, and 4
long_data_numerical <- long_data_numerical %>%
  filter(Response %in% c("1", "2", "3", "4"))

# convert to numeric
long_data_numerical$Response <- as.numeric(long_data_numerical$Response)

# filter choice (1, 2, 3, 4)
choice_1_data <- long_data_numerical %>%
  filter(Response == 1)
choice_2_data <- long_data_numerical %>%
  filter(Response == 2)
choice_3_data <- long_data_numerical %>%
  filter(Response == 3)
choice_4_data <- long_data_numerical %>%
  filter(Response == 4)

# individual graphs
plot_choice_1 <- ggplot(choice_1_data, aes(x = Question, y = Response, fill = Species_Taxa)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = required_colors) +
  labs(
    title = "Response Distribution for Choice 1",
    x = "Question",
    y = "Count",
    fill = "Species Taxa"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() +
  facet_wrap(~Response, scales = "free", ncol = 1)

print(plot_choice_1)

plot_choice_2 <- ggplot(choice_2_data, aes(x = Question, y = Response, fill = Species_Taxa)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = required_colors) +
  labs(
    title = "Response Distribution for Choice 2",
    x = "Question",
    y = "Count",
    fill = "Species Taxa"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() +
  facet_wrap(~Response, scales = "free", ncol = 1)

print(plot_choice_2)

plot_choice_3 <- ggplot(choice_3_data, aes(x = Question, y = Response, fill = Species_Taxa)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = required_colors) +
  labs(
    title = "Response Distribution for Choice 3",
    x = "Question",
    y = "Count",
    fill = "Species Taxa"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() +
  facet_wrap(~Response, scales = "free", ncol = 1)

print(plot_choice_3)

plot_choice_4 <- ggplot(choice_4_data, aes(x = Question, y = Response, fill = Species_Taxa)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = required_colors) +
  labs(
    title = "Response Distribution for Choice 4",
    x = "Question",
    y = "Count",
    fill = "Species Taxa"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() +
  facet_wrap(~Response, scales = "free", ncol = 1)

print(plot_choice_4)

#3R graphs

ssa_3r_dataset <- ssa_data %>%
  select("Species_Taxa", "After reading this, how do the experts (ie writers of the SSA) rate the resilience of the species overall?",
         "After reading this, how do the experts (ie writers of the SSA) rate the redundancy of the species?", "After reading this, how do the experts (ie writers of the SSA) rate the representation of the species?")

# long format
long_data_3r <- pivot_longer(ssa_3r_dataset, cols = -1, names_to = "Question", values_to = "Response")


# filter
choice_low_data <- long_data_3r %>%
  filter(Response == "Low")

choice_medium_data <- long_data_3r %>%
  filter(Response == "Medium")

choice_high_data <- long_data_3r %>%
  filter(Response == "High")

choice_unknown_data <- long_data_3r %>%
  filter(Response == "Unknown")

# graphs
plot_choice_low <- ggplot(choice_low_data, aes(x = Question, y = Response, fill = Species_Taxa)) +
  geom_col(position = "stack") +
  labs(title = "Current 3R Responses: Low",
       x = "Question",
       y = "Count",
       fill = "Species Taxa") +
  scale_fill_manual(values = required_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() +
  facet_wrap(~Response, scales = "free", ncol = 1)

print(plot_choice_low)

plot_choice_medium <- ggplot(choice_medium_data, aes(x = Question, y = Response, fill = Species_Taxa)) +
  geom_col(position = "stack") +
  labs(title = "Current 3R Responses: Medium",
       x = "Question",
       y = "Count",
       fill = "Species Taxa") +
  scale_fill_manual(values = required_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() +
  facet_wrap(~Response, scales = "free", ncol = 1)

print(plot_choice_medium)

plot_choice_high <- ggplot(choice_high_data, aes(x = Question, y = Response, fill = Species_Taxa)) +
  geom_col(position = "stack") +
  labs(title = "Current 3R Responses: High",
       x = "Question",
       y = "Count",
       fill = "Species Taxa") +
  scale_fill_manual(values = required_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() +
  facet_wrap(~Response, scales = "free", ncol = 1)

print(plot_choice_high)

plot_choice_unknown <- ggplot(choice_unknown_data, aes(x = Question, y = Response, fill = Species_Taxa)) +
  geom_col(position = "stack") +
  labs(title = "Current 3R Responses: Unknown",
       x = "Question",
       y = "Count",
       fill = "Species Taxa") +
scale_fill_manual(values = required_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 8)) +
  coord_flip() +
  facet_wrap(~Response, scales = "free", ncol = 1)

print(plot_choice_unknown)


##SOURCES##


ssa_sources_dataframe <- ssa_data %>%
  select("Species_Taxa", "Primary_Sources")

# long format
long_data_sources <- pivot_longer(ssa_sources_dataframe, cols = -1, names_to = "Question", values_to = "Response")

# amphibians #
amp_taxa_sources <- "Amphibians"
amp_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == amp_taxa_sources, ]

# omit na
amp_taxa_sources_data <- na.omit(amp_taxa_sources_data)

# separate choices
separated_amp_taxa_sources_data <- amp_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

# calculate counts
counted_data <- separated_amp_taxa_sources_data %>%
  count(Species_Taxa, Response)

# Define source colors
source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")

# bubble chart
ggplot(counted_data, aes(x = Species_Taxa, y = Response, size = n, fill = factor(Response))) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Amphibians",
    x = "Amphibians",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()


# Arachnids #
ara_taxa_sources <- "Arachnids"
ara_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                             
                                             ara_taxa_sources, ]
# omit na
ara_taxa_sources_data <- na.omit(ara_taxa_sources_data)

# seperate choices
separated_ara_taxa_sources_data <- ara_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# Calculate counts
ara_counted_data <- separated_ara_taxa_sources_data %>%
  count(Species_Taxa, Response)


# Create the bubble chart
ggplot(ara_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Arachnids",
    x = "Arachnids",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()



##BIRDS##

bird_taxa_sources <- "Birds"
bird_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                             
                                             bird_taxa_sources, ]
# omit na
bird_taxa_sources_data <- na.omit(bird_taxa_sources_data)

# seperate choices
separated_bird_taxa_sources_data <- bird_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
bird_counted_data <- separated_bird_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(bird_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Birds",
    x = "Birds",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()


##CLAMS##

clam_taxa_sources <- "Clams"
clam_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                              
                                              clam_taxa_sources, ]
# omit na
clam_taxa_sources_data <- na.omit(clam_taxa_sources_data)

# seperate choices
separated_clam_taxa_sources_data <- clam_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
clam_counted_data <- separated_clam_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(clam_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Clams",
    x = "Clams",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()


##CONIFERS & CYCADS##

coni_taxa_sources <- "Conifers_and_Cycads"
coni_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                              
                                              coni_taxa_sources, ]
# omit na
coni_taxa_sources_data <- na.omit(coni_taxa_sources_data)

# seperate choices
separated_coni_taxa_sources_data <- coni_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
coni_counted_data <- separated_coni_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(coni_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Conifers & Cycads",
    x = "Conifers & Cycads",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()

##Crustaceans##

crust_taxa_sources <- "Crustaceans"
crust_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                              
                                               crust_taxa_sources, ]
# omit na
crust_taxa_sources_data <- na.omit(crust_taxa_sources_data)

# seperate choices
separated_crust_taxa_sources_data <- crust_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
crust_counted_data <- separated_crust_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(crust_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Crustaceans",
    x = "Crustaceans",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()

##Ferns_and_Allies##

faa_taxa_sources <- "Ferns_and_Allies"
faa_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                               
                                             faa_taxa_sources, ]
# omit na
faa_taxa_sources_data <- na.omit(faa_taxa_sources_data)

# seperate choices
separated_faa_taxa_sources_data <- faa_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
faa_counted_data <- separated_faa_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(faa_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Ferns & Allies",
    x = "Ferns & Allies",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()


##Fishes##

fish_taxa_sources <- "Fishes"
fish_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                             
                                              fish_taxa_sources, ]
# omit na
fish_taxa_sources_data <- na.omit(fish_taxa_sources_data)

# seperate choices
separated_fish_taxa_sources_data <- fish_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
fish_counted_data <- separated_fish_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(fish_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Fishes",
    x = "Fishes",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()


## Insects ##

insect_taxa_sources <- "Insects"
insect_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                              
                                              insect_taxa_sources, ]
# omit na
insect_taxa_sources_data <- na.omit(insect_taxa_sources_data)

# seperate choices
separated_insect_taxa_sources_data <- insect_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
insect_counted_data <- separated_insect_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(insect_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Insects",
    x = "Insects",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()


## Lichen ##

lichen_taxa_sources <- "Lichen"
lichen_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                                
                                                lichen_taxa_sources, ]
# omit na
lichen_taxa_sources_data <- na.omit(lichen_taxa_sources_data)

# seperate choices
separated_lichen_taxa_sources_data <- lichen_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
lichen_counted_data <- separated_lichen_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(lichen_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Lichen",
    x = "Lichen",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()


##Mammasls##

mammal_taxa_sources <- "Mammals"
mammal_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                                
                                                mammal_taxa_sources, ]
# omit na
mammal_taxa_sources_data <- na.omit(mammal_taxa_sources_data)

# seperate choices
separated_mammal_taxa_sources_data <- mammal_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
mammal_counted_data <- separated_mammal_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(mammal_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Mammals",
    x = "Mammals",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()


##Plants##

plants_taxa_sources <- "Plants"
plants_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                                
                                                plants_taxa_sources, ]
# omit na
plants_taxa_sources_data <- na.omit(plants_taxa_sources_data)

# seperate choices
separated_plants_taxa_sources_data <- plants_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
plants_counted_data <- separated_plants_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(plants_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Plants",
    x = "Plants",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()

##Reptiles##

reptiles_taxa_sources <- "Reptiles"
reptiles_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                                
                                                  reptiles_taxa_sources, ]
# omit na
reptiles_taxa_sources_data <- na.omit(reptiles_taxa_sources_data)

# seperate choices
separated_reptiles_taxa_sources_data <- reptiles_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
reptiles_counted_data <- separated_reptiles_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(reptiles_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Reptiles",
    x = "Reptiles",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()

##Snails##

snails_taxa_sources <- "Snails"
snails_taxa_sources_data <- long_data_sources[long_data_sources$Species_Taxa == 
                                                  
                                                snails_taxa_sources, ]
# omit na
snails_taxa_sources_data <- na.omit(snails_taxa_sources_data)

# seperate choices
separated_snails_taxa_sources_data <- snails_taxa_sources_data %>%
  separate_rows(Response, sep = ", ")

source_colors <- c("1" = "red", "2" = "blue", "3" = "green", "4" = "purple")


# counts
snails_counted_data <- separated_snails_taxa_sources_data %>%
  count(Species_Taxa, Response)


# bubble chart
ggplot(snails_counted_data, aes(x = Species_Taxa, y = Response, size = n)) +
  geom_point() +
  labs(
    title = "Bubble Chart of Primary Sources: Snails",
    x = "Snails",
    y = "Primary Sources",
    size = "Count"
  ) +
  scale_size_continuous(range = c(5, 20)) +  # Adjust the size range as needed
  scale_fill_manual(values = source_colors) +
  theme_minimal()

##SSA MOTIVATION##

ssa_motivation_dataset <- ssa_data %>%
  select("Species_Taxa", "SSA_Motivation")


# omit na
ssa_motivation_dataset <- na.omit(ssa_motivation_dataset)

# counts
ssa_motivation_counted_dataset <- ssa_motivation_dataset %>%
count(Species_Taxa, SSA_Motivation)


ggplot(ssa_motivation_counted_dataset, aes(x = SSA_Motivation, fill = Species_Taxa)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  labs(
    title = "Stacked Bar Graph of SSA Motivations by Species Taxa",
    x = "SSA Motivation",
    y = "Count",
    fill = "Species Taxa"
  ) +
  scale_fill_manual(values = required_colors) +
  theme(legend.position = "top", legend.title = element_blank())
