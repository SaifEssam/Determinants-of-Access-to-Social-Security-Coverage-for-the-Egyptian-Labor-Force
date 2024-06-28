## Loading the Data:
setwd("D:/Fourth Year (Second Term)/Graduation Project/Our Work/Current Work")

data <- read.csv("new_data8.csv", header=TRUE)

#Categorical Variables:
data$SocialSecurity <- as.factor(data$SocialSecurity)
data$Sex <- as.factor(data$Sex)
data$MaritalStatus <- as.factor(data$MaritalStatus)
data$Father.s.Present.at.Home <- as.factor(data$Father.s.Present.at.Home)
data$EducationalLevel<- as.factor(data$EducationalLevel)
data$DisabilityStatus <- as.factor(data$DisabilityStatus)
data$HealthInsurance <- as.factor(data$HealthInsurance)
data$ChronicDisease <- as.factor(data$ChronicDisease)
data$EmploymentStatus <- as.factor(data$EmploymentStatus)
data$Occupation <- as.factor(data$Occupation)
data$MainActivityStatus <- as.factor(data$MainActivityStatus)
data$Area <- as.factor(data$Area)
data$Industry <- as.factor(data$Industry)
data$Sector.of.Employment <- as.factor(data$Sector.of.Employment)

#Continuous Variables:
data$Age <- as.numeric(data$Age)
str(data)



##### Data Visualization:

library(ggplot2)

##1. Social Security (Response Variable):

library(ggplot2)
library(dplyr)

social_security_data <- data %>% 
  count(SocialSecurity) %>% 
  mutate(percentage = n / sum(n) * 100)

# Creating the pie chart
ggplot(social_security_data, aes(x = "", y = percentage, fill = SocialSecurity)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3")) + # Blue color theme
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white") +
  ggtitle("Social Security Coverage")

################################################################
## Another Colors:

social_security_counts <- table(data$SocialSecurity)

social_security_df <- as.data.frame(social_security_counts)
names(social_security_df) <- c("Status", "Count")

# Calculating the Percentages:
social_security_df$Percentage <- social_security_df$Count / sum(social_security_df$Count) * 100

# Plot:
ggplot(social_security_df, aes(x = "", y = Percentage, fill = Status)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#4D77FF", "orange1")) +
  labs(x = NULL, y = NULL, fill = "Social Security Status") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5))
###################################################################################
####################################################################################

###2. Age Distribution:

# Histogram with a density line:
ggplot(data, aes(x = Age)) + 
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "#4D77FF", color = "black") +
  geom_density(colour = "blue", linewidth = 1) +
  labs(title = "Age Distribution with Density Plot",
       x = "Age",
       y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12))

########################################################################
###########################################################################

###3. Gender:

sex_counts <- table(data$Sex)
sex_df <- as.data.frame(sex_counts)
names(sex_df) <- c("Sex", "Count")
sex_df$Percentage <- round((sex_df$Count / sum(sex_df$Count)) * 100, 1)
##x = reorder(Area, -Count)
# Bar Chart
ggplot(sex_df, aes(x = reorder(Sex, -Count), y = Count, fill = Sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("orange1", "#1E90FF")) +
  geom_text(aes(label = paste(Percentage, "%")), vjust = 1.8) +
  labs(x = "Sex", y = "Count") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12))
###########################################################
###########################################################
## Stacked Gender with SS:

# Calculate the counts and percentages
sex_social_counts <- data %>%
  group_by(Sex, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Occupation based on total counts
total_counts <- data %>%
  group_by(Sex) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

sex_social_counts$Sex <- factor(sex_social_counts$Sex, levels = total_counts$Sex)

# Create the horizontal stacked bar chart with percentages inside the bars
ggplot(sex_social_counts, aes(x = count, y = Sex, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3.8) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Proportion", y = "Gender", fill = "Social Security") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))
################################################################
########################################################################

###4. Marital Status:

marital_counts <- table(data$MaritalStatus)
marital_df <- as.data.frame(marital_counts)
names(marital_df) <- c("MaritalStatus", "Count")
marital_df$Percentage <- round((marital_df$Count / sum(marital_df$Count)) * 100, 1)
##x = reorder(Marital, -Count)
# Bar Chart
ggplot(marital_df, aes(x = reorder(MaritalStatus, -Count), y = Count, fill = MaritalStatus)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#1E90FF","#1E90FF","#1E90FF","#1E90FF","#1E90FF")) +
  geom_text(aes(label = paste(Percentage, "%")), vjust = 1.3) +
  labs(x = "Marital Status", y = "Count") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10))



# Create a clustered bar chart for Marital Status by Social Security
ggplot(data, aes(x = MaritalStatus, fill = SocialSecurity)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#1f77b4", "#aec7e8")) +
  labs(title = "Distribution of Marital Status by Social Security Status", x = "Marital Status", y = "Count", fill = "Social Security") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

############################################

## CLustered: 
library(ggplot2)
library(dplyr)

# Calculate the counts and percentages
marital_social_counts <- data %>%
  group_by(MaritalStatus, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder MaritalStatus based on total counts
total_counts <- data %>%
  group_by(MaritalStatus) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

marital_social_counts$MaritalStatus <- factor(marital_social_counts$MaritalStatus, levels = total_counts$MaritalStatus)

# Create the clustered bar chart
ggplot(marital_social_counts, aes(x = MaritalStatus, y = count, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.9), vjust = -0.1, color = "black") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Marital Status", y = "Count", fill = "Social Security") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"),axis.title = element_text(size = 12, face = "bold"))


#####################################################################
####################################################################

###5. Area: 

# Calculating the count and percentage for each area
area_summary <- data %>%
  count(Area) %>%
  mutate(Percentage = n / sum(n) * 100)

# Creating the bar chart with counts and add percentage labels
ggplot(area_summary, aes(x = reorder(Area, -n), y = n, fill = Area)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = 1, color = "black") +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of Individuals by Area", x = "Area", y = "Count") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))
######################################################
#####################################################

## Clustered Area with SS:
library(ggplot2)
library(dplyr)

# Calculate the counts and percentages
area_social_counts <- data %>%
  group_by(Area, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Area based on total counts
total_counts <- data %>%
  group_by(Area) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

area_social_counts$Area <- factor(area_social_counts$Area, levels = total_counts$Area)

# Create the horizontal clustered bar chart with smaller bars
ggplot(area_social_counts, aes(x = count, y = Area, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), hjust = -0.1, color = "black", size = 3.3) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Count", y = "Area", fill = "Social Security") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

##################################################################################
###############################################################################

###6. Father's Presence at Home:

fathers_presence_counts <- table(data$Father.s.Present.at.Home)
fathers_presence_df <- as.data.frame(fathers_presence_counts)
names(fathers_presence_df) <- c("FatherPresence", "Count")

# Calculating the percentages
fathers_presence_df$Percentage <- round((fathers_presence_df$Count / sum(fathers_presence_df$Count)) * 100, 2)

# Creating Pie chart
ggplot(fathers_presence_df, aes(x = "", y = Percentage, fill = FatherPresence)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Blues") +
  theme_void() +
  geom_text(aes(label = paste(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "Father's Presence") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"), plot.title = element_text(size = 12, hjust = 0.3, face = "bold"),
        axis.text = element_text(size = 12))
#######################################################
#######################################################

## Father's:

##################
library(ggplot2)
library(dplyr)

# Calculate the counts and percentages
father_social_counts <- data %>%
  group_by(Father.s.Present.at.Home, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Father's Presence at Home based on total counts
total_counts <- data %>%
  group_by(Father.s.Present.at.Home) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

father_social_counts$Father.s.Present.at.Home <- factor(father_social_counts$Father.s.Present.at.Home, levels = total_counts$Father.s.Present.at.Home)

# Create the horizontal clustered bar chart with smaller bars and smaller percentages
ggplot(father_social_counts, aes(x = count, y = Father.s.Present.at.Home, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), hjust = 1.1, color = "black", size = 3) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Count", y = "Father's Presence at Home", fill = "Social Security") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

#################
# Calculate the counts and percentages
father_presence_counts <- data %>%
  group_by(Father.s.Present.at.Home) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Create the pie chart
ggplot(father_presence_counts, aes(x = "", y = percentage, fill = Father.s.Present.at.Home)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("blue","#1f77b4" ,"green4")) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5), color = "white", size = 5) +
  labs(title = "Distribution of Father's Presence at Home") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(face = "bold"))

############################################################################
#########################################################################

###7. Educational Level:

education_counts <- table(data$EducationalLevel)
education_df <- as.data.frame(education_counts)
names(education_df) <- c("EducationalLevel", "Count")

# Calculating percentages
education_df$Percentage <- round((education_df$Count / sum(education_df$Count)) * 100, 1)

# Creating the bar chart
ggplot(education_df, aes(x = reorder(EducationalLevel, -Count), y = Count, fill = EducationalLevel)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = paste(Percentage, "%")), vjust = 1, color = "black") +
  labs(title = "Distribution of Individuals by Educational Level", x = "Educational Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8))
###########################################################
###########################################################


# Calculate the counts and percentages
education_social_counts <- data %>%
  group_by(EducationalLevel, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Educational Level based on total counts
total_counts <- data %>%
  group_by(EducationalLevel) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

education_social_counts$EducationalLevel <- factor(education_social_counts$EducationalLevel, levels = total_counts$EducationalLevel)

# Create the horizontal clustered bar chart with smaller bars and smaller percentages
ggplot(education_social_counts, aes(x = count, y = EducationalLevel, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), hjust = 0
            , color = "black", size = 3) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Count", y = "Educational Level", fill = "Social Security") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

############################################################################

###8. Employment Status:

employment_status_summary <- data %>%
  count(EmploymentStatus) %>%
  mutate(Percentage = n / sum(n) * 100)

# Creating the Bar Chart
ggplot(employment_status_summary, aes(x = reorder(EmploymentStatus, -n), y = n, fill = EmploymentStatus)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = 1, color = "black") +
  labs(title = "Distribution of Employment Status", x = "Employment Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8))
##########################################################################################
### Horizontal:




##################################################################################
###################################################################################

###9. Main Activity Status:

main_activity_summary <- data %>%
  count(MainActivityStatus) %>%
  mutate(Percentage = n / sum(n) * 100)

# Creating the bar chart
ggplot(main_activity_summary, aes(x = reorder(MainActivityStatus, -n), y = n, fill = MainActivityStatus)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust =1, color = "black") +
  labs(title = "Distribution of Main Activity Status", x = "Main Activity Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))
## Variable Malosh Lazma.
table(data$MainActivityStatus)

###############################################################################
#################################################################################

###10. Disability Status:

disability_status_counts <- table(data$DisabilityStatus)
disability_status_df <- as.data.frame(disability_status_counts)
names(disability_status_df) <- c("DisabilityStatus", "Count")

# Calculating the Percentages
disability_status_df$Percentage <- round((disability_status_df$Count / sum(disability_status_df$Count)) * 100, 1)

# Creating Pie Chart
ggplot(disability_status_df, aes(x = "", y = Percentage, fill = DisabilityStatus)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("orange1","#4D77FF")) +
  theme_void() +
  geom_text(aes(label = paste(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "Disability Status", title = "Distribution of Disability Status")
#################################
##################################

## Stacked:

# Calculate the counts and percentages
disability_social_counts <- data %>%
  group_by(DisabilityStatus, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Disability Status based on total counts
total_counts <- data %>%
  group_by(DisabilityStatus) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

disability_social_counts$DisabilityStatus <- factor(disability_social_counts$DisabilityStatus, levels = total_counts$DisabilityStatus)

# Create the horizontal clustered bar chart with smaller bars and smaller percentages
ggplot(disability_social_counts, aes(x = count, y = DisabilityStatus, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(width = 0.7), hjust = -0.3, color = "black", size = 3) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(title = "Distribution of Disability Status by Social Security Status", x = "Count", y = "Disability Status", fill = "Social Security") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
######################################################

## Stacked: 

# Calculate the counts and percentages
disability_social_counts <- data %>%
  group_by(DisabilityStatus, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Disability Status based on total counts
total_counts <- data %>%
  group_by(DisabilityStatus) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

disability_social_counts$DisabilityStatus <- factor(disability_social_counts$DisabilityStatus, levels = total_counts$DisabilityStatus)

# Create the horizontal stacked bar chart with percentages exactly inside the bars
ggplot(disability_social_counts, aes(x = count, y = DisabilityStatus, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3.8) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Proportion", y = "Disability Status", fill = "Social Security") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12))


########################################################################################
########################################################################################

###11. Chronic Disease:

chronic_disease_counts <- table(data$ChronicDisease)
chronic_disease_df <- as.data.frame(chronic_disease_counts)
names(chronic_disease_df) <- c("ChronicDisease", "Count")

# Calculating the percentages
chronic_disease_df$Percentage <- round((chronic_disease_df$Count / sum(chronic_disease_df$Count)) * 100, 1)

# Creating Pie chart
ggplot(chronic_disease_df, aes(x = "", y = Percentage, fill = ChronicDisease)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#4D77FF","orange1")) +
  theme_void() +
  geom_text(aes(label = paste(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Chronic Diseases", fill = "Chronic Disease Status")
######################################################
#######################################################3

## Stacked: 

# Calculate the counts and percentages
chronic_social_counts <- data %>%
  group_by(ChronicDisease, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Chronic Disease based on total counts
total_counts <- data %>%
  group_by(ChronicDisease) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

chronic_social_counts$ChronicDisease <- factor(chronic_social_counts$ChronicDisease, levels = total_counts$ChronicDisease)

# Create the horizontal stacked bar chart with percentages inside the bars
ggplot(chronic_social_counts, aes(x = count, y = ChronicDisease, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3.8) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs( x = "Proportion", y = "Chronic Disease Status", fill = "Social Security") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12))

#################################################################################################
############################################################################################

###12. Health Insurance:

health_insurance_counts <- table(data$HealthInsurance)
health_insurance_df <- as.data.frame(health_insurance_counts)
names(health_insurance_df) <- c("HealthInsurance", "Count")

# Calculate percentages
health_insurance_df$Percentage <- round((health_insurance_df$Count / sum(health_insurance_df$Count)) * 100, 1)

# Create a pie chart
ggplot(health_insurance_df, aes(x = "", y = Percentage, fill = HealthInsurance)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("orange1","#4D77FF")) +
  theme_void() +
  geom_text(aes(label = paste(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Health Insurance", fill = "Health Insurance Status")
#########################################################################
#########################################################################

## Stacked Health Insurance:

# Calculate the counts and percentages
health_insurance_social_counts <- data %>%
  group_by(HealthInsurance, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Health Insurance based on total counts
total_counts <- data %>%
  group_by(HealthInsurance) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

health_insurance_social_counts$HealthInsurance <- factor(health_insurance_social_counts$HealthInsurance, levels = total_counts$HealthInsurance)

# Create the horizontal stacked bar chart with percentages inside the bars
ggplot(health_insurance_social_counts, aes(x = count, y = HealthInsurance, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3.8) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Proportion", y = "Health Insurance Status", fill = "Social Security") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12))
#################################################################################
###########################################################################

###13. Employment Status:

# Calculate the counts and percentages
employment_status_counts <- data %>%
  group_by(EmploymentStatus) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Employment Status based on total counts
employment_status_counts$EmploymentStatus <- factor(employment_status_counts$EmploymentStatus, levels = employment_status_counts$EmploymentStatus[order(-employment_status_counts$count)])

# Create the bar chart with percentages inside the bars
ggplot(employment_status_counts, aes(x = EmploymentStatus, y = count, fill = EmploymentStatus)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            vjust = -0.5, color = "black", size = 3) +
  scale_fill_manual(values = c("blue4", "#1f77b4","#4D77FF" , "#aec7e8")) +
  labs(x = "Employment Status", y = "Count", fill = "Employment Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))


###################################################################
employmentStatus_counts <- data %>%
  group_by(EmploymentStatus) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Sector of Employment based on percentage in descending order
employmentStatus_counts <- employmentStatus_counts %>%
  arrange(percentage)

employmentStatus_counts$EmploymentStatus <- factor(employmentStatus_counts$EmploymentStatus, levels = employmentStatus_counts$EmploymentStatus)

# Create the horizontal bar chart with percentages inside the bars
ggplot(employmentStatus_counts, aes(x = count, y = EmploymentStatus, fill = EmploymentStatus)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#1f77b4") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2, color = "black", size = 3) +
  labs(x = "Count", y = "Employment Status", fill = "EmploymentStatus") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))


####################################################################

## Stacked EMPloyment with SS:

# Calculate the counts and percentages
employment_social_counts <- data %>%
  group_by(EmploymentStatus, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Employment Status based on total counts
total_counts <- data %>%
  group_by(EmploymentStatus) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

employment_social_counts$EmploymentStatus <- factor(employment_social_counts$EmploymentStatus, levels = total_counts$EmploymentStatus)

# Create the horizontal stacked bar chart with percentages inside the bars
ggplot(employment_social_counts, aes(x = count, y = EmploymentStatus, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3.3) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Proportion", y = "Employment Status", fill = "Social Security") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"))
#############################################################################
#############################################################################

###14. Sector of Employment:

# Calculate the counts and percentages
sector_counts <- data %>%
  group_by(Sector.of.Employment) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Sector of Employment based on percentage in descending order
sector_counts <- sector_counts %>%
  arrange(percentage)

sector_counts$Sector.of.Employment <- factor(sector_counts$Sector.of.Employment, levels = sector_counts$Sector.of.Employment)

# Create the horizontal bar chart with percentages inside the bars
ggplot(sector_counts, aes(x = count, y = Sector.of.Employment, fill = Sector.of.Employment)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#1f77b4") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2, color = "black", size = 3) +
  labs(title = "Distribution of Sector of Employment", x = "Count", y = "Sector of Employment", fill = "Sector of Employment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1))
########################################################

# Calculate the counts and percentages
sector_counts <- data %>%
  group_by(Sector.of.Employment) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Sector of Employment based on percentage in descending order
sector_counts <- sector_counts %>%
  arrange(percentage)

sector_counts$Sector.of.Employment <- factor(sector_counts$Sector.of.Employment, levels = sector_counts$Sector.of.Employment)

# Create the horizontal bar chart with percentages inside the bars
ggplot(sector_counts, aes(x = count, y = Sector.of.Employment, fill = Sector.of.Employment)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#1f77b4") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2, color = "black", size = 3) +
  labs(x = "Count", y = "Sector of Employment", fill = "Sector of Employment") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))


#######################################
#######################################

## Stacked: 
# Calculate the counts and percentages
sector_social_counts <- data %>%
  group_by(Sector.of.Employment, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Sector of Employment based on total counts
total_counts <- data %>%
  group_by(Sector.of.Employment) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

sector_social_counts$Sector.of.Employment <- factor(sector_social_counts$Sector.of.Employment, levels = total_counts$Sector.of.Employment)

# Create the horizontal stacked bar chart with percentages inside the bars
ggplot(sector_social_counts, aes(x = count, y = Sector.of.Employment, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3.3) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Proportion", y = "Sector of Employment", fill = "Social Security") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))
##################################################################################
##############################################################################
###15. Occupation:
occupation_counts <- data %>%
  group_by(Occupation) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Occupation based on count in ascending order
occupation_counts <- occupation_counts %>%
  arrange(count)

occupation_counts$Occupation <- factor(occupation_counts$Occupation, levels = occupation_counts$Occupation)

# Create the horizontal bar chart with percentages inside the bars
ggplot(occupation_counts, aes(x = count, y = Occupation)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#1f77b4") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2, color = "black", size = 3) +
  labs(x = "Count", y = "Occupation") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

############################################################

# Calculate the counts and percentages
occupation_counts <- data %>%
  group_by(Occupation) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Occupation based on percentage in descending order
occupation_counts <- occupation_counts %>%
  arrange(desc(percentage))

occupation_counts$Occupation <- factor(occupation_counts$Occupation, levels = occupation_counts$Occupation)

# Create the horizontal bar chart with percentages inside the bars
ggplot(occupation_counts, aes(x = count, y = Occupation)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#1f77b4") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2, color = "black", size = 3) +
  labs(x = "Count", y = "Occupation") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

###########################################
###########################################

## Clustered: 

# Calculate the counts and percentages
occupation_social_counts <- data %>%
  group_by(Occupation, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Occupation based on total counts
total_counts <- data %>%
  group_by(Occupation) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

occupation_social_counts$Occupation <- factor(occupation_social_counts$Occupation, levels = total_counts$Occupation)

# Create the horizontal stacked bar chart with percentages inside the bars
ggplot(occupation_social_counts, aes(x = count, y = Occupation, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3.3) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Proportion", y = "Occupation", fill = "Social Security") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))
#################################################################
##########################################################################

###16. Industry:

# Calculate the counts and percentages
industry_counts <- data %>%
  group_by(Industry) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Industry based on count in ascending order
industry_counts <- industry_counts %>%
  arrange(count)

industry_counts$Industry <- factor(industry_counts$Industry, levels = industry_counts$Industry)

# Create the horizontal bar chart with percentages inside the bars
ggplot(industry_counts, aes(x = count, y = Industry)) +
  geom_bar(stat = "identity", width = 0.6, fill = "#1f77b4") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            hjust = -0.2, color = "black", size = 3) +
  labs(x = "Count", y = "Industry") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

##########################################################
##########################################################

### Stacked Bar Chart:

# Calculate the counts and percentages
industry_social_counts <- data %>%
  group_by(Industry, SocialSecurity) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Occupation based on total counts
total_counts <- data %>%
  group_by(Industry) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

industry_social_counts$Industry <- factor(industry_social_counts$Industry, levels = total_counts$Industry)

# Create the horizontal stacked bar chart with percentages inside the bars
ggplot(industry_social_counts, aes(x = count, y = Industry, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3.2) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Proportion", y = "Industry", fill = "Social Security") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))


############################################################################################
############################################################################################
######################################################################################

#### Visualizing Two Variables (Clustered Bar Charts):

###1. Marital Status by Social Security:

ggplot(data, aes(fill = SocialSecurity, x = MaritalStatus, y = ..count..)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("No" = "blue2", "Yes" = "#1E90FF")) +
  labs(title = "Marital Status by Social Security Enrollment",
       x = "Marital Status",
       y = "Count",
       fill = "Social Security Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"))
#############################################################
#############################################################

###2. Educational Level by Social Security:

social_edu_counts <- data %>%
  group_by(EducationalLevel, SocialSecurity) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Total = sum(Count), Percentage = (Count / Total) * 100)

# Creating the horizontal stacked bar chart:
ggplot(education_area_counts, aes(x = EducationalLevel, y = Count, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +  # Flip coordinates for horizontal bars
  scale_y_continuous(labels = scales::percent_format()) +  # Convert y-axis into percentage
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_fill(vjust = 0.5), size = 3) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Stacked Bar Chart for Educational Levels by Social Security", x = "Educational Level", y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom")
##############################################################
##############################################################

###3. Disability Status by Social Security:

disability_social_security <- data %>%
  group_by(DisabilityStatus, SocialSecurity) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Total = sum(Count, na.rm = TRUE),
         Percentage = (Count / Total) * 100)

# Create a horizontal stacked bar chart with percentages inside
ggplot(disability_social_security, aes(x = DisabilityStatus, y = Percentage, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Social Security Enrollment by Disability Status",
       x = "Percentage",
       y = "Disability Status") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"), plot.title = element_text(size = 12, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))
###########################################################
##########################################################

###4. Chronic Disease with Social Security:

chronic_social_data <- data %>%
  count(ChronicDisease, SocialSecurity) %>%
  group_by(ChronicDisease) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  ungroup()

# Create a horizontal stacked bar chart with percentages inside the bars
ggplot(chronic_social_data, aes(x = ChronicDisease, y = Percentage, fill = SocialSecurity)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +  # Make the bar chart horizontal
  scale_fill_brewer(palette = "Set1") +  # Use color brewer for a nice color scheme
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_fill(vjust = 0.5), size = 4, color = "white") +
  labs(title = "Chronic Disease Status vs. Social Security Enrollment",
       y = "Chronic Disease Status",
       x = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"), plot.title = element_text(face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))

###############################################################################
###############################################################################

###1. Age Structure:

#### C. Outliers: 

boxplot(data$Age, horizontal = TRUE)

##Identifing where outliers:
boxplot(data$Age, horizontal = TRUE,
        col = "navy", xlab= "Age", border = "royalblue4",
        staplewex = 0.6, width= 1.5, )$out

summary(data$Age)

Q1 = 29.00
Q3 = 49.00

lower_limit = Q1 - (1.5*IQR(data$Age))
upper_limit = Q3 + (1.5*IQR(data$Age))


## For Age Dist, Summary stat:
library(psych)
describe(data$Age)
summary(data$Age)
Freq(data$Age,breaks = 4)

str(data)
summary(data)


####### Histogram:
library(ggplot2)

# Calculate summary statistics
mean_age <- mean(data$Age, na.rm = TRUE)
sd_age <- sd(data$Age, na.rm = TRUE)

# Create a histogram with a density plot, mean, and median lines
ggplot(data, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), binwidth = 3, fill = "blue", color = "black", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1) +
  geom_vline(aes(xintercept = mean_age), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_age + 5, y = 0.03, label = paste("Mean:", round(mean_age, 2)), color = "red") +
  labs(x = "Age", y = "Density") +
  theme_minimal() +
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12))
####################################################################################
####################################################################################

####2.  Gender:


library(epitools)

oddsratio.wald(data$Sex, data$SocialSecurity)
oddsratio.wald(data$ChronicDisease, data$SocialSecurity)

library(vcd)
gkgamma(table(data$EducationalLevel, data$SocialSecurity))

## p-value < 2.2e-16


##########################

t1 <- table(data$ChronicDisease, data$HealthInsurance)

prop.table(t1)


# Calculate the counts and percentages
chr_social_counts <- data %>%
  group_by(ChronicDisease, HealthInsurance) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

# Reorder Occupation based on total counts
total_counts <- data %>%
  group_by(ChronicDisease) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

chr_social_counts$ChronicDisease <- factor(chr_social_counts$ChronicDisease, levels = total_counts$ChronicDisease)

# Create the horizontal stacked bar chart with percentages inside the bars
ggplot(chr_social_counts, aes(x = count, y = ChronicDisease, fill = HealthInsurance)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_fill(vjust = 0.5), color = "white", size = 3.3) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(x = "Proportion", y = "Chronic Disease", fill = "Health Insurance") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))
