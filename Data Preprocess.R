## Load Data:
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
data$Indusrty <- as.factor(data$Indusrty)
data$Sector.of.Employment <- as.factor(data$Sector.of.Employment)

#Continuous Variables:
data$Age <- as.numeric(data$Age)
str(data)

######################################################################################
######################################################################################
# Load necessary library
library(dplyr)

table(data$EducationalLevel)
table(data$LaborForceStatus)
table(filtered_data$MainActivityStatus)
table(data$MaritalStatus)

# Filter out individuals who are in category "Still at School" in EducationalLevel
# and those who are in category "Below Legal Age" in LaborForceStatus
filtered_data <- data %>%
  filter(data$EducationalLevel != "Still at School" & data$LaborForceStatus != "Below Legal Age" & 
           !(data$LaborForceStatus == "Inactive" & data$MainActivityStatus == "Student") & 
           data$EmploymentStatus != "Inactive and Below Legal Age")

write.csv(filtered_data, file = "new_data8.csv", row.names = FALSE)
######################################################################################################
######################################################################################################
######################################################################################################

## Trial:

library(ggplot2)

# Assuming your data frame is named 'data' and has a column 'SocialSecurity'
social_security_counts <- table(data$SocialSecurity)

# Convert counts to data frame
social_security_df <- as.data.frame(social_security_counts)
names(social_security_df) <- c("Status", "Count")

# Calculate percentages
social_security_df$Percentage <- social_security_df$Count / sum(social_security_df$Count) * 100

# Plot
ggplot(social_security_df, aes(x = "", y = Percentage, fill = Status)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#4D77FF", "orange1")) +
  labs(title = "Distribution of Social Security Status", x = NULL, y = NULL, fill = "Social Security Status") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), position = position_stack(vjust = 0.5))
###########################################################################################

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
####################################################################

sex_counts <- table(data$Sex)
sex_df <- as.data.frame(sex_counts)
names(sex_df) <- c("Sex", "Count")
sex_df$Percentage <- round((sex_df$Count / sum(sex_df$Count)) * 100, 1)

# Plot the bar chart
ggplot(sex_df, aes(x = Sex, y = Count, fill = Sex)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#4D77FF", "#1E90FF")) +
  geom_text(aes(label = Count), vjust = -0.3) +
  geom_text(aes(label = paste(Percentage, "%")), vjust = 1.8) +
  labs(title = "Distribution of Gender", x = "Sex", y = "Count") +
  theme_minimal() +
  theme(text = element_text(size = 12))
##########################################################################

sex_counts <- table(data1$Sex)

# Convert to data frame
sex_df <- as.data.frame(sex_counts)
names(sex_df) <- c("Sex", "Count")

# Calculate percentages
sex_df$Percentage <- round((sex_df$Count / sum(sex_df$Count)) * 100, 1)

# Create a pie chart
ggplot(sex_df, aes(x = "", y = Percentage, fill = Sex)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Male" = "navy", "Female" = "blue")) +
  theme_void() +
  geom_text(aes(label = paste(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "Sex")
##############################################################

marital_counts <- table(data$MaritalStatus)
marital_df <- as.data.frame(marital_counts)
names(marital_df) <- c("MaritalStatus", "Count")

# Create the bar chart
ggplot(marital_df, aes(x = MaritalStatus, y = Count, fill = MaritalStatus)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of Marital Status", x = "Marital Status", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(color = "#333333")) # Rotate the x labels for better readability


# Output the counts and proportions
marital_df$Proportion <- round((marital_df$Count / sum(marital_df$Count)) * 100, 1)
marital_df


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
#########################################################

# Assuming the column for father's presence in your dataset is named 'FathersPresenceAtHome'
fathers_presence_counts <- table(data$Father.s.Present.at.Home)
fathers_presence_df <- as.data.frame(fathers_presence_counts)
names(fathers_presence_df) <- c("FatherPresence", "Count")

# Calculate percentages
fathers_presence_df$Percentage <- round((fathers_presence_df$Count / sum(fathers_presence_df$Count)) * 100, 2)

# Create a pie chart
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
####################################################################

education_counts <- table(data$EducationalLevel)
education_df <- as.data.frame(education_counts)
names(education_df) <- c("EducationalLevel", "Count")

# Calculate percentages
education_df$Percentage <- round((education_df$Count / sum(education_df$Count)) * 100, 1)

# Create the bar chart
ggplot(education_df, aes(x = reorder(EducationalLevel, -Count), y = Count, fill = EducationalLevel)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = paste(Percentage, "%")), vjust = 0.1, color = "black") +
  labs(title = "Distribution of Individuals by Educational Level", x = "Educational Level", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(color = "#333333"),
        text = element_text(color = "#333333"), plot.title = element_text(size = 14, hjust = 0.3, face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 8)) # Rotate x labels for better readability
###############################################################################################



# Visualize the relationship between Sector and Social Security
ggplot(data, aes(x = Sector.of.Employment, fill = SocialSecurity)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Social Security Enrollment by Sector",
       y = "Proportion")

# Visualize the relationship between Industry and Social Security
ggplot(data, aes(x = Indusrty, fill = SocialSecurity)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Social Security Enrollment by Industry",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(data, aes(x = HealthInsurance, fill = SocialSecurity)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Social Security Enrollment by Health Insurance",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(x = Area, fill = SocialSecurity)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Social Security Enrollment by Area",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data, aes(x = Sector.of.Employment, fill = HealthInsurance)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Social Security Enrollment by Health Insurance",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data1, aes(x = EmploymentStatus, fill = SocialSecurity)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Social Security Enrollment by Health Insurance",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data1, aes(x =MainActivityStatus, fill = SocialSecurity)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Social Security Enrollment by Health Insurance",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data1, aes(x = Occupation, fill = SocialSecurity)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Social Security Enrollment by Health Insurance",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
##############################################################################
#############################################################################
#################################################################################



