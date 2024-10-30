rm(list = ls())
graphics.off()
cat("\014")
if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)}
devtools::install_github("DejanDraschkow/mixedpower")

##install.packages("Matrix")

## For saving code to GitHub Type:
# git add "Stats_for_embodiment.R"
# git commit -m "initials, date, change"
# git push

## For pulling code from github
# git pull
# NOTE: For this to work you must be in this directory: C:\0_School\UC_Davis\Research\Schofield_Lab\Embodiment\embodiment_data_analysis\Embodiment-Projects

## NLME install this package - Rich Suggestion!

#hello

library(readr)
library(tidyverse)
library(pwr2ppl)
library(lme4)
library(lmerTest)
library(car)
library(arm)
library(readxl)
library(dplyr)
library(emmeans)
library(MuMIn)
library(influence.ME)
library(xlsx)
library(nortest)
library(DHARMa)
library(pwr)
library(glmmTMB)
library(ggplot2)
library(plotrix)
library(mixedpower)
library(simr)
library(e1071)
library(nlme)


legend_data <- read_csv("sona_combined_Participants.csv",show_col_types = FALSE) %>%
  mutate(position = factor(POS),
         feedback = factor(FDBK),
         delay = factor(DLY),
         Participant = factor(Participant))

# write.csv(legend_data,"Embodiment_data.csv")

## head(legend_data)

# Code for subset data with standardized ownership and agency 
ownership_control = (legend_data$Q5 + legend_data$Q6 + legend_data$Q7 + legend_data$Q8) / 4
print(ownership_control)
ownership_stan = legend_data$OWN - ownership_control
print(ownership_stan)
legend_data$own_score <- ownership_stan

agency_control = (legend_data$Q13 + legend_data$Q14 + legend_data$Q15 + legend_data$Q16) / 4
print(agency_control)
agency_stan = legend_data$AGN - agency_control
legend_data$agn_score <-agency_stan

legend_data$OWN <- legend_data$OWN + 4
legend_data$AGN <- legend_data$AGN + 4

legend_data$Q1 <- legend_data$Q1 + 4
legend_data$Q2 <- legend_data$Q2 + 4
legend_data$Q3 <- legend_data$Q3 + 4
legend_data$Q4 <- legend_data$Q4 + 4
legend_data$Q5 <- legend_data$Q5 + 4
legend_data$Q6 <- legend_data$Q6 + 4
legend_data$Q7 <- legend_data$Q7 + 4
legend_data$Q8 <- legend_data$Q8 + 4
legend_data$Q9 <- legend_data$Q9 + 4
legend_data$Q10 <- legend_data$Q10 + 4
legend_data$Q11 <- legend_data$Q11 + 4
legend_data$Q12 <- legend_data$Q12 + 4
legend_data$Q13 <- legend_data$Q13 + 4
legend_data$Q14 <- legend_data$Q14 + 4
legend_data$Q15 <- legend_data$Q15 + 4
legend_data$Q16 <- legend_data$Q16 + 4

ownership_control2 = (legend_data$Q5 + legend_data$Q6 + legend_data$Q7 + legend_data$Q8) / 4
print(ownership_control2)
ownership_stan2 = legend_data$OWN / ownership_control2
print(ownership_stan2)
legend_data$own_div <- ownership_stan2

agency_control2 = (legend_data$Q13 + legend_data$Q14 + legend_data$Q15 + legend_data$Q16) / 4
print(agency_control2)
agency_stan2 = legend_data$AGN / agency_control2
legend_data$agn_div <- agency_stan2

## Code for subset data based on factors: Position and Condition
legend_data_FWatch = subset(x = legend_data, subset = POS == 'F' & FDBK == 'WATCH')
legend_data_FControl = subset(x = legend_data, subset = POS == 'F' & FDBK == 'NB')
legend_data_FBuzz = subset(x = legend_data, subset = POS == 'F' & FDBK == 'BUZ')
legend_data_MWatch = subset(x = legend_data, subset = POS == 'M' & FDBK == 'WATCH')
legend_data_MControl = subset(x = legend_data, subset = POS == 'M' & FDBK == 'NB')
legend_data_MBuzz = subset(x = legend_data, subset = POS == 'M' & FDBK == 'BUZ')
legend_data_CWatch = subset(x = legend_data, subset = POS == 'C' & FDBK == 'WATCH')
legend_data_CControl = subset(x = legend_data, subset = POS == 'C' & FDBK == 'NB')
legend_data_CBuzz = subset(x = legend_data, subset = POS == 'C' & FDBK == 'BUZ')

## Plotting Histograms and QQ-plots based on subset data
# Proprioceptive Drift Plots
hist(legend_data_FWatch$PD)
qqnorm(legend_data_FWatch$PD)
qqline(legend_data_FWatch$PD)
hist(legend_data_FControl$PD)
qqnorm(legend_data_FControl$PD)
qqline(legend_data_FControl$PD)
hist(legend_data_FBuzz$PD)
qqnorm(legend_data_FBuzz$PD)
qqline(legend_data_FBuzz$PD)
hist(legend_data_MWatch$PD)
qqnorm(legend_data_MWatch$PD)
qqline(legend_data_MWatch$PD)
hist(legend_data_MControl$PD)
qqnorm(legend_data_MControl$PD)
qqline(legend_data_MControl$PD)
hist(legend_data_MBuzz$PD)
qqnorm(legend_data_MBuzz$PD)
qqline(legend_data_MBuzz$PD)
hist(legend_data_CWatch$PD)
qqnorm(legend_data_CWatch$PD)
qqline(legend_data_CWatch$PD)
hist(legend_data_CControl$PD)
qqnorm(legend_data_CControl$PD)
qqline(legend_data_CControl$PD)
hist(legend_data_CBuzz$PD)
qqnorm(legend_data_CBuzz$PD)
qqline(legend_data_CBuzz$PD)

# Intentional Binding Plots
hist(legend_data_FWatch$IB_DIFF)
qqnorm(legend_data_FWatch$IB_DIFF)
qqline(legend_data_FWatch$IB_DIFF)
hist(legend_data_FControl$IB_DIFF)
qqnorm(legend_data_FControl$IB_DIFF)
qqline(legend_data_FControl$IB_DIFF)
hist(legend_data_FBuzz$IB_DIFF)
qqnorm(legend_data_FBuzz$IB_DIFF)
qqline(legend_data_FBuzz$IB_DIFF)
hist(legend_data_MWatch$IB_DIFF)
qqnorm(legend_data_MWatch$IB_DIFF)
qqline(legend_data_MWatch$IB_DIFF)
hist(legend_data_MControl$IB_DIFF)
qqnorm(legend_data_MControl$IB_DIFF)
qqline(legend_data_MControl$IB_DIFF)
hist(legend_data_MBuzz$IB_DIFF)
qqnorm(legend_data_MBuzz$IB_DIFF)
qqline(legend_data_MBuzz$IB_DIFF)
hist(legend_data_CWatch$IB_DIFF)
qqnorm(legend_data_CWatch$IB_DIFF)
qqline(legend_data_CWatch$IB_DIFF)
hist(legend_data_CControl$IB_DIFF)
qqnorm(legend_data_CControl$IB_DIFF)
qqline(legend_data_CControl$IB_DIFF)
hist(legend_data_CBuzz$IB_DIFF)
qqnorm(legend_data_CBuzz$IB_DIFF)
qqline(legend_data_CBuzz$IB_DIFF)

# Agency Plots
hist(legend_data_FWatch$agn_score)
qqnorm(legend_data_FWatch$agn_score)
qqline(legend_data_FWatch$agn_score)
hist(legend_data_FControl$agn_score)
qqnorm(legend_data_FControl$agn_score)
qqline(legend_data_FControl$agn_score)
hist(legend_data_FBuzz$agn_score)
qqnorm(legend_data_FBuzz$agn_score)
qqline(legend_data_FBuzz$agn_score)
hist(legend_data_MWatch$agn_score)
qqnorm(legend_data_MWatch$agn_score)
qqline(legend_data_MWatch$agn_score)
hist(legend_data_MControl$agn_score)
qqnorm(legend_data_MControl$agn_score)
qqline(legend_data_MControl$agn_score)
hist(legend_data_MBuzz$agn_score)
qqnorm(legend_data_MBuzz$agn_score)
qqline(legend_data_MBuzz$agn_score)
hist(legend_data_CWatch$agn_score)
qqnorm(legend_data_CWatch$agn_score)
qqline(legend_data_CWatch$agn_score)
hist(legend_data_CControl$agn_score)
qqnorm(legend_data_CControl$agn_score)
qqline(legend_data_CControl$agn_score)
hist(legend_data_CBuzz$agn_score)
qqnorm(legend_data_CBuzz$agn_score)
qqline(legend_data_CBuzz$agn_score)

# Ownership Plots
hist(legend_data_FWatch$own_score)
qqnorm(legend_data_FWatch$own_score)
qqline(legend_data_FWatch$own_score)
hist(legend_data_FControl$own_score)
qqnorm(legend_data_FControl$own_score)
qqline(legend_data_FControl$own_score)
hist(legend_data_FBuzz$own_score)
qqnorm(legend_data_FBuzz$own_score)
qqline(legend_data_FBuzz$own_score)
hist(legend_data_MWatch$own_score)
qqnorm(legend_data_MWatch$own_score)
qqline(legend_data_MWatch$own_score)
hist(legend_data_MControl$own_score)
qqnorm(legend_data_MControl$own_score)
qqline(legend_data_MControl$own_score)
hist(legend_data_MBuzz$own_score)
qqnorm(legend_data_MBuzz$own_score)
qqline(legend_data_MBuzz$own_score)
hist(legend_data_CWatch$own_score)
qqnorm(legend_data_CWatch$own_score)
qqline(legend_data_CWatch$own_score)
hist(legend_data_CControl$own_score)
qqnorm(legend_data_CControl$own_score)
qqline(legend_data_CControl$own_score)
hist(legend_data_CBuzz$own_score)
qqnorm(legend_data_CBuzz$own_score)
qqline(legend_data_CBuzz$own_score)

## Normality tests for data (Kolmogorov-Smirnov) !!! 0 = no & 1 = yes !!!
#Proprioceptive drift Normality
normalFWatchPDk = ks.test( x = legend_data_FWatch$PD, y = 'pnorm')
sprintf('Is Far Watch Normally distributed: %f', normalFWatchPDk$p.value)
normalFControlPDk = ks.test( x = legend_data_FWatch$PD, y = 'pnorm')
sprintf('Is Far Control Normally distributed: %f', normalFControlPDk$p.value)
normalFBuzzPDk = ks.test( x = legend_data_FWatch$PD, y = 'pnorm')
sprintf('Is Far Buzz Normally distributed: %f', normalFBuzzPDk$p.value)
normalMWatchPDk = ks.test( x = legend_data_FWatch$PD, y = 'pnorm')
sprintf('Is Middle Watch Normally distributed: %f', normalMWatchPDk$p.value)
normalMControlPDk = ks.test( x = legend_data_FWatch$PD, y = 'pnorm')
sprintf('Is Middle Control Normally distributed: %f', normalMControlPDk$p.value)
normalMBuzzPDk = ks.test( x = legend_data_FWatch$PD, y = 'pnorm')
sprintf('Is Middle Buzz Normally distributed: %f', normalMBuzzPDk$p.value)
normalCWatchPDk = ks.test( x = legend_data_FWatch$PD, y = 'pnorm')
sprintf('Is Center Watch Normally distributed: %f', normalCWatchPDk$p.value)
normalCControlPDk = ks.test( x = legend_data_FWatch$PD, y = 'pnorm')
sprintf('Is Center Control Normally distributed: %f', normalCControlPDk$p.value)
normalCBuzzPDk = ks.test( x = legend_data_FWatch$PD, y = 'pnorm')
sprintf('Is Center Buzz Normally distributed: %f', normalCBuzzPDk$p.value)

# Intentional Binding Normality
normalFWatchIBk = ks.test( x = legend_data_FWatch$IB_GUESS, y = 'pnorm')
sprintf('Is Far Watch Normally distributed: %f', normalFWatchIBk$p.value)
normalFControlIBk = ks.test( x = legend_data_FWatch$IB_GUESS, y = 'pnorm')
sprintf('Is Far Control Normally distributed: %f', normalFControlIBk$p.value)
normalFBuzzIBk = ks.test( x = legend_data_FWatch$IB_GUESS, y = 'pnorm')
sprintf('Is Far Buzz Normally distributed: %f', normalFBuzzIBk$p.value)
normalMWatchIBk = ks.test( x = legend_data_FWatch$IB_GUESS, y = 'pnorm')
sprintf('Is Middle Watch Normally distributed: %f', normalMWatchIBk$p.value)
normalMControlIBk = ks.test( x = legend_data_FWatch$IB_GUESS, y = 'pnorm')
sprintf('Is Middle Control Normally distributed: %f', normalMControlIBk$p.value)
normalMBuzzIBk = ks.test( x = legend_data_FWatch$IB_GUESS, y = 'pnorm')
sprintf('Is Middle Buzz Normally distributed: %f', normalMBuzzIBk$p.value)
normalCWatchIBk = ks.test( x = legend_data_FWatch$IB_GUESS, y = 'pnorm')
sprintf('Is Center Watch Normally distributed: %f', normalCWatchIBk$p.value)
normalCControlIBk = ks.test( x = legend_data_FWatch$IB_GUESS, y = 'pnorm')
sprintf('Is Center Control Normally distributed: %f', normalCControlIBk$p.value)
normalCBuzzIBk = ks.test( x = legend_data_FWatch$IB_GUESS, y = 'pnorm')
sprintf('Is Center Buzz Normally distributed: %f', normalCBuzzIBk$p.value)

# Agency Normality
normalFWatchANGk = ks.test( x = legend_data_FWatch$agn_score, y = 'pnorm')
sprintf('Is Far Watch Normally distributed: %f', normalFWatchANGk$p.value)
normalFControlANGk = ks.test( x = legend_data_FWatch$agn_score, y = 'pnorm')
sprintf('Is Far Control Normally distributed: %f', normalFControlANGk$p.value)
normalFBuzzANGk = ks.test( x = legend_data_FWatch$agn_score, y = 'pnorm')
sprintf('Is Far Buzz Normally distributed: %f', normalFBuzzANGk$p.value)
normalMWatchANGk = ks.test( x = legend_data_FWatch$agn_score, y = 'pnorm')
sprintf('Is Middle Watch Normally distributed: %f', normalMWatchANGk$p.value)
normalMControlANGk = ks.test( x = legend_data_FWatch$agn_score, y = 'pnorm')
sprintf('Is Middle Control Normally distributed: %f', normalMControlANGk$p.value)
normalMBuzzANGk = ks.test( x = legend_data_FWatch$agn_score, y = 'pnorm')
sprintf('Is Middle Buzz Normally distributed: %f', normalMBuzzANGk$p.value)
normalCWatchANGk = ks.test( x = legend_data_FWatch$agn_score, y = 'pnorm')
sprintf('Is Center Watch Normally distributed: %f', normalCWatchANGk$p.value)
normalCControlANGk = ks.test( x = legend_data_FWatch$agn_score, y = 'pnorm')
sprintf('Is Center Control Normally distributed: %f', normalCControlANGk$p.value)
normalCBuzzANGk = ks.test( x = legend_data_FWatch$agn_score, y = 'pnorm')
sprintf('Is Center Buzz Normally distributed: %f', normalCBuzzANGk$p.value)

# Ownership Normality
normalFWatchOWNk = ks.test( x = legend_data_FWatch$own_score, y = 'pnorm')
sprintf('Is Far Watch Normally distributed: %f', normalFWatchOWNk$p.value)
normalFControlOWNk = ks.test( x = legend_data_FWatch$own_score, y = 'pnorm')
sprintf('Is Far Control Normally distributed: %f', normalFControlOWNk$p.value)
normalFBuzzOWNk = ks.test( x = legend_data_FWatch$own_score, y = 'pnorm')
sprintf('Is Far Buzz Normally distributed: %f', normalFBuzzOWNk$p.value)
normalMWatchOWNk = ks.test( x = legend_data_FWatch$own_score, y = 'pnorm')
sprintf('Is Middle Watch Normally distributed: %f', normalMWatchOWNk$p.value)
normalMControlOWNk = ks.test( x = legend_data_FWatch$own_score, y = 'pnorm')
sprintf('Is Middle Control Normally distributed: %f', normalMControlOWNk$p.value)
normalMBuzzOWNk = ks.test( x = legend_data_FWatch$own_score, y = 'pnorm')
sprintf('Is Middle Buzz Normally distributed: %f', normalMBuzzOWNk$p.value)
normalCWatchOWNk = ks.test( x = legend_data_FWatch$own_score, y = 'pnorm')
sprintf('Is Center Watch Normally distributed: %f', normalCWatchOWNk$p.value)
normalCControlOWNk = ks.test( x = legend_data_FWatch$own_score, y = 'pnorm')
sprintf('Is Center Control Normally distributed: %f', normalCControlOWNk$p.value)
normalCBuzzOWNk = ks.test( x = legend_data_FWatch$own_score, y = 'pnorm')
sprintf('Is Center Buzz Normally distributed: %f', normalCBuzzOWNk$p.value)

## Normality tests for data (Shapiro-Wilk) !!! 0 = no & 1 = yes !!!
#Proprioceptive drift Normality
normalFWatchPD = shapiro.test( x = legend_data_FWatch$PD)
sprintf('Is Far Watch Normally distributed: %f', normalFWatchPD$p.value)
normalFControlPD = shapiro.test( x = legend_data_FWatch$PD)
sprintf('Is Far Control Normally distributed: %f', normalFControlPD$p.value)
normalFBuzzPD = shapiro.test( x = legend_data_FWatch$PD)
sprintf('Is Far Buzz Normally distributed: %f', normalFBuzzPD$p.value)
normalMWatchPD = shapiro.test( x = legend_data_FWatch$PD)
sprintf('Is Middle Watch Normally distributed: %f', normalMWatchPD$p.value)
normalMControlPD = shapiro.test( x = legend_data_FWatch$PD)
sprintf('Is Middle Control Normally distributed: %f', normalMControlPD$p.value)
normalMBuzzPD = shapiro.test( x = legend_data_FWatch$PD)
sprintf('Is Middle Buzz Normally distributed: %f', normalMBuzzPD$p.value)
normalCWatchPD = shapiro.test( x = legend_data_FWatch$PD)
sprintf('Is Center Watch Normally distributed: %f', normalCWatchPD$p.value)
normalCControlPD = shapiro.test( x = legend_data_FWatch$PD)
sprintf('Is Center Control Normally distributed: %f', normalCControlPD$p.value)
normalCBuzzPD = shapiro.test( x = legend_data_FWatch$PD)
sprintf('Is Center Buzz Normally distributed: %f', normalCBuzzPD$p.value)

# Intentional Binding Normality
normalFWatchIB = shapiro.test( x = legend_data_FWatch$IB_GUESS)
sprintf('Is Far Watch Normally distributed: %f', normalFWatchIB$p.value)
normalFControlIB = shapiro.test( x = legend_data_FWatch$IB_GUESS)
sprintf('Is Far Control Normally distributed: %f', normalFControlIB$p.value)
normalFBuzzIB = shapiro.test( x = legend_data_FWatch$IB_GUESS)
sprintf('Is Far Buzz Normally distributed: %f', normalFBuzzIB$p.value)
normalMWatchIB = shapiro.test( x = legend_data_FWatch$IB_GUESS)
sprintf('Is Middle Watch Normally distributed: %f', normalMWatchIB$p.value)
normalMControlIB = shapiro.test( x = legend_data_FWatch$IB_GUESS)
sprintf('Is Middle Control Normally distributed: %f', normalMControlIB$p.value)
normalMBuzzIB = shapiro.test( x = legend_data_FWatch$IB_GUESS)
sprintf('Is Middle Buzz Normally distributed: %f', normalMBuzzIB$p.value)
normalCWatchIB = shapiro.test( x = legend_data_FWatch$IB_GUESS)
sprintf('Is Center Watch Normally distributed: %f', normalCWatchIB$p.value)
normalCControlIB = shapiro.test( x = legend_data_FWatch$IB_GUESS)
sprintf('Is Center Control Normally distributed: %f', normalCControlIB$p.value)
normalCBuzzIB = shapiro.test( x = legend_data_FWatch$IB_GUESS)
sprintf('Is Center Buzz Normally distributed: %f', normalCBuzzIB$p.value)

# Agency Normality
normalFWatchANG = shapiro.test( x = legend_data_FWatch$agn_score)
sprintf('Is Far Watch Normally distributed: %f', normalFWatchANG$p.value)
normalFControlANG = shapiro.test( x = legend_data_FWatch$agn_score)
sprintf('Is Far Control Normally distributed: %f', normalFControlANG$p.value)
normalFBuzzANG = shapiro.test( x = legend_data_FWatch$agn_score)
sprintf('Is Far Buzz Normally distributed: %f', normalFBuzzANG$p.value)
normalMWatchANG = shapiro.test( x = legend_data_FWatch$agn_score)
sprintf('Is Middle Watch Normally distributed: %f', normalMWatchANG$p.value)
normalMControlANG = shapiro.test( x = legend_data_FWatch$agn_score)
sprintf('Is Middle Control Normally distributed: %f', normalMControlANG$p.value)
normalMBuzzANG = shapiro.test( x = legend_data_FWatch$agn_score)
sprintf('Is Middle Buzz Normally distributed: %f', normalMBuzzANG$p.value)
normalCWatchANG = shapiro.test( x = legend_data_FWatch$agn_score)
sprintf('Is Center Watch Normally distributed: %f', normalCWatchANG$p.value)
normalCControlANG = shapiro.test( x = legend_data_FWatch$agn_score)
sprintf('Is Center Control Normally distributed: %f', normalCControlANG$p.value)
normalCBuzzANG = shapiro.test( x = legend_data_FWatch$agn_score)
sprintf('Is Center Buzz Normally distributed: %f', normalCBuzzANG$p.value)

# Ownership Normality
normalFWatchOWN = shapiro.test( x = legend_data_FWatch$own_score)
sprintf('Is Far Watch Normally distributed: %f', normalFWatchOWN$p.value)
normalFControlOWN = shapiro.test( x = legend_data_FWatch$own_score)
sprintf('Is Far Control Normally distributed: %f', normalFControlOWN$p.value)
normalFBuzzOWN = shapiro.test( x = legend_data_FWatch$own_score)
sprintf('Is Far Buzz Normally distributed: %f', normalFBuzzOWN$p.value)
normalMWatchOWN = shapiro.test( x = legend_data_FWatch$own_score)
sprintf('Is Middle Watch Normally distributed: %f', normalMWatchOWN$p.value)
normalMControlOWN = shapiro.test( x = legend_data_FWatch$own_score)
sprintf('Is Middle Control Normally distributed: %f', normalMControlOWN$p.value)
normalMBuzzOWN = shapiro.test( x = legend_data_FWatch$own_score)
sprintf('Is Middle Buzz Normally distributed: %f', normalMBuzzOWN$p.value)
normalCWatchOWN = shapiro.test( x = legend_data_FWatch$own_score)
sprintf('Is Center Watch Normally distributed: %f', normalCWatchOWN$p.value)
normalCControlOWN = shapiro.test( x = legend_data_FWatch$own_score)
sprintf('Is Center Control Normally distributed: %f', normalCControlOWN$p.value)
normalCBuzzOWN = shapiro.test( x = legend_data_FWatch$own_score)
sprintf('Is Center Buzz Normally distributed: %f', normalCBuzzOWN$p.value)


## Creating Boxplots Based On Data
control_groups <- unique(legend_data$position)
for (group in control_groups) {
  # Filter data for the specific control group
  group_data <- legend_data %>% filter(position == group)
  y_limits <- range(group_data$IB_DIFF, na.rm = TRUE)   # Create the box-whisker plot
  p <- ggplot(group_data, aes(x = feedback, y = IB_DIFF, fill = feedback)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~ position) +
    labs(title = paste("Intentional Binding Score for Each Condition", group),
         x = "Haptic Condition",
         y = "Intentional Binding (ms)") +
    scale_y_continuous(limits = y_limits) +  # Adjust according to your data range
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Print the plot
  print(p)
}
control_groups <- unique(legend_data$position)
for (group in control_groups) {
  # Filter data for the specific control group
  group_data <- legend_data %>% filter(position == group)
  y_limits <- range(group_data$PD, na.rm = TRUE)   # Create the box-whisker plot
  p <- ggplot(group_data, aes(x = feedback, y = PD, fill = feedback)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~ position) +
    labs(title = paste("Proprioceptive Drift Score for Each Condition", group),
         x = "Haptic Condition",
         y = "Proprioceptive Drift (mm)") +
    scale_y_continuous(limits = y_limits) +  # Adjust according to your data range
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Print the plot
  print(p)
}
control_groups <- unique(legend_data$position)
for (group in control_groups) {
  # Filter data for the specific control group
  group_data <- legend_data %>% filter(position == group)
  y_limits <- range(group_data$agn_score, na.rm = TRUE)   # Create the box-whisker plot
  p <- ggplot(group_data, aes(x = feedback, y = agn_score, fill = feedback)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~ position) +
    labs(title = paste("Agency Score for Each Condition", group),
         x = "Haptic Condition",
         y = "Agency Score") +
    scale_y_continuous(limits = y_limits) +  # Adjust according to your data range
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Print the plot
  print(p)
}
control_groups <- unique(legend_data$position)
for (group in control_groups) {
  # Filter data for the specific control group
  group_data <- legend_data %>% filter(position == group)
  y_limits <- range(group_data$own_score, na.rm = TRUE)   # Create the box-whisker plot
  p <- ggplot(group_data, aes(x = feedback, y = own_score, fill = feedback)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~ position) +
    labs(title = paste("Ownership Score for Each Condition", group),
         x = "Haptic Condition",
         y = "Ownership Score") +
    scale_y_continuous(limits = y_limits) +  # Adjust according to your data range
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Print the plot
  print(p)
}
## Creating The Linear Mixed Effect Models
#summary(legend_data$subject_number)
summary(legend_data$Participant)

# can add +(1|delay)

## Intentional Binding Model +(1|delay)
m1_testIB <- glmmTMB(formula = IB_DIFF ~ position*feedback +(1|delay) +(1|Participant), data=legend_data, family = tweedie(link = "log"))
m2_testIB <- lmer(formula = IB_DIFF ~ position*feedback +(1|delay) +(1|Participant), data = legend_data)
simulateResiduals(m2_testIB,plot=T)
emmeans(m2_testIB,specs=pairwise~position*feedback, type ="response")
pred <- predict(m2_testIB, newdata = legend_data, type = "response", se.fit = TRUE)
pred_df <- data.frame(
  feedback = legend_data$feedback,
  position = legend_data$position,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)
fixef(m2_testIB)
summary(m2_testIB)
# powerSim(m2_testIB,nsim=100, test = fcompare(y~feedback*position))
# Plot the predicted values with confidence intervals
agg_pred_df <- aggregate(cbind(fit, lower, upper) ~ feedback  + position, data = pred_df, FUN = mean)

p <- ggplot(agg_pred_df, aes(x = feedback, y = fit, fill = position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.9), width = 0.2) +  
  labs(x = "feedback", y = "Intentional Binding") +
  theme_minimal() +
  theme(legend.position = "top")


# Printing the plot
print(p)
summary(m2_testIB)
m2_test_sim <- extend(m2_testIB, along = "Participant", n = 23)
# pwer <- mixedpower(model = m2_testIB, data = legend_data, fixed_effects = c("position","feedback"),simvar = "Participant",steps = c(20,30,40,50,60),critical_value =2  )
summary(m2_testIB)
# pc1 <- powerCurve(m2_testIB,along="Participant",nsim=50)

# print(power_interaction)

n <- 23 # number of subjects
k <- 9  # number of groups
p <- 9  # number of repeated measures
alpha <- 0.05 # significance level
effect_size <- 0.37# example effect size
rho <- 0.5 # correlation between repeated measures
power_result <- pwr.anova.test(k = k, n = n, f = effect_size, sig.level = alpha)
print(power_result)

simulateResiduals(m2_testIB,plot=T)

## Proprioceptive Drift Model
m1_testPD <- glmmTMB(formula = PD ~ position*feedback +(1|Participant), data=legend_data, family = tweedie(link = "log"))
m2_testPD <- lmer(formula = PD ~ position*feedback +(1|Participant), data = legend_data)
simulateResiduals(m2_testPD,plot=T)
emmeans(m2_testPD,specs=pairwise~position*feedback, type ="response")
pred <- predict(m2_testPD, newdata = legend_data, type = "response", se.fit = TRUE)
pred_df <- data.frame(
  feedback = legend_data$feedback,
  position = legend_data$position,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)
fixef(m2_testPD)
summary(m2_testPD)
# powerSim(m2_testPD,nsim=100, test = fcompare(y~feedback*position))
# Plot the predicted values with confidence intervals
agg_pred_df <- aggregate(cbind(fit, lower, upper) ~ feedback  + position, data = pred_df, FUN = mean)

p <- ggplot(agg_pred_df, aes(x = feedback, y = fit, fill = position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.9), width = 0.2) +  
  labs(x = "feedback", y = "Intentional Binding") +
  theme_minimal() +
  theme(legend.position = "top")


# Printing the plot
print(p)
summary(m2_testPD)
m2_test_sim <- extend(m2_testPD, along = "Participant", n = 23)
# pwer <- mixedpower(model = m2_testPD, data = legend_data, fixed_effects = c("position","feedback"),simvar = "Participant",steps = c(20,30,40,50,60),critical_value =2  )
summary(m2_testPD)
# pc1 <- powerCurve(m2_testPD,along="Participant",nsim=50)

# print(power_interaction)

n <- 23 # number of subjects
k <- 9  # number of groups
p <- 9  # number of repeated measures
alpha <- 0.05 # significance level
effect_size <- 0.37# example effect size
rho <- 0.5 # correlation between repeated measures
power_result <- pwr.anova.test(k = k, n = n, f = effect_size, sig.level = alpha)
print(power_result)

simulateResiduals(m2_testPD,plot=T)

## Agency Model
m1_testAGN <- glmmTMB(formula = agn_score ~ position*feedback +(1|Participant), data=legend_data, family = tweedie(link = "log"))
m2_testAGN <- lmer(formula = agn_score ~ position*feedback +(1|Participant), data = legend_data)
leveneTest(resid(m2_testAGN) ~ legend_data$feedback)
simulateResiduals(m2_testAGN,plot=T)
test2 <- lme(agn_score ~ position*feedback, random = ~1|Participant,
             data = legend_data, method = "REML", weights = varIdent(form=~ 1|feedback),
             control = lmeControl(singular.ok=TRUE,returnObject = TRUE))
plot(m2_testAGN)
emmeans(m2_testAGN,specs=pairwise~position*feedback, type ="response")
pred <- predict(m2_testAGN, newdata = legend_data, type = "response", se.fit = TRUE)
pred_df <- data.frame(
  feedback = legend_data$feedback,
  position = legend_data$position,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)
fixef(m2_testAGN)
summary(m2_testAGN)
# powerSim(m2_testAGN,nsim=100, test = fcompare(y~feedback*position))
# Plot the predicted values with confidence intervals
agg_pred_df <- aggregate(cbind(fit, lower, upper) ~ feedback  + position, data = pred_df, FUN = mean)

p <- ggplot(agg_pred_df, aes(x = feedback, y = fit, fill = position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.9), width = 0.2) +  
  labs(x = "feedback", y = "Intentional Binding") +
  theme_minimal() +
  theme(legend.position = "top")


# Printing the plot
print(p)
summary(m2_testAGN)
m2_test_sim <- extend(m2_testAGN, along = "Participant", n = 23)
# pwer <- mixedpower(model = m2_testAGN, data = legend_data, fixed_effects = c("position","feedback"),simvar = "Participant",steps = c(20,30,40,50,60),critical_value =2  )
summary(m2_testAGN)
# pc1 <- powerCurve(m2_testAGN,along="Participant",nsim=50)

# print(power_interaction)

n <- 23 # number of subjects
k <- 9  # number of groups
p <- 9  # number of repeated measures
alpha <- 0.05 # significance level
effect_size <- 0.37# example effect size
rho <- 0.5 # correlation between repeated measures
power_result <- pwr.anova.test(k = k, n = n, f = effect_size, sig.level = alpha)
print(power_result)

simulateResiduals(m2_testAGN,plot=T)

## Ownership Model
m1_testOWN <- glmmTMB(formula = own_score ~ position*feedback +(1|Participant), data=legend_data, family = tweedie(link = "log"))
m2_testOWN <- lmer(formula = own_score ~ position*feedback +(1|Participant), data = legend_data)
simulateResiduals(m2_testOWN,plot=T)
emmeans(m2_testOWN,specs=pairwise~position*feedback, type ="response")
pred <- predict(m2_testOWN, newdata = legend_data, type = "response", se.fit = TRUE)
pred_df <- data.frame(
  feedback = legend_data$feedback,
  position = legend_data$position,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)
fixef(m2_testOWN)
summary(m2_testOWN)
# powerSim(m2_testOWN,nsim=100, test = fcompare(y~feedback*position))
# Plot the predicted values with confidence intervals
agg_pred_df <- aggregate(cbind(fit, lower, upper) ~ feedback  + position, data = pred_df, FUN = mean)

p <- ggplot(agg_pred_df, aes(x = feedback, y = fit, fill = position)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.9), width = 0.2) +  
  labs(x = "feedback", y = "Intentional Binding") +
  theme_minimal() +
  theme(legend.position = "top")


# Printing the plot
print(p)
summary(m2_testOWN)
m2_test_sim <- extend(m2_testOWN, along = "Participant", n = 23)
# pwer <- mixedpower(model = m2_testOWN, data = legend_data, fixed_effects = c("position","feedback"),simvar = "Participant",steps = c(20,30,40,50,60),critical_value =2  )
summary(m2_testOWN)
# pc1 <- powerCurve(m2_testOWN,along="Participant",nsim=50)

# print(power_interaction)

n <- 23 # number of subjects
k <- 9  # number of groups
p <- 9  # number of repeated measures
alpha <- 0.05 # significance level
effect_size <- 0.37# example effect size
rho <- 0.5 # correlation between repeated measures
power_result <- pwr.anova.test(k = k, n = n, f = effect_size, sig.level = alpha)
print(power_result)

simulateResiduals(m2_testOWN,plot=T)


# Making the multivariate LMER model Based on Rich's Instructions

colnames(legend_data)
legend_data <- legend_data %>%
  rename(id = `#`)

legend_data_short <- legend_data[,c('block','id','OWN','AGN','IB_DIFF','PD','Participant','position','feedback')]
is.data.frame(legend_data_short)

# Scale the response variables
morph_vars <- c("IB_DIFF","PD","AGN","OWN")
# morph_vars <- c("IB_DIFF","PD","agn_score","own_score")
# morph_vars <- c("IB_DIFF","PD","agn_div","own_div")
morph_vars_sc <- paste(morph_vars,"s",sep="_")
legend_data_scaled <- NULL
legend_data_scaled <- legend_data
for (i in 1:length(morph_vars)) {
  legend_data_scaled[[morph_vars_sc[i]]] <- c(scale(legend_data[[morph_vars[i]]]))
}
# legend_data_scaled = subset(legend_data_scaled, select = -c(IB_DIFF,PD,own_div,agn_div))
# legend_data_scaled = subset(legend_data_scaled, select = -c(IB_DIFF,PD,agn_score,own_score))
legend_data_scaled = subset(legend_data_scaled, select = -c(IB_DIFF,PD,AGN,OWN))
colnames(legend_data_scaled)
legend_data2 <- NULL
# Creating the long table
legend_data2 <- (legend_data_scaled # Could also use legend_data_short
                 # %>% pivot_longer(cols = -c(block,id,Participant,position,feedback), names_to = "trait", values_to = "values")
                 %>% pivot_longer(cols = -c(block,id,Participant,position,feedback,FDBK,DLY,IB_GUESS,POS,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,delay,own_score,agn_score,own_div,agn_div), names_to = "trait", values_to = "values")
                 # %>% pivot_longer(cols = -c(block,id,Participant,position,feedback,FDBK,DLY,IB_GUESS,POS,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,delay,OWN,AGN,own_div,agn_div), names_to = "trait", values_to = "values")
                 # %>% pivot_longer(cols = -c(block,id,Participant,position,feedback,FDBK,DLY,IB_GUESS,POS,Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,delay,OWN,AGN,own_score,agn_score), names_to = "trait", values_to = "values")
                 %>% drop_na()
                 %>% mutate(units = factor(1:n()))
                 %>% dplyr::arrange(units)
)
# Creating the model
FULL_mlmer <- lmer(values ~ trait:(position*feedback) - 1 + (trait - 1|Participant),
                   data=legend_data2,
                   control=lmerControl(
                     optimizer="bobyqa",
                     check.nobs.vs.nlev="ignore",
                     check.nobs.vs.nRE="ignore"))

#install.packages("reshape2")
#install.packages("corrplot")
library(corrplot)
library(reshape2)
library(ggplot2)

VarCorr(FULL_mlmer)
summary(FULL_mlmer)
print(FULL_mlmer, correlation=TRUE)
vcov(FULL_mlmer)  

vv1 <- VarCorr(FULL_mlmer)
corrplot.mixed(cov2cor(vv1$Participant),upper="ellipse")

vcov_matrix <- vcov(FULL_mlmer)
vcov_matrix_regular <- as.matrix(vcov(FULL_mlmer))
vcov_df <- as.data.frame(vcov_matrix_regular)

str(vcov_df)

vcov_long <- melt(vcov_matrix_regular)

ggplot(vcov_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white", lwd = 0.5, linetype = "solid") +  # Add tile borders
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(min(vcov_long$value), max(vcov_long$value)), 
                       space = "Lab", name = "Variance-Covariance") +
  theme_minimal(base_size = 15) +   # Increase base font size for better readability
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 4, hjust = 1),
        axis.text.y = element_text(size = 4)) +   # Customize axis text sizes
  labs(x = 'Variables', y = 'Variables', title = 'Variance-Covariance Matrix') +
  coord_fixed() +  # Fix the aspect ratio to make squares even
  geom_text(aes(label = round(value, 2)), color = "black", size = 0.7)  # Add labels to tiles

# Corrplot Version

cor_matrix <- cov2cor(vcov_matrix_regular)

corrplot(cor_matrix, 
         method = "color",       # Use colored tiles
         type = 'full',         # Only show the upper triangle
         addCoef.col = NULL,  # Add correlation coefficients
         tl.cex = 0.25,           # Increase axis text size
         tl.col = "black",       # Text label color
         tl.srt = 45,            # Text label rotation
         col = colorRampPalette(c("blue", "white", "red"))(200), # Color gradient
         title = "Variance-Covariance Matrix", 
         mar = c(0, 0, 1, 0))    # Add some space for the title


### DEVELOPING THE EFFECT SIZE SUMMARY TABLE:

## STEP 1: Adding two columns to the data: One for Active vs. Passive and the second for Buzz vs. No Buzz
new_data <- legend_data
Active <- c()
Buzz <- c()

for(i in 1:(nrow(new_data))) {
  if(new_data[i, 4] == 'BUZ'){
    x = 1
    y = 1
    Active[i] <- x
    Buzz[i] <- y
  }
  
  if(new_data[i, 4] == 'NB'){
    x = 1
    y = 0
    Active[i] <- x
    Buzz[i] <- y
  }
  
  if(new_data[i, 4] == 'WATCH'){
    x = 0
    y = 0
    Active[i] <- x
    Buzz[i] <- y
  }
}

#print(Active)
#print(Buzz)

new_data$Active <- factor(Active)
new_data$Buzz <- factor(Buzz)

# New data is set up. Step 1 is complete.

## STEP 2: Create LMER and GMER for EACH dependent variable replacing "Feedback" with "Active" and "Buzz".
##         Since we are now working with three variables, we must get models for all interactions = 10 models/variable

# Intentional Binding
IB1_gmer <- glmmTMB(formula = IB_DIFF ~ position+Active+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
IB1_lmer <- lmer(formula = IB_DIFF ~ position+Active+Buzz +(1|Participant), data = new_data)
IB2_gmer <- glmmTMB(formula = IB_DIFF ~ position*Active+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
IB2_lmer <- lmer(formula = IB_DIFF ~ position*Active+Buzz +(1|Participant), data = new_data)
IB3_gmer <- glmmTMB(formula = IB_DIFF ~ position+Active*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
IB3_lmer <- lmer(formula = IB_DIFF ~ position+Active*Buzz +(1|Participant), data = new_data)
IB4_gmer <- glmmTMB(formula = IB_DIFF ~ Active+position*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
IB4_lmer <- lmer(formula = IB_DIFF ~ Active+position*Buzz +(1|Participant), data = new_data)
IB5_gmer <- glmmTMB(formula = IB_DIFF ~ position*Active*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
IB5_lmer <- lmer(formula = IB_DIFF ~ position*Active*Buzz +(1|Participant), data = new_data)

# Proprioceptive Drift
PD1_gmer <- glmmTMB(formula = PD ~ position+Active+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
PD1_lmer <- lmer(formula = PD ~ position+Active+Buzz +(1|Participant), data = new_data)
PD2_gmer <- glmmTMB(formula = PD ~ position*Active+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
PD2_lmer <- lmer(formula = PD ~ position*Active+Buzz +(1|Participant), data = new_data)
PD3_gmer <- glmmTMB(formula = PD ~ position+Active*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
PD3_lmer <- lmer(formula = PD ~ position+Active*Buzz +(1|Participant), data = new_data)
PD4_gmer <- glmmTMB(formula = PD ~ Active+position*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
PD4_lmer <- lmer(formula = PD ~ Active+position*Buzz +(1|Participant), data = new_data)
PD5_gmer <- glmmTMB(formula = PD ~ position*Active*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
PD5_lmer <- lmer(formula = PD ~ position*Active*Buzz +(1|Participant), data = new_data)

# Agency Questionnaire
AGN1_gmer <- glmmTMB(formula = agn_score ~ position+Active+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
AGN1_lmer <- lmer(formula = agn_score ~ position+Active+Buzz +(1|Participant), data = new_data)
AGN2_gmer <- glmmTMB(formula = agn_score ~ position*Active+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
AGN2_lmer <- lmer(formula = agn_score ~ position*Active+Buzz +(1|Participant), data = new_data)
AGN3_gmer <- glmmTMB(formula = agn_score ~ position+Active*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
AGN3_lmer <- lmer(formula = agn_score ~ position+Active*Buzz +(1|Participant), data = new_data)
AGN4_gmer <- glmmTMB(formula = agn_score ~ Active+position*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
AGN4_lmer <- lmer(formula = agn_score ~ Active+position*Buzz +(1|Participant), data = new_data)
AGN5_gmer <- glmmTMB(formula = agn_score ~ position*Active*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
AGN5_lmer <- lmer(formula = agn_score ~ position*Active*Buzz +(1|Participant), data = new_data)

# Ownership Questionnaire
OWN1_gmer <- glmmTMB(formula = own_score ~ position+Active+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
OWN1_lmer <- lmer(formula = own_score ~ position+Active+Buzz +(1|Participant), data = new_data)
OWN2_gmer <- glmmTMB(formula = own_score ~ position*Active+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
OWN2_lmer <- lmer(formula = own_score ~ position*Active+Buzz +(1|Participant), data = new_data)
OWN3_gmer <- glmmTMB(formula = own_score ~ position+Active*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
OWN3_lmer <- lmer(formula = own_score ~ position+Active*Buzz +(1|Participant), data = new_data)
OWN4_gmer <- glmmTMB(formula = own_score ~ Active+position*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
OWN4_lmer <- lmer(formula = own_score ~ Active+position*Buzz +(1|Participant), data = new_data)
OWN5_gmer <- glmmTMB(formula = own_score ~ position*Active*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
OWN5_lmer <- lmer(formula = own_score ~ position*Active*Buzz +(1|Participant), data = new_data)

# New models have been created total = 32... step 2 complete.

## STEP 3: Finding models which best fit the data using the AIC score from ANOVA. Must run Anova for each dependent variable
# IB ANOVA
#anova(IB1_gmer,IB1_lmer,IB2_gmer,IB2_lmer,IB3_gmer,IB3_lmer,IB4_gmer,IB4_lmer)
anova(IB1_gmer,IB2_gmer,IB3_gmer,IB4_gmer,IB5_gmer)
anova(IB1_lmer,IB2_lmer,IB3_lmer,IB4_lmer,IB5_lmer)
# PD ANOVA
#anova(PD1_gmer,PD1_lmer,PD2_gmer,PD2_lmer,PD3_gmer,PD3_lmer,PD4_gmer,PD4_lmer)
anova(PD1_gmer,PD2_gmer,PD3_gmer,PD4_gmer,PD5_gmer)
anova(PD1_lmer,PD2_lmer,PD3_lmer,PD4_lmer,PD5_lmer)
# AGN ANOVA
#anova(AGN1_gmer,AGN1_lmer,AGN2_gmer,AGN2_lmer,AGN3_gmer,AGN3_lmer,AGN4_gmer,AGN4_lmer)
anova(AGN1_gmer,AGN2_gmer,AGN3_gmer,AGN4_gmer,AGN5_gmer)
anova(AGN1_lmer,AGN2_lmer,AGN3_lmer,AGN4_lmer,AGN5_lmer)
# OWN ANOVA
#anova(OWN1_gmer,OWN1_lmer,OWN2_gmer,OWN2_lmer,OWN3_gmer,OWN3_lmer,OWN4_gmer,OWN4_lmer)
anova(OWN1_gmer,OWN2_gmer,OWN3_gmer,OWN4_gmer,OWN5_gmer)
anova(OWN1_lmer,OWN2_lmer,OWN3_lmer,OWN4_lmer,OWN5_lmer)

# WINNERS: GMER for all!
# IB1_gmer or IB2_gmer
# PD5_gmer
# AGN5_gmer
# OWN5_gmer

# Step 3 complete.

## STEP 4: Summarize the selected models with QQ plots !!!
# IB
simulateResiduals(IB1_gmer,plot=T)
simulateResiduals(IB2_gmer,plot=T)
#PD
simulateResiduals(PD4_gmer,plot=T)
# AGN
simulateResiduals(AGN4_gmer,plot=T)
# OWN
simulateResiduals(OWN4_gmer,plot=T)

# These plots look rough :( ... Let me look at the LMER ones
## STEP 5: Looking at best LMER models and their residuals
# IB
simulateResiduals(IB1_lmer,plot=T)
simulateResiduals(IB2_lmer,plot=T)
#PD
simulateResiduals(PD4_lmer,plot=T)
# AGN
simulateResiduals(AGN4_lmer,plot=T)
# OWN
simulateResiduals(OWN4_lmer,plot=T)

### New Approach, Building Models based on two variable interactions instead of three (get rid of correlation)
### Will ask how to do three way interaction if we have to.
## Building Models:
# IB
IB_lmer_actv_pos <- lmer(formula = IB_DIFF ~ position*Active +(1|Participant), data = new_data)
IB_gmer_actv_pos <- glmmTMB(formula = IB_DIFF ~ position*Active +(1|Participant), data=new_data, family = tweedie(link = "log"))
IB2_lmer_buz_pos <- lmer(formula = IB_DIFF ~ position*Buzz +(1|Participant), data = new_data)
IB2_gmer_buz_pos <- glmmTMB(formula = IB_DIFF ~ position*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))

IB3_lmer_actv_pos <- lmer(formula = IB_DIFF ~ position+Active +(1|Participant), data = new_data)
IB3_gmer_actv_pos <- glmmTMB(formula = IB_DIFF ~ position+Active +(1|Participant), data=new_data, family = tweedie(link = "log"))
IB4_lmer_buz_pos <- lmer(formula = IB_DIFF ~ position+Buzz +(1|Participant), data = new_data)
IB4_gmer_buz_pos <- glmmTMB(formula = IB_DIFF ~ position+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
# PD
PD_lmer_actv_pos <- lmer(formula = PD ~ position*Active +(1|Participant), data = new_data)
PD_gmer_actv_pos <- glmmTMB(formula = PD ~ position*Active +(1|Participant), data=new_data, family = tweedie(link = "log"))
PD2_lmer_buz_pos <- lmer(formula = PD ~ position*Buzz +(1|Participant), data = new_data)
PD2_gmer_buz_pos <- glmmTMB(formula = PD ~ position*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))

PD3_lmer_actv_pos <- lmer(formula = PD ~ position+Active +(1|Participant), data = new_data)
PD3_gmer_actv_pos <- glmmTMB(formula = PD ~ position+Active +(1|Participant), data=new_data, family = tweedie(link = "log"))
PD4_lmer_buz_pos <- lmer(formula = PD ~ position+Buzz +(1|Participant), data = new_data)
PD4_gmer_buz_pos <- glmmTMB(formula = PD ~ position+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
# AGN
AGN_lmer_actv_pos <- lmer(formula = agn_div ~ position*Active +(1|Participant), data = new_data)
AGN_gmer_actv_pos <- glmmTMB(formula = agn_div ~ position*Active +(1|Participant), data=new_data, family = tweedie(link = "log"))
AGN2_lmer_buz_pos <- lmer(formula = agn_div ~ position*Buzz +(1|Participant), data = new_data)
AGN2_gmer_buz_pos <- glmmTMB(formula = agn_div ~ position*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))

AGN3_lmer_actv_pos <- lmer(formula = agn_div ~ position+Active +(1|Participant), data = new_data)
AGN3_gmer_actv_pos <- glmmTMB(formula = agn_div ~ position+Active +(1|Participant), data=new_data, family = tweedie(link = "log"))
AGN4_lmer_buz_pos <- lmer(formula = agn_div ~ position+Buzz +(1|Participant), data = new_data)
AGN4_gmer_buz_pos <- glmmTMB(formula = agn_div ~ position+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))
# OWN
OWN_lmer_actv_pos <- lmer(formula = own_div ~ position*Active +(1|Participant), data = new_data)
OWN_gmer_actv_pos <- glmmTMB(formula = own_div ~ position*Active +(1|Participant), data=new_data, family = tweedie(link = "log"))
OWN2_lmer_buz_pos <- lmer(formula = own_div ~ position*Buzz +(1|Participant), data = new_data)
OWN2_gmer_buz_pos <- glmmTMB(formula = own_div ~ position*Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))

OWN3_lmer_actv_pos <- lmer(formula = own_div ~ position+Active +(1|Participant), data = new_data)
OWN3_gmer_actv_pos <- glmmTMB(formula = own_div ~ position+Active +(1|Participant), data=new_data, family = tweedie(link = "log"))
OWN4_lmer_buz_pos <- lmer(formula = own_div ~ position+Buzz +(1|Participant), data = new_data)
OWN4_gmer_buz_pos <- glmmTMB(formula = own_div ~ position+Buzz +(1|Participant), data=new_data, family = tweedie(link = "log"))

## Comparison Tests:
# IB ANOVA
#anova(IB1_gmer,IB1_lmer,IB2_gmer,IB2_lmer,IB3_gmer,IB3_lmer,IB4_gmer,IB4_lmer)
anova(IB_gmer_actv_pos,IB3_gmer_actv_pos)
anova(IB2_gmer_buz_pos,IB4_gmer_buz_pos)
anova(IB_lmer_actv_pos,IB3_lmer_actv_pos)
anova(IB2_lmer_buz_pos,IB4_lmer_buz_pos)
# PD ANOVA
#anova(PD1_gmer,PD1_lmer,PD2_gmer,PD2_lmer,PD3_gmer,PD3_lmer,PD4_gmer,PD4_lmer)
anova(PD_gmer_actv_pos,PD3_gmer_actv_pos)
anova(PD2_gmer_buz_pos,PD4_gmer_buz_pos)
anova(PD_lmer_actv_pos,PD3_lmer_actv_pos)
anova(PD2_lmer_buz_pos,PD4_lmer_buz_pos)
# AGN ANOVA
#anova(AGN1_gmer,AGN1_lmer,AGN2_gmer,AGN2_lmer,AGN3_gmer,AGN3_lmer,AGN4_gmer,AGN4_lmer)
anova(AGN_gmer_actv_pos,AGN3_gmer_actv_pos)
anova(AGN2_gmer_buz_pos,AGN4_gmer_buz_pos)
anova(AGN_lmer_actv_pos,AGN3_lmer_actv_pos)
anova(AGN2_lmer_buz_pos,AGN4_lmer_buz_pos)
# OWN ANOVA
#anova(OWN1_gmer,OWN1_lmer,OWN2_gmer,OWN2_lmer,OWN3_gmer,OWN3_lmer,OWN4_gmer,OWN4_lmer)
anova(OWN_gmer_actv_pos,OWN3_gmer_actv_pos)
anova(OWN2_gmer_buz_pos,OWN4_gmer_buz_pos)
anova(OWN_lmer_actv_pos,OWN3_lmer_actv_pos)
anova(OWN2_lmer_buz_pos,OWN4_lmer_buz_pos)

## Simulating Residuals of Models:
# IB:
simulateResiduals(IB3_gmer_actv_pos,plot=T)
simulateResiduals(IB4_gmer_buz_pos,plot=T)
# PD:
simulateResiduals(PD3_gmer_actv_pos,plot=T)
simulateResiduals(PD2_gmer_buz_pos,plot=T)
# AGN:
simulateResiduals(AGN_lmer_actv_pos,plot=T)
simulateResiduals(AGN4_gmer_buz_pos,plot=T)
# OWN:
simulateResiduals(OWN_gmer_actv_pos,plot=T)
simulateResiduals(OWN2_gmer_buz_pos,plot=T)
