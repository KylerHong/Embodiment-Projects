rm(list = ls())
graphics.off()
cat("\014")
if (!require("devtools")) {
  install.packages("devtools", dependencies = TRUE)}
devtools::install_github("DejanDraschkow/mixedpower")

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

legend_data <- read_csv("sona_combined_Participants.csv",show_col_types = FALSE) %>%
  mutate(position = factor(POS),
         feedback = factor(FDBK),
         delay = factor(DLY),
         Participant = factor(Participant))

control_groups <- unique(legend_data$position)
for (group in control_groups) {
  # Filter data for the specific control group
  group_data <- legend_data %>% filter(position == group)
  y_limits <- range(group_data$IB_DIFF, na.rm = TRUE)   # Create the box-whisker plot
  p <- ggplot(group_data, aes(x = feedback, y = IB_DIFF, fill = feedback)) +
    geom_boxplot(alpha = 0.7) +
    facet_wrap(~ position) +
    labs(title = paste("Coordination Score for Control Group", group),
         x = "Haptic Condition",
         y = "Coordination Score") +
    scale_y_continuous(limits = y_limits) +  # Adjust according to your data range
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Print the plot
  print(p)
}
summary(legend_data$subject_number)


m1_test <- glmmTMB(formula = IB_DIFF ~ position*feedback +(1|delay) +(1|subject_numer), data=legend_data, family = tweedie(link = "log"))
m2_test <- lmer(formula = IB_DIFF ~ position*feedback + (1|delay)+(1|Participant), data = legend_data)
simulateResiduals(m2_test,plot=T)
emmeans(m2_test,specs=pairwise~position*feedback, type ="response")
pred <- predict(m2_test, newdata = legend_data, type = "response", se.fit = TRUE)
pred_df <- data.frame(
  feedback = legend_data$feedback,
  position = legend_data$position,
  fit = pred$fit,
  lower = pred$fit - 1.96 * pred$se.fit,
  upper = pred$fit + 1.96 * pred$se.fit
)
fixef(m2_test)
summary(m2_test)
powerSim(m2_test,nsim=100, test = fcompare(y~feedback*position))
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
summary(m2_test)
m2_test_sim <- extend(m2_test, along = "Participant", n = 23)
pwer <- mixedpower(model = m2_test, data = legend_data, fixed_effects = c("position","feedback"),simvar = "Participant",steps = c(20,30,40,50,60),critical_value =2  )
summary(m2_test)
pc1 <- powerCurve(m2_test,along="Participant",nsim=50)
  
  print(power_interaction)

n <- 23 # number of subjects
k <- 9  # number of groups
p <- 9  # number of repeated measures
alpha <- 0.05 # significance level
effect_size <- 0.37# example effect size
rho <- 0.5 # correlation between repeated measures
power_result <- pwr.anova.test(k = k, n = n, f = effect_size, sig.level = alpha)
print(power_result)

simulateResiduals(m2_test,plot=T)
