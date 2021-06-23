library(tidyverse)
library(lubridate)
library(augsynth)
# library(wakefield)

# Setting seed in order to 'recreate' the randomness
set.seed(456)

# Loading in prepared RDS, adding monthly synthetic data & aggregating by quarter
treatment_data <- readRDS("data/treatment_data.RDS")
treatment_data$live_births <- rpois(n = length(treatment_data$live_births), 100)
treatment_data$low_birth_weights[(treatment_data$treatment_group == "Control") | (treatment_data$treatment_group == "Treatment" & treatment_data$month < "2019-07-01")] <- abs(rpois(n = length(treatment_data$low_birth_weights[(treatment_data$treatment_group == "Control") | (treatment_data$treatment_group == "Treatment" & treatment_data$month < "2019-07-01")]), 5))
treatment_data$low_birth_weights[treatment_data$treatment_group == "Treatment" & treatment_data$month >= "2019-07-01"] <- abs(rpois(n = length(treatment_data$low_birth_weights[treatment_data$treatment_group == "Treatment" & treatment_data$month >= "2019-07-01"]),4.5))

treatment_data <- treatment_data %>%
  dplyr::mutate(quarter = lubridate::floor_date(month, "quarter")) %>%
  dplyr::group_by(
    treatment_group,
    district_orgunitid, 
    district_ou_name,   
    organisationunitid,
    quarter,
    treated
  ) %>%
  dplyr::summarize(
    live_births = sum(live_births),
    low_birth_weights = sum(low_birth_weights),
    rate = log(low_birth_weights/live_births + 0.00001)
  ) %>%
  dplyr::ungroup()

# Some plots
jpeg(file="presentation_images/sample_trends.jpg",
    width=12, height=8, units="in", res=500)
ggplot2::ggplot(
  treatment_data %>% 
    dplyr::group_by(treatment_group, quarter) %>%
    dplyr::summarize(
    rate = (sum(low_birth_weights)/sum(live_births))*100
  ),
  aes(x = quarter, y = rate, color = treatment_group)
) + geom_line(size = 0.8) + geom_point(size = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 8)) +
  geom_vline(xintercept = as.Date("2019-07-01"), linetype = "dotted") +
  ggtitle("Rates of Low Birth Weights: Treatment vs. Control Groups") +
  theme_bw()
dev.off()

# Trying the model
synth_model <- multisynth(
  rate ~ treated, organisationunitid, quarter,
  treatment_data, fixedeff = TRUE, n_leads = 6
)
model_summary <- summary(synth_model)
jpeg(file="presentation_images/sample_results.jpg",
    width=12, height=8, units="in", res=500)
plot(model_summary, levels = "Average")
dev.off()

# Extracting results & plotting them
model_results <- data.frame(
  model_summary$att
) 

model_results_pivoted <- model_results %>%
  reshape2::dcast(., Level ~ Time, value.var = "Estimate") %>%
  dplyr::rename(Overall_Average = "NA") %>%
  dplyr::select("Level", "0", "1", "2", "3", "4", "5", "Overall_Average") %>%
  dplyr::mutate(
    `0` = round(transform.exp(`0`),2),
    `1` = round(transform.exp(`1`),2),
    `2` = round(transform.exp(`2`),2),
    `3` = round(transform.exp(`3`),2),
    `4` = round(transform.exp(`4`),2),
    `5` = round(transform.exp(`5`),2),
  ) %>%
  dplyr::rename(
    Q1 = `0`,
    Q2 = `1`,
    Q3 = `2`,
    Q4 = `3`,
    Q5 = `4`,
    Q6 = `5`
  )

# Pulling out a couple facilities to plot
levels <- c("222709", "222706") # "222679"
jpeg(file="presentation_images/sample_facility_results.jpg",
     width=12, height=8, units="in", res=500)
  ggplot(
    data = model_results %>% filter(Level %in% levels), 
    aes(
      x = Time, 
      y = Estimate, 
      linetype = Level,
      color = Level
    )
  ) + geom_line(size = 1) + theme_bw() + ggtitle("Facility-Level Results Sample")
  dev.off()
  
library(data.table)  
model_results_transposed <- transpose(model_results_pivoted)
names(model_results_transposed) <- model_results_transposed[1,]
model_results_transposed <- model_results_transposed[-1,]
model_results_transposed <- model_results_transposed %>% select(
  "222709", "222706", "Average"
)
write.csv(model_results_transposed, "model_results_sample.csv", row.names = FALSE)
