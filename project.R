library(readxl)
library(tidyverse)
library(moments)
library(ggpubr)
library(fitdistrplus)

industry <- readxl::read_xlsx(
  path = "data/insurance_industry_stats_2016-2020.xlsx", 
  skip = 1
) %>% 
  dplyr::select(
    `Class Name`, `2016`:`2020`
  )

industry_long <- industry %>% 
  tidyr::pivot_longer(
    cols = !`Class Name`, 
    names_to = "Year", 
    values_to = "Amount"
  )

# ----desc stats----
# standard error:
se <- function(x) {
  sqrt(var(x) / length(x))
}

# descriptive stats:
desc_stats <- industry_long %>% 
  group_by(`Class Name`) %>%
  summarise(
    `No. Of Observations` = n(), 
    Mean = mean(Amount), 
    `Standard Error` = se(Amount), 
    Median = median(Amount), 
    `Standard Deviation` = sd(Amount), 
    Kurtosis = kurtosis(Amount), 
    Skewness = skewness(Amount), 
    Minimum = min(Amount), 
    Maximum = max(Amount), 
    Sum = sum(Amount)
  ) %>% 
  t() %>% 
  as.data.frame()

# set column names:
colnames(desc_stats) <- desc_stats[1, 1:2] %>% 
  gsub(pattern = "_", replacement = " ") %>% 
  stringr::str_to_title()

# remove first row:
desc_stats <- desc_stats[-1, ]

# Make rownames the `Stat` column:
desc_stats["Stat"] <- rownames(desc_stats)

# remove rownames:
rownames(desc_stats) <- NULL

# make stat the first column:
desc_stats <- desc_stats %>% 
  dplyr::relocate(Stat)

# ----hist----
mc_hist <- industry_long %>%
  dplyr::filter(`Class Name` == "motor_commercial") %>% 
  ggplot(
    aes(x = Amount)
  ) + 
  geom_histogram(
    mapping = aes(y = ..density..), 
    color = "green", 
    fill = "lightgreen"
  ) + 
  ylab(label = "Density") + 
  xlab("Claim Size") + 
  ggtitle(label = "Motor Commercial") + 
  geom_density(
    color = "firebrick", 
    lwd = 1
  ) + 
  theme_classic()

mp_hist <- industry_long %>%
  dplyr::filter(`Class Name` == "motor_private") %>% 
  ggplot(
    aes(x = Amount)
  ) + 
  geom_histogram(
    mapping = aes(y = ..density..), 
    color = "blue", 
    fill = "lightblue"
  ) + 
  ylab(label = "Density") + 
  xlab("Claim Size") + 
  ggtitle(label = "Motor Private") + 
  geom_density(
    color = "firebrick", 
    lwd = 1
  ) + 
  theme_classic()

# ----qqplots----
# From the descriptive stats and now from the histograms, 
# we can affirm that the data is positively skewed.

# This implies the need to use continuous distributions that 
# are +vely skewed to fit the data

mc_qqplot <- industry_long %>%
  dplyr::filter(`Class Name` == "motor_commercial") %>% 
  ggqqplot(
    x = "Amount", 
    title = "Motor Commercial", 
    size = 0.8
  )

mp_qqplot <- industry_long %>%
  dplyr::filter(`Class Name` == "motor_private") %>% 
  ggqqplot(
    x = "Amount", 
    title = "Motor Private", 
    size = 0.8
  )

# We used the cube root function to transform the data and 
# make the claim sizes become closer to normally distributed

# transform data:
industry_long_trans <- industry_long |> 
  dplyr::mutate(
    Amount = Amount ^ (1 / 3)
  )


# qqplots of transformed data:
mc_trans_qqplot <- industry_long_trans %>% 
  dplyr::filter(`Class Name` == "motor_commercial") %>% 
  ggqqplot(
    x = "Amount", 
    title = "Motor Commercial", 
    size = 0.8
  )

mp_trans_qqplot <- industry_long_trans %>% 
  dplyr::filter(`Class Name` == "motor_private") %>% 
  ggqqplot(
    x = "Amount", 
    title = "Motor Private", 
    size = 0.8
  )

# ----fit distrs----
# fitting distributions to the data

#Extract positive values for fitting models, x > 0
# positive_data <- industry_long_trans$Amount[industry_long_trans$Amount > 0] %>% 
#   na.omit()

industry_long_trans <- industry_long_trans |> 
  dplyr::filter(Amount > 0, !is.na(Amount))

# motor commercial data:
mc <- industry_long_trans |> 
  dplyr::filter(`Class Name` %in% "motor_commercial") |> 
  dplyr::pull(Amount)

# motor private data:
mp <- industry_long_trans |> 
  dplyr::filter(`Class Name` %in% "motor_private") |> 
  dplyr::pull(Amount)


# positive_data <- c(positive_data) #make it a vector
# positive_data

# Fit models
# (i) Exponential model, x > 0
exp_model <- fitdist(positive_data, "exp")
exp_model$estimate       #parameter estimate
exp_model$sd    #standard error of parameter
exp_model$loglik    #log likelihood estimate
gofstat(exp_model) #extract K-S, A-D, AIC, BIC of the model


# (ii) Gamma model, x > 0
gamma_model <- fitdist(positive_data, "gamma")
gamma_model$estimate       #parameters estimates
gamma_model$sd    #standard error of estimates
gamma_model$loglik    #log likelihood estimate
gofstat(gamma_model) #extract K-S, A-D, AIC, BIC of the model

# (iii) Lognormal model, x > 0
lognormal_model <- fitdist(positive_data, "lnorm")
lognormal_model$estimate       #parameters estimates
lognormal_model$sd    #standard error of estimates
lognormal_model$loglik    #log likelihood estimate
gofstat(lognormal_model) #extract K-S, A-D, AIC, BIC of the model

# (iv) Weibull model, x > 0
weibull_model <- fitdist(positive_data, "weibull")
weibull_model$estimate       #parameters estimates
weibull_model$sd    #standard error of estimates
weibull_model$loglik    #log likelihood estimate
gofstat(weibull_model) #extract K-S, A-D, AIC, BIC of the model

# (v) Pareto model, x > 0
par_data <- c(scale(positive_data))
pareto_model <- fitdist(par_data, "pareto", lower = c(0, 0), start = list(location = 1, shape = 1))
pareto_model$estimate       #parameters estimates
pareto_model$sd    #standard error of estimates
pareto_model$loglik    #log likelihood estimate
gofstat(pareto_model) #extract K-S, A-D, AIC, BIC of the model
