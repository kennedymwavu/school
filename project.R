library(readxl)
library(tidyverse)
library(moments)
library(ggpubr)
library(actuar)
library(fitdistrplus)
library(kableExtra)
library(glue)

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
# Extract positive values for fitting models, x > 0, and remove 
# missing values:
industry_long_trans <- industry_long_trans |> 
  dplyr::filter(Amount > 0, !is.na(Amount))

# positive data:
positive_data <- industry_long_trans |> 
  dplyr::filter(Amount > 0, !is.na(Amount)) |> 
  dplyr::group_by(`Class Name`) |> 
  dplyr::mutate(indices = 1:n()) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider(
    id_cols = indices, 
    names_from = `Class Name`, 
    values_from = Amount
  ) |> 
  dplyr::select(-indices)

# |- exp----
exp_model <- purrr::map(
  .x = positive_data, 
  .f = ~ fitdist(
    data = na.omit(.x) |> as.vector(), 
    distr = "exp"
  )
)

exp_gof <- purrr::map(.x = exp_model, .f = gofstat)
# extract K-S, A-D, AIC, BIC of the model

# exp model data.frame:
exp_model_df <- exp_model |> 
  purrr::imap(
    .f = ~ tibble::tibble(
      Distribution = "Exponential", 
      Parameter = c("Rate", "Std. Error", "LLF")
    ) |> 
      dplyr::mutate(
        "{.y}" := c(.x$estimate, .x$sd, .x$loglik)
      )
  ) |> 
  Reduce(
    f = function(...) {
      dplyr::full_join(..., by = c("Distribution", "Parameter"))
    }
  )

# |- gamma----
gamma_model <- purrr::map(
  .x = positive_data, 
  .f = ~ fitdist(
    data = na.omit(.x) |> as.vector(), 
    distr = "gamma"
  )
)

gamma_gof <- purrr::map(
  .x = gamma_model, 
  .f = gofstat
)

gamma_model_df <- gamma_model |> 
  purrr::imap(
    .f = ~ tibble::tibble(
      Distribution = "Gamma", 
      Parameter = c(
        "Shape", "Shape Std. Error", "Rate", "Rate Std. Error", "LLF"
      )
    ) |> 
      dplyr::mutate(
        "{.y}" := c(
          .x$estimate[["shape"]], .x$sd[["shape"]], 
          .x$estimate[["rate"]], .x$sd[["rate"]], 
          .x$loglik
        )
      )
  ) |> 
  Reduce(
    f = function(...) {
      dplyr::full_join(..., by = c("Distribution", "Parameter"))
    }
  )

# |- lognormal----
lnorm_model <- purrr::map(
  .x = positive_data, 
  .f = ~ fitdist(
    data = na.omit(.x) |> as.vector(), 
    distr = "lnorm"
  )
)

lnorm_gof <- purrr::map(
  .x = lnorm_model, 
  .f = gofstat
)

lnorm_model_df <- lnorm_model |> 
  purrr::imap(
    .f = ~ tibble::tibble(
      Distribution = "Log Normal", 
      Parameter = c(
        "Mean Log", "Mean Log Std. Error", "SD Log", "SD Log Std. Error", "LLF"
      )
    ) |> 
      dplyr::mutate(
        "{.y}" := c(
          .x$estimate[["meanlog"]], .x$sd[["meanlog"]], 
          .x$estimate[["sdlog"]], .x$sd[["sdlog"]], 
          .x$loglik
        )
      )
  ) |> 
  Reduce(
    f = function(...) {
      dplyr::full_join(..., by = c("Distribution", "Parameter"))
    }
  )

# |- weibull----
weibull_model <- purrr::map(
  .x = positive_data, 
  .f = ~ fitdist(
    data = na.omit(.x) |> as.vector(), 
    distr = "weibull"
  )
)

weibull_gof <- purrr::map(
  .x = weibull_model, 
  .f = gofstat
)

weibull_model_df <- weibull_model |> 
  purrr::imap(
    .f = ~ tibble::tibble(
      Distribution = "Weibull", 
      Parameter = c(
        "Mean Log", "Mean Log Std. Error", "SD Log", "SD Log Std. Error", "LLF"
      )
    ) |> 
      dplyr::mutate(
        "{.y}" := c(
          .x$estimate[["shape"]], .x$sd[["shape"]], 
          .x$estimate[["scale"]], .x$sd[["scale"]], 
          .x$loglik
        )
      )
  ) |> 
  Reduce(
    f = function(...) {
      dplyr::full_join(..., by = c("Distribution", "Parameter"))
    }
  )

# |- pareto----

# !!! NOT working: error code 100 !!!
# scale_data <- function(x) {
#   (x - min(x) + 0.01) / (max(x) - min(x) + 0.02)
# }
# 
# pareto_model <- purrr::map(
#   .x = positive_data,
#   .f = ~ fitdist(
#     data = na.omit(.x) |> as.vector() |> scale_data(),
#     distr = "pareto"
#   )
# )
# 
# pareto_gof <- purrr::map(
#   .x = pareto_model, 
#   .f = gofstat
# )

# ----distrs-df----
distrs_df <- dplyr::bind_rows(
  exp_model_df, 
  gamma_model_df, 
  lnorm_model_df, 
  weibull_model_df
)
