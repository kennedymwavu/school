library(readxl)
library(tidyverse)
library(moments)
library(ggpubr)

industry <- readxl::read_xlsx(
  path = "data/insurance_industry_stats_2016-2020.xlsx", 
  skip = 1
) |> 
  dplyr::select(
    `Class Name`, `2016`:`2020`
  )

industry_long <- industry |> 
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
desc_stats <- industry_long |> 
  group_by(`Class Name`) |> 
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
  ) |> 
  t() |> 
  as.data.frame()

# set column names:
colnames(desc_stats) <- desc_stats[1, 1:2] |> 
  gsub(pattern = "_", replacement = " ") |> 
  stringr::str_to_title()

# remove first row:
desc_stats <- desc_stats[-1, ]

# Make rownames the `Stat` column:
desc_stats["Stat"] <- rownames(desc_stats)

# remove rownames:
rownames(desc_stats) <- NULL

# make stat the first column:
desc_stats <- desc_stats |> 
  dplyr::relocate(Stat)

# ----hist----
