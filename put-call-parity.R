library(quantmod)
library(tidyverse)
library(ggpubr)

# ----data----
# Get AMC options data:
# amc_front <- getOptionChain(
#   Symbols = "AMC"
# )
# 
# calls <- amc_front$calls
# puts <- amc_front$puts

# write those to a csv so that the data remains constant in future:
# write.csv(x = calls, file = "calls.csv")
# write.csv(x = puts, file = "puts.csv")

# Read in the data:
calls <- read_csv(file = "calls.csv")
puts <- read_csv(file = "puts.csv")

market_prices <- calls |> 
  inner_join(puts, by = "Strike", suffix = c("_call", "_put")) |> 
  transmute(
    Strike = Strike, 
    Call = (Bid_call + Ask_call) / 2, 
    Put = (Bid_put + Ask_put) / 2
  )

market_prices_long <- market_prices |> 
  pivot_longer(
    cols = c(Call, Put), 
    names_to = "option", 
    values_to = "market_price"
  )

# ----model----
# Linear regression model:
fit1 <- lm(Call ~ Put + Strike, data = market_prices)
summary(fit1)

# error residuals:
error <- fit1$residuals

gghistogram(
  error, fill = "lightblue", color = "blue", alpha = 0.7
)

# A density plot of the residuals:
ggdensity(
  error, fill = "lightblue", color = "blue", alpha = 0.7
)

# qqplot:
ggqqplot(error, color = "blue", alpha = 0.7)
# All points fall approximately along the reference line, so we can assume
# normality

# Test the errors for normality:
shapiro.test(error)
# p-value > 0.05 implying that the distribution of the errors is not 
# significantly different from the normal distribution, hence we can 
# assume normality

# ----plot----
# Plot strike price against call and put market price:
p1 <- market_prices_long |> 
  ggplot(
    mapping = aes(x = Strike, y = market_price, color = option)
  ) + 
  geom_point() + 
  # add intersection:
  geom_vline(xintercept = fit1$coefficients[[1]]) + 
  scale_color_manual(
    values = c("blue", "red"), 
    name = "Market Price"
  ) + 
  ggtitle(label = "AMC Options Price") + 
  ylab(label = "Payoff") + 
  xlab(label = "Strike Price") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )
p1

