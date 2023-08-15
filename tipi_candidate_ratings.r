library(anesr)
library(tidyverse)

data(timeseries_2016)

df <- timeseries_2016 |>
  select(V162333:V162342, V162078, V162079) |>
  filter(if_all(everything(), ~ . >= 0 & . <= 100)) |>
  mutate(extraversion_mean = (V162333 + (8 - V162338)) / 2,
         agreeableness_mean = ((8 - V162334) + V162339) / 2,
         conscientiousness_mean = (V162335 + (8 - V162340)) / 2,
         neuroticism_mean = ((8 - V162336) + V162341) / 2,
         openness_mean = (V162337 + (8 - V162342)) / 2,) |>
  select(contains("mean"), V162078, V162079)

df |>
  ggplot(aes(x = V162078)) +
  geom_histogram(binwidth = 1)

df |>
  ggplot(aes(x = V162079)) +
  geom_histogram(binwidth = 1)

cor(df)

summary(lm(V162079 ~ extraversion_mean + agreeableness_mean + conscientiousness_mean + neuroticism_mean + openness_mean, data = df))
