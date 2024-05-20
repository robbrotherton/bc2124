remotes::install_github("kjhealy/gssr")
download.file("https://gss.norc.org/documents/stata/2006_stata.zip", "data/gss06.zip")
unzip("data/gss06.zip")

df <- haven::read_dta("data/GSS2006.dta")
df$zodiac

readr::write_csv(df, "data/gss_2006.csv")

gss <- readr::read_csv("data/gss_2006.csv")


gss$zodiac

library(tidyverse)

gss_clean <- gss |>
  select(zodiac,
         big5_extraversion1_r = big5a1, # reserved
         big5_extraversion2 = big5a2, # outgoing, sociable
         big5_agreeableness1 = big5b1, # trusting
         big5_agreeableness2_r = big5b2, # finds fault with others
         big5_conscientiousness1 = big5c1, # does thorough job
         big5_conscientiousness2_r = big5c2, # lazy
         big5_neuroticism1 = big5d1, # relaxed
         big5_neuroticism2_r = big5d2, # gets nervous easily
         big5_openness1 = big5e1, # active imagination
         big5_openness2_r = big5e2) |> # few artistic interests

  # reverse-code as needed
  mutate(across(contains("_r"), ~ 6 - .)) |>

  # compute means of relevant items
  mutate(openness_mean = rowMeans(across(contains("openness"))),
         conscientiousness_mean = rowMeans(across(contains("conscientiousness"))),
         extraversion_mean = rowMeans(across(contains("extraversion"))),
         agreeableness_mean = rowMeans(across(contains("agreeableness"))),
         neuroticism_mean = rowMeans(across(contains("neuroticism")))) |>

  # turn zodiac codes into labels
  mutate(zodiac_name = case_when(
    zodiac == 1 ~ "Aries",
    zodiac == 2 ~ "Taurus",
    zodiac == 3 ~ "Gemini",
    zodiac == 4 ~ "Cancer",
    zodiac == 5 ~ "Leo",
    zodiac == 6 ~ "Virgo",
    zodiac == 7 ~ "Libra",
    zodiac == 8 ~ "Scorpio",
    zodiac == 9 ~ "Sagittarius",
    zodiac == 10 ~ "Capricorn",
    zodiac == 11 ~ "Aquarius",
    zodiac == 12 ~ "Pisces"
  ))

# histograms
gss_clean |>
  select(zodiac_name, contains("big5")) |>
  pivot_longer(everything()) |>
  ggplot(aes(x = value)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~name)

ci <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  qt(.975, df = length(x) - 1) * sqrt(var(x) / length(x))
}

# summary stats
summary <- gss_clean |>
  select(zodiac_name, contains("_mean")) |>
  pivot_longer(-zodiac_name) |>
  summarize(n = n(),
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            ci = ci(value, na.rm = TRUE),
            .by = c(name, zodiac_name))

# graph of means
summary |>
  drop_na() |>
  ggplot(aes(x = zodiac_name, y = mean, fill = name)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7, color = "black") +
  geom_errorbar(aes(ymax = mean + ci, ymin = mean - ci),
                position = position_dodge(width = 0.7),
                width = 0.2)

summary |>
  drop_na() |>
  ggplot(aes(x = zodiac_name, y = mean)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7, color = "black") +
  geom_errorbar(aes(ymax = mean + ci, ymin = mean - ci),
                position = position_dodge(width = 0.7),
                width = 0.2) +
  facet_wrap(~name, nrow = 1)



grouped_data <- gss_clean |>
  select(zodiac_name, contains("_mean")) |>
  pivot_longer(-zodiac_name) |>
  nest(.by = name)

all_comparisons <- map(grouped_data$data, .f = ~pairwise.t.test(.$value, .$zodiac_name, p.adjust.method = "none"))
names(all_comparisons) <- grouped_data$name

all_p_values <- map_df(all_comparisons, ~.$p.value |>
      as.data.frame() |>
      rownames_to_column(var = "x1") |>
      pivot_longer(-x1, names_to = "x2") |>
      drop_na(),
      .id = "trait")

all_p_values |>
  mutate(sig = value < .05) |>
  summarise(prop = sum(sig) / n())

anovas <- map(grouped_data$data, ~aov(value ~ zodiac_name, data = .))
names(anovas) <- grouped_data$name
map(anovas, ~summary(.))
map(anovas, TukeyHSD)
