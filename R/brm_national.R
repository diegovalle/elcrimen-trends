

df <- read_csv("https://data.diegovalle.net/elcrimen/nm-estatal-victimas.csv.gz") %>%
  filter(subtipo == "HOMICIDIO DOLOSO" | subtipo == "FEMINICIDIO") %>%
  #filter(date >= "2016-01") %>%
  group_by(date, state, state_code) %>%
  summarise(n = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  mutate(time = as.numeric(date)) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(dim = days_in_month(date))

# add missing oaxaca oct 2016 number
df[which(df$state == "OAXACA" & df$date == "2016-10-01"), "n"] <- 76
# add missing oaxaca 2015 homicides
df[which(df$state == "OAXACA")[1:12], "n"] <- c(56,67,57,49,86,73,93,69,74,63,89,73)

pop <- monthly_pop() %>% filter(year %in% 2015:2019) %>%
  mutate(date = as.Date(str_c(year, "-", month, "-01")))
df <- left_join(df, pop, by = c("date", "state_code", "year", "month"))
df$rate <- ((df$n / df$dim) * 30) / df$population * 10^5 * 12
df <- df %>% arrange(state, date)
df$state <- as.factor(df$state)

df <- filter(df, state == "SONORA")

library(brms)
m1_brm <- brm(log(rate) ~ s(time) + s(month,  bs = 'cc', k = 12), data = df,
         autocor = cor_arma(~ time, 0, 1, 1), chains = 1, iter = 200)

fixed_form <- brms:::extract_effects(m1_brm$formula)$fixed
brms:::get_model_matrix(m1_brm, df)

sims <- as.matrix(m1)[, 1:10] %*% t(as.matrix(m1$x[, 1:10])) %>% as.data.frame()
sims$sim <- 1:nrow(sims)
sims <- sims[sample(1:4000, 1000),]
sims <- gather(sims, "time", "rate", -sim) %>%
  mutate(time = as.numeric(time)) %>%
  arrange(sim, time)
sims$date <- seq(as.Date("2015-01-01"), as.Date(max(df$date)), by = "month")
df$sim <- NA
ggplot(sims, aes(x = date, y = rate, group = sim)) +
  geom_line(alpha = 0.0075, color = "red") +
  geom_point(data = df, aes(date, rate), color = "#f8766d", size = 1.2) +
  theme_bw() +
  expand_limits(y = 0) +
  xlab("fecha") +
  ylab("tasa anualizada") +
  labs(title = "Tasas de homicidio y 1000 simulaciones del posterior\ndel GAM ajustado por estacionalidad",
       subtitle = "Incluye homicidios dolosos y feminicidios. Las tasas son por 100,000 habitantes y con\nmeses de 30 días.",
       caption = "Fuente: SNSP víctimas y proyecciones del CONAPO con datos del 2015") +
  theme_ft_rc()
