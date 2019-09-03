

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


m1 <- stan_gamm4(log(rate) ~ s(time, by = state) + s(month, bs = "cc", k = 12), 
                 data = df,
                 adapt_delta = .999)

dim(as.matrix(m1))

sims <- as.matrix(m1)[, 1:10] %*% t(as.matrix(m1$x[, 1:10])) %>% as.data.frame()
sims$sim <- 1:nrow(sims)
sims <- sims[sample(1:4000, 1000),]
sims <- gather(sims, "time", "rate", -sim) %>%
  mutate(time = as.numeric(time)) %>%
  arrange(sim, time)
sims$date <- seq(as.Date("2015-01-01"), as.Date(max(df$date)), by = "month")
df$sim <- NA
ggplot(sims, aes(x = date, y = exp(rate), group = sim)) +
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




X0 <- predict(m1$jam, type = "lpmatrix")[,1:10]

eps <- 1e-7
newDFeps <- df 
newDFeps$time <- df$time + eps
X1 <- predict(m1$jam, newDFeps, type = 'lpmatrix')[,1:10]

sims_o <- as.matrix(m1)[, 1:10] %*% t(X0)
sims_n <- as.matrix(m1)[, 1:10] %*% t(X1)

#4000 x 55 * 55 * 10
d1 <- ((sims_n - sims_o) / eps) 
sum(d1[,55] >= 0)
quantile(d1[, 55], c(.05, .95))

apply(d1, 2,  function(x) quantile(x, c(.05, .95)))
apply(d1, 2,  function(x) quantile(x, c(.025, .975)))
