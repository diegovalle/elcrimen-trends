url <- "https://data.diegovalle.net/elcrimen/nm-estatal-victimas.csv.gz?cache"

temp_file_path <- tempfile(fileext = ".gz")
download.file(url, destfile = temp_file_path, mode = "wb")

df <- read_csv(temp_file_path,
               col_types = cols(
                 state_code = col_double(),
                 state = col_character(),
                 bien_juridico = col_character(),
                 tipo = col_character(),
                 subtipo = col_character(),
                 modalidad = col_character(),
                 date = col_character(),
                 sex = col_character(),
                 age_group = col_character(),
                 population = col_double(),
                 count = col_double()
               )) %>%
  filter(subtipo == "HOMICIDIO DOLOSO" | subtipo == "FEMINICIDIO") %>%
  #filter(date >= "2016-01") %>%
  group_by(date) %>%
  summarise(n = sum(count, na.rm = TRUE)) %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  mutate(time = as.numeric(date)) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(dim = days_in_month(date)) 
print(max(df$date))

# add missing oaxaca oct 2016 number
df$n[22] <- df$n[22] + 76
# add missing oaxaca 2015 homicides
df$n[1:12] <- df$n[1:12] + c(56,67,57,49,86,73,93,69,74,63,89,73)


#pop <- monthly_pop("total") %>% filter(year %in% 2015:max(df$year))
pop <- read_csv("data/national_pop.csv", col_types = cols(
  state_name = col_character(),
  state_code = col_double(),
  year = col_double(),
  month = col_double(),
  population = col_double()
)) %>% 
  filter(year %in% 2015:max(df$year))
df$pop <- pop$population[1:nrow(df)]
df$rate <- ((df$n / df$dim) * 30) / df$pop * 10^5 * 12

duration <- days_in_month(df$date) / (365/12) * (df$pop / df$pop[1])

p1 <- ggplot(df, aes(date, rate)) +
  geom_point()
p1 + geom_line()


df$diff <- c(rep(NA, 12), diff(df$rate, 12))
ggplot(filter(df, year >= 2016), aes(date, diff)) +
  geom_line() +
  geom_smooth(se = FALSE, method = 'loess',formula = 'y ~ x') +
  theme_ft_rc() +
  geom_hline(yintercept = 0) +
  ylab("diferencia en tasa anualizada") +
  xlab("fecha") +
  labs(title = "Diferencias en Tasas de Homicidioen México con el Mismo Mes del Año Pasado",
       subtitle = "Incluye homicidios dolosos y feminicidios. Las diferencias son en tasas son por 100,000 habitantes y con\nmeses de 30 días.",
       caption = "Fuente: SNSP víctimas y proyecciones del CONAPO con datos del 2015")
ggsave("graphs/diff.png", height = 6, width = 12, dpi = 100)



p <- ggplot(df, aes(month, rate, group = year, color = year)) +
  geom_line() +
  scale_color_gradient(low = "#fff7ec", high = "#cb181d") +
  scale_x_continuous(breaks = seq(1, 12, 3), labels = month.abb[seq(1, 12, 3)]) +
  expand_limits(y = 0) +
  theme_ft_rc() +
  ylab("tasa anualizada") +
  xlab("mes") +
  labs(title = "Tasas de Homicidio por Año en México",
       subtitle = "Incluye homicidios dolosos y feminicidios. Las tasas son por 100,000 habitantes y con\nmeses de 30 días.",
       caption = "Fuente: SNSP víctimas y proyecciones del CONAPO con datos del 2015")
direct.label(p, "top.bumptwice")
ggsave("graphs/year.png", height = 6, width = 10, dpi = 100)

# m <- brm(rate ~ s(time) + s(month,  bs = 'cc', k = 12), data = df,
#          autocor = cor_arma(~ time, 0, 1, 1))

## Estacionaliad simple
m1 <- stan_gamm4(n ~ s(time, bs="gp") + s(month,  bs = 'cp', k = 12) + offset(log(duration)), 
                 data = df,
                 iter = iter_national, 
                 chains = 4,
                 control = list(max_treedepth = 15),
                 adapt_delta = .999, 
                 family = poisson, 
                 cores = parallel::detectCores(),
                 seed = 12345)
                 #prior = normal(0,1),
                 #prior_intercept = normal(location = 8, scale = .5),
                 #prior_smooth = exponential(1))
save(m1, file = "output/m1_national.RData")
prior_summary(m1)
#modelsummary::modelsummary(m1, statistic = "conf.int")
#load("cache/m1_national.RData")
#pairs(m1, pars = c("s(time).1", "(Intercept)"))
# m2 <- stan_gamm4(log(rate) ~ s(time) + rate.lag12 + s(month,  bs = 'cc', k = 12), data = df[13:55,], 
#                  adapt_delta = .999)
#m2 <- stan_gamm4(rate ~ s(time) + s(month, k = 12), data = df, adapt_delta = .999)
#m3 <- stan_gamm4(rate ~ s(time), data = df, adapt_delta = .999)
#m_gp <- stan_gamm4(rate ~ s(time, bs = "gp") , data = df, 
#                   adapt_delta = .999)
# Which model is best?
#loo1 <- loo(m1, k_threshold = 0.7)
#loo2 <- loo(m2)
#loo3 <- loo(m3)
#loo4 <- loo(m_gp)
#(comp <- compare_models(loo1, loo2))#, loo3, loo4))

# model checks
summary(m1, digits = 3)
prior_summary(m1)
# plot_nonlinear(m1)
# plot_nonlinear(m1, smooths = "s(time)")
# pp_check(m1)
# pp_check(m1, plotfun = "ppc_ecdf_overlay")

first_deriv_national <- function(m1, df) {
  df2 <- df
  df2$duration <- log(1)
  inc <- grep("s\\(time\\).*", colnames(predict(m1$jam, df2, type = "lpmatrix")))
  X0 <- predict(m1$jam, df2, type = "lpmatrix")[, c(1,inc)]
  
  eps <- .1
  newDFeps <- df 
  newDFeps$time <- df$time + eps
  newDFeps$duration <- log(1)
  X1 <- predict(m1$jam, newDFeps, type = 'lpmatrix')[, c(1,inc)]
  
  sims_o <- as.matrix(m1)[, 1:(length(inc) + 1)] %*% t(X0)
  sims_o <- sims_o - log(df$pop[1]/100000)
  sims_o <- exp(sims_o) * 12
  dim(sims_o)
  sims_n <- as.matrix(m1)[, 1:(length(inc) + 1)] %*% t(X1)
  sims_n <- sims_n - log(df$pop[1]/100000)
  sims_n <- exp(sims_n) * 12
  
  d1 <- ((sims_n - sims_o) / eps) 
  apply(d1, 2,  function(x) quantile(x, c(.05, .95)))
}
fd_last <- first_deriv_national(m1, df)[, nrow(df)]
if (fd_last[1] < 0 & fd_last[2] < 0) {
  col <- "#1f78b4"
  sub_fd <- "La tendencia del último mes es negativa (primera derivada, intervalo de credibilidad del 90%). Incluye homicidios dolosos y feminicidios.\nLas tasas son por 100,000 habitantes y conmeses de 30 días."
} else if (fd_last[1] > 0 & fd_last[2] > 0) {
  col <- "#e41a1c"
  sub_fd <- "La tendencia del último mes es positiva (primera derivada, intervalo de credibilidad del 90%). Incluye homicidios dolosos y feminicidios.\nLas tasas son por 100,000 habitantes y con meses de 30 días."
} else {
  col <- "#cab2d6"
  sub_fd <- "La tendencia no es significativa (primera derivada, intervalo de credibilidad del 90%). Incluye homicidios dolosos y feminicidios.\nLas tasas son por 100,000 habitantes y con meses de 30 días."
}

df$duration <- log(1)
inc <- grep("s\\(time\\).*", colnames(predict(m1$jam, df, type = "lpmatrix")))
sims <- as.matrix(m1)[, c(1,inc)] %*% t(as.matrix(m1$x[, c(1,inc)])) %>% as.data.frame()# substract offset
# Add offset in 100,000 persons
sims <- sims - log(df$pop[1]/100000)
sims$sim <- 1:nrow(sims)
sims <- sims[sample(1:4000, 1000),]
sims <- gather(data.frame(sims), "time", "rate", -sim) %>%
  mutate(time = as.numeric(str_replace(time, "V", ""))) %>%
  arrange(sim, time)
sims$date <- seq(as.Date("2015-01-01"), as.Date(max(df$date)), by = "month")
df$sim <- NA
ggplot(sims, aes(x = date, y = exp(rate)*12, group = sim)) +
  geom_line(alpha = 0.01, color = col) +
  geom_point(data = df, aes(date, rate), 
             fill = "#f8766d", 
             color = "black",
             shape = 21,
             size = 2) +
  theme_bw() +
  expand_limits(y = 0) +
  xlab("fecha") +
  ylab("tasa anualizada") +
  labs(title = str_c("Tasas de homicidio en México y ", iter_national, 
                     " simulaciones del posterior\nde un modelo aditivo ",
                     "ajustado por estacionalidad"),
       subtitle = sub_fd,
       caption = "Fuente: SNSP víctimas y proyecciones del CONAPO con datos del 2015") +
  theme_ft_rc()
ggsave("graphs/time_trend.png", height = 7, width = 13, dpi = 100)

#library(brms)
#brm(bf(rate ~ s(time) + s(month, bs = "cc", k = 12)),
#    data = df, family = gaussian())

# add_fitted_draws(df[, 1:7], m1, n = 1000) %>%
#   ggplot(aes(x = date, y = n)) +
#   stat_lineribbon(aes(y = .value)) +
#   geom_point() +
#   scale_fill_brewer(palette = "Greys") +
#   scale_color_brewer(palette = "Set2")
print("add_predicted_draws")
df$duration <- duration
add_predicted_draws(na.omit(df[, c("date", "n", "time", "month",
                                   "duration")]), m1, ndraws = 10^3) %>%
  ggplot(aes(x = date, y = n)) +
  stat_lineribbon(aes(y =  .prediction)) +
  geom_point(color = "#ef3b2c", size = 1) +
  expand_limits(y = 0) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Modelo con estacionalidad",
       caption = "Fuente: SNSP víctimas y CONAPO con datos del 2015") +
  theme_ipsum_rc()
ggsave("graphs/predicted.png", height = 7, width = 14, dpi = 100)

