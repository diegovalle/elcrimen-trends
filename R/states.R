if(Sys.getenv("CI") == "true") {
  url <- Sys.getenv("VICTIMAS_URL")
} else {
  url <- "https://data.diegovalle.net/elcrimen/nm-estatal-victimas.csv.gz"
}

df <- read_csv(url,
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
  group_by(date, state, state_code) %>%
  summarise(n = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(date = as.Date(as.yearmon(date))) %>%
  mutate(time = as.numeric(date)) %>%
  mutate(month = month(date)) %>%
  mutate(year = year(date)) %>%
  mutate(dim = days_in_month(date) / (365/12))

# add missing oaxaca oct 2016 number
df[which(df$state == "OAXACA" & df$date == "2016-10-01"), "n"] <- 76
# add missing oaxaca 2015 homicides
df[which(df$state == "OAXACA")[1:12], "n"] <- c(56,67,57,49,86,73,93,69,74,63,89,73)

#pop <- monthly_pop() %>% filter(year %in% 2015:max(df$year)) %>%
#  mutate(date = as.Date(str_c(year, "-", month, "-01")))
pop <- read_csv("data/states_pop.csv", col_types = cols(
  state_name = col_character(),
  state_code = col_double(),
  year = col_double(),
  month = col_double(),
  population = col_double()
))  %>% 
  filter(year %in% 2015:max(df$year)) %>% 
  mutate(date = as.Date(str_c(year, "-", month, "-01")))
df <- left_join(df, pop, by = c("date", "state_code", "year", "month"))
df$rate <- ((df$n / days_in_month(df$date)) * 30) / df$population * 10^5 * 12
df <- df %>% arrange(state, date)
df$state <- as.factor(df$state)
df$lograte <- log1p(df$rate)

# has to be a separate vector not part of the data.frame
# or else stan_gamm4 returns an error
df <- df %>%
  group_by(state, state_code) %>%
  mutate(duration = dim * (population / population[1])) %>%
  ungroup()

# Subset so that we only use data after 2017 for the model
# This is IMPORTANT to make sure the CI job doesn't take
# too long
max_year <- year(max(df$date))
start_year <- max_year - state_years
df <- filter(df, date >= paste0(start_year, "-01-01"))

duration <- df$duration 
df$duration <- NULL


print("######################################################")
print("######################################################")
print(paste0("running states stan_gamm4 with ", iterations_states, " iterations"))
print("######################################################")
print("######################################################")


m1 <- stan_gamm4(n ~ s(time, by = state)+ s(month, bs = "cc", k = 12) +
                   offset(log(duration)), #,
                 family = poisson,
                 random = ~(1 | state), 
                 data = df, 
                 chains = 4, 
                 iter = iterations_states,
                 #adapt_delta = .99, 
                 cores = parallel::detectCores(), 
                 seed = 12345)
                 #prior = normal(0,1),
                 #prior_intercept = normal(location = 4, scale = .5),
                 #prior_smooth = exponential(1))
save(m1, file = "output/m1_states.RData")
#modelsummary::modelsummary(m1, statistic = "conf.int")
#load("cache/m1_states.RData")

# bayesplot::mcmc_trace(as.array(m1), pars = c( "sigma"),
#            facet_args = list(ncol = 1, strip.position = "left"))

# plot_nonlinear(m1, "s(time):stateVERACRUZ")
# plot_nonlinear(m1)
# pp_check(m1)
# pp_check(m1, plotfun = "ppc_ecdf_overlay")

df$duration <- duration
# p <- add_fitted_draws(na.omit(df), m1) %>%
#   ggplot(aes(x = date, y = n)) +
#   stat_lineribbon(aes(y = .value), alpha = 1) +
#   geom_point(color = "#fc9272", alpha = .5, size = .6) +
#   scale_fill_brewer(palette = "Greys") +
#   scale_color_brewer(palette = "Set2") +
#   facet_wrap(~state, scale = "free_y", ncol = 4)
# ggsave("graphs/predicted_states.png", plot = p, height = 20, width = 14, dpi = 100)


dates <- seq(as.Date(min(df$date)), as.Date(max(df$date)), by = "month")
ndates <- length(dates)

trends <- do.call(rbind, lapply(as.character(unique(df$state)), function(x) {
  state_name <- x
  inc <- grep(state_name, colnames(predict(m1$jam, type = "lpmatrix")))
  #X0 <- predict(m1$jam, type = 'lpmatrix')[, c(1, inc)]
  
  
  eps <- 30
  newDFeps <- df 
  newDFeps$time <- df$time + eps
  newDFeps$duration <- log(1)
  X1 <- predict(m1$jam, newDFeps, type = 'lpmatrix')
  
  sims_o <- as.matrix(m1)[, c(1, inc)] %*% t(as.matrix(m1$x[which(df$state == state_name), c(1, inc)])) 
  sims_n <- as.matrix(m1)[, c(1, inc)] %*% t(X1[which(df$state == state_name), c(1, inc)])
  
  #100 x 10 * ndates * 10
  d1 <- ((sims_n - sims_o) / eps) 
  (sims_n - sims_o)[1:5, 1:5]
  dim(d1)
  d1[1:5, 1:5]
  sum(d1[, ndates] >= 0)
  qt <- quantile(d1[, ndates], c(.05, .95))
  med <- median(d1[, ndates])
  if (qt[1] < 0 & qt[2] < 0)
    return(data.frame(state = state_name, 
                      trend = "negative", 
                      fd = med))
  else if (qt[1] > 0 & qt[2] > 0)
    return(data.frame(state = state_name, 
                      trend = "positive", 
                      fd = med))
  else
    return(data.frame(state = state_name, 
                      trend = NA, 
                      fd = med))
})
)


sims <- do.call(rbind, lapply(as.character(unique(df$state)), function(x) {
  state_name <- x
  print(x)
  inc <- grep(state_name, colnames(predict(m1$jam, type = "lpmatrix")))
  binc <- grep(paste0("b\\[\\(Intercept\\) state:", 
                      str_replace_all(state_name," ", "_"),
                      "\\]$"), 
               colnames(m1$x))
  X0 <- as.matrix(m1$x)[which(df$state == state_name), c(1, inc)]
  sims <- as.matrix(m1)[, c(1, inc)] %*% t(X0)
  b = as.matrix(m1)[, binc, drop = FALSE]
  sims <- apply(sims, 2, function(x) {x + b}) %>% as.data.frame()
  # substract offset
  sims[,] <- apply(sims[,], 2, 
                        function(x) x - log(df[which(df$state == state_name), ]$population[1]/10^5))
  sims$sim <- 1:nrow(sims)
  # Use only 1k (number_of_samples) simulations
  if (nrow(sims) > number_of_samples)
    sims <- sims[sample(1:nrow(sims), number_of_samples),]
  
  sims <- gather(data.frame(sims), "time", "rate", -sim) %>%
    mutate(time = as.numeric(str_replace(time, "X", ""))) %>%
    arrange(sim, time)
  sims$date <- dates
  sims$state <- state_name
  sims$count <- exp(sims$rate)
  return(sims)
}))

sims <- left_join(sims, trends, by = "state")


sims <- sims %>%
  group_by(state, sim) %>%
  mutate(der = exp(rate[81]) - exp(rate[80])) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(fd = median(der)) %>%
  mutate(sign = fd < 0) %>%
  mutate(fd2 = abs(fd)) %>%
  group_by(sign) %>%
  arrange(desc(fd2), state, sim, date, .by_group = TRUE) %>%
  mutate(state = factor(state, levels = unique(state)))

p <- ggplot(sims, aes(x = date, y = exp(rate) * 12, group = sim)) +
  geom_line(alpha = 0.1, aes(color = trend), size = .05) +
  scale_color_manual("tendencia\núltimo mes",
                     values = c("positive" = "#e41a1c", 
                                "negative" = "#1f78b4"), 
                     labels = c("al alza", "a la baja", "no significativa"),
                     breaks = c("positive", "negative", NA),
                                na.value = "#cab2d6") +
  geom_point(data = df, aes(date, rate, group = state), 
             fill = "#f8766d", 
             color = "black",
             shape = 21,
             size = .9) +
  theme_bw() +
  expand_limits(y = 0) +
  xlab("fecha") +
  ylab("tasa anualizada") +
  labs(title = str_c("Tasas de homicidio y ", number_of_samples, " simulaciones del ", 
                     "posterior de un modelo aditivo multinivel ajustado por ",
                     "estacionalidad,\npor estado"),
       subtitle = str_c("Los estados están ordenados por el valor de la ",
                        "primera derivada durante el último mes.", 
                        "\n\nLa tendencia del último mes corresponde al color ",
                        "de cada estado (primera derivada, intervalo de ",
                        "credibilidad del 90%).\nIncluye homicidios dolosos ",
                        "y feminicidios. Las tasas son por 100,000 habitantes ",
                        "y con meses de 30 días."),
       caption = "Fuente: SNSP víctimas y proyecciones del CONAPO con datos del 2015") +
  theme_ft_rc(base_family = "Arial Narrow") +
  facet_wrap(~state, ncol = 4) + 
  scale_y_continuous(trans = 'log10') +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1)))
ggsave("graphs/state_time_trend.png", plot = p, height = 20, width = 14, 
       dpi = 100)


jsondata <- lapply(as.character(unique(sims$state)), function(x) {
  state_name <- x
  print(x)
  inc <- grep(state_name, colnames(predict(m1$jam, type = "lpmatrix")))
  binc <- grep(paste0("b\\[\\(Intercept\\) state:", 
                      str_replace_all(state_name," ", "_"),
                      "\\]$"), 
               colnames(m1$x))
  X0 <- as.matrix(m1$x)[which(df$state == state_name), c(1, inc)]
  sims <- as.matrix(m1)[, c(1, inc)] %*% t(X0)
  b = as.matrix(m1)[, binc, drop = FALSE]
  sims <- apply(sims, 2, function(x) {x + b}) %>% as.data.frame()
  # substract offset
  sims[,] <- apply(sims[,], 2, 
                   function(x) x - log(df[which(df$state == state_name), ]$population[1]/10^5))
  
  ll <- apply(sims[,], 2, function(x) quantile(exp(x)*12, c(.05, .5, .95)))
  ll <- rbind(ll, df[which(df$state == state_name),]$rate)
  ll <- round(ll, 1)
  rownames(ll) <- c("l", "m", "u", "r")
  ll <- lst(!!state_name := ll,
            trend = trends[which(trends$state == state_name), ]$trend,
            start_year = start_year)
  return(ll)
})

write(toJSON(jsondata), "web/states_trends.json")
