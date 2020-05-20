

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
duration <- df$duration 
df$duration <- NULL

iterations_states <- 1500

m1 <- stan_gamm4(n ~ s(time, by = state)+ s(month, bs = "cc", k = 12) + offset(log(duration)), #,
                 family = poisson,
                 random = ~(1 | state), 
                 data = df, 
                 chains = 2, 
                 iter = iterations_states,
                 adapt_delta = .99, 
                 cores = 2, 
                 seed = 12345)
save(m1, file = "output/m1_states.RData")
#load("output/m1_states.RData")

# bayesplot::mcmc_trace(as.array(m1), pars = c( "sigma"),
#            facet_args = list(ncol = 1, strip.position = "left"))

plot_nonlinear(m1, "s(time):stateVERACRUZ")
plot_nonlinear(m1)
pp_check(m1)
pp_check(m1, plotfun = "ppc_ecdf_overlay")

df$duration <- duration
p <- add_fitted_draws(na.omit(df), m1) %>%
  ggplot(aes(x = date, y = n)) +
  stat_lineribbon(aes(y = .value), alpha = 1) +
  geom_point(color = "#fc9272", alpha = .5, size = .6) +
  scale_fill_brewer(palette = "Greys") +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(~state, scale = "free_y", ncol = 4)
ggsave("graphs/predicted_states.png", plot = p, height = 20, width = 14, dpi = 100)


dates <- seq(as.Date(min(df$date)), as.Date(max(df$date)), by = "month")
ndates <- length(dates)

trends <- do.call(rbind, lapply(as.character(unique(df$state)), function(x) {
  state_name <- x
  inc <- grep(state_name, colnames(predict(m1$jam, type = "lpmatrix")))
  #X0 <- predict(m1$jam, type = 'lpmatrix')[, c(1, inc)]
  
  
  eps <- .1
  newDFeps <- df 
  newDFeps$time <- df$time + eps
  newDFeps$duration <- log(1)
  X1 <- predict(m1$jam, newDFeps, type = 'lpmatrix')
  
  sims_o <- as.matrix(m1)[, c(1, inc)] %*% t(as.matrix(m1$x[which(df$state == state_name), c(1, inc)])) 
  sims_n <- as.matrix(m1)[, c(1, inc)] %*% t(X1[which(df$state == state_name), c(1, inc)])
  
  #100 x 10 * ndates * 10
  d1 <- ((sims_n - sims_o) / eps) 
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
  mutate(fd = as.numeric(fd)) %>%
  arrange(desc(fd)) %>%
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
             size = 1.1) +
  theme_bw() +
  expand_limits(y = 0) +
  xlab("fecha") +
  ylab("tasa anualizada") +
  labs(title = str_c("Tasas de homicidio y ", iterations_states, " simulaciones del ", 
                     "posterior de un modelo aditivo multinivel ajustado por ",
                     "estacionalidad,\npor estado"),
       subtitle = str_c("Los estados están ordenados por el valor de la ",
                        "primera derivada durante el último mes y sus tasas ",
                        "no son comparables\npor usar una escala diferente ", 
                        "para facilitar la visualización de las tendencias.",
                        "\n\nLa tendencia del último mes corresponde al color ",
                        "de cada estado (primera derivada, intervalo de ",
                        "credibilidad del 90%).\nIncluye homicidios dolosos ",
                        "y feminicidios. Las tasas son por 100,000 habitantes ",
                        "y con meses de 30 días."),
       caption = "Fuente: SNSP víctimas y proyecciones del CONAPO con datos del 2015") +
  theme_ft_rc(base_family = "Arial Narrow") +
  facet_wrap(~state, scale = "free_y", ncol = 4) + 
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
  ll <- lst(!!state_name := ll, trend = trends[which(trends$state == state_name), ]$trend)
  return(ll)
})

write(toJSON(jsondata), "output/states_trends.json")
