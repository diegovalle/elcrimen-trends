
inegi <- read_json("https://elcri.men/assets/json/national_1990.json", simplifyVector = TRUE)
inegi <- inegi[[1]][[2]]
names(inegi) <- c("s", "count", "date", "population", "rate")
inegi$date <- as.Date(inegi$date)
inegi$time <- as.numeric(as.Date(inegi$date))
inegi$month <- month(inegi$date)
inegi$year <- year(inegi$date)
inegi <- inegi %>% 
  group_by(year) %>%
  mutate(rate_scaled = rescale(rate))

#acf2(inegi$count)
#acf2(diff(inegi$count))

ggplot(inegi, aes(date, rate)) +
  geom_point()

ggplot(inegi, aes(month, rate_scaled, color = year, group = year)) +
  geom_line()


m1 <- stan_gamm4(rate ~ s(time) + s(month,  bs = 'cc', k = 12), data = inegi, 
                 adapt_delta = .99)
m2 <- stan_gamm4(rate ~ s(time, bs = "gp", m=c(2)) + s(month,  bs = 'cc', k = 12), data = inegi, 
                 adapt_delta = .99)
m3 <- stan_gamm4(rate ~ s(time), data = inegi, adapt_delta = .99)
m4 <- stan_gamm4(rate ~ s(time) + s(month, k = 12), data = inegi, adapt_delta = .99)

loo1 <- loo(m1)
loo2 <- loo(m2)
loo3 <- loo(m3)
loo4 <- loo(m4)
(comp <- compare_models(loo1, loo2, loo3, loo4))


plot_nonlinear(m1)
plot_nonlinear(m1, smooths = "s(time)")
plot_nonlinear(m1, smooths = "s(month)")
pp_check(m1)
pp_check(m1, plotfun = "ppc_ecdf_overlay")



