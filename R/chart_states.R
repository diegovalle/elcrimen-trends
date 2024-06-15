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
