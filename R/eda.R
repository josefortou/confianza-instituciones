theme_set(theme_light(base_size = 14))

wvs_com %>%
  count(COQ78) %>%
  mutate(prop = n/sum(n),
         COQ78 = fct_relevel(COQ78, "Ninguna", "No mucha", "Mucha", "Bastante")) %>%
  ggplot(aes(COQ78, y = prop)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL, y = "Porcentaje de encuestados") +
  scale_color_grey() +
  theme(legend.position = "bottom") + 
  ggsave("output/cajas_wvs.png")

wvs_com %>%
  mutate(
    COQ78 = case_when(
      COQ78 %in% c("Bastante", "Mucha") ~ "Alta confianza",
      COQ78 %in% c("No mucha", "Ninguna") ~ "Baja confianza"
    )
  ) %>%
  count(COQ78) %>%
  mutate(prop = n/sum(n),
         COQ78 = fct_relevel(COQ78, "Baja confianza", "Alta confianza")) %>%
  ggplot(aes(COQ78, y = prop)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_grey() +
  labs(x = NULL, y = "Porcentaje de encuestados") +
  scale_color_grey() +
  theme(legend.position = "bottom") + 
  ggsave("output/cajas_wvs_2cat.png")