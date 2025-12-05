tabla_solo_mujeres <- Diagnostico %>%
  filter(Genero_2 == 1) %>%
  select(Nombre, Promedio, Conducta)



#| code-fold: true

datos_M <- left_join(tabla_D_hombres, tabla_b)
datos_F <- left_join(tabla_B2_hombres, tabla_B2_mujeres)

ggplot(data = datos_M, aes(sample = Promedio)) +
  stat_qq(color = "#551A8B", size = 2, alpha = 0.7) +
  stat_qq_line(color = "#528B8B", linewidth = 1, linetype = "dashed") +
  scale_x_continuous(name = "Cuantiles Teóricos (Distribución Normal)",
                     breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(name = "Cuantiles Muestrales (Promedio)",
                     breaks = pretty_breaks(n = 6),
                     labels = function(x) paste(x, "años")) +
  labs(title = "Gráfico Q-Q para Normalidad",
       subtitle = "Promedio - Grupo de Tercer Grado)" +
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
                                      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


#| code-fold: true
#| fig-width: 7
#| fig-align: center

ggplot(data = tabla_B2_mujeres, aes(x = COnducta_2, y = Promedio_2)) +
    geom_boxplot(
    fill = "#AB82FF",           # Color de relleno
    color = "#DA70D6",          # Color del borde más oscuro
    alpha = 0.8,                # Transparencia
    outlier.color = "#E74C3C",  # Color rojo para outliers
    outlier.size = 2.5,         # Tamaño de outliers
    outlier.alpha = 0.7,        # Transparencia de outliers
    width = 0.5,                # Ancho del boxplot
    notch = TRUE,               # Muescas para intervalo de confianza
    notchwidth = 0.7            # Ancho de las muescas
  ) +
  geom_jitter(alpha = 0.3, color = "#5D478B", size = 1.5, width = 0.1) +
  scale_y_continuous(breaks = pretty_breaks(n = 8), labels = function(x) paste(x, "años")) +
  labs(title = "Distribución de la Edad (mediana)", 
  subtitle = "Población del Estado de Zacatecas",
  y = "Edad (años)",
  x = "")+
  theme_bw()+
  theme(
    legend.title = element_text(size=13, face="bold"),
    legend.text = element_text(size=13),
    plot.title = element_text(size=15, face="bold"),
    plot.subtitle = element_text(size=13),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    axis.title.x = element_text(face="bold", size=13),
    axis.title.y = element_text(face="bold", size=13),
    panel.grid.major.y = element_line(color = "#ECF0F1", linewidth = 0.5,
      linetype = "dashed"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#FAFAFA", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


hist(
  x, 
  main = "Título de mi Histograma", 
  col = "lightblue", 
  xlab = "Etiqueta del Eje X"
)

