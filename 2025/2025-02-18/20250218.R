# Cargar paquetes
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggtext)
library(ggplot2)
library(grid)
library(geofacet)  # Para organizar los hexágonos por estado


library(showtext)
font_add_google("Roboto Condensed", "roboto_condensed")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2025-02-18')
tuesdata

df <- tuesdata$agencies

df_summary <- df %>%
  group_by(state_abbr, agency_name, state, is_nibrs) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(state_abbr, state) %>%
  mutate(percent = count / sum(count),  # Calcular el porcentaje
         nibrs_agencies = sum(is_nibrs == TRUE),  # Agencias que cumplen con NIBRS
         total_agencies = n_distinct(agency_name))  # Total de agencias por estado

p <- ggplot(df_summary, aes(x = "", y = percent, fill = is_nibrs)) +
  geom_col(width = 1, alpha = 0.9) +  # Barras apiladas (100% stacked)
  facet_geo(~ state_abbr, grid = "us_state_grid2") +  # Distribución geográfica
  scale_fill_manual(values = c("TRUE" = "#1F3A64", "FALSE" = "#E74C3C"),
                    labels = c("TRUE" = "Is NIBRS", "FALSE" = "No NIBRS"),
                    guide = guide_legend(
                      direction = "horizontal",  # Coloca la leyenda en fila
                      keywidth = unit(4, "lines"),  # Ancho de la barra de la leyenda
                      keyheight = unit(1, "lines"),  # Mantener la altura de la barra de la leyenda igual
                      label.position = "bottom",  # Posición de las etiquetas
                      title = NULL,  # Elimina el título de la leyenda
                      nrow = 1,  # Una sola fila
                      override.aes = list(size = 8)  # Aumenta el tamaño de las barras de la leyenda
                    )) +  # Leyenda horizontal sin espacio
  scale_y_continuous(expand = c(0, 0)) +  # Evita espacio extra en las barras
  theme_void() +  # Elimina ejes y fondos
  theme(
    strip.text = element_blank(),  # Elimina las etiquetas de facetas
    legend.position = "bottom",  # Elimina la leyenda
    plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Margen adicional
    plot.background = element_rect(fill = "gray95"),  # Fondo de todo el gráfico
    plot.title = element_text(
      size = 36, face = "bold", hjust =0.015, vjust = 0.15,  
      margin = margin(b = 25), family = "Helvetica"  # Fuente aplicada correctamente
    ), legend.text = element_text(size = 14, family = "roboto_condensed", face = "bold"),
  ) +
  geom_text(aes(x = 0.80, y = 0.75, label = state_abbr), size = 10,
            fontface = "bold", color = "white",family = "Helvetica", check_overlap = TRUE)+
  geom_text(aes(y = 0.25, label = paste(nibrs_agencies, "/", total_agencies)),
            size = 6, fontface = "bold", color = "white", family = "Helvetica", check_overlap = TRUE) +
  ggtitle("NIBRS Adoption Across U.S. Law Enforcement Agencies")

print(p)

texto_nibrs <- paste(
  "The National Incident-Based Reporting System (NIBRS) is a comprehensive,",
  "nationwide system for collecting, analyzing, and reporting crime data.It is",
  "designed to provide law enforcement with more detailed and accurate crime",
  "data by capturing individual offenses, victims, and offenders in each ",
  "incident. NIBRS aims to improve the understanding of crime in the  U.S.",
  "and help agencies in their response to crime.",
  sep = "\n"
)

# Crear el grob con el texto y ajustarlo
grob_text <- textGrob(
  texto_nibrs,
  x = unit(0.12, "npc"),     # 12% desde el borde izquierdo
  y = unit(0.80, "npc"),     # 88% desde el borde inferior
  just = c("left", "bottom"),   # Alineación izquierda y arriba
  gp = gpar(
    fontsize = 25,
    family = "Helvetica",
    lineheight = 0.7,        # Ajuste de la altura entre líneas
    col = "azure4"
  )
)

# Dibujar el texto en el gráfico
grid.draw(grob_text)

zoom_size <- dev.size()

ggsave("grafico_zoom.png", plot = p, width = 1920 / 300, height = 1200 / 300, dpi = 300)