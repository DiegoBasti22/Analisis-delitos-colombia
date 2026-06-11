# =============================================================================
# FASE 3: VISUALIZACIONES FINALES PARA EL PORTAFOLIO
# Proyecto: Análisis de Delitos en Colombia 2020–2025
# Herramienta: R
# =============================================================================
# En esta fase creamos visualizaciones de nivel profesional.
# La diferencia con las fases anteriores es que aquí nos enfocamos en
# que las gráficas sean claras, bonitas y cuenten una historia con los datos.
# Estas son las gráficas que irán en tu portafolio y LinkedIn.
#
# Gráficas que crearemos:
#   1. Mapa de calor: delitos por departamento y año
#   2. Gráfica de área apilada: evolución de tipos de delito por año
#   3. Gráfica de barras agrupadas: víctimas por género y rango de edad
#   4. Panel resumen: 4 indicadores clave en una sola imagen
#
# Todas las gráficas se guardan automáticamente como archivos PNG
# en la carpeta del proyecto.
# =============================================================================


# -----------------------------------------------------------------------------
# PASO 1: CARGAR LIBRERÍAS Y DATOS
# -----------------------------------------------------------------------------

library(tidyverse)  # Manipulación de datos y gráficas con ggplot2
library(lubridate)  # Manejo de fechas
library(scales)     # Formateo de números y ejes en gráficas

# patchwork permite combinar varias gráficas de ggplot2 en un solo panel.
# Es muy útil para crear dashboards o resúmenes visuales.
# Lo instalamos aquí porque es nuevo en esta fase.
install.packages("patchwork")
library(patchwork)

cat("✅ Librerías cargadas\n")

# Cargamos el dataset limpio generado en la Fase 1
df <- read_csv2(
  "C:/Users/diego/OneDrive/Imagenes1/Escritorio/CURSOS DE DATOS/CURSO PYTHON/KAGGLE/DELITOS COLOMBIA/v_delitos_limpio.csv"
)

cat("✅ Dataset cargado:", format(nrow(df), big.mark = ","), "filas\n\n")

# Definimos la carpeta donde guardaremos las gráficas
# Usamos la misma carpeta donde están los datos
RUTA_OUTPUTS <- "C:/Users/diego/OneDrive/Imagenes1/Escritorio/CURSOS DE DATOS/CURSO PYTHON/KAGGLE/DELITOS COLOMBIA/"

# dir.create() crea la carpeta si no existe
# showWarnings = FALSE evita que muestre advertencia si ya existe
dir.create(RUTA_OUTPUTS, showWarnings = FALSE, recursive = TRUE)

cat("📂 Las gráficas se guardarán en:\n  ", RUTA_OUTPUTS, "\n\n")


# -----------------------------------------------------------------------------
# GRÁFICA 1: MAPA DE CALOR — Delitos por Departamento y Año
# -----------------------------------------------------------------------------
# Un mapa de calor (heatmap) muestra la intensidad de un valor usando colores.
# Es ideal para ver patrones en dos dimensiones al mismo tiempo:
# en este caso, departamento (eje Y) vs año (eje X).
# Los colores más oscuros indican más delitos.

cat("========================================\n")
cat("GRÁFICA 1: Mapa de calor por departamento y año\n")
cat("========================================\n")

# Preparamos los datos: top 15 departamentos con más casos en total
# Esto lo hacemos para que la gráfica no sea muy grande con los 32 departamentos
top15_dptos <- df %>%
  group_by(departamento) %>%
  summarise(total = sum(cantidad)) %>%
  arrange(desc(total)) %>%
  slice(1:15) %>%
  pull(departamento)  # pull() extrae la columna como un vector simple

# Filtramos el dataset solo para esos 15 departamentos y años completos
heatmap_data <- df %>%
  filter(departamento %in% top15_dptos, año >= 2020, año <= 2025) %>%
  # %in% comprueba si cada valor está dentro de un vector
  # Es como decir: "donde departamento sea cualquiera de estos 15"
  group_by(departamento, año) %>%
  summarise(total = sum(cantidad), .groups = "drop")

g1 <- heatmap_data %>%
  ggplot(aes(x = factor(año), y = reorder(departamento, total), fill = total)) +

  # geom_tile() dibuja los rectángulos del mapa de calor
  # color = "white" y linewidth = 0.5 agregan bordes blancos entre celdas
  geom_tile(color = "white", linewidth = 0.5) +

  # Etiqueta dentro de cada celda con el número formateado
  geom_text(aes(label = comma(total, accuracy = 1)), size = 2.8, color = "white", fontface = "bold") +

  # scale_fill_gradient2 define un degradado de 3 colores:
  # low = pocos casos (azul claro), mid = casos medios (naranja), high = muchos casos (rojo oscuro)
  # midpoint define en qué valor está el color del medio
  scale_fill_gradient2(
    low      = "#E3F2FD",
    mid      = "#FF8A65",
    high     = "#B71C1C",
    midpoint = median(heatmap_data$total),
    labels   = comma
  ) +

  labs(
    title    = "Mapa de Calor: Delitos por Departamento y Año",
    subtitle = "Top 15 departamentos con más casos registrados (2020–2025)",
    x        = "Año",
    y        = NULL,   # NULL elimina el título del eje Y (no es necesario aquí)
    fill     = "Total\nde Casos",
    caption  = "Fuente: Policía Nacional de Colombia"
  ) +

  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "gray40", size = 10),
    plot.caption    = element_text(color = "gray50", hjust = 0),
    # panel.grid elimina las líneas de cuadrícula del fondo
    panel.grid      = element_blank(),
    legend.position = "right"
  )

# ggsave() guarda la gráfica como un archivo PNG en tu computador
# width y height definen el tamaño en pulgadas
# dpi = 150 define la resolución (150 es buena calidad para pantalla)
ggsave(
  filename = paste0(RUTA_OUTPUTS, "g1_heatmap_departamentos.png"),
  plot     = g1,
  width    = 12,
  height   = 7,
  dpi      = 150
)

print(g1)
cat("✅ Gráfica 1 guardada: g1_heatmap_departamentos.png\n\n")


# -----------------------------------------------------------------------------
# GRÁFICA 2: ÁREA APILADA — Evolución de tipos de delito por año
# -----------------------------------------------------------------------------
# Una gráfica de área apilada muestra cómo varios grupos contribuyen
# a un total a lo largo del tiempo. Aquí veremos la composición
# de los delitos año a año: qué tipo de delito pesa más en cada año.

cat("========================================\n")
cat("GRÁFICA 2: Evolución de tipos de delito por año\n")
cat("========================================\n")

area_data <- df %>%
  filter(año >= 2020, año <= 2025) %>%
  group_by(año, tipo) %>%
  summarise(total = sum(cantidad), .groups = "drop")

# Paleta de colores manual para que cada tipo de delito tenga un color distinto
# Estos colores están elegidos para que sean fáciles de distinguir entre sí
colores_tipo <- c(
  "#1565C0", "#E53935", "#2E7D32", "#F57F17",
  "#6A1B9A", "#00838F", "#4E342E", "#37474F"
)

g2 <- area_data %>%
  ggplot(aes(x = año, y = total, fill = tipo)) +

  # geom_area con position = "stack" apila las áreas una encima de la otra
  # alpha = 0.85 hace las áreas ligeramente transparentes
  geom_area(position = "stack", alpha = 0.85) +

  scale_fill_manual(values = colores_tipo) +

  scale_x_continuous(breaks = 2020:2025) +

  scale_y_continuous(labels = comma) +

  labs(
    title    = "Evolución de Delitos por Tipo (2020–2025)",
    subtitle = "Composición anual por categoría de delito",
    x        = "Año",
    y        = "Total de Casos",
    fill     = "Tipo de Delito",
    caption  = "Fuente: Policía Nacional de Colombia"
  ) +

  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "gray40"),
    plot.caption    = element_text(color = "gray50", hjust = 0),
    # Movemos la leyenda abajo para que la gráfica tenga más espacio
    legend.position = "bottom",
    # legend.ncol define en cuántas columnas se organiza la leyenda
    legend.ncol     = 2,
    legend.title    = element_text(face = "bold")
  )

ggsave(
  filename = paste0(RUTA_OUTPUTS, "g2_area_tipos_delito.png"),
  plot     = g2,
  width    = 12,
  height   = 7,
  dpi      = 150
)

print(g2)
cat("✅ Gráfica 2 guardada: g2_area_tipos_delito.png\n\n")


# -----------------------------------------------------------------------------
# GRÁFICA 3: BARRAS AGRUPADAS — Víctimas por género y rango de edad
# -----------------------------------------------------------------------------
# Las barras agrupadas permiten comparar dos variables categóricas al mismo tiempo.
# Aquí comparamos el género de las víctimas dentro de cada rango de edad.

cat("========================================\n")
cat("GRÁFICA 3: Víctimas por género y rango de edad\n")
cat("========================================\n")

edad_genero <- df %>%
  # Excluimos los registros sin información de género o edad
  filter(genero != "NO REPORTADO", rango_edad != "NO REPORTADO") %>%
  group_by(rango_edad, genero) %>%
  summarise(total = sum(cantidad), .groups = "drop")

cat("\n📋 Víctimas por género y rango de edad:\n")
print(edad_genero %>% mutate(total = format(total, big.mark = ",")))

g3 <- edad_genero %>%
  ggplot(aes(x = rango_edad, y = total, fill = genero)) +

  # position = "dodge" coloca las barras de cada grupo una al lado de la otra
  # en vez de apilarlas. "dodge" viene del inglés "esquivar".
  geom_col(position = "dodge", width = 0.6) +

  geom_text(
    aes(label = comma(total)),
    # position_dodge(width) alinea las etiquetas con cada barra
    position = position_dodge(width = 0.6),
    vjust    = -0.4,
    size     = 3.2,
    fontface = "bold"
  ) +

  scale_fill_manual(values = c("FEMENINO" = "#E91E63", "MASCULINO" = "#1E88E5")) +

  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +

  labs(
    title    = "Víctimas por Género y Rango de Edad (2020–2025)",
    subtitle = "Comparación entre víctimas femeninas y masculinas por grupo etario",
    x        = "Rango de Edad",
    y        = "Total de Víctimas",
    fill     = "Género",
    caption  = "Fuente: Policía Nacional de Colombia"
  ) +

  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "gray40"),
    plot.caption    = element_text(color = "gray50", hjust = 0),
    legend.position = "top"
  )

ggsave(
  filename = paste0(RUTA_OUTPUTS, "g3_barras_edad_genero.png"),
  plot     = g3,
  width    = 11,
  height   = 6,
  dpi      = 150
)

print(g3)
cat("✅ Gráfica 3 guardada: g3_barras_edad_genero.png\n\n")


# -----------------------------------------------------------------------------
# GRÁFICA 4: PANEL RESUMEN — 4 indicadores clave en una sola imagen
# -----------------------------------------------------------------------------
# Esta es la gráfica más importante para el portafolio.
# Combinamos 4 gráficas pequeñas en un solo panel usando patchwork.
# Es el tipo de imagen que más impacta en LinkedIn porque muestra
# mucha información de forma organizada y profesional.

cat("========================================\n")
cat("GRÁFICA 4: Panel resumen (4 en 1)\n")
cat("========================================\n")

# --- Mini gráfica A: Top 5 departamentos ---
mini_a <- df %>%
  group_by(departamento) %>%
  summarise(total = sum(cantidad)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  mutate(departamento = reorder(departamento, total)) %>%
  ggplot(aes(x = total, y = departamento, fill = total)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(total)), hjust = -0.1, size = 3) +
  scale_fill_gradient(low = "#90CAF9", high = "#1565C0") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Top 5 Departamentos", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11))

# --- Mini gráfica B: Evolución anual total ---
mini_b <- df %>%
  filter(año >= 2020, año <= 2025) %>%
  group_by(año) %>%
  summarise(total = sum(cantidad)) %>%
  ggplot(aes(x = año, y = total)) +
  geom_line(color = "#1565C0", linewidth = 1.5) +
  geom_point(color = "#1565C0", size = 3) +
  geom_text(aes(label = comma(total)), vjust = -0.8, size = 2.8) +
  scale_x_continuous(breaks = 2020:2025) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.1, 0.15))) +
  labs(title = "Evolución Anual Total", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11))

# --- Mini gráfica C: Distribución por género ---
mini_c <- df %>%
  filter(genero != "NO REPORTADO") %>%
  group_by(genero) %>%
  summarise(total = sum(cantidad)) %>%
  mutate(pct = round(total / sum(total) * 100, 1)) %>%
  ggplot(aes(x = "", y = total, fill = genero)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = paste0(pct, "%")),
    position  = position_stack(vjust = 0.5),
    color     = "white", fontface = "bold", size = 4
  ) +
  scale_fill_manual(values = c("FEMENINO" = "#E91E63", "MASCULINO" = "#1E88E5")) +
  labs(title = "Víctimas por Género", fill = NULL) +
  theme_void() +
  theme(
    plot.title      = element_text(face = "bold", size = 11, hjust = 0.5),
    legend.position = "bottom"
  )

# --- Mini gráfica D: Top 5 tipos de delito ---
mini_d <- df %>%
  group_by(tipo) %>%
  summarise(total = sum(cantidad)) %>%
  arrange(desc(total)) %>%
  slice(1:5) %>%
  mutate(tipo = reorder(tipo, total)) %>%
  ggplot(aes(x = total, y = tipo, fill = total)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(total)), hjust = -0.1, size = 3) +
  scale_fill_gradient(low = "#FFCC80", high = "#E65100") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Top 5 Tipos de Delito", x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11))

# Combinamos las 4 mini gráficas con patchwork
# El operador | coloca gráficas una al lado de la otra (en la misma fila)
# El operador / coloca gráficas una encima de la otra (en filas distintas)
# (mini_a | mini_b) / (mini_c | mini_d) crea un panel de 2x2
panel_final <- (mini_a | mini_b) / (mini_c | mini_d) +

  # plot_annotation() agrega títulos y subtítulos al panel completo
  plot_annotation(
    title    = "Análisis de Delitos en Colombia 2020–2025",
    subtitle = "Resumen ejecutivo | Fuente: Policía Nacional de Colombia",
    caption  = "Elaborado con R | Proyecto de portafolio de análisis de datos",
    # theme() dentro de plot_annotation controla el estilo de los títulos del panel
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(color = "gray40", size = 11, hjust = 0.5),
      plot.caption  = element_text(color = "gray50", hjust = 0.5)
    )
  )

ggsave(
  filename = paste0(RUTA_OUTPUTS, "g4_panel_resumen.png"),
  plot     = panel_final,
  width    = 14,
  height   = 10,
  dpi      = 150
)

print(panel_final)
cat("✅ Gráfica 4 guardada: g4_panel_resumen.png\n\n")


# -----------------------------------------------------------------------------
# RESUMEN FINAL
# -----------------------------------------------------------------------------
cat("========================================\n")
cat("🎉 FASE 3 COMPLETADA — PROYECTO FINALIZADO\n")
cat("========================================\n")
cat("
Archivos generados en tu carpeta DELITOS COLOMBIA:
  ✅ g1_heatmap_departamentos.png
  ✅ g2_area_tipos_delito.png
  ✅ g3_barras_edad_genero.png
  ✅ g4_panel_resumen.png  ← Esta es la imagen para LinkedIn

Tu proyecto de portafolio está listo.
Puedes subir las imágenes a LinkedIn con una descripción
del análisis que realizaste.
")
