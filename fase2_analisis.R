# =============================================================================
# FASE 2: ANÁLISIS DE PREGUNTAS CLAVE
# Proyecto: Análisis de Delitos en Colombia 2020–2025
# Herramienta: R
# =============================================================================
# En esta fase respondemos preguntas concretas con los datos.
# Esto es exactamente lo que hace un analista de datos en el mundo real:
# tomar un dataset y extraer conclusiones útiles que ayuden a entender
# la realidad detrás de los números.
#
# Preguntas que responderemos:
#   1. ¿Cuáles departamentos tienen más delitos?
#   2. ¿Cómo evolucionaron los homicidios año a año?
#   3. ¿Qué género es más víctima de violencia intrafamiliar?
#   4. ¿Cuáles son los meses con más delitos?
#   5. ¿Qué tipo de arma se usa más en cada tipo de delito?
# =============================================================================


# -----------------------------------------------------------------------------
# PASO 1: CARGAR LIBRERÍAS Y DATOS
# -----------------------------------------------------------------------------
# Cargamos las mismas librerías de la Fase 1.
# Ahora cargamos el dataset LIMPIO que guardamos al final de la Fase 1,
# no el original. Así aprovechamos todo el trabajo de limpieza que ya hicimos.

library(tidyverse)  # Para manipular datos y crear gráficas
library(lubridate)  # Para trabajar con fechas
library(scales)     # Para formatear números en gráficas

cat("✅ Librerías cargadas\n")

# Leemos el archivo limpio que generamos en la Fase 1
# read_csv2() lee archivos CSV con ; como separador (el que usamos al guardar)
df <- read_csv2(
  "C:/Users/diego/OneDrive/Imagenes1/Escritorio/CURSOS DE DATOS/CURSO PYTHON/KAGGLE/DELITOS COLOMBIA/v_delitos_limpio.csv"
)

cat("✅ Dataset limpio cargado:", nrow(df), "filas\n\n")


# -----------------------------------------------------------------------------
# PREGUNTA 1: ¿Cuáles departamentos tienen más delitos?
# -----------------------------------------------------------------------------
# Aquí agrupamos todos los registros por departamento y sumamos sus casos.
# Luego tomamos los 10 con más casos para comparar.

cat("========================================\n")
cat("PREGUNTA 1: Top 10 departamentos con más delitos\n")
cat("========================================\n")

# Creamos una tabla resumen por departamento
# group_by() → agrupa los datos por la columna indicada
# summarise() → calcula un valor resumen para cada grupo (aquí: suma de casos)
# arrange(desc()) → ordena de mayor a menor
# slice(1:10) → se queda solo con los primeros 10 resultados
top_departamentos <- df %>%
  group_by(departamento) %>%
  summarise(total_casos = sum(cantidad)) %>%
  arrange(desc(total_casos)) %>%
  slice(1:10)

# Imprimimos la tabla en consola
# format() con big.mark="," agrega comas a los miles (ej: 1,234,567)
cat("\n📋 Top 10 departamentos:\n")
print(top_departamentos %>%
  mutate(total_casos = format(total_casos, big.mark = ",")))

# Creamos la gráfica de barras horizontales
top_departamentos %>%
  # Reordenamos los departamentos para que el mayor quede arriba en la gráfica
  mutate(departamento = reorder(departamento, total_casos)) %>%
  ggplot(aes(x = total_casos, y = departamento, fill = total_casos)) +

  geom_col(show.legend = FALSE) +

  # geom_text agrega las etiquetas numéricas al final de cada barra
  # comma() formatea el número con comas para los miles
  geom_text(aes(label = comma(total_casos)), hjust = -0.1, size = 3.5) +

  # scale_fill_gradient define un degradado de color:
  # los departamentos con menos casos son de color azul claro
  # los de más casos son de color azul oscuro
  scale_fill_gradient(low = "#90CAF9", high = "#1565C0") +

  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +

  labs(
    title    = "Top 10 Departamentos con más Delitos (2020–2025)",
    subtitle = "Suma total de todos los tipos de delito registrados",
    x        = "Total de Casos",
    y        = "Departamento",
    # caption agrega una nota al pie de la gráfica
    caption  = "Fuente: Policía Nacional de Colombia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", hjust = 0)
  )

cat("✅ Gráfica 1 generada → revisa el panel Plots\n\n")


# -----------------------------------------------------------------------------
# PREGUNTA 2: ¿Cómo evolucionaron los homicidios año a año?
# -----------------------------------------------------------------------------
# Filtramos solo los registros de homicidios y analizamos su evolución
# en el tiempo. Esto nos permite ver si los homicidios aumentaron o
# disminuyeron durante el período analizado.

cat("========================================\n")
cat("PREGUNTA 2: Evolución de homicidios por año\n")
cat("========================================\n")

# str_detect() → detecta si un texto contiene un patrón específico
# "HOMICIDIO" es el patrón que buscamos dentro de la columna 'tipo'
# Esto filtra todas las filas donde el tipo de delito mencione "HOMICIDIO"
homicidios_año <- df %>%
  filter(str_detect(tipo, "HOMICIDIO")) %>%
  filter(año >= 2020, año <= 2025) %>%
  group_by(año) %>%
  summarise(total = sum(cantidad)) %>%
  arrange(año)

cat("\n📋 Homicidios por año:\n")
print(homicidios_año %>%
  mutate(total = format(total, big.mark = ",")))

# Calculamos el cambio porcentual año a año
# lag(total) → devuelve el valor del año ANTERIOR
# Con esto calculamos si los homicidios subieron o bajaron respecto al año pasado
homicidios_año <- homicidios_año %>%
  mutate(
    cambio_pct = round((total - lag(total)) / lag(total) * 100, 1),
    # paste0() une texto y números en una sola cadena
    # ifelse() es como un "si/sino": si cambio_pct > 0 agrega "+", si no, nada
    etiqueta = paste0(comma(total), "\n(", ifelse(cambio_pct > 0, "+", ""), cambio_pct, "%)")
  )

# Reemplazamos el NA del primer año (no tiene año anterior para comparar)
homicidios_año$etiqueta[1] <- comma(homicidios_año$total[1])

homicidios_año %>%
  ggplot(aes(x = año, y = total)) +

  # geom_area dibuja el área bajo la línea con un color de relleno
  # alpha = 0.2 hace el relleno semitransparente (0 = invisible, 1 = sólido)
  geom_area(fill = "#EF5350", alpha = 0.2) +

  geom_line(color = "#C62828", linewidth = 1.8) +

  geom_point(color = "#C62828", size = 5) +

  # Las etiquetas muestran el total y el cambio porcentual respecto al año anterior
  geom_text(aes(label = etiqueta), vjust = -0.8, size = 3.2, lineheight = 0.9) +

  scale_x_continuous(breaks = 2020:2025) +
  scale_y_continuous(
    labels = comma,
    # limits define el rango del eje Y. El máximo es 120% del valor más alto
    # para que las etiquetas no queden cortadas arriba
    limits = c(0, max(homicidios_año$total) * 1.2)
  ) +

  labs(
    title    = "Evolución Anual de Homicidios en Colombia (2020–2025)",
    subtitle = "Incluye variación porcentual respecto al año anterior",
    x        = "Año",
    y        = "Total de Homicidios",
    caption  = "Fuente: Policía Nacional de Colombia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", hjust = 0)
  )

cat("✅ Gráfica 2 generada → revisa el panel Plots\n\n")


# -----------------------------------------------------------------------------
# PREGUNTA 3: ¿Qué género es más víctima de violencia intrafamiliar?
# -----------------------------------------------------------------------------
# Filtramos solo los delitos de violencia intrafamiliar y comparamos
# cuántas víctimas son hombres vs mujeres.

cat("========================================\n")
cat("PREGUNTA 3: Violencia intrafamiliar por género\n")
cat("========================================\n")

# Filtramos violencia intrafamiliar y excluimos género no reportado
violencia_genero <- df %>%
  filter(str_detect(tipo, "VIOLENCIA")) %>%
  filter(genero != "NO REPORTADO") %>%
  group_by(genero) %>%
  summarise(total = sum(cantidad)) %>%
  # Calculamos el porcentaje de cada género
  mutate(porcentaje = round(total / sum(total) * 100, 1))

cat("\n📋 Violencia intrafamiliar por género:\n")
print(violencia_genero %>%
  mutate(total = format(total, big.mark = ",")))

# Definimos colores específicos para cada género
colores_genero <- c("FEMENINO" = "#E91E63", "MASCULINO" = "#1E88E5")

violencia_genero %>%
  ggplot(aes(x = genero, y = total, fill = genero)) +

  geom_col(show.legend = FALSE, width = 0.5) +

  # Etiqueta con el total arriba de la barra
  geom_text(
    aes(label = comma(total)),
    vjust = -0.5, size = 4, fontface = "bold"
  ) +

  # Etiqueta con el porcentaje dentro de la barra
  # position_stack(vjust = 0.5) centra el texto verticalmente en la barra
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    position = position_stack(vjust = 0.5),
    size = 5, color = "white", fontface = "bold"
  ) +

  # scale_fill_manual permite asignar colores específicos a cada categoría
  scale_fill_manual(values = colores_genero) +

  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +

  labs(
    title    = "Víctimas de Violencia Intrafamiliar por Género (2020–2025)",
    subtitle = "Comparación entre víctimas femeninas y masculinas",
    x        = "Género",
    y        = "Total de Víctimas",
    caption  = "Fuente: Policía Nacional de Colombia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", hjust = 0)
  )

cat("✅ Gráfica 3 generada → revisa el panel Plots\n\n")


# -----------------------------------------------------------------------------
# PREGUNTA 4: ¿Cuáles son los meses con más delitos?
# -----------------------------------------------------------------------------
# Analizamos si hay meses del año donde los delitos se concentran más.
# Esto puede revelar patrones estacionales muy útiles para la toma de decisiones.

cat("========================================\n")
cat("PREGUNTA 4: ¿En qué meses ocurren más delitos?\n")
cat("========================================\n")

# Creamos un vector con los nombres de los meses en español
# Este vector lo usamos para reemplazar los números de mes (1,2,3...) por nombres
nombres_meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                   "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

delitos_mes <- df %>%
  filter(año >= 2020, año <= 2025) %>%
  group_by(mes) %>%
  summarise(total = sum(cantidad)) %>%
  # Convertimos el número de mes en nombre usando el vector que creamos arriba
  # factor() con levels asegura que los meses se muestren en orden cronológico
  mutate(
    nombre_mes = factor(nombres_meses[mes], levels = nombres_meses)
  )

cat("\n📋 Total de delitos por mes:\n")
print(delitos_mes %>%
  select(nombre_mes, total) %>%
  mutate(total = format(total, big.mark = ",")))

delitos_mes %>%
  ggplot(aes(x = nombre_mes, y = total, fill = total)) +

  geom_col(show.legend = FALSE) +

  geom_text(aes(label = comma(total)), vjust = -0.4, size = 3.2) +

  # Degradado de color: meses con menos delitos en azul, más delitos en rojo
  scale_fill_gradient(low = "#81D4FA", high = "#E53935") +

  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +

  labs(
    title    = "Total de Delitos por Mes (2020–2025)",
    subtitle = "Suma acumulada de todos los años disponibles",
    x        = "Mes",
    y        = "Total de Casos",
    caption  = "Fuente: Policía Nacional de Colombia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", hjust = 0)
  )

cat("✅ Gráfica 4 generada → revisa el panel Plots\n\n")


# -----------------------------------------------------------------------------
# PREGUNTA 5: ¿Qué tipo de arma se usa más en los delitos?
# -----------------------------------------------------------------------------
# Analizamos las armas más frecuentes en los registros de delitos.
# Esto puede ayudar a entender los patrones de violencia.

cat("========================================\n")
cat("PREGUNTA 5: Tipos de arma más frecuentes\n")
cat("========================================\n")

top_armas <- df %>%
  # Excluimos registros sin información de arma
  filter(!str_detect(tipo_arma, "NO REPORTA|SIN EMPLEO|DESCONOCIDA")) %>%
  group_by(tipo_arma) %>%
  summarise(total = sum(cantidad)) %>%
  arrange(desc(total)) %>%
  slice(1:10) %>%
  mutate(tipo_arma = reorder(tipo_arma, total))

cat("\n📋 Top 10 tipos de arma:\n")
print(top_armas %>%
  mutate(total = format(total, big.mark = ",")))

top_armas %>%
  ggplot(aes(x = total, y = tipo_arma, fill = total)) +

  geom_col(show.legend = FALSE) +

  geom_text(aes(label = comma(total)), hjust = -0.1, size = 3.5) +

  scale_fill_gradient(low = "#FFCC80", high = "#E65100") +

  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +

  labs(
    title    = "Top 10 Tipos de Arma más Frecuentes (2020–2025)",
    subtitle = "Excluye registros sin información del arma",
    x        = "Total de Casos",
    y        = "Tipo de Arma",
    caption  = "Fuente: Policía Nacional de Colombia"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40"),
    plot.caption  = element_text(color = "gray50", hjust = 0)
  )

cat("✅ Gráfica 5 generada → revisa el panel Plots\n\n")


# -----------------------------------------------------------------------------
# RESUMEN FINAL
# -----------------------------------------------------------------------------
cat("========================================\n")
cat("🎉 FASE 2 COMPLETADA EXITOSAMENTE\n")
cat("========================================\n")
cat("
Preguntas respondidas:
  ✅ P1: Top 10 departamentos con más delitos
  ✅ P2: Evolución anual de homicidios con variación %
  ✅ P3: Género más afectado por violencia intrafamiliar
  ✅ P4: Meses con más concentración de delitos
  ✅ P5: Tipos de arma más frecuentes

Próximo paso → Fase 3: Visualizaciones finales para el portafolio
")
