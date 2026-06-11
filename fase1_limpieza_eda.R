# =============================================================================
# FASE 1: LIMPIEZA DE DATOS Y ANÁLISIS EXPLORATORIO (EDA)
# Proyecto: Análisis de Delitos en Colombia 2020–2025
# Herramienta: R
# =============================================================================
# ¿Qué es el EDA?
# EDA significa "Exploratory Data Analysis" o Análisis Exploratorio de Datos.
# Es el PRIMER paso en todo proyecto de datos. Su objetivo es ENTENDER
# los datos antes de analizarlos: cuántos registros hay, si hay errores,
# qué valores tienen las columnas, si hay datos faltantes, etc.
# =============================================================================


# -----------------------------------------------------------------------------
# PASO 1: INSTALAR Y CARGAR LIBRERÍAS
# -----------------------------------------------------------------------------
# En R, las librerías (también llamadas "paquetes") son conjuntos de
# funciones que otras personas crearon para facilitarnos el trabajo.
# Sin librerías, tendríamos que programar todo desde cero.
#
# install.packages() → descarga e instala el paquete en tu computador.
#                      Solo se hace UNA VEZ. Después no es necesario repetirlo.
#
# library() → activa el paquete para poder usarlo en este script.
#             Esto SÍ se debe hacer cada vez que abras RStudio.
#
# Los paquetes que usamos:
#   tidyverse → es una colección de paquetes para manipular, limpiar
#               y visualizar datos. Incluye ggplot2 (gráficas), dplyr
#               (manipulación de tablas), readr (leer archivos), entre otros.
#   lubridate → hace muy fácil trabajar con fechas (convertir, extraer
#               el año, el mes, el día, etc.)
#   scales    → ayuda a formatear números en las gráficas (por ejemplo,
#               mostrar 1,000,000 en vez de 1e+06)

install.packages("tidyverse")  # Instala tidyverse (solo la primera vez)
install.packages("lubridate")  # Instala lubridate (solo la primera vez)
install.packages("scales")     # Instala scales (solo la primera vez)

library(tidyverse)  # Activa tidyverse para este script
library(lubridate)  # Activa lubridate para este script
library(scales)     # Activa scales para este script

# cat() es una función de R que imprime texto en la consola.
# "\n" significa "nueva línea" (como presionar Enter).
cat("✅ Librerías cargadas correctamente\n")


# -----------------------------------------------------------------------------
# PASO 2: CARGAR LOS DATOS
# -----------------------------------------------------------------------------
# read_delim() lee un archivo de texto y lo convierte en una tabla (data frame).
# Un data frame es como una hoja de Excel dentro de R: tiene filas y columnas.
#
# Parámetros que usamos:
#   El primer valor → es la ruta completa del archivo en tu computador.
#                     En R siempre usamos / (barra hacia adelante), nunca \.
#   delim = ";"     → le decimos que las columnas están separadas por ; (punto y coma)
#                     Si usáramos coma sería delim = ","
#   locale(encoding = "UTF-8") → le decimos que el archivo usa UTF-8,
#                     que es la codificación que permite leer tildes y ñ correctamente.
#
# El símbolo <- es el operador de asignación en R.
# df <- read_delim(...) significa: "lee el archivo y guárdalo en la variable df"
# df es el nombre que le damos a nuestra tabla. Podría llamarse como quieras,
# pero "df" es una convención común (viene de "data frame").

cat("Cargando los datos...\n")

df <- read_delim(
  "C:/Users/diego/OneDrive/Imagenes1/Escritorio/CURSOS DE DATOS/CURSO PYTHON/KAGGLE/DELITOS COLOMBIA/v_delitos.csv",
  delim = ";",
  locale = locale(encoding = "UTF-8")
)

cat("✅ Datos cargados exitosamente\n")


# -----------------------------------------------------------------------------
# PASO 3: PRIMERA MIRADA A LOS DATOS
# -----------------------------------------------------------------------------
# Siempre que cargamos datos nuevos, lo primero es entender su estructura:
# cuántas filas y columnas tiene, cómo se llaman las columnas, y cómo
# lucen los primeros registros.

cat("\n========================================\n")
cat("PASO 3: PRIMERA MIRADA A LOS DATOS\n")
cat("========================================\n")

# nrow() → devuelve el número de filas (registros) del data frame
# ncol() → devuelve el número de columnas (variables) del data frame
cat("\n📋 Dimensiones del dataset:\n")
cat("   Filas   :", nrow(df), "\n")
cat("   Columnas:", ncol(df), "\n")

# colnames() → devuelve los nombres de todas las columnas como una lista
cat("\n📋 Nombres de las columnas:\n")
print(colnames(df))

# head() → muestra las primeras 6 filas del data frame por defecto.
# Es útil para ver cómo lucen los datos reales.
cat("\n📋 Primeras 6 filas:\n")
print(head(df))

# summary() → genera un resumen estadístico de cada columna:
#   - Para columnas numéricas: mínimo, máximo, promedio, mediana, percentiles
#   - Para columnas de texto: cantidad de registros
# Es una de las funciones más útiles para entender los datos rápidamente.
cat("\n📋 Resumen estadístico general:\n")
print(summary(df))


# -----------------------------------------------------------------------------
# PASO 4: REVISAR VALORES NULOS
# -----------------------------------------------------------------------------
# Los valores nulos (NA en R) son celdas vacías o sin información.
# Son muy comunes en datos reales y pueden causar errores en los análisis
# si no los manejamos correctamente.
# El proceso es: primero DETECTAR cuántos hay, luego DECIDIR qué hacer.

cat("\n========================================\n")
cat("PASO 4: VALORES NULOS\n")
cat("========================================\n")

# is.na(df)     → crea una tabla de TRUE/FALSE: TRUE donde hay un NA
# colSums(...)  → suma los TRUE de cada columna (TRUE = 1, FALSE = 0)
# El resultado es el conteo de NAs por columna
nulos <- colSums(is.na(df))

cat("\n📋 Valores nulos por columna:\n")
print(nulos)

# sum(nulos) → suma todos los nulos de todas las columnas
# Con esto sabemos si hay algún problema general en los datos
if (sum(nulos) == 0) {
  cat("\n✅ ¡No hay valores nulos en el dataset!\n")
} else {
  cat("\n⚠️  Hay valores nulos. Se muestran arriba por columna.\n")
}


# -----------------------------------------------------------------------------
# PASO 5: REVISAR VALORES ÚNICOS
# -----------------------------------------------------------------------------
# Los valores únicos nos dicen cuántas categorías DISTINTAS hay en cada columna.
# Por ejemplo, si la columna "genero" tiene 3 valores únicos,
# probablemente son: MASCULINO, FEMENINO y NO REPORTADO.
# Esto nos ayuda a entender qué tipo de datos tenemos.

cat("\n========================================\n")
cat("PASO 5: VALORES ÚNICOS POR COLUMNA\n")
cat("========================================\n")

# sapply() → aplica una función a cada columna del data frame
# n_distinct() → cuenta cuántos valores únicos hay en cada columna
cat("\n📋 Cantidad de valores únicos por columna:\n")
print(sapply(df, n_distinct))

# unique() → muestra cuáles son los valores únicos de una columna específica
# El símbolo $ se usa para acceder a una columna: df$nombre_columna
cat("\n📋 Tipos de delito (columna 'tipo'):\n")
print(unique(df$tipo))

cat("\n📋 Géneros (columna 'genero'):\n")
print(unique(df$genero))

cat("\n📋 Rangos de edad (columna 'rango_edad'):\n")
print(unique(df$rango_edad))


# -----------------------------------------------------------------------------
# PASO 6: LIMPIEZA DE DATOS
# -----------------------------------------------------------------------------
# Aquí transformamos los datos para corregir inconsistencias y dejarlos
# listos para el análisis. Los pasos más comunes son:
#   1. Convertir fechas (de texto a formato de fecha real)
#   2. Estandarizar texto (quitar espacios, unificar mayúsculas/minúsculas)
#   3. Crear columnas nuevas útiles

cat("\n========================================\n")
cat("PASO 6: LIMPIEZA DE DATOS\n")
cat("========================================\n")

# El operador %>% se llama "pipe" (tubo en español).
# Funciona así: toma el resultado de la izquierda y se lo pasa a la función
# de la derecha. Es como decir "y luego haz esto".
# Ejemplo: df %>% mutate(...) significa "toma df, y luego modifícalo con mutate"
#
# mutate() → crea columnas nuevas o modifica columnas existentes dentro del data frame

# 6.1 Convertir la columna 'fecha' de texto a formato de fecha real
# dmy() viene de lubridate y significa día/mes/año (como 15/12/2020 en Colombia)
# Esto es necesario para poder filtrar por año, calcular diferencias, etc.
df <- df %>%
  mutate(fecha = dmy(fecha))

# 6.2 Extraer el año y el mes de la fecha como columnas nuevas
# year() → extrae el año de una fecha (ej: 2023)
# month() → extrae el mes de una fecha (ej: 7 para julio)
# Estas columnas nuevas nos serán muy útiles para agrupar datos por tiempo
df <- df %>%
  mutate(
    año = year(fecha),   # Nueva columna con el año del evento
    mes = month(fecha)   # Nueva columna con el mes del evento
  )

# 6.3 Estandarizar el texto en las columnas de texto
# str_trim() → elimina espacios en blanco al inicio y al final del texto
#              Ejemplo: "  BOGOTÁ  " se convierte en "BOGOTÁ"
# str_to_upper() → convierte todo el texto a MAYÚSCULAS
#              Ejemplo: "Bogotá" y "BOGOTÁ" y "bogotá" se vuelven lo mismo: "BOGOTÁ"
# Esto evita que el mismo valor aparezca como categorías diferentes por
# diferencias de espacios o capitalización.
df <- df %>%
  mutate(
    tipo         = str_trim(str_to_upper(tipo)),
    delito       = str_trim(str_to_upper(delito)),
    tipo_arma    = str_trim(str_to_upper(tipo_arma)),
    departamento = str_trim(str_to_upper(departamento)),
    municipio    = str_trim(str_to_upper(municipio)),
    genero       = str_trim(str_to_upper(genero)),
    rango_edad   = str_trim(str_to_upper(rango_edad))
  )

cat("✅ Limpieza completada\n")
cat("   Columnas actuales:", paste(colnames(df), collapse = ", "), "\n")


# -----------------------------------------------------------------------------
# PASO 7: RESUMEN GENERAL
# -----------------------------------------------------------------------------
# Con los datos ya limpios, calculamos algunos números clave que nos dan
# una visión general del dataset.

cat("\n========================================\n")
cat("PASO 7: RESUMEN GENERAL\n")
cat("========================================\n")

# sum() → suma todos los valores de una columna numérica
total_casos <- sum(df$cantidad)

# sort(unique()) → obtiene los años únicos y los ordena de menor a mayor
años <- sort(unique(df$año))

# Para encontrar el departamento con más casos usamos una cadena de pasos:
# group_by(departamento) → agrupa los datos por departamento
# summarise(total = sum(cantidad)) → suma los casos de cada departamento
# arrange(desc(total)) → ordena de mayor a menor
# slice(1) → toma solo la primera fila (el mayor)
# pull(departamento) → extrae el valor como texto
dpto_top <- df %>%
  group_by(departamento) %>%
  summarise(total = sum(cantidad)) %>%
  arrange(desc(total)) %>%
  slice(1) %>%
  pull(departamento)

# Lo mismo para el tipo de delito más común
tipo_top <- df %>%
  group_by(tipo) %>%
  summarise(total = sum(cantidad)) %>%
  arrange(desc(total)) %>%
  slice(1) %>%
  pull(tipo)

# format(número, big.mark = ",") → formatea el número con comas para miles
# Ejemplo: 1048575 se muestra como 1,048,575
cat("\n   Total de casos registrados :", format(total_casos, big.mark = ","), "\n")
cat("   Años disponibles           :", paste(años, collapse = ", "), "\n")
cat("   Departamento con más casos :", dpto_top, "\n")
cat("   Tipo de delito más común   :", tipo_top, "\n")


# -----------------------------------------------------------------------------
# PASO 8: VISUALIZACIONES
# -----------------------------------------------------------------------------
# Usamos ggplot2 (incluido en tidyverse) para crear las gráficas.
# ggplot2 funciona por capas: primero defines los datos y los ejes,
# luego agregas el tipo de gráfica, luego los colores, títulos, etc.
# Cada capa se agrega con el símbolo +

cat("\n========================================\n")
cat("PASO 8: VISUALIZACIONES\n")
cat("========================================\n")


# --- Gráfica 1: Total de casos por tipo de delito ---
cat("\n🎨 Gráfica 1: Casos por tipo de delito...\n")

df %>%
  # Agrupamos por tipo de delito y sumamos los casos de cada uno
  group_by(tipo) %>%
  summarise(total = sum(cantidad)) %>%
  # Ordenamos de menor a mayor para que la barra más larga quede arriba
  arrange(total) %>%
  # fct_inorder() convierte 'tipo' en un factor ordenado según el orden actual
  # Esto le dice a ggplot en qué orden dibujar las barras
  mutate(tipo = fct_inorder(tipo)) %>%

  # ggplot() inicializa la gráfica
  # aes() define los "aesthetics": qué va en cada eje y qué define los colores
  ggplot(aes(x = total, y = tipo, fill = tipo)) +

  # geom_col() dibuja las barras horizontales
  # show.legend = FALSE oculta la leyenda porque los nombres ya están en el eje Y
  geom_col(show.legend = FALSE) +

  # geom_text() agrega las etiquetas con el número al final de cada barra
  # hjust = -0.1 mueve el texto un poco a la derecha del borde de la barra
  # size = 3 define el tamaño del texto
  geom_text(aes(label = comma(total)), hjust = -0.1, size = 3) +

  # scale_x_continuous() controla el eje X
  # labels = comma formatea los números con comas (1,000,000)
  # expansion() agrega espacio extra para que las etiquetas no queden cortadas
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +

  # labs() define los títulos de la gráfica y los ejes
  labs(
    title = "Total de Casos por Tipo de Delito (2020–2025)",
    x     = "Total de Casos",
    y     = "Tipo de Delito"
  ) +

  # theme_minimal() aplica un estilo limpio y moderno a la gráfica
  # base_size = 12 define el tamaño base de la letra
  theme_minimal(base_size = 12) +

  # theme() permite ajustes finos del estilo
  # element_text(face = "bold") pone el título en negrilla
  theme(plot.title = element_text(face = "bold"))

cat("   ✅ Gráfica 1 visible en el panel Plots\n")
cat("   → Para guardarla: clic en 'Export' en el panel Plots\n")


# --- Gráfica 2: Evolución anual de casos ---
cat("\n🎨 Gráfica 2: Evolución anual...\n")

df %>%
  # filter() filtra filas según una condición
  # Solo nos quedamos con los años 2020 a 2025 (años completos)
  filter(año >= 2020, año <= 2025) %>%
  group_by(año) %>%
  summarise(total = sum(cantidad)) %>%

  ggplot(aes(x = año, y = total)) +

  # geom_line() dibuja la línea que conecta los puntos
  # linewidth = 1.5 define el grosor de la línea
  geom_line(color = "#2196F3", linewidth = 1.5) +

  # geom_point() dibuja los puntos en cada año
  # size = 4 define el tamaño de los puntos
  geom_point(color = "#2196F3", size = 4) +

  # geom_text() agrega las etiquetas con el total encima de cada punto
  # vjust = -1 mueve el texto hacia arriba del punto
  geom_text(aes(label = comma(total)), vjust = -1, size = 3.5) +

  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.1, 0.15))) +

  # scale_x_continuous con breaks define qué valores mostrar en el eje X
  scale_x_continuous(breaks = 2020:2025) +

  labs(
    title = "Evolución Anual del Total de Casos (2020–2025)",
    x     = "Año",
    y     = "Total de Casos"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

cat("   ✅ Gráfica 2 visible en el panel Plots\n")


# --- Gráfica 3: Top 10 departamentos ---
cat("\n🎨 Gráfica 3: Top 10 departamentos...\n")

df %>%
  group_by(departamento) %>%
  summarise(total = sum(cantidad)) %>%
  # arrange(desc()) ordena de MAYOR a menor (desc = descendente)
  arrange(desc(total)) %>%
  # slice(1:10) toma solo las primeras 10 filas (el top 10)
  slice(1:10) %>%
  arrange(total) %>%
  mutate(departamento = fct_inorder(departamento)) %>%

  ggplot(aes(x = total, y = departamento, fill = departamento)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = comma(total)), hjust = -0.1, size = 3) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Top 10 Departamentos con más Casos Registrados",
    x     = "Total de Casos",
    y     = "Departamento"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

cat("   ✅ Gráfica 3 visible en el panel Plots\n")


# --- Gráfica 4: Distribución por género ---
cat("\n🎨 Gráfica 4: Distribución por género...\n")

df %>%
  # Excluimos los registros donde el género no fue reportado
  # El símbolo != significa "diferente de"
  filter(genero != "NO REPORTADO") %>%
  group_by(genero) %>%
  summarise(total = sum(cantidad)) %>%
  # Calculamos el porcentaje de cada género sobre el total
  # round(..., 1) redondea a 1 decimal
  mutate(porcentaje = round(total / sum(total) * 100, 1)) %>%

  ggplot(aes(x = "", y = total, fill = genero)) +

  # geom_col() con coord_polar() convierte las barras en un gráfico de torta
  geom_col(width = 1) +

  # coord_polar() transforma las coordenadas rectangulares en polares (círculo)
  # theta = "y" significa que el eje Y es el que se convierte en ángulo
  coord_polar(theta = "y") +

  # position_stack(vjust = 0.5) centra las etiquetas dentro de cada porción
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    position = position_stack(vjust = 0.5),
    size = 5, fontface = "bold", color = "white"
  ) +

  labs(
    title = "Distribución de Víctimas por Género",
    fill  = "Género"
  ) +

  # theme_void() elimina todos los ejes y el fondo, ideal para gráficas de torta
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

cat("   ✅ Gráfica 4 visible en el panel Plots\n")


# -----------------------------------------------------------------------------
# PASO 9: GUARDAR EL DATASET LIMPIO
# -----------------------------------------------------------------------------
# Siempre guardamos el dataset limpio en un archivo nuevo.
# NUNCA modificamos el archivo original — es una buena práctica preservar
# los datos crudos tal como llegaron.
#
# write_csv2() guarda el data frame como un archivo CSV con ; como separador.
# El primer argumento es el data frame a guardar.
# El segundo argumento es la ruta y nombre del archivo de destino.

cat("\n========================================\n")
cat("PASO 9: GUARDANDO DATASET LIMPIO\n")
cat("========================================\n")

write_csv2(
  df,
  "C:/Users/diego/OneDrive/Imagenes1/Escritorio/CURSOS DE DATOS/CURSO PYTHON/KAGGLE/DELITOS COLOMBIA/v_delitos_limpio.csv"
)

cat("✅ Dataset limpio guardado como: v_delitos_limpio.csv\n")
cat("   En la misma carpeta donde está tu archivo original\n")


# -----------------------------------------------------------------------------
# RESUMEN FINAL
# -----------------------------------------------------------------------------
cat("\n========================================\n")
cat("🎉 FASE 1 COMPLETADA EXITOSAMENTE\n")
cat("========================================\n")
cat("
Resumen de lo que hicimos:
  ✅ Cargamos más de 1,000,000 registros de delitos
  ✅ Revisamos tipos de datos y valores nulos
  ✅ Limpiamos y estandarizamos el texto
  ✅ Convertimos fechas al formato correcto
  ✅ Extrajimos año y mes como columnas nuevas
  ✅ Generamos 4 visualizaciones del EDA
  ✅ Guardamos el dataset limpio

Próximo paso → Fase 2: Análisis de preguntas clave
")
