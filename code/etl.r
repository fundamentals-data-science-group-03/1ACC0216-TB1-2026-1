
rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

library(ggplot2)
library(cowplot)
library(patchwork)
library(tidyverse)
library(VIM)
library(mlr)
library(naniar)
library(dplyr)
library(tidyr)








setwd("D:/Asus/Downloads/db-tb1-ds")

df <- read.csv('hotel_bookings.csv', header= TRUE, sep=',',dec='.', na.strings="")

#====ELIMINAR DATOS DUPLICADOS=====
duplicated(df)
df[duplicated(df),]

df <- unique(df)

#===CONVERSIONES====

cols_factores <- c("is_canceled", "hotel", "arrival_date_month", 
                   "meal", "country", "market_segment", "distribution_channel", 
                   "is_repeated_guest", "reserved_room_type", "assigned_room_type", 
                   "deposit_type", "agent", "company", "customer_type", 
                   "reservation_status")


df[cols_factores] <- lapply(df[cols_factores], as.factor)

df$children <- as.integer(df$children)

df$reservation_status_date <- as.Date(df$reservation_status_date)

df$arrival_date_month <- factor(
  df$arrival_date_month,
  levels = c("January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December"),
  ordered = TRUE
)


#====IDENTIFICACIÓN DE DATOS FALTANTES=====

df_original <- df


# Función para contar valores específicos dentro de una columna
contar_valores <- function(data, variable, valores) {
  x <- trimws(as.character(data[[variable]]))
  
  data.frame(
    variable = variable,
    valor_faltante_detectado = valores,
    frecuencia = sapply(valores, function(v) sum(x == v, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
}

# Valores que son faltantes o no informativos





faltantes_detectados <- bind_rows(
  contar_valores(df, "children", c("", " ", "NA", "N/A", "NULL", "Undefined")),
  contar_valores(df, "meal", c("Undefined", "", " ", "NA", "N/A", "NULL")),
  contar_valores(df, "market_segment", c("Undefined", "", " ", "NA", "N/A", "NULL")),
  contar_valores(df, "distribution_channel", c("Undefined", "", " ", "NA", "N/A", "NULL")),
  contar_valores(df, "agent", c("NULL", "", " ", "NA", "N/A", "Undefined")),
  contar_valores(df, "company", c("NULL", "", " ", "NA", "N/A", "Undefined"))
)


faltantes_detectados <- faltantes_detectados %>%
  filter(frecuencia > 0) %>%
  arrange(desc(frecuencia))

faltantes_detectados
sum(is.na(df$children))

# Convertir de valores faltantes escondidos a NA
df_limpio <- df %>%
  mutate(
    across(where(is.factor), as.character),
    across(where(is.character), trimws),
    
    across(where(is.character), ~ na_if(.x, "")),
    across(where(is.character), ~ na_if(.x, " ")),
    across(where(is.character), ~ na_if(.x, "NA")),
    across(where(is.character), ~ na_if(.x, "N/A")),
    
    meal = na_if(meal, "Undefined"),
    market_segment = na_if(market_segment, "Undefined"),
    distribution_channel = na_if(distribution_channel, "Undefined"),
    agent = na_if(agent, "NULL"),
    company = na_if(company, "NULL")
  )

# Restaurar tipos de datos correctos
df_limpio$children <- suppressWarnings(as.integer(df_limpio$children))

cols_factores <- c(
  "is_canceled", "hotel", "meal", "country", "market_segment",
  "distribution_channel", "is_repeated_guest", "reserved_room_type",
  "assigned_room_type", "deposit_type", "agent", "company",
  "customer_type", "reservation_status"
)

df_limpio[cols_factores] <- lapply(df_limpio[cols_factores], as.factor)

df_limpio$arrival_date_month <- factor(
  df_limpio$arrival_date_month,
  levels = c("January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December"),
  ordered = TRUE
)

df_limpio$reservation_status_date <- as.Date(df_limpio$reservation_status_date)




# Frecuencia de datos faltantes
frecuencia_na <- df_limpio %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_faltantes"
  ) %>%
  mutate(
    porcentaje_faltantes = round(n_faltantes / nrow(df_limpio) * 100, 2)
  ) %>%
  filter(n_faltantes > 0) %>%
  arrange(desc(n_faltantes))


frecuencia_na

# Gráfico de barras de datos faltantes
ggplot(frecuencia_na, aes(
  x = reorder(variable, porcentaje_faltantes),
  y = porcentaje_faltantes
)) +
  geom_col() +
  coord_flip() +
  geom_text(
    aes(label = paste0(porcentaje_faltantes, "%")),
    hjust = -0.1,
    size = 3.5
  ) +
  labs(
    title = "Porcentaje de datos faltantes por variable",
    subtitle = "Valores vacíos, NULL y Undefined fueron tratados como NA",
    x = "Variable",
    y = "Porcentaje de valores faltantes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  ) +
  ylim(0, max(frecuencia_na$porcentaje_faltantes) * 1.15)


# Gráfico de patrón de datos faltantes
vars_con_na <- frecuencia_na$variable
if (.Platform$OS.type == "windows") {
  windows(width = 14, height = 6)
}

aggr(
  df_limpio[, vars_con_na],
  col      = c("steelblue", "tomato"),
  numbers  = TRUE,
  sortVars = TRUE,
  labels   = vars_con_na,
  cex.axis = 0.7,
  gap      = 2,
  ylab     = c("Proporción de valores faltantes", "Patrón de faltantes")
)


# Mapa visual de datos faltantes
vis_miss(df_limpio[, vars_con_na]) +
  labs(
    title = "Mapa de datos faltantes",
    subtitle = "Variables con al menos un valor faltante"
  ) +
  theme_minimal()


#Patrón de combinación de faltantes
gg_miss_upset(df_limpio[, vars_con_na])



#===========Eliminar registros sin sentido===============


# Variables que, por definición, no deberían ser negativas
cols_no_negativas <- c(
  "lead_time",
  "stays_in_weekend_nights",
  "stays_in_week_nights",
  "adults",
  "children",
  "babies",
  "previous_cancellations",
  "previous_bookings_not_canceled",
  "booking_changes",
  "days_in_waiting_list",
  "required_car_parking_spaces",
  "total_of_special_requests"
)

# Auditoría de registros sin sentido lógico
df_auditoria <- df_limpio %>%
  mutate(
    id_registro = row_number(),
    
    # Reconstrucción de fecha de llegada
    arrival_month_num = match(as.character(arrival_date_month), month.name),
    arrival_date = suppressWarnings(
      make_date(arrival_date_year, arrival_month_num, arrival_date_day_of_month)
    ),
    
    # Fecha aproximada de creación de la reserva
    # Según documentación: lead_time = arrival_date - fecha de ingreso al PMS
    booking_date_estimada = arrival_date - lead_time,
    
    # Variables auxiliares
    total_noches = stays_in_weekend_nights + stays_in_week_nights,
    total_huespedes = adults + children + babies,
    
    is_canceled_chr = as.character(is_canceled),
    reservation_status_chr = as.character(reservation_status),
    
    # Reserva sin ningún huésped
    flag_sin_huespedes = 
      !is.na(adults) & !is.na(children) & !is.na(babies) &
      adults == 0 & children == 0 & babies == 0,
    
    # Valores negativos en variables de conteo
    flag_valores_negativos = if_any(
      all_of(cols_no_negativas),
      ~ !is.na(.x) & .x < 0
    ),
    
    # ADR negativo
    flag_adr_negativo = !is.na(adr) & adr < 0,
    
    # Semana inválida
    flag_semana_invalida =
      !is.na(arrival_date_week_number) &
      !between(arrival_date_week_number, 1, 53),
    
    # Día del mes inválido
    flag_dia_invalido =
      !is.na(arrival_date_day_of_month) &
      !between(arrival_date_day_of_month, 1, 31),
    
    # Fecha de llegada inválida
    # Solo se marca como inválida si las partes existen pero forman una fecha imposible.
    flag_fecha_llegada_invalida =
      !is.na(arrival_date_year) &
      !is.na(arrival_month_num) &
      !is.na(arrival_date_day_of_month) &
      is.na(arrival_date),
    
    # Fecha de llegada fuera del rango oficial del dataset
    flag_fecha_llegada_fuera_rango =
      !is.na(arrival_date) &
      (
        arrival_date < as.Date("2015-07-01") |
          arrival_date > as.Date("2017-08-31")
      ),
    
    # Inconsistencia lógica entre is_canceled y reservation_status
    flag_estado_cancelacion_inconsistente = case_when(
      is_canceled_chr == "0" & reservation_status_chr != "Check-Out" ~ TRUE,
      is_canceled_chr == "1" & !(reservation_status_chr %in% c("Canceled", "No-Show")) ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # Estado final registrado antes de que la reserva exista
    flag_status_antes_de_creacion =
      !is.na(reservation_status_date) &
      !is.na(booking_date_estimada) &
      reservation_status_date < booking_date_estimada,
    
    # Check-Out antes de la llegada
    flag_checkout_antes_llegada =
      reservation_status_chr == "Check-Out" &
      !is.na(reservation_status_date) &
      !is.na(arrival_date) &
      reservation_status_date < arrival_date
  )


#Resumen de registros eliminables
resumen_eliminables <- df_auditoria %>%
  summarise(across(starts_with("flag_"), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "regla",
    values_to = "n_registros"
  ) %>%
  mutate(
    porcentaje = round(100 * n_registros / nrow(df_auditoria), 4)
  ) %>%
  arrange(desc(n_registros))

resumen_eliminables

#Gráfico de barras de registros eliminables
ggplot(
  resumen_eliminables %>% filter(n_registros > 0),
  aes(x = reorder(regla, n_registros), y = n_registros)
) +
  geom_col() +
  coord_flip() +
  geom_text(
    aes(label = n_registros),
    hjust = -0.1,
    size = 3.5
  ) +
  labs(
    title = "Registros sin sentido lógico detectados",
    subtitle = "Reglas de eliminación basadas en el significado real de las columnas",
    x = "Regla de validación",
    y = "Número de registros"
  ) +
  theme_minimal() +
  ylim(0, max(resumen_eliminables$n_registros, na.rm = TRUE) * 1.15)


#Ver cuáles son los registros que serían eliminados
flags_eliminar <- c(
  "flag_sin_huespedes",
  "flag_valores_negativos",
  "flag_adr_negativo",
  "flag_semana_invalida",
  "flag_dia_invalido",
  "flag_fecha_llegada_invalida",
  "flag_fecha_llegada_fuera_rango",
  "flag_estado_cancelacion_inconsistente",
  "flag_status_antes_de_creacion",
  "flag_checkout_antes_llegada"
)

registros_eliminables <- df_auditoria %>%
  filter(if_any(all_of(flags_eliminar), ~ .x))

nrow(registros_eliminables)



df_depurado <- df_auditoria %>%
  filter(!if_any(all_of(flags_eliminar), ~ .x)) %>%
  select(all_of(names(df_limpio)))

resumen_depuracion <- tibble(
  registros_iniciales = nrow(df_limpio),
  registros_eliminados = nrow(df_limpio) - nrow(df_depurado),
  registros_finales = nrow(df_depurado),
  porcentaje_eliminado = round(
    100 * (nrow(df_limpio) - nrow(df_depurado)) / nrow(df_limpio),
    4
  )
)

resumen_depuracion


write.csv(df_depurado, 'hotel_bookings_depurado.csv', row.names = FALSE)


#======TRATAMIENTO DE DATOS FALTANTES========

df_tratado <- df_depurado


# Tratamiento de company, agent y meal
df_tratado <- df_tratado %>%
  mutate(
    company = as.character(company),
    agent = as.character(agent),
    meal = as.character(meal),

    company = if_else(is.na(company), "0", company),
    agent = if_else(is.na(agent), "0", agent),
    
    meal = if_else(is.na(meal), "SC", meal),
    
    has_company = factor(
      if_else(company == "0", "No", "Yes"),
      levels = c("No", "Yes")
    ),
    
    has_agent = factor(
      if_else(agent == "0", "No", "Yes"),
      levels = c("No", "Yes")
    ),
    
    company = factor(company),
    agent = factor(agent),
    meal = factor(meal)
  )


#Tratamiento de distribution_channel

moda <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  names(sort(table(x), decreasing = TRUE))[1]
}

#Moda global
moda_global_distribution <- moda(df_tratado$distribution_channel)

moda_global_distribution

#Gráfico moda global
grafico_distribution <- df_tratado %>%
  filter(!is.na(distribution_channel)) %>%
  count(distribution_channel, sort = TRUE) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 2))

ggplot(grafico_distribution, aes(x = reorder(distribution_channel, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(
    aes(label = paste0(porcentaje, "%")),
    hjust = -0.1,
    size = 3.5
  ) +
  labs(
    title = "Distribución general de distribution_channel",
    subtitle = "Frecuencia de cada canal de distribución observado",
    x = "distribution_channel",
    y = "Cantidad de reservas"
  ) +
  theme_minimal() +
  ylim(0, max(grafico_distribution$n) * 1.15)



#Moda por market_segment
moda_por_market <- df_tratado %>%
  filter(!is.na(distribution_channel)) %>%
  group_by(market_segment) %>%
  summarise(
    moda_market = moda(distribution_channel),
    n_grupo = n(),
    .groups = "drop"
  )

moda_por_market

#moda por market_segment + customer_type
moda_por_market_customer <- df_tratado %>%
  filter(!is.na(distribution_channel)) %>%
  group_by(market_segment, customer_type) %>%
  summarise(
    moda_market_customer = moda(distribution_channel),
    n_grupo = n(),
    .groups = "drop"
  )

moda_por_market_customer
  

#moda por hotel + market_segment + customer_type
moda_por_hotel_market_customer <- df_tratado %>%
  filter(!is.na(distribution_channel)) %>%
  group_by(hotel, market_segment, customer_type) %>%
  summarise(
    moda_hotel_market_customer = moda(distribution_channel),
    n_grupo = n(),
    .groups = "drop"
  )

moda_por_hotel_market_customer


#Comparacion de posible imputacion por cada método
casos_distribution_na <- df_tratado %>%
  mutate(row_id = row_number()) %>%
  filter(is.na(distribution_channel)) %>%
  select(
    row_id,
    hotel,
    market_segment,
    customer_type,
    distribution_channel
  )

comparacion_distribution <- casos_distribution_na %>%
  left_join(
    moda_por_market %>%
      select(market_segment, moda_market),
    by = "market_segment"
  ) %>%
  left_join(
    moda_por_market_customer %>%
      select(market_segment, customer_type, moda_market_customer),
    by = c("market_segment", "customer_type")
  ) %>%
  left_join(
    moda_por_hotel_market_customer %>%
      select(hotel, market_segment, customer_type, moda_hotel_market_customer),
    by = c("hotel", "market_segment", "customer_type")
  ) %>%
  mutate(
    moda_global = moda_global_distribution
  )

comparacion_distribution


#Gráfico relación entre market_segment y distribution_channel
df_tratado %>%
  filter(!is.na(distribution_channel), !is.na(market_segment)) %>%
  count(market_segment, distribution_channel) %>%
  group_by(market_segment) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  ggplot(aes(x = distribution_channel, y = market_segment, fill = porcentaje)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(porcentaje, "%")), size = 3) +
  labs(
    title = "Relación entre market_segment y distribution_channel",
    subtitle = "Porcentaje calculado dentro de cada market_segment",
    x = "distribution_channel",
    y = "market_segment",
    fill = "%"
  ) +
  theme_minimal()

#Gráfico para comparar posibles métodos de imputación a usar
comparacion_distribution %>%
  pivot_longer(
    cols = c(moda_global, moda_market, moda_market_customer, moda_hotel_market_customer),
    names_to = "metodo",
    values_to = "valor_imputado"
  ) %>%
  ggplot(aes(x = metodo, y = factor(row_id), fill = valor_imputado)) +
  geom_tile(color = "white") +
  geom_text(aes(label = valor_imputado), size = 3.5) +
  labs(
    title = "Comparación de métodos de imputación para distribution_channel",
    subtitle = "Valor sugerido para cada registro faltante",
    x = "Método de imputación",
    y = "Registro",
    fill = "Valor imputado"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

#Imputación
df_tratado <- df_tratado %>%
  left_join(
    moda_por_hotel_market_customer %>%
      select(hotel, market_segment, customer_type, imp_1 = moda_hotel_market_customer),
    by = c("hotel", "market_segment", "customer_type")
  ) %>%
  left_join(
    moda_por_market_customer %>%
      select(market_segment, customer_type, imp_2 = moda_market_customer),
    by = c("market_segment", "customer_type")
  ) %>%
  left_join(
    moda_por_market %>%
      select(market_segment, imp_3 = moda_market),
    by = "market_segment"
  ) %>%
  mutate(
    distribution_channel = as.character(distribution_channel),
    distribution_channel = if_else(
      is.na(distribution_channel),
      coalesce(imp_1, imp_2, imp_3, moda_global_distribution),
      distribution_channel
    ),
    distribution_channel = factor(distribution_channel)
  ) %>%
  select(-imp_1, -imp_2, -imp_3)

sum(is.na(df_tratado$distribution_channel))


#Tratamiento de la variable children

moda <- function(x) {
  x <- x[!is.na(x)]
  names(sort(table(x), decreasing = TRUE))[1]
}

resumen_children <- df_tratado %>%
  summarise(
    media_children = mean(children, na.rm = TRUE),
    mediana_children = median(children, na.rm = TRUE),
    moda_children = as.integer(moda(children))
  )

resumen_children


df_tratado %>%
  filter(!is.na(children)) %>%
  count(children) %>%
  ggplot(aes(x = factor(children), y = n)) +
  geom_col() +
  geom_text(
    aes(label = n),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    title = "Distribución de la variable children",
    subtitle = "Frecuencia de reservas según número de niños",
    x = "children",
    y = "Cantidad de reservas"
  ) +
  theme_minimal()


moda_children <- as.integer(moda(df_tratado$children))

moda_children

df_tratado <- df_tratado %>%
  mutate(
    children = if_else(
      is.na(children),
      moda_children,
      children
    ),
    children = as.integer(children)
  )

sum(is.na(df_tratado$children))


#Imputación de variable market_segment

moda <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x)]
  names(sort(table(x), decreasing = TRUE))[1]
}

# Moda inteligente por grupo
moda_market_grupo <- df_tratado %>%
  filter(!is.na(market_segment)) %>%
  group_by(hotel, distribution_channel, customer_type) %>%
  summarise(
    imp_market = moda(market_segment),
    .groups = "drop"
  )

# Moda global de respaldo
moda_global_market <- moda(df_tratado$market_segment)

# Imputación de market_segment
df_tratado <- df_tratado %>%
  left_join(
    moda_market_grupo,
    by = c("hotel", "distribution_channel", "customer_type")
  ) %>%
  mutate(
    market_segment = as.character(market_segment),
    market_segment = if_else(
      is.na(market_segment),
      coalesce(imp_market, moda_global_market),
      market_segment
    ),
    market_segment = factor(market_segment)
  ) %>%
  select(-imp_market)


write.csv(df_tratado, 'hotel_bookings_tratado.csv', row.names = FALSE)


#=========DETECCIÓN DE OUTLIERS======================

#Variables numéricas candidatas a outliers
vars_outliers <- c(
  "lead_time",
  "stays_in_weekend_nights",
  "stays_in_week_nights",
  "adults",
  "children",
  "babies",
  "previous_cancellations",
  "previous_bookings_not_canceled",
  "booking_changes",
  "days_in_waiting_list",
  "adr",
  "required_car_parking_spaces",
  "total_of_special_requests"
)

#Resumen estadístico
resumen_numericas <- df_tratado %>%
  select(all_of(vars_outliers)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  summarise(
    n = n(),
    minimo = min(valor, na.rm = TRUE),
    q1 = quantile(valor, 0.25, na.rm = TRUE),
    mediana = median(valor, na.rm = TRUE),
    media = mean(valor, na.rm = TRUE),
    q3 = quantile(valor, 0.75, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    iqr = IQR(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(maximo))

resumen_numericas


#Detección con RIQ
outliers_iqr <- df_tratado %>%
  select(all_of(vars_outliers)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  mutate(
    q1 = quantile(valor, 0.25, na.rm = TRUE),
    q3 = quantile(valor, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    limite_inferior = q1 - 1.5 * iqr,
    limite_superior = q3 + 1.5 * iqr,
    es_outlier_iqr = valor < limite_inferior | valor > limite_superior
  ) %>%
  summarise(
    limite_inferior = first(limite_inferior),
    limite_superior = first(limite_superior),
    n_outliers_iqr = sum(es_outlier_iqr, na.rm = TRUE),
    porcentaje_outliers_iqr = round(100 * mean(es_outlier_iqr, na.rm = TRUE), 2),
    valor_min_outlier = ifelse(any(es_outlier_iqr, na.rm = TRUE),
                               min(valor[es_outlier_iqr], na.rm = TRUE), NA),
    valor_max_outlier = ifelse(any(es_outlier_iqr, na.rm = TRUE),
                               max(valor[es_outlier_iqr], na.rm = TRUE), NA),
    .groups = "drop"
  ) %>%
  arrange(desc(n_outliers_iqr))

outliers_iqr

#Detección con z score
outliers_zscore <- df_tratado %>%
  select(all_of(vars_outliers)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  mutate(
    media = mean(valor, na.rm = TRUE),
    desviacion = sd(valor, na.rm = TRUE),
    z_score = ifelse(desviacion == 0, 0, (valor - media) / desviacion),
    es_outlier_z = abs(z_score) > 3
  ) %>%
  summarise(
    n_outliers_z = sum(es_outlier_z, na.rm = TRUE),
    porcentaje_outliers_z = round(100 * mean(es_outlier_z, na.rm = TRUE), 2),
    valor_min_outlier = ifelse(any(es_outlier_z, na.rm = TRUE),
                               min(valor[es_outlier_z], na.rm = TRUE), NA),
    valor_max_outlier = ifelse(any(es_outlier_z, na.rm = TRUE),
                               max(valor[es_outlier_z], na.rm = TRUE), NA),
    .groups = "drop"
  ) %>%
  arrange(desc(n_outliers_z))

outliers_zscore


#Deteccion con z score pero con mediana y mad
outliers_zmod <- df_tratado %>%
  select(all_of(vars_outliers)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  group_by(variable) %>%
  mutate(
    mediana = median(valor, na.rm = TRUE),
    mad_valor = mad(valor, constant = 1, na.rm = TRUE),
    z_modificado = ifelse(
      mad_valor == 0,
      0,
      0.6745 * (valor - mediana) / mad_valor
    ),
    es_outlier_zmod = abs(z_modificado) > 3.5
  ) %>%
  summarise(
    n_outliers_zmod = sum(es_outlier_zmod, na.rm = TRUE),
    porcentaje_outliers_zmod = round(100 * mean(es_outlier_zmod, na.rm = TRUE), 2),
    valor_min_outlier = ifelse(any(es_outlier_zmod, na.rm = TRUE),
                               min(valor[es_outlier_zmod], na.rm = TRUE), NA),
    valor_max_outlier = ifelse(any(es_outlier_zmod, na.rm = TRUE),
                               max(valor[es_outlier_zmod], na.rm = TRUE), NA),
    .groups = "drop"
  ) %>%
  arrange(desc(n_outliers_zmod))

outliers_zmod

#Comparar métodos
comparacion_outliers <- outliers_iqr %>%
  select(variable, n_outliers_iqr, porcentaje_outliers_iqr) %>%
  left_join(
    outliers_zscore %>%
      select(variable, n_outliers_z, porcentaje_outliers_z),
    by = "variable"
  ) %>%
  left_join(
    outliers_zmod %>%
      select(variable, n_outliers_zmod, porcentaje_outliers_zmod),
    by = "variable"
  ) %>%
  arrange(desc(n_outliers_iqr))

comparacion_outliers

#Graficos
df_tratado %>%
  select(all_of(vars_outliers)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Detección visual de outliers mediante boxplots",
    subtitle = "Variables numéricas del dataset Hotel Booking Demand",
    x = "Variable",
    y = "Valor"
  ) +
  theme_minimal()


#Grafico mas bonito, en facetas
df_tratado %>%
  select(all_of(vars_outliers)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = "", y = valor)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.5) +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  labs(
    title = "Boxplots individuales para detección de outliers",
    subtitle = "Cada variable se muestra con escala independiente",
    x = "",
    y = "Valor"
  ) +
  theme_minimal()


#Histograma
df_tratado %>%
  select(all_of(vars_outliers)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 40, color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  labs(
    title = "Histogramas de variables numéricas",
    subtitle = "Distribución de variables candidatas para detección de outliers",
    x = "Valor",
    y = "Frecuencia"
  ) +
  theme_minimal()


#Análisis adr
variable <- df_tratado$adr

Q1 <- quantile(variable, 0.25, na.rm = TRUE)
Q3 <- quantile(variable, 0.75, na.rm = TRUE)
RIC <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * RIC
limite_superior <- Q3 + 1.5 * RIC

Q1
Q3
RIC
limite_inferior
limite_superior

outliers_adr <- df_tratado %>%
  filter(adr < limite_inferior | adr > limite_superior)

nrow(outliers_adr)

summary(outliers_adr$adr)

#Gráfico adr
ggplot(df_tratado, aes(x = adr)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40, color = "white") +
  geom_density(linewidth = 1) +
  geom_vline(xintercept = limite_superior, linetype = "dashed") +
  labs(
    title = "Histograma de adr con límite superior IQR",
    subtitle = "Detección de valores atípicos en la tarifa diaria promedio",
    x = "adr",
    y = "Densidad"
  ) +
  theme_minimal()

ggplot(df_tratado, aes(y = adr)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.5) +
  labs(
    title = "Boxplot de adr",
    subtitle = "Valores atípicos detectados mediante regla IQR",
    x = "",
    y = "adr"
  ) +
  theme_minimal()



#=========TRATAMIENTO DE OUTLIERS=========
df_outliers_tratado <- df_tratado

vars_winsor <- c(
  "adr",
  "lead_time",
  "stays_in_week_nights",
  "stays_in_weekend_nights"
)

#Límites de winsorización
limites_winsor <- df_outliers_tratado %>%
  summarise(across(
    all_of(vars_winsor),
    list(
      p01 = ~ quantile(.x, 0.01, na.rm = TRUE),
      p99 = ~ quantile(.x, 0.99, na.rm = TRUE),
      max = ~ max(.x, na.rm = TRUE)
    )
  ))

limites_winsor


#Función winsorización
winsorizar <- function(x) {
  limite_inferior <- quantile(x, 0.01, na.rm = TRUE)
  limite_superior <- quantile(x, 0.99, na.rm = TRUE)
  
  ifelse(
    x < limite_inferior, limite_inferior,
    ifelse(x > limite_superior, limite_superior, x)
  )
}

#Aplicar winsoriización

for (v in vars_winsor) {
  df_outliers_tratado[[paste0(v, "_win")]] <- winsorizar(df_outliers_tratado[[v]])
}

#Comparación
comparacion_winsor <- data.frame(
  variable = vars_winsor,
  max_original = sapply(vars_winsor, function(v) max(df_outliers_tratado[[v]], na.rm = TRUE)),
  max_winsorizado = sapply(vars_winsor, function(v) max(df_outliers_tratado[[paste0(v, "_win")]], na.rm = TRUE)),
  media_original = sapply(vars_winsor, function(v) mean(df_outliers_tratado[[v]], na.rm = TRUE)),
  media_winsorizada = sapply(vars_winsor, function(v) mean(df_outliers_tratado[[paste0(v, "_win")]], na.rm = TRUE)),
  mediana_original = sapply(vars_winsor, function(v) median(df_outliers_tratado[[v]], na.rm = TRUE)),
  mediana_winsorizada = sapply(vars_winsor, function(v) median(df_outliers_tratado[[paste0(v, "_win")]], na.rm = TRUE))
)

comparacion_winsor


#Gráfico antes vs después
datos_originales <- df_outliers_tratado %>%
  select(all_of(vars_winsor)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(version = "Original")

datos_winsor <- df_outliers_tratado %>%
  select(all_of(paste0(vars_winsor, "_win"))) %>%
  rename(
    adr = adr_win,
    lead_time = lead_time_win,
    stays_in_week_nights = stays_in_week_nights_win,
    stays_in_weekend_nights = stays_in_weekend_nights_win
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(version = "Winsorizado")

datos_comparacion <- bind_rows(datos_originales, datos_winsor)

ggplot(datos_comparacion, aes(x = version, y = valor)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(
    title = "Comparación antes y después de la winsorización",
    subtitle = "Tratamiento aplicado al percentil 1% y 99%",
    x = "Versión de la variable",
    y = "Valor"
  ) +
  theme_minimal()




#Ver columnas nuevass
names(df_outliers_tratado)


write.csv(df_outliers_tratado, "hotel_bookings_outliers_tratado.csv", row.names = FALSE)


vars_winsor <- c(
  "adr",
  "lead_time",
  "stays_in_week_nights",
  "stays_in_weekend_nights"
)

#=======================================================


datos_originales <- df_outliers_tratado %>%
  select(all_of(vars_winsor)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(version = "Original")

datos_winsor <- df_outliers_tratado %>%
  select(all_of(paste0(vars_winsor, "_win"))) %>%
  rename(
    adr = adr_win,
    lead_time = lead_time_win,
    stays_in_week_nights = stays_in_week_nights_win,
    stays_in_weekend_nights = stays_in_weekend_nights_win
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valor"
  ) %>%
  mutate(version = "Winsorizado")

datos_comparacion <- bind_rows(datos_originales, datos_winsor)



#Boxplot antes y después
ggplot(datos_comparacion, aes(x = version, y = valor)) +
  geom_boxplot(outlier.colour = "red", outlier.alpha = 0.4) +
  facet_wrap(~ variable, scales = "free", ncol = 2) +
  labs(
    title = "Comparación antes y después del tratamiento de outliers",
    subtitle = "Winsorización aplicada al percentil 1% y 99%",
    x = "Versión de la variable",
    y = "Valor"
  ) +
  theme_minimal()


#HIstrograma antes y después
ggplot(datos_comparacion, aes(x = valor)) +
  geom_histogram(bins = 40, color = "white") +
  facet_grid(variable ~ version, scales = "free") +
  labs(
    title = "Distribución antes y después de la winsorización",
    subtitle = "Comparación visual de la concentración y reducción de valores extremos",
    x = "Valor",
    y = "Frecuencia"
  ) +
  theme_minimal()



#Comparación de máximos antes y después

comparacion_maximos <- data.frame(
  variable = vars_winsor,
  Original = sapply(vars_winsor, function(v) max(df_outliers_tratado[[v]], na.rm = TRUE)),
  Winsorizado = sapply(vars_winsor, function(v) max(df_outliers_tratado[[paste0(v, "_win")]], na.rm = TRUE))
) %>%
  pivot_longer(
    cols = c(Original, Winsorizado),
    names_to = "version",
    values_to = "maximo"
  )

ggplot(comparacion_maximos, aes(x = variable, y = maximo, fill = version)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(maximo, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    title = "Reducción de valores máximos después de la winsorización",
    subtitle = "Los valores extremos se limitaron sin eliminar registros",
    x = "Variable",
    y = "Valor máximo",
    fill = "Versión"
  ) +
  theme_minimal()




#Comparación de medias antes y después
comparacion_medias <- data.frame(
  variable = vars_winsor,
  Original = sapply(vars_winsor, function(v) mean(df_outliers_tratado[[v]], na.rm = TRUE)),
  Winsorizado = sapply(vars_winsor, function(v) mean(df_outliers_tratado[[paste0(v, "_win")]], na.rm = TRUE))
) %>%
  pivot_longer(
    cols = c(Original, Winsorizado),
    names_to = "version",
    values_to = "media"
  )

ggplot(comparacion_medias, aes(x = variable, y = media, fill = version)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(media, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    title = "Comparación de medias antes y después de la winsorización",
    subtitle = "La media se estabiliza sin modificar la estructura central de los datos",
    x = "Variable",
    y = "Media",
    fill = "Versión"
  ) +
  theme_minimal()


#Comparación de medianas antes y después
comparacion_medianas <- data.frame(
  variable = vars_winsor,
  Original = sapply(vars_winsor, function(v) median(df_outliers_tratado[[v]], na.rm = TRUE)),
  Winsorizado = sapply(vars_winsor, function(v) median(df_outliers_tratado[[paste0(v, "_win")]], na.rm = TRUE))
) %>%
  pivot_longer(
    cols = c(Original, Winsorizado),
    names_to = "version",
    values_to = "mediana"
  )

ggplot(comparacion_medianas, aes(x = variable, y = mediana, fill = version)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = round(mediana, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    title = "Comparación de medianas antes y después de la winsorización",
    subtitle = "La mediana se mantiene estable, indicando que el centro de la distribución no se alteró",
    x = "Variable",
    y = "Mediana",
    fill = "Versión"
  ) +
  theme_minimal()


#Valores editados
valores_modificados <- data.frame(
  variable = vars_winsor,
  n_modificados = sapply(vars_winsor, function(v) {
    sum(df_outliers_tratado[[v]] != df_outliers_tratado[[paste0(v, "_win")]], na.rm = TRUE)
  })
) %>%
  mutate(
    porcentaje = round(n_modificados / nrow(df_outliers_tratado) * 100, 2)
  )

ggplot(valores_modificados, aes(x = reorder(variable, n_modificados), y = n_modificados)) +
  geom_col() +
  coord_flip() +
  geom_text(
    aes(label = paste0(n_modificados, " (", porcentaje, "%)")),
    hjust = -0.1,
    size = 3.5
  ) +
  labs(
    title = "Cantidad de valores modificados por winsorización",
    subtitle = "Solo se ajustaron los valores ubicados en los extremos de la distribución",
    x = "Variable",
    y = "Número de valores modificados"
  ) +
  theme_minimal() +
  ylim(0, max(valores_modificados$n_modificados) * 1.15)




#Grafico resumen
comparacion_resumen <- comparacion_winsor %>%
  select(
    variable,
    max_original,
    max_winsorizado,
    media_original,
    media_winsorizada,
    mediana_original,
    mediana_winsorizada
  ) %>%
  pivot_longer(
    cols = -variable,
    names_to = "metrica",
    values_to = "valor"
  ) %>%
  separate(
    metrica,
    into = c("metrica", "version"),
    sep = "_(?=[^_]+$)"
  ) %>%
  mutate(
    version = ifelse(version == "original", "Original", "Winsorizado"),
    metrica = recode(
      metrica,
      "max" = "Máximo",
      "media" = "Media",
      "mediana" = "Mediana"
    )
  )

ggplot(comparacion_resumen, aes(x = version, y = valor, fill = version)) +
  geom_col() +
  geom_text(
    aes(label = round(valor, 2)),
    vjust = -0.3,
    size = 3
  ) +
  facet_grid(metrica ~ variable, scales = "free_y") +
  labs(
    title = "Resumen del efecto de la winsorización",
    subtitle = "Comparación de máximos, medias y medianas antes y después del tratamiento",
    x = "Versión",
    y = "Valor",
    fill = "Versión"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )







