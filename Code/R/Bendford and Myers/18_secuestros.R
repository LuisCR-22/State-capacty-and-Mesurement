# ---- Paquetes ----
# install.packages(c("data.table","janitor","lubridate","haven","stringr")) # si hiciera falta
library(data.table)
library(janitor)
library(lubridate)
library(haven)
library(stringr)

# ---- Rutas ----
in_dir  <- "C:/Users/wb593225/OneDrive - WBG/Desktop/Luis - Private/Mesurement project/03_Data/DANE"
in_file <- file.path(in_dir, "SECUESTRO_20250909.csv")
out_file <- file.path(in_dir, "03_secuestros.dta")

# ---- Lectura (autodetección de delimitador/encoding) ----
dt <- fread(
  in_file,
  encoding   = "Latin-1",
  na.strings = c("", "NA", "N/A", "NULL")
)

# ---- Nombres en minúscula (snake_case) ----
dt <- clean_names(dt)  # e.g., "FECHA HECHO" -> "fecha_hecho"

# ---- Validación de columnas esperadas (opcional, pero útil) ----
cols_needed <- c("fecha_hecho","cod_depto","departamento","cod_muni","municipio","tipo_delito","cantidad")
missing_cols <- setdiff(cols_needed, names(dt))
if (length(missing_cols) > 0) {
  stop(sprintf("Faltan columnas en el CSV: %s", paste(missing_cols, collapse=", ")))
}

# ---- Filtrar por delitos que contengan 'secuestro' (por si hay otros tipos) ----
dt <- dt[grepl("secuestro", tipo_delito, ignore.case = TRUE)]

# ---- Parseo de fecha y extracción de año ----
# Formato principal: "d/m/Y"; fallback a ISO si fuera el caso
dt[, fecha := as.IDate(fecha_hecho, format = "%d/%m/%Y")]
dt[is.na(fecha), fecha := as.IDate(fecha_hecho, format = "%Y-%m-%d")]
dt[, anio := year(fecha)]

# ---- Tipos y limpieza ligeras ----
# Conservar el código de dpto con ceros a la izquierda (p.ej. "05")
dt[, cod_depto := str_pad(as.character(cod_depto), width = 2, side = "left", pad = "0")]
# Asegurar cantidad numérica
dt[, cantidad := as.numeric(cantidad)]

# ---- Agregación por departamento–año ----
agg <- dt[!is.na(anio) & !is.na(cod_depto) & !is.na(departamento),
          .(secuestros = sum(cantidad, na.rm = TRUE)),
          by = .(cod_depto, departamento, anio)]

setorder(agg, cod_depto, anio)

# ---- Guardar a Stata (.dta) ----
# Columnas ya están en minúsculas gracias a clean_names; las nuevas también lo están.
write_dta(as.data.frame(agg), out_file, version = 14)

# Mensaje opcional
cat("Archivo guardado en:", out_file, "\n",
    "Columnas:", paste(names(agg), collapse = ", "), "\n")
