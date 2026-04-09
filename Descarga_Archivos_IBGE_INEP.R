################################################################################

# Flujo de trabajo optimizado en R para la descarga de los archivos de referencia

################################################################################

# 1. Instalación de paquetes necesarios

################################################################################

rm(list = ls(all = TRUE))
options(scipen = 999)
setwd(".../Dataton")
if ("curl" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "curl", dependencies = TRUE)
}
library(package = "curl", verbose = TRUE)
if ("data.table" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "data.table", dependencies = TRUE)
}
library(package = "data.table", verbose = TRUE)
if ("dplyr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "dplyr", dependencies = TRUE)
}
library(package = "dplyr", verbose = TRUE)
if ("furrr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "furrr", dependencies = TRUE)
}
library(package = "furrr", verbose = TRUE)
if ("future" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "future", dependencies = TRUE)
}
library(package = "future", verbose = TRUE)
if ("httr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "httr", dependencies = TRUE)
}
library(package = "httr", verbose = TRUE)
if ("progressr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "progressr", dependencies = TRUE)
}
library(package = "progressr", verbose = TRUE)
if ("purrr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "purrr", dependencies = TRUE)
}
library(package = "purrr", verbose = TRUE)
if ("qgisprocess" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "qgisprocess", dependencies = TRUE)
}
library(package = "qgisprocess", verbose = TRUE)
if ("readr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "readr", dependencies = TRUE)
}
library(package = "readr", verbose = TRUE)
if ("rvest" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "rvest", dependencies = TRUE)
}
library(package = "rvest", verbose = TRUE)
if ("sf" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "sf", dependencies = TRUE)
}
library(package = "sf", verbose = TRUE)
if ("stringr" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "stringr", dependencies = TRUE)
}
library(package = "stringr", verbose = TRUE)
if ("xlsx" %in% rownames(installed.packages()) == FALSE)
{
  install.packages(pkgs = "xlsx", dependencies = TRUE)
}
library(package = "xlsx", verbose = TRUE)

################################################################################

# 2. Generar URLs de los archivos

################################################################################

generate_urls <- function(base_url, source, year = NULL) {
  if(source == "IBGE")
  {
    page <- read_html(base_url)
    
    files <- page |>
      html_elements("a") |>
      html_attr("href")
    
    zip_files <- files |>
      na.omit() |>
      str_subset("\\.zip$")
    
    return(unique(paste0(base_url, zip_files)))
  }
  
  else if(source == "INEP")
  {
    page <- read_html(base_url)
    
    links <- page %>%
      html_elements("a") %>%
      html_attr("href") %>%
      na.omit()
    
    links_anos <- links %>%
      str_subset("microdados_censo_escolar") %>%
      unique()
    
    links_anos <- ifelse(str_detect(links_anos, "^http"), links_anos, paste0("https://www.download.inep.gov.br", links_anos))
    anos <- as.numeric(stringr::str_extract(links_anos, "\\d{4}"))
    
    if(is.null(year))
    {
      link_ref <- links_anos[which.max(anos)]
    }
    else
    {
      link_ref <- links_anos[grepl(year, links_anos)]
    }
    
    return(link_ref)
  }
  
  else
  {
    stop("El argumento `source` debe contener un valor entre IBGE y INEP.")
  }
}

################################################################################

# 3. Validar URLs en paralelo

################################################################################

plan(multisession, workers = 2)

validate_urls <- function(urls) {
  ok <- future_map_lgl(urls, function(u) {
    tryCatch({
      res <- GET(u, add_headers(Range = "bytes=0-10"), timeout(30))
      status_code(res) %in% c(200, 206)
    }, error = function(e)
      FALSE)
  })
  
  return(urls[ok])
}

################################################################################

# 4. Descarga paralela de archivos

################################################################################

download_files <- function(urls, path) {
  dir.create(path, showWarnings = FALSE)
  
  options(timeout = 600)
  
  handlers(global = TRUE)
  
  with_progress({
    p <- progressor(along = urls)
    
    future_walk(urls, function(u) {
      name <- basename(u)
      dest <- file.path(path, name)
      
      tryCatch({
        if (!file.exists(dest)) {
          curl::curl_download(u, dest, mode = "wb")
        }
        p(name)
      }, error = function(e) {
        message("Erro: ", name)
      })
    })
  })
}

################################################################################

# 5. Descompresión automática de archivos ZIP

################################################################################

descompress_zips <- function(path, remove_zip = FALSE, overwrite = TRUE, filter = NULL) {
  if (!dir.exists(path)) {
    stop("El directorio especificado no existe.")
  }
  
  zip_files <- list.files(path = path, pattern = "\\.zip$", full.names = TRUE)
  
  if(!is.null(filter))
  {
    zip_files <- zip_files[grepl(filter, zip_files)]
  }
  
  if (length(zip_files) == 0) {
    message("No se encontraron archivos .zip en el directorio.")
    return(invisible(NULL))
  }
  
  for (zip in zip_files) {
    message("Descomprimiendo: ", basename(zip))
    
    unzip(zipfile = zip, exdir = path, overwrite = overwrite, unzip = "unzip")
    
    if (remove_zip) {
      file.remove(zip)
    }
  }
  
  message("¡Proceso completado!")
}

################################################################################

# 6. Ejecución del flujo de trabajo completo para datos del Censo Demografico del IBGE

################################################################################

base_url <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/"

# Paso 1: generar URLs

urls <- generate_urls(base_url=base_url, source="IBGE")
urls <- append(urls,"https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios_Rendimento_do_Responsavel/Agregados_por_setores_renda_responsavel_BR_csv.zip")
urls <- append(urls,"https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/malha_com_atributos/setores/shp/BR/BR_setores_CD2022.zip")
urls <- append(urls,"https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/malha_com_atributos/distritos/shp/BR/BR_distritos_CD2022.zip")

# Paso 2: validar URLs

valid_urls <- validate_urls(urls=urls)

# Paso 3: descargar archivos

download_files(urls=valid_urls, path=parte0(getwd(),"/IBGE_CENSO"))

# Paso 4: descomprimir archivos

descompress_zips(path=parte0(getwd(),"/IBGE_CENSO"), remove_zip=TRUE, overwrite=TRUE)

################################################################################

# 7. Ejecución del flujo de trabajo completo para datos del Censo Escolar del INEP

################################################################################

base_url <- "https://www.gov.br/inep/pt-br/acesso-a-informacao/dados-abertos/microdados/censo-escolar"

# Paso 1: generar URLs

urls <- generate_urls(base_url=base_url, source="INEP", year=2022)

# Paso 2: validar URLs

valid_urls <- validate_urls(urls=urls)

# Paso 3: descargar archivos

download_files(urls=valid_urls, path=paste0(getwd(),"/INEP_CENSO"))

# Paso 4: descomprimir archivos

descompress_zips(path=paste0(getwd(),"/INEP_CENSO"), remove_zip=TRUE, overwrite=TRUE)

################################################################################
