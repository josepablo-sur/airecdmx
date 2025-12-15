# script.R
# VERSION: ESPIA V2 (Si no ves esto en el log, no se actualizó)

library(httr)
library(stringr)
library(dplyr)
library(readr)

# URL del gobierno
base_url <- "https://www.aire.cdmx.gob.mx/"

# Header falso (User Agent)
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")

# Configuración SSL básica (Ubuntu 20.04 permite esto)
server_config <- config(ssl_verifypeer = 0, ssl_verifyhost = 0)

tryCatch({
  print("--- INICIANDO DIAGNÓSTICO (VERSION ESPIA) ---")
  print("1. Intentando descargar página principal...")
  
  # Hacemos la petición
  resp_home <- GET(paste0(base_url, "default.php"), ua, server_config, timeout(60))
  
  # Verificamos status
  print(paste("   -> Status Code:", status_code(resp_home)))
  
  html_home <- content(resp_home, "text", encoding = "UTF-8")
  print(paste("   -> Descarga finalizada. Tamaño:", nchar(html_home), "caracteres."))

  # --- EL MOMENTO DE LA VERDAD: ¿QUÉ DESCARGAMOS? ---
  print("--- VISTA PREVIA DEL HTML (PRIMEROS 1000 CARACTERES) ---")
  print(substr(html_home, 1, 1000))
  print("--------------------------------------------------------")
  
  # Búsqueda del JS
  # Buscamos patrones típicos del mapa de la CDMX
  ruta_relativa <- str_extract(html_home, "src=['\"]([^'\"]*paths[^'\"]*\\.js)['\"]")
  
  if(is.na(ruta_relativa)) {
      print("   -> No encontré 'paths...js'. Buscando cualquier JS en carpeta js/...")
      ruta_relativa <- str_extract(html_home, "js/[a-zA-Z0-9_.-]+\\.js")
  }
  
  if(is.na(ruta_relativa)) {
    # Si falla, fallamos a propósito para ver el log
    stop("NO SE ENCONTRÓ EL ARCHIVO JS. REVISA LA VISTA PREVIA DEL HTML ARRIBA.")
  }
  
  # Limpieza y descarga del JS
  ruta_relativa <- str_remove_all(ruta_relativa, "src=|['\"]")
  url_js <- paste0(base_url, ruta_relativa)
  print(paste("2. Archivo JS detectado:", url_js))
  
  resp_js <- GET(url_js, ua, server_config, timeout(60))
  js_texto <- content(resp_js, "text", encoding = "UTF-8")
  
  # Extracción de datos
  bloques <- unlist(str_split(js_texto, "zona\\d+:\\s*\\{"))
  lista_datos <- list()
  
  for(bloque in bloques) {
    if(str_detect(bloque, "slug") && str_detect(bloque, "name")) {
      slug <- str_match(bloque, 'slug:\\s*"([^"]+)"')[2]
      fill <- str_match(bloque, 'fill:\\s*"([^"]+)"')[2]
      raw_name <- str_match(bloque, '(?s)name:\\s*(.*?),\\s*fill')[2]
      
      if(!is.na(slug) && slug != "discriminate") {
        clean_txt <- raw_name %>% 
          str_replace_all('"\\s*\\+\"\\s*\\\\n\\s*\"\\s*\\+\"\\s*', " | ") %>% 
          str_remove_all('[\\\"\\+]') %>% 
          trimws()
        color_final <- if(!is.na(fill)) paste0(fill, "FF") else "#999999FF"
        lista_datos[[length(lista_datos)+1]] <- data.frame(slug=slug, color_hex=color_final, info=clean_txt, stringsAsFactors=FALSE)
      }
    }
  }
  
  if(length(lista_datos) == 0) stop("JS descargado pero sin datos legibles.")
  
  df_final <- bind_rows(lista_datos)
  write_csv(df_final, "datos_aire.csv")
  print("¡ÉXITO! CSV generado.")
  
}, error = function(e) {
  print(paste("ERROR FINAL:", e$message))
  quit(status = 1)
})
