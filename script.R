library(httr)
library(stringr)
library(dplyr)
library(readr)

# URL del gobierno
base_url <- "https://www.aire.cdmx.gob.mx/"

# Header falso (User Agent)
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")

# Configuración SSL básica (Al usar Ubuntu 20.04, ya no necesitamos la config compleja de ciphers)
server_config <- config(ssl_verifypeer = 0, ssl_verifyhost = 0)

tryCatch({
  print("1. Descargando página principal...")
  
  resp_home <- GET(paste0(base_url, "default.php"), ua, server_config, timeout(60))
  
  if(status_code(resp_home) != 200) stop(paste("Error Status Home:", status_code(resp_home)))
  
  html_home <- content(resp_home, "text", encoding = "UTF-8")
  print(paste("   -> Descarga correcta. Tamaño:", nchar(html_home), "caracteres."))

  # --- DEBUG: IMPRIMIR LOS PRIMEROS 500 CARACTERES ---
  # Esto nos dirá si es una página de bloqueo o la real
  print("   -> VISTA PREVIA DEL HTML:")
  print(substr(html_home, 1, 500))
  # ---------------------------------------------------

  # 2. Buscar JS (Búsqueda ampliada)
  # Buscamos primero 'paths', si no, buscamos cualquier JS en la carpeta js/
  ruta_relativa <- str_extract(html_home, "src=['\"]([^'\"]*paths[^'\"]*\\.js)['\"]")
  
  if(is.na(ruta_relativa)) {
      print("   -> No se encontró 'paths'. Buscando alternativas...")
      # Intentamos encontrar cualquier referencia a js/
      ruta_relativa <- str_extract(html_home, "js/[a-zA-Z0-9_.-]+\\.js")
  }
  
  if(is.na(ruta_relativa)) stop("No se encontró ningún archivo JS válido en el HTML")
  
  # Limpieza de la ruta
  ruta_relativa <- str_remove_all(ruta_relativa, "src=|['\"]")
  url_js <- paste0(base_url, ruta_relativa)
  print(paste("2. JS Encontrado:", url_js))
  
  # 3. Descargar JS
  resp_js <- GET(url_js, ua, server_config, timeout(60))
  if(status_code(resp_js) != 200) stop("Error descargando archivo JS")
  
  js_texto <- content(resp_js, "text", encoding = "UTF-8")
  
  # 4. Parsear Datos
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
        lista_datos[[length(lista_datos)+1]] <- data.frame(slug = slug, color_hex = color_final, info = clean_txt, stringsAsFactors = FALSE)
      }
    }
  }
  
  if(length(lista_datos) == 0) stop("El JS se descargó pero no contenía datos de zonas.")
  
  df_final <- bind_rows(lista_datos)
  write_csv(df_final, "datos_aire.csv")
  print("¡ÉXITO TOTAL! CSV generado.")
  
}, error = function(e) {
  print(paste("ERROR CRÍTICO:", e$message))
  quit(status = 1)
})
