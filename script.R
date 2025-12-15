library(httr)
library(stringr)
library(dplyr)
library(readr)

# URL del gobierno
base_url <- "https://www.aire.cdmx.gob.mx/"

# Header falso
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")

# --- CONFIGURACIÓN DE SEGURIDAD PARA SERVIDORES VIEJOS ---
# Esto baja el nivel de seguridad de OpenSSL para aceptar llaves "dh key too small"
# Es necesario para aire.cdmx.gob.mx
old_server_config <- config(
  ssl_verifypeer = 0, 
  ssl_verifyhost = 0,
  ssl_cipher_list = "DEFAULT:@SECLEVEL=1"
)

tryCatch({
  print("Iniciando conexión con seguridad reducida...")
  
  # 1. Descargar Home
  resp_home <- GET(paste0(base_url, "default.php"), ua, old_server_config, timeout(60))
  
  # Chequeo de status
  if(status_code(resp_home) != 200) stop(paste("Error Status Home:", status_code(resp_home)))
  
  html_home <- content(resp_home, "text", encoding = "UTF-8")
  
  # 2. Buscar JS
  ruta_relativa <- str_extract(html_home, "src=['\"]([^'\"]*paths[^'\"]*\\.js)['\"]")
  if(is.na(ruta_relativa)) ruta_relativa <- str_extract(html_home, "src=['\"](js/[^'\"]+\\.js)['\"]")
  
  if(is.na(ruta_relativa)) stop("No se encontró el archivo JS en el HTML")
  
  url_js <- paste0(base_url, str_remove_all(ruta_relativa, "src=|['\"]"))
  print(paste("JS encontrado:", url_js))
  
  # 3. Descargar JS
  resp_js <- GET(url_js, ua, old_server_config, timeout(60))
  if(status_code(resp_js) != 200) stop("Error descargando JS")
  
  js_texto <- content(resp_js, "text", encoding = "UTF-8")
  
  # 4. Parsear
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
        
        lista_datos[[length(lista_datos)+1]] <- data.frame(
          slug = slug,
          color_hex = color_final,
          info = clean_txt,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if(length(lista_datos) == 0) stop("No se extrajeron datos (lista vacía)")
  
  df_final <- bind_rows(lista_datos)
  
  # 5. Guardar CSV
  write_csv(df_final, "datos_aire.csv")
  print("¡ÉXITO! Archivo datos_aire.csv generado correctamente.")
  
}, error = function(e) {
  print(paste("ERROR CRÍTICO:", e$message))
  # Hacemos que el script falle para que GitHub nos avise en rojo
  quit(status = 1)
})
