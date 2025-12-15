# script.R
# VERSION: DECODIFICADOR ROBUSTO

library(httr)
library(stringr)
library(dplyr)
library(readr)

base_url <- "https://www.aire.cdmx.gob.mx/"
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")

# Usamos la configuración insegura que ya sabemos que funciona
server_config <- config(ssl_verifypeer = 0, ssl_verifyhost = 0)

tryCatch({
  print("--- INTENTO DE LECTURA ROBUSTA ---")
  
  # 1. Descargamos como RAW (Crudo, sin intentar leer letras todavía)
  resp_home <- GET(paste0(base_url, "default.php"), ua, server_config, timeout(60))
  
  print(paste("Status Code:", status_code(resp_home)))
  
  # Verificamos si descargó algo (bytes)
  contenido_raw <- content(resp_home, "raw")
  print(paste("Bytes descargados:", length(contenido_raw)))
  
  if(length(contenido_raw) < 100) stop("El servidor envió una respuesta vacía (posible bloqueo silencioso).")

  # 2. Intentamos convertir a texto (Probando ISO-8859-1 primero, que es típico de gobierno)
  html_home <- tryCatch({
    content(resp_home, "text", encoding = "ISO-8859-1")
  }, error = function(e) {
    # Si falla, intentamos UTF-8
    content(resp_home, "text", encoding = "UTF-8") 
  })
  
  if(is.na(html_home)) stop("No se pudo decodificar el texto (sigue saliendo NA)")
  
  print("--- VISTA PREVIA HTML (Ya legible) ---")
  print(substr(html_home, 1, 500))
  print("------------------------------------")

  # 3. Buscamos el JS
  # Buscamos 'paths...js' o cualquier JS en carpeta 'js/'
  ruta_relativa <- str_extract(html_home, "src=['\"]([^'\"]*paths[^'\"]*\\.js)['\"]")
  if(is.na(ruta_relativa)) ruta_relativa <- str_extract(html_home, "src=['\"](js/[^'\"]+\\.js)['\"]")
  
  if(is.na(ruta_relativa)) {
    # INTENTO DESESPERADO: Buscar js sin comillas o con espacios
    ruta_relativa <- str_extract(html_home, "js/[a-zA-Z0-9_.-]+\\.js")
  }

  if(is.na(ruta_relativa)) stop("No encuentro el archivo JS en el HTML legible.")
  
  # Limpiamos ruta
  ruta_relativa <- str_remove_all(ruta_relativa, "src=|['\"]")
  url_js <- paste0(base_url, ruta_relativa)
  print(paste("JS Encontrado:", url_js))
  
  # 4. Descargamos el JS (También forzando encoding)
  resp_js <- GET(url_js, ua, server_config, timeout(60))
  js_texto <- content(resp_js, "text", encoding = "ISO-8859-1")
  
  if(is.na(js_texto)) js_texto <- content(resp_js, "text", encoding = "UTF-8")

  # 5. Parseo final
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
