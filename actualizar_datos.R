# archivo: actualizar_datos.R
library(httr)
library(stringr)
library(dplyr)
library(readr)

# URL del gobierno
base_url <- "https://www.aire.cdmx.gob.mx/"
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36")

tryCatch({
  # 1. Descargar
  resp_home <- GET(paste0(base_url, "default.php"), ua, config(ssl_verifypeer = 0), timeout(60))
  html_home <- content(resp_home, "text", encoding = "UTF-8")
  
  # 2. Buscar JS
  ruta_relativa <- str_extract(html_home, "src=['\"]([^'\"]*paths[^'\"]*\\.js)['\"]")
  if(is.na(ruta_relativa)) ruta_relativa <- str_extract(html_home, "src=['\"](js/[^'\"]+\\.js)['\"]")
  
  url_js <- paste0(base_url, str_remove_all(ruta_relativa, "src=|['\"]"))
  
  # 3. Parsear
  resp_js <- GET(url_js, ua, config(ssl_verifypeer = 0), timeout(60))
  js_texto <- content(resp_js, "text", encoding = "UTF-8")
  
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
  
  df_final <- bind_rows(lista_datos)
  
  # 4. Guardar CSV
  write_csv(df_final, "datos_aire.csv")
  print("Datos actualizados correctamente")
  
}, error = function(e) {
  print(paste("Error en scraping:", e$message))
  # No guardamos nada si falla, para no romper el mapa con un archivo vacío
})
