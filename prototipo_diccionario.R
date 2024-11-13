## Prototipo Diccionario 
# Creado por: MHD. Nancy Hesed 

#cargar librerías necesarias, si no se tienen instaladas, se puede hacer descomentando las siguientes líneas de código
#install.packages("shiny)
#install.packages("shinythemes")
#install.packages("dplyr")
library(shiny)
library(shinythemes)
library(dplyr)

#Leer la base de datos para el diccionario y las unidades frasológicas de los siguientes documentos de drive
url_diccionario = "https://docs.google.com/spreadsheets/d/1KfhMd0Xw00OSo-PcZQSGkcbdspaSwT70CdQxZMHxxh8/gviz/tq?tqx=out:csv&sheet=Definiciones"
diccionario = read.csv(url_diccionario)
url_uf = "https://docs.google.com/spreadsheets/d/1KfhMd0Xw00OSo-PcZQSGkcbdspaSwT70CdQxZMHxxh8/gviz/tq?tqx=out:csv&sheet=UnidadesFraselogicas"
UF = read.csv(url_uf)

#Acceder a la carpeta de audios
audio_url = "https://raw.githubusercontent.com/Laboratorio-de-HD/labHD-web/5b1ef818f47f95630be2b8c9b9a65e72489c6116/www/audio_sample.mp3"

#Lo siguiente es para estandarizar variables y cambiar formatos para desplegar en la shiny app 
diccionario = diccionario %>% 
  mutate(gramm = categoria_gramatical) %>% 
  mutate(gramm = recode(gramm, !!!correct_terms)) %>% 
  mutate(corpus_format = paste0("(", corpus, ")*")) %>% 
  mutate(definicion_adicional = ifelse(is.na(definicion_adicional), "", definicion_adicional)) %>%
  select(where(~ all(!is.na(.))))


UF = UF %>% 
  mutate(corpus_format = paste0("(", corpus, ")*"))%>%
  select(where(~ all(!is.na(.))))

# Definir UI
ui = fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("DICCIONARIO REGIOMONTANO"),
  
  # Agregar el estilo CSS para personalizar los reproductores de audio
  tags$head(
    tags$style(HTML("
      /* Ocultar todos los controles del reproductor */
      .custom-audio {
        width: 0;
        height: 0;
        visibility: hidden;
      }
      
      /* Crear un botón de play/pause personalizado */
      .audio-button {
        width: 50px;
        height: 50px;
        background-color: #BE9D6A;
        border-radius: 50%;
        color: white;
        font-size: 24px;
        border: none;
        cursor: pointer;
        display: inline-block;
        text-align: center;
        vertical-align: middle;
      }
      .audio-button:hover {
        background-color: #A38550;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      textInput("txt1", "Palabra a buscar:", ""),  # Input de texto
      actionButton("update", "Buscar")
    ),
    mainPanel(
      h2("Definición"),
      uiOutput("txtout"),
      uiOutput("full_word_info")
    )
  )
)


# Definir server 
server = function(input, output, session) {
  folder_url = "https://raw.githubusercontent.com/Laboratorio-de-HD/labHD-web/master/www/"
  
  # Function to check if a file exists
  file_exists = function(url) {
    tryCatch({
      # Usa GET en lugar de HEAD para verificar la URL
      response = httr::GET(url)
      
      # Verifica si la respuesta tiene un código de estado 200 (OK)
      return(httr::status_code(response) == 200)
    }, error = function(e) {
      # Captura el error y muestra un mensaje
      message("Error al verificar la URL: ", url)
      return(FALSE)
    })
  }
  
  output$txtout = renderUI({
    req(input$update)
    isolate({
      if (substr(input$txt1, nchar(input$txt1), nchar(input$txt1)) == "o") {
        h3(paste(input$txt1, "/a"))
      } else if (substr(input$txt1, nchar(input$txt1), nchar(input$txt1)) == "a") {
        h3(paste(input$txt1, "/o"))
      }
    })
  })
  
  output$full_word_info = renderUI({
    req(input$update)
    isolate({
      if (!input$txt1 %in% diccionario$palabra) {
        return(h4("Prueba con una palabra diferente."))
      }
      
      matching_rows = which(diccionario$palabra == input$txt1)
      subset_diccionario = diccionario[matching_rows, ]
      
      matching_rows_uf = which(UF$palabra == input$txt1)
      subset_uf = UF[matching_rows_uf, ]
      
      definitions = tagList(
        lapply(1:length(unique(subset_diccionario$definicion_principal)), function(i) {
          def = unique(subset_diccionario$definicion_principal)[i]
          indices_def = which(subset_diccionario$definicion_principal == def)
          
          definition_html = HTML(paste("<br>", i, ".", subset_diccionario$gramm[indices_def[1]], "<b>", def, ":</b><br>",
                                       "<i>", subset_diccionario$ejemplo[indices_def[1]], "</i>", subset_diccionario$corpus_format[indices_def[1]], "<br>",
                                       paste0("*", subset_diccionario$genero[indices_def[1]], ", ", subset_diccionario$edad[indices_def[1]], ", ", subset_diccionario$educacion[indices_def[1]], "<br>")))
          
          notes_html = lapply(indices_def[-1], function(j) {
            HTML(paste("<br>","-", subset_diccionario$definicion_adicional[j], ":", "<i>", subset_diccionario$ejemplo[j], "</i>", subset_diccionario$corpus_format[j], "<br>",
                       paste0("*", subset_diccionario$genero[j], ", ", subset_diccionario$edad[j], ", ", subset_diccionario$educacion[j], "<br>")))
          })
          
          corpus_audio_players = lapply(indices_def, function(idx) {
            url = paste0(folder_url, subset_diccionario$corpus[idx], ".mp3")
            if (file_exists(url)) {
              # Crear el reproductor de audio sin controles, pero con el botón play/pause visible
              audio_player = tags$audio(
                id = paste0("audio_", idx), 
                src = url, 
                type = "audio/mpeg", 
                class = "custom-audio" # Se oculta el reproductor real
              )
              play_button = actionButton(
                inputId = paste0("play_", idx), 
                label = "▶", 
                class = "audio-button"
              )
              # Este código JavaScript se usará para controlar el play/pause del audio
              script = tags$script(HTML(
                paste0("
                  $('#", paste0("play_", idx), "').click(function() {
                    var audio = document.getElementById('", paste0("audio_", idx), "');
                    if (audio.paused) {
                      audio.play();
                      $('#", paste0("play_", idx), "').html('❚❚'); // Cambiar el botón a pause
                    } else {
                      audio.pause();
                      $('#", paste0("play_", idx), "').html('▶'); // Cambiar el botón a play
                    }
                  });
                ")
              ))
              tagList(audio_player, play_button, script)
            } else {
              NULL
            }
          })
          
          tagList(definition_html, corpus_audio_players, notes_html)
        })
      )
      
      uf_section = tagList(
        h3("Unidades Fraselológicas"),
        lapply(1:nrow(subset_uf), function(k) {
          url_uf = paste0(folder_url, subset_uf$corpus[k], ".mp3")
          if (file_exists(url_uf)) {
            # Crear el reproductor de audio sin controles, pero con el botón play/pause visible
            audio_player_uf = tags$audio(
              id = paste0("audio_uf_", k), 
              src = url_uf, 
              type = "audio/mpeg", 
              class = "custom-audio" # Se oculta el reproductor real
            )
            play_button_uf = actionButton(
              inputId = paste0("play_uf_", k), 
              label = "▶", 
              class = "audio-button"
            )
            script_uf = tags$script(HTML(
              paste0("
                $('#", paste0("play_uf_", k), "').click(function() {
                  var audio = document.getElementById('", paste0("audio_uf_", k), "');
                  if (audio.paused) {
                    audio.play();
                    $('#", paste0("play_uf_", k), "').html('❚❚'); // Cambiar el botón a pause
                  } else {
                    audio.pause();
                    $('#", paste0("play_uf_", k), "').html('▶'); // Cambiar el botón a play
                  }
                });
              ")
            ))
          } else {
            audio_player_uf = NULL
            play_button_uf = NULL
            script_uf = NULL
          }
          tagList(
            HTML(paste("<br>", "-", subset_uf$frase[k], ",", "<b>", subset_uf$definicion[k], "</b>", ":", "<i>", subset_uf$ejemplo[k], "</i>", subset_uf$corpus_format[k], "<br>",
                       paste0("*", subset_uf$genero[k], ", ", subset_uf$edad[k], ", ", subset_uf$educacion[k], "<br>"))),
            audio_player_uf, play_button_uf, script_uf
          )
        })
      )
      
      tagList(definitions, uf_section)
    })
  })
}

# Crear el objeto Shiny
shinyApp(ui = ui, server = server)


