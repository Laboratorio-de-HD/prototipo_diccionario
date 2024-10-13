## Prototipo Diccionario 
# Creado por: MHD. Nancy Hesed 

#cargar librerías necesarias, si no se tienen instaladas, se puede hacer descomentando las siguientes líneas de código
#install.packages("shiny)
#install.packages("shinythemes")
#install.packages("dplyr")
#install.packages("googlesheets4")
library(shiny)
library(shinythemes)
library(dplyr)
library(googlesheets4)

#Leer la base de datos para el diccionario y las unidades frasológicas de los siguientes documentos de drive: 
diccionario = read_sheet("https://docs.google.com/spreadsheets/d/1KfhMd0Xw00OSo-PcZQSGkcbdspaSwT70CdQxZMHxxh8/edit?usp=sharing") #diccionario es la base de datos con la información de las definiciones 
UF = read_sheet("https://docs.google.com/spreadsheets/d/1KfhMd0Xw00OSo-PcZQSGkcbdspaSwT70CdQxZMHxxh8/edit?gid=768113611#gid=768113611", sheet = 2) #UF es la base de datos con la información de las unidades frasológicas 

#Lo siguiente es para estandarizar variables y cambiar formatos para desplegar en la shiny app 
correct_terms = c("Sustantivo" = "(sust)",
                  "Adjetivo" = "(adj)",
                  "Articulo" = "(art)",
                  "Pronombre" = "(pron)",
                  "Verbo" = "(vb)",
                  "Adverbio" = "(adv)", 
                  "Interjeccion" = "(int)",
                  "Preposicion" = "(prep)",
                  "Conjuncion" = "(conj)")

diccionario = diccionario %>%
  mutate(gramm = categoria_gramatical) %>%
  mutate(gramm = recode(gramm, !!!correct_terms)) %>%
  mutate(corpus_format = paste0("(",corpus,")*"))%>%
  mutate(definicion_adicional = ifelse(is.na(definicion_adicional), "", definicion_adicional))

UF = UF %>% 
  mutate(corpus_format = paste0("(",corpus,")*"))


# Definir UI
ui = fluidPage(theme = shinytheme("sandstone"), #cambiar el tema si se desea 
               titlePanel("DICCIONARIO REGIOMONTANO"),
               sidebarLayout(
                 sidebarPanel(
                   textInput("txt1", "Palabra a buscar:", ""), #textInput es para definir un input de texto // txt1 es el "nombre de la var" // "Palabra a buscar" es el texto que se despliega // las "" son el default (en este caso, no hay nada)
                   actionButton("update", "Buscar")
                 ),
                 mainPanel(
                   h2("Definición"),
                   uiOutput("txtout"),
                   uiOutput("full_word_info")
                 )# mainPanel
               )
)

# Definir server 
server = function(input, output, session) {
  output$txtout = renderUI({
    req(input$update) # se corre el código hasta que se dé click en el botón de "buscar" 
    isolate({
      if (substr(input$txt1, nchar(input$txt1), nchar(input$txt1)) == "o") {
        h3(paste(input$txt1, "/a"))
      } else if (substr(input$txt1, nchar(input$txt1), nchar(input$txt1)) == "a") {
        h3(paste(input$txt1, "/o")) # desplegar la palabra /a o /o, dependiendo del formato original en el que se buscó 
      }
    })
  })
  
  output$full_word_info = renderUI({
    req(input$update)
    isolate({
      if (!input$txt1 %in% diccionario$palabra) {
        return(h4("Prueba con una palabra diferente.")) #si no se encuentra la palabra en la base, se muestra el mensaje 
      }
      
      matching_rows = which(diccionario$palabra == input$txt1) #encontrar dónde la palabra ingresada coincide con la base 
      subset_diccionario = diccionario[matching_rows, ] #crear variable con la información de la palabra buscada, para el diccionario 
      
      matching_rows_uf = which(UF$palabra == input$txt1) #encontrar dónde la palabra ingresada coincide con la base 
      subset_uf = UF[matching_rows_uf, ] #crear variable con la información de la palabra buscada, para las unidades frasológicas 
      
      definitions = tagList(
        lapply(1:length(unique(subset_diccionario$definicion_principal)), function(i) {
          def = unique(subset_diccionario$definicion_principal)[i]
          indices_def = which(subset_diccionario$definicion_principal == def)
          
          definition_html = HTML(paste(i, ".", subset_diccionario$gramm[indices_def[1]], "<b>", def, ":</b><br>", 
                                        "<i>", subset_diccionario$ejemplo[indices_def[1]], "</i>", subset_diccionario$corpus_format[indices_def[1]], "<br>", 
                                        paste0("*", subset_diccionario$genero[indices_def[1]], ", ", subset_diccionario$edad[indices_def[1]], ", ", subset_diccionario$educacion[indices_def[1]], "<br><br>"))) #para organizar la información de la definición principal 
          
          notes_html = lapply(indices_def[-1], function(j) {
            HTML(paste("-", subset_diccionario$definicion_adicional[j], ":", "<i>", subset_diccionario$ejemplo[j], "</i>", subset_diccionario$corpus_format[j], "<br>",
                       paste0("*", subset_diccionario$genero[j], ", ", subset_diccionario$edad[j], ", ", subset_diccionario$educacion[j], "<br><br>"))) #para organizar la información de las definiciones adicionales 
          })
          
          tagList(definition_html, notes_html) #desplegar la información de la definición principal + adicionales 
        })
      )
      
      uf_section = tagList(
        h3("Unidades Fraselógicas"),
        lapply(1:nrow(subset_uf), function(k) {
          HTML(paste("-", subset_uf$frase[k], ",", "<b>", subset_uf$definicion[k], "</b>", ":", "<i>", subset_uf$ejemplo[k], "</i>", subset_uf$corpus_format[k], "<br>",
                     paste0("*", subset_uf$genero[k], ", ", subset_uf$edad[k], ", ", subset_uf$educacion[k], "<br><br>"))) #para organizar la información de la definición principal 
        })
      )
      
      tagList(definitions, uf_section) #desplegar la información de las unidades frasológicas 
    })
  })
}


# Crear Shiny object
shinyApp(ui = ui, server = server)


