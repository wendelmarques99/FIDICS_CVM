

ui <- page_navbar(
  tags$head(
    tags$style(HTML('#run{background-color:#78c2ad}'))
  ),
  fillable = TRUE,
  title = "Fidics (scrapping)",
  theme = bs_theme(version = 5, bootswatch = "minty", bg = "#fff", fg = "#000",  "navbar-bg" = "#fff"),
  sidebar = sidebar(
    title = "Escolhas e inputs",
    selectInput(
      inputId = "var", 
      label = "Período", 
      choices = c('LM', "L24M"), 
    ),
    
    shiny::fileInput(inputId = "cadastro_fidic", 
                     label = "Insira o arquivo cadastral aqui:",
                     buttonLabel = "Arquivo", 
                     placeholder = ""),
    shiny::br(),
    shiny::actionButton(
      inputId = "run", 
      label = "Rodar",
      
    )
  ),
  layout_columns(
    fluidRow(
      column(3),
      column(6,
             uiOutput(outputId = "arquivos_dowload")
      ),
      column(3)
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$run, {
    
    shiny::req(input$cadastro_fidic)
    shiny::req(input$var)
    
    df_fidics <- readxl::read_excel(input$cadastro_fidic$datapath) %>% 
      dplyr::mutate(cnpj =  gsub("[^0-9]", "", x = cnpj))
    
    if(input$var == "LM"){
      
      df_fidics <- df_fidics %>% 
        dplyr::filter(
          s == 0
        )
    }
    
    
    
    n <- nrow(df_fidics)  
    
    withProgress(message = 'Gerando arquivo...', value = 0, {
      
      dados_tidy <- purrr::pmap(list(df_fidics$cnpj, df_fidics$Fundo, df_fidics$s, periodo = input$var), function(cnpj, fundo, s, periodo) {
        incProgress(1/n, detail = paste("Processando:", fundo))
        puxar_info_fidic_cvm(cnpj = cnpj, fundo = fundo, s = s, periodo = periodo)
      })
      
      
      fundos_disponiveis <- purrr::keep(dados_tidy, tem_fundo_encontrado)
      
      df_fidics_resultado <-  bind_rows(fundos_disponiveis, .id = "source") %>%  
        replace(is.na(.), 0) %>% 
        dplyr::mutate(competencia = lubridate::dmy(paste0("01/", competencia))) %>% 
        dplyr::select(-source) 
      
      
      output$arquivos_dowload <- renderUI({
        
        if (!is.null(df_fidics_resultado)){
          
          shiny::downloadLink(outputId = "baixar_arquivos",
                              label = paste0("Arquivo disponível para download ", 
                                             format(Sys.time(), "%d/%m/%Y %H:%M:%S")))
          
        }
      })
      
      output$baixar_arquivos <- shiny::downloadHandler(
        filename = function(){
          "FIDIC_dados.xlsx"
        },
        content = function(file){
          writexl::write_xlsx(df_fidics_resultado, path = file)
        }
      )
      
      # updateProgressBar(session, id = "progress", value = 100)
      
    })
    
  })
  
  
  
  
}

shinyApp(ui, server)
