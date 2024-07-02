library(rvest)
library(dplyr)
library(xml2)
library(XML)
library(httr)
library(base64enc)
library(shiny)
library(bslib)
library(thematic)
library(progress)


tem_fundo_encontrado <- function(df) {
  !any(df$fundo == "nao encontrado")
}



# leia s como parametro para maximizar a quantidade de observacoes na pagina
puxar_info_fidic_cvm <- function(cnpj, fundo, s, periodo){
  
  if (periodo == "LM"){
    s = 0
  }else{
    s = s
  }
  
  # ------------------------------------------------------------------
  url <- glue::glue("https://fnet.bmfbovespa.com.br/fnet/publico/pesquisarGerenciadorDocumentosDados?d=2&s={s}&l=100&o%5B0%5D%5BdataEntrega%5D=desc&cnpjFundo={cnpj}&idCategoriaDocumento=0&idTipoDocumento=0&idEspecieDocumento=0&_=171512114623")
  
  dados <- httr::GET(url) %>% 
    httr::content("text") %>% 
    jsonlite::fromJSON() %>% 
    .$data
  
  if(length(dados) == 0){
    
    tibble::tibble(
      competencia = "", 
      fundo = "nao encontrado",
      Classe = "nao encontrado",
      series = "nao encontrado",
      tipo = "nao encontrado",
      qtd_cotas = "nao encontrado",
      vl_cotas = "nao encontrado"
    )
  }else{
    
    if (periodo == "LM"){
      
      id <- dados %>% 
        dplyr::filter(
          tipoDocumento == "Informe Mensal Estruturado ",
          status == "AC"
        ) %>% 
        head(1) %>% dplyr::pull(id)
      
    }else{
      
      id <- dados %>% 
        dplyr::filter(
          tipoDocumento == "Informe Mensal Estruturado ",
          status == "AC"
        ) %>% head(24) %>% dplyr::pull(id)
    }
     
    
    cotas <- c("Cota Mezanino", "Cota Sub Mezanino", "Cota Subordinada")
    
    classe <- c("//DESC_SERIE_CLASSE_SENIOR", "//DESC_SERIE_CLASSE_SUBORD")
    
    nome_do_fundo <- fundo
    
    retorno_dos_ids <- function(id, fundos){
      
      url <- glue::glue("https://fnet.bmfbovespa.com.br/fnet/publico/downloadDocumento?id={id}")
      
      # Fazer a solicitação GET
      response <- GET(url)
      
      # Extrair o conteúdo do XML
      xml_content <- content(response, as = "text")
      
      conteudo <- base64decode(xml_content)
      
      xml_text <- rawToChar(conteudo)
      
      xml_data <- as.list(read_xml(xml_text))
      
      xml_data <- xml_ns_strip(xml_data)
      
      competencia <- xml_data %>% xml2::xml_find_all(".//DT_COMPT") %>% html_text() %>% unique()
      
      recompra <- xml_data %>% xml_find_all(".//DICRED_MES_ALIEN_RECOMP") %>% xml_find_all(".//VL_DICRED_ALIEN") %>% html_text() %>% 
        gsub(",", ".", .) %>% as.numeric()
      
      
      inadi_sem_aqui <- xml_data %>% 
        xml_find_all(".//COMPMT_DICRED_SEM_AQUIS") %>%
        xml_find_all(".//VL_SOM_INAD_VENC") %>% 
        html_text() %>% 
        trasnforma_string_em_numerico
      
      inadi_com_aqui <- xml_data %>%
        xml_find_all(".//COMPMT_DICRED_AQUIS") %>% 
        xml_find_all(".//VL_SOM_INAD_VENC") %>%
        html_text() %>% 
        trasnforma_string_em_numerico
      
      aquisicoes <- xml_data %>%
        xml_find_all(".//AQUISICOES") %>% 
        xml_find_all(".//VL_DICRED_AQUIS") %>%
        html_text() %>% 
        trasnforma_string_em_numerico
      
      provisao_de_recuperacao_com_aquisicao <- xml_data %>%
        xml_find_all(".//VL_PROVIS_REDUC_RECUP") %>%
        html_text() %>% 
        trasnforma_string_em_numerico
      
      provisao_de_recuperacao_sem_aquisicao <- xml_data %>%
        xml_find_all(".//VL_DICRED_PROVIS_REDUC_RECUP") %>%
        html_text() %>% 
        trasnforma_string_em_numerico
      
      rentabilidade_sub <- function(){
        
        var <- xml_find_all(xml_data, ".//RENT_CLASSE_SUBORD[contains(TIPO, 'Subordinada') or contains(TIPO, 'SUB')]") %>% 
          xml_find_all(".//PR_APURADA") %>%
          html_text() %>% 
          trasnforma_string_em_numerico
        
        if(length(var) == 0) 0 else var
      }
      
      
      df_agrupado_com_info_inicias <- tibble(
        Recompra = recompra,
        aquisicoes = aquisicoes,
        "Inadimplentes (com aquisicao substancial dos riscos e beneficios)" = inadi_com_aqui,
        "Inadimplentes (sem aquisicao substancial dos riscos e beneficios)" = inadi_sem_aqui,
        "PDD" = provisao_de_recuperacao_com_aquisicao+provisao_de_recuperacao_sem_aquisicao,
        "Rentabilidade da subordinada" = rentabilidade_sub()
      )
      
      items <- xml2::xml_find_all(xml_data, ".//DESC_SERIE_CLASSE")
      
      node_senior <- xml_find_all(items, ".//DESC_SERIE_CLASSE_SENIOR") 
      
      node_sub <- xml_find_all(items, ".//DESC_SERIE_CLASSE_SUBORD") 
      
      if (!is.null(node_senior)){
        
        series <- function(){
          if(length(node_senior %>% xml_find_all(".//SERIE") %>% html_text()) == 0) "" else node_senior %>% xml_find_all(".//SERIE") %>% html_text()
        }
        
        qtd_cotas <- function(){
          if(length(node_senior %>% xml_find_all(".//QT_COTAS") %>% html_text()) == 0) 0 else node_senior %>% xml_find_all(".//QT_COTAS") %>% html_text() %>% gsub(",", ".", .) %>% as.numeric()
        }
        
        vl_cotas <- node_senior %>% xml_find_all(".//VL_COTAS") %>% html_text() %>% gsub(",", ".", .) %>% as.numeric()
        
        df_senior <- tibble::tibble(Classe = "Senior",
                                    series = series(),
                                    tipo = "",
                                    qtd_cotas = qtd_cotas(),
                                    vl_cotas = vl_cotas) %>% 
          dplyr::mutate(PL_SENIOR = qtd_cotas*vl_cotas) %>% 
          dplyr::group_by(Classe) %>% 
          dplyr::summarise(PL_SENIOR = sum(PL_SENIOR, na.rm = TRUE)) %>% 
          dplyr::select(-Classe)
        
      }
      
      if(!is.null(node_sub)){
        
        tipo <- function(){
          if (length(node_sub %>% xml_find_all(".//TIPO") %>% html_text()) == 0) '' else node_sub %>% xml_find_all(".//TIPO") %>% html_text()
        }
        
        series_sub <- function(){
          if (length(node_sub %>% xml_find_all(".//SERIE") %>% html_text()) == 0) '' else node_sub %>% xml_find_all(".//SERIE") %>% html_text()
        }
        
        qtd_cotas_sub <- node_sub %>% xml_find_all(".//QT_COTAS") %>% html_text() %>%  gsub(",", ".", .) %>% as.numeric()
        
        vl_cotas_sub <-  node_sub %>% xml_find_all(".//VL_COTAS") %>% html_text() %>%  gsub(",", ".", .) %>% as.numeric()
        
        df_sub <- tibble::tibble(
          Classe = "Subordinada",
          series = series_sub(),
          tipo = ifelse(tipo() == "", "Subordinada", tipo()),
          qtd_cotas = qtd_cotas_sub,
          vl_cotas = vl_cotas_sub
        ) %>% 
          dplyr::mutate(
            tipo = stringr::str_extract(tipo, "Mezanino|Subordinada|subordinada|mezanino|SUB|única|Única|unica|Unica|MEZANINO")
          ) %>% 
          dplyr::mutate(
            tipo =  dplyr::case_when(
              tipo == "SUB" ~ "Subordinada",
              tipo == "subordinada" ~ "Subordinada",
              tipo %in% c("Única", "Unica") ~"Subordinada",
              tipo %in% c("MEZANINO") ~ 'Mezanino',
              TRUE ~ tipo
            )
          ) %>% 
          dplyr::group_by(tipo) %>% 
          dplyr::mutate(PL = qtd_cotas*vl_cotas) %>% 
          dplyr::summarise(PL = sum(PL)) %>% 
          dplyr::ungroup() %>% 
          tidyr::pivot_wider(names_prefix = "PL_", names_from = tipo, values_from = PL)
        
        
        
      }
      
      
      cbind(tibble::tibble(competencia = competencia, fundo = fundos), 
            df_sub, df_senior, df_agrupado_com_info_inicias) 
      
    }
    
    purrr::pmap_dfr(list(id, nome_do_fundo), function(id, fundo) retorno_dos_ids(id = id, fundos = fundo))
  }
  
}

remover_palavras_iguais <- function(string){
  
  string_no_accents <- stringi::stri_trans_general(string, "Latin-ASCII")
  
  words <- unlist(strsplit(string_no_accents, " "))
  
  # 3. Remove palavras duplicadas
  unique_words <- unique(words)
  
  # 4. Recombina as palavras em uma única string
  result <- paste(unique_words, collapse = " ")
  
  # Exibe o resultado
  result
}

trasnforma_string_em_numerico <- function(valor){
  gsub(",", ".", valor) %>% as.numeric()
}

# Achar o id do documento -------------------------------------------------
# df_fidics <- readxl::read_excel("dados/fidics_dados_cadastrais.xlsx") %>% 
#   dplyr::mutate(cnpj =  gsub("[^0-9]", "", x = cnpj))



