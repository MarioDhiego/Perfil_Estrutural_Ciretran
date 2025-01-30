
#-------------------------------------------------------------------------------
# PACOTES
library(readr)
library(readxl)
library(dplyr)
library(curl)
library(plyr)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(plotly)
library(leaflet)
library(tidygeocoder)
library(likert)
library(scales) 
library(htmlwidgets)
library(htmltools)
library(RColorBrewer)
library(table1)
library(flextable)
library(rstatix)
library(haven)
library(DiagrammeR) 
library(rlang)
library(forcats)
library(DT)  
library(openrouteservice)
library(osmdata)
library(httr2)
library(glue)
library(rjson)
library(googleway)
library(echarts4r)
#-------------------------------------------------------------------------------
# Carregar Dados
setwd("C:/Users/mario.valente/Documents/github_2024/Perfil_Estrutural_Ciretran-main")
dados <- read_excel("Banco_Pesquisa_Ciretran.xlsx")

#colnames(dados)

gerar_grafico <- function(dados, x_var, fill_var, title, order = "asc") {
  # Verificar se a coluna está presente nos dados
  if (!(x_var %in% colnames(dados))) {
    stop(paste("A coluna", x_var, "não foi encontrada nos dados"))
  }
  # Converter a coluna para fator se necessário
  dados[[x_var]] <- as.factor(dados[[x_var]])
  # Ordenação opcional das categorias
  dados[[x_var]] <- if (order == "asc") {
    forcats::fct_infreq(dados[[x_var]]) # Ordem crescente
  } else if (order == "desc") {
    forcats::fct_rev(forcats::fct_infreq(dados[[x_var]])) # Ordem decrescente
  } else {
    factor(dados[[x_var]]) # Ordem original
  }
  # Criação do gráfico com ggplot2
  ggplot(dados, aes_string(x = x_var, fill = fill_var)) +
    geom_bar(color = "black") +
    geom_text(stat = "count", aes(label = scales::percent(..count.. / sum(..count..))),
              position = position_stack(vjust = 0.5), color = "white") +
    labs(title = title, x = "", y = "Nº de Entrevistados") +
    theme_minimal()
}

#-------------------------------------------------------------------------------
# UI

ui <- dashboardPage(title = "Dashboard", skin = "blue",
                    dashboardHeader(title = "PERFIL DAS CIRETRANS", titleWidth = 300,
                                    tags$li(class = "dropdown",style = "margin-right: 15px; display: inline-block;",  
                                            a(href = "https://www.facebook.com/detranPARA", class = "fa fa-facebook fa-lg", target = "_blank", title = "Facebook", style = "color: #3b5998; transition: color 0.3s;"),
                                            tags$style(HTML(".fa-facebook:hover {color: #8b9dc3;}"))),
                                    tags$li(class = "dropdown",style = "margin-right: 15px; display: inline-block;", 
                                            a(href = "https://www.instagram.com/detranpa_", class = "fa fa-instagram", target = "_blank", title = "InstaGram",style = "color: #e1306c; transition: color 0.3s;"),
                                            tags$style(HTML(".fa-instagram:hover {color: #fd1d1d;}"))),
                                    tags$li(class = "dropdown",style = "margin-right: 15px; display: inline-block;", 
                                            a(href = "https://twitter.com/DETRAN_PA",class = "fa fa-twitter",target = "_blank",title = "Twitter",
                                              style = "color: #1da1f2; transition: color 0.3s;"),tags$style(HTML(".fa-twitter:hover {color: #0d95e8;}"))),
                                    tags$li(class = "dropdown",style = "margin-right: 15px; display: inline-block;", 
                                            a(href = "https://github.com/MarioDhiego", icon("github"), "Suporte", target = "_blank", title = "Suporte",style = "color: #333; transition: color 0.3s;"),
                                            tags$style(HTML(".fa-github:hover {color: #6e6e6e;}")))
                    ),
                    dashboardSidebar(
                      tags$img(
                        src = "detran1.jpeg",
                        width = 230,
                        height = 120
                      ),
                      sidebarMenu(
                        menuItem("RECURSOS HUMANOS", tabName = "socio1", icon = icon("book")),
                        menuItem("EQUIPAMENTOS", tabName = "coleta1", icon = icon("wrench")),
                        menuItem("SERVIÇOS", tabName = "destino1", icon = icon("tasks")),
                        menuItem("ESTRUTURAL", tabName = "estrutura1", icon = icon("building")),
                        selectInput("municipio", "MUNICÍPIOS:", 
                                    choices = unique(dados$Municípios), 
                                    selected = unique(dados$Municípios)[56]),
                        selectInput("regiao", "REGIÃO DE INTEGRAÇÃO:", 
                                    choices = unique(dados$`Região Integração`), 
                                    selected = unique(dados$`Região Integração`)[1]),
                        selectInput("tipo_ciretran", "TIPO DE CIRETRAN:", 
                                    choices = unique(dados$`Tipo Ciretran`), 
                                    selected = unique(dados$`Tipo Ciretran`)[1]),
                        selectInput("situacao_imovel", "SITUAÇÃO DO IMÓVEL:", 
                                    choices = unique(dados$`Situação do Imóvel`), 
                                    selected = unique(dados$`Situação do Imóvel`)[1]),
                        actionButton("reset_button", "Reiniciar Filtros", class = "btn-success")
                      )
                    ),
                    dashboardBody(
                      tags$style(".btn-success { background-color: #004c99; color: white; border: none; }"),
                      fluidRow(
                        valueBoxOutput("valuebox_servidores", width = 3),
                        valueBoxOutput("valuebox_agentes", width = 3),
                        valueBoxOutput("valuebox_vistoriador", width = 3),
                        valueBoxOutput("valuebox_assistente", width = 3)
                      ),
                      
                      
                      tabItem(
                        tabName = "analises",
                      fluidRow(
                        tabBox(title = "", width = 12,
                               tabPanel("Vistoriador",
                                        fluidRow(
                        box(
                          title = "Vistoriador",width = 7, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          plotlyOutput("grafico_vistoriador")  %>% withSpinner(color = "#17a2b8")
                        )
                        )
                        ),
                        tabPanel("AFT",
                                 fluidRow(
                        box(
                          title = "Agente de Trânsito (AFT)", width = 7,status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          plotlyOutput("grafico_aft")  %>% withSpinner(color = "#17a2b8")
                        )
                        )
                        )
                        ,
                        tabPanel("Auxiliar",
                                 fluidRow(
                        
                        box(
                          title = "Auxiliar de Trânsito", width = 7, status = "primary", solidHeader = TRUE, collapsible = TRUE,
                          plotlyOutput("grafico_auxiliar")  %>% withSpinner(color = "#17a2b8")
                        )
                                 )
                        )
                      )
                      )
                      ),

                      
                      footer = dashboardFooter(left = "COPYRIGHT© 2025 DETRAN-PA - Todos os Direitos Reservados.", right = "Belém - PA")
                    )
)

#-------------------------------------------------------------------------------
# SERVER

server <- function(input, output, session) {
  
  # Filtrar dados com base no município selecionado
  dados_filtrados <- reactive({
    if (is.null(input$municipio)) {
      return(dados) # Caso nenhum município tenha sido selecionado, retorna todos os dados
    } else {
      dados %>% filter(Municípios == input$municipio)
    }
  })
  
  # Adicionar o valueBox de Servidores
  output$valuebox_servidores <- renderValueBox({
    total_servidores <- sum(dados_filtrados()$Servidores, na.rm = TRUE)
    valueBox(
      formatC(total_servidores, format = "d", big.mark = ","), 
      "SERVIDORES", 
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  # Adicionar o valueBox de Agentes
  output$valuebox_agentes <- renderValueBox({
    total_agentes <- sum(dados_filtrados()$N_Agentes, na.rm = TRUE)
    valueBox(
      formatC(total_agentes, format = "d", big.mark = ","), 
      "AGENTES DE TRÂNSITO", 
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  # Adicionar o valueBox de Vistoriador
  output$valuebox_vistoriador <- renderValueBox({
    total_vistoriador <- sum(dados_filtrados()$N_Vistoriador, na.rm = TRUE)
    valueBox(
      formatC(total_vistoriador, format = "d", big.mark = ","), 
      "VISTORIADOR", 
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  # Adicionar o valueBox de Assistente
  output$valuebox_assistente <- renderValueBox({
    total_assistente <- sum(dados_filtrados()$N_Assistente, na.rm = TRUE)
    valueBox(
      formatC(total_assistente, format = "d", big.mark = ","), 
      "ASSISTENTE DE TRÂNSITO", 
      icon = icon("users"),
      color = "aqua"
    )
  })
  
  # Botão de reset
  observeEvent(input$reset_button, {
    updateSelectInput(session, "municipio", selected = unique(dados$Municípios)[56])
    updateSelectInput(session, "regiao", selected = unique(dados$`Região Integração`)[1])
    updateSelectInput(session, "tipo_ciretran", selected = unique(dados$`Tipo Ciretran`)[1])
    updateSelectInput(session, "situacao_imovel", selected = unique(dados$`Situação do Imóvel`)[1])
  })
  
  # Gerar gráfico de barras interativo para a variável Vistoriador
  output$grafico_vistoriador <- renderPlotly({
  
    if ("Vistoriador" %in% colnames(dados)) {
     
      vistoriador_count <- dados %>%
        group_by(Vistoriador) %>%
        summarise(contagem = n())
      
      vistoriador_count <- vistoriador_count %>%
        mutate(percentual = contagem / sum(contagem) * 100)
      
      
      p1<- ggplot(vistoriador_count, aes(x = Vistoriador, y = contagem, fill = Vistoriador)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = paste0(round(percentual, 1), "%")), 
                  position = position_stack(vjust = 0.5), color = "white") +
        labs(title = "", x = "", y = "N° de Ciretrans") +
        theme_minimal()
      
    
      ggplotly(p1)
    }
  })
  
  output$grafico_aft <- renderPlotly({
    
    if ("AFT" %in% colnames(dados)) {
      
      aft_count <- dados %>%
        group_by(AFT) %>%
        summarise(contagem = n())
      
      aft_count <- aft_count %>%
        mutate(percentual = contagem / sum(contagem) * 100)
      
      
      p2 <- ggplot(aft_count, aes(x = AFT, y = contagem, fill = AFT)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = paste0(round(percentual, 1), "%")), 
                  position = position_stack(vjust = 0.5), color = "white") +
        labs(title = "", x = "", y = "N° de Ciretrans") +
        theme_minimal()
  
      ggplotly(p2)
    }
  })
  
  
  output$grafico_auxiliar <- renderPlotly({
    
    if ("Auxiliar" %in% colnames(dados)) {
      
      auxiliar_count <- dados %>%
        group_by(Auxiliar) %>%
        summarise(contagem = n())
      
      auxiliar_count <- auxiliar_count %>%
        mutate(percentual = contagem / sum(contagem) * 100)
      
      
      p3 <- ggplot(auxiliar_count, aes(x = Auxiliar, y = contagem, fill = Auxiliar)) +
        geom_bar(stat = "identity", show.legend = FALSE) +
        geom_text(aes(label = paste0(round(percentual, 1), "%")), 
                  position = position_stack(vjust = 0.5), color = "white") +
        labs(title = "", x = "", y = "N° de Ciretrans") +
        theme_minimal()
      
      ggplotly(p3)
    }
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

#-------------------------------------------------------------------------------
# APP

shinyApp(ui, server)
