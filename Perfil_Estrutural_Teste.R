
library(shiny)
library(shinydashboard)
library(bslib)
library(leaflet)
library(ggplot2)
library(plotly)
library(dplyr)

# Define UI
ui <- page_sidebar(
  title = "PERFIL ESTRUTURAL DAS CIRETRAN'S",
  theme = bs_theme(
    bootswatch = "lux",
    primary = "#007BFF" # Cor azul
  ),
  
  sidebar = div(
    # Menu de navegação
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Recursos Humanos", tabName = "recursos_humanos"),
      menuItem("Estrutural", tabName = "estrutural"),
      menuItem("Serviços", tabName = "servicos")
    ),
    # Filtros aparecem depois do menu
    br(),
    hr(),
    selectInput(
      inputId = "ciretran_filter",
      label = "Municípios:",
      choices = c("Ananindeua", 
                  "Abaetetuba", 
                  "Altamira", 
                  "Belém", 
                  "Bragança", 
                  "Breves", 
                  "Castanhal", 
                  "Capanema", 
                  "Cametá", 
                  "Dom Eliseu", 
                  "Eldorado dos Carajás",
                  "Itaituba", 
                  "Jacundá", 
                  "Marabá", 
                  "Parauapebas", 
                  "Santarém"),
      selected = "Belém"
    )
  ),
  
  # Main content area com abas
  tabsetPanel(
    id = "main_tabs",
    
    # Aba Recursos Humanos com o mapa
    tabPanel(
      title = "Recursos Humanos",
      fluidRow(
        column(12,
               h3("Mapa de Municípios"),
               leafletOutput("mapa_municipios", height = 600)  # Mapa interativo
        )
      )
    ),
    
    # Aba Estrutural com gráficos alinhados
    tabPanel(
      title = "Estrutural",
      div(
        h2("Estrutural"),
        p("Informações sobre a estrutura organizacional."),
        fluidRow(
          column(6,
                 h3("Situação do Imóvel"),
                 plotlyOutput("grafico_situacao_imovel")
          ),
          column(6,
                 h3("Possui Computadores"),
                 plotlyOutput("grafico_computadores")
          )
        ),
        
        fluidRow(
          column(6,
                 h3("Possui Impressoras"),
                 plotlyOutput("grafico_impressoras")
          ),
          column(6,
                 h3("Possui Extintor"),
                 plotlyOutput("grafico_extintor")
          )
        ),
        
        fluidRow(
          column(6,
                 h3("Possui Ar"),
                 plotlyOutput("grafico_ar")
          )
        )
      )
    ),
    
    # Aba de Serviços
    tabPanel(
      title = "Serviços",
      div(
        h2("Serviços"),
        p("Detalhes sobre os serviços oferecidos.")
      )
    )
  ),
  
  # Footer
  footer = div(
    style = "text-align: center; padding: 10px; background-color: #f8f9fa; color: #6c757d;",
    HTML("&copy; Todos os direitos autorais.")
  )
)

# Coordenadas de alguns municípios com a coluna 'tipo'
coordenadas_municipios <- data.frame(
  municipio = c("Ananindeua", 
                "Abaetetuba", 
                "Altamira", 
                "Belém", 
                "Bragança", 
                "Breves", 
                "Castanhal", 
                "Capanema", 
                "Cametá", 
                "Dom Eliseu", 
                "Eldorado dos Carajás",
                "Itaituba", 
                "Jacundá", 
                "Marabá", 
                "Parauapebas", 
                "Santarém"),
  lat = c(-1.373, -1.455, -5.914, -1.040, -3.123, -3.513, -5.658, -4.265, 
          -3.747, -5.368, -6.072, -2.429, -6.266, -5.292, -6.189, -2.437),
  lng = c(-48.308, -48.497, -53.128, -46.732, -50.248, -47.416, -51.150, 
          -55.983, -47.707, -49.114, -49.892, -54.711, -55.456, -50.016, -49.892, -54.701),
  tipo = c("Tipo B", "Tipo A", "Tipo A", "Tipo A", "Tipo A", "Tipo A", 
           "Tipo A", "Tipo A", "Tipo A", "Tipo B", "Tipo B", "Tipo A", 
           "Tipo B", "Tipo A", "Tipo A", "Tipo A")
)

# Define Server
server <- function(input, output, session) {
  
  # Mapa dos municípios
  output$mapa_municipios <- renderLeaflet({
    # Filtra as coordenadas para o município selecionado
    municipio_selecionado <- coordenadas_municipios %>%
      filter(municipio == input$ciretran_filter)
    
    leaflet(data = coordenadas_municipios) %>%
      addTiles() %>%
      addMarkers(lng = coordenadas_municipios$lng, lat = coordenadas_municipios$lat,
                 popup = ~paste("<b>", municipio, "</b><br>Tipo: ", tipo)) %>%
      setView(lng = municipio_selecionado$lng, lat = municipio_selecionado$lat, zoom = 6)
  })
  
  # Dados fictícios para os gráficos
  situacao_imovel <- data.frame(
    situacao = c("Alugado", "Cedido", "Proprio"),
    quantidade = c(5, 2, 8)
  )
  
  computadores <- data.frame(
    possui = c("Sim", "Não"),
    quantidade = c(10, 5)
  )
  
  impressoras <- data.frame(
    possui = c("Sim", "Não"),
    quantidade = c(7, 8)
  )
  
  extintor <- data.frame(
    possui = c("Sim", "Não"),
    quantidade = c(12, 3)
  )
  
  ar <- data.frame(
    possui = c("Sim", "Não"),
    quantidade = c(11, 4)
  )
  
  # Gráfico 1: Situação do Imóvel
  output$grafico_situacao_imovel <- renderPlotly({
    p <- ggplot(situacao_imovel, aes(x = situacao, y = quantidade, fill = situacao)) +
      geom_bar(stat = "identity") +
      labs(title = "Situação do Imóvel", x = "Situação", y = "Quantidade") +
      theme_minimal() +
      scale_fill_manual(values = c("#FF9999", "#66B3FF", "#99FF99"))
    
    ggplotly(p)
  })
  
  # Gráfico 2: Possui Computadores
  output$grafico_computadores <- renderPlotly({
    p <- ggplot(computadores, aes(x = possui, y = quantidade, fill = possui)) +
      geom_bar(stat = "identity") +
      labs(title = "Possui Computadores", x = "Resposta", y = "Quantidade") +
      theme_minimal() +
      scale_fill_manual(values = c("#FF6666", "#66FF66"))
    
    ggplotly(p)
  })
  
  # Gráfico 3: Possui Impressoras
  output$grafico_impressoras <- renderPlotly({
    p <- ggplot(impressoras, aes(x = possui, y = quantidade, fill = possui)) +
      geom_bar(stat = "identity") +
      labs(title = "Possui Impressoras", x = "Resposta", y = "Quantidade") +
      theme_minimal() +
      scale_fill_manual(values = c("#FF6666", "#66FF66"))
    
    ggplotly(p)
  })
  
  # Gráfico 4: Possui Extintor
  output$grafico_extintor <- renderPlotly({
    p <- ggplot(extintor, aes(x = possui, y = quantidade, fill = possui)) +
      geom_bar(stat = "identity") +
      labs(title = "Possui Extintor", x = "Resposta", y = "Quantidade") +
      theme_minimal() +
      scale_fill_manual(values = c("#FF6666", "#66FF66"))
    
    ggplotly(p)
  })
  
  # Gráfico 5: Possui Ar
  output$grafico_ar <- renderPlotly({
    p <- ggplot(ar, aes(x = possui, y = quantidade, fill = possui)) +
      geom_bar(stat = "identity") +
      labs(title = "Possui Ar", x = "Resposta", y = "Quantidade") +
      theme_minimal() +
      scale_fill_manual(values = c("#FF6666", "#66FF66"))
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
