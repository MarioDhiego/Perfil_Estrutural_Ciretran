

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)

# Define UI
ui <- page_sidebar(
  title = "PERFIL ESTRUTURAL DAS CIRETRAN'S",
  theme = bs_theme(
    bootswatch = "lux",
    primary = "#007BFF" # Cor azul
  ),
  
  sidebar = div(
    # Menu de navegação
    nav(
      id = "sidebar_menu",
      nav_item("Recursos Humanos", tabName = "recursos_humanos"),
      nav_item("Estrutural", tabName = "estrutural"),
      nav_item("Serviços", tabName = "servicos")
    ),
    # Filtros aparecem depois do menu
    br(),
    hr(),
    selectInput(
      inputId = "ciretran_filter",
      label = "Municípios:",
      choices = c("Ananindeua", "Belém", "Castanhal", "Bragança", 
                  "Cametá", "Dom Eliseu", "Eldorado", "Itaituba", 
                  "Jacundá", "Marabá", "Parauapebas", "Santarém"),
      selected = "Belém"
    ),
    selectInput(
      inputId = "ciretran_type_filter",
      label = "Tipos de CIRETRAN:",
      choices = c("Tipo A", "Tipo B", "Homologada"),
      selected = "Tipo A"
    )
  ),
  
  # Main content area com abas
  tabsetPanel(
    id = "sidebar_menu",
    
    # Aba principal com as caixas de valores
    tabPanel(
      title = "Recursos Humanos",
      fluidRow(
        div(class = "col-md-4", 
            div(class = "card text-white bg-primary mb-3",
                div(class = "card-body",
                    h5(class = "card-title", "Municípios"),
                    h3(class = "card-text", textOutput("num_municipios"))
                )
            )
        ),
        div(class = "col-md-4", 
            div(class = "card text-white bg-success mb-3",
                div(class = "card-body",
                    h5(class = "card-title", "Serviços Realizados"),
                    h3(class = "card-text", "320")
                )
            )
        ),
        div(class = "col-md-4", 
            div(class = "card text-white bg-info mb-3",
                div(class = "card-body",
                    h5(class = "card-title", "CIRETRAN's Ativas"),
                    h3(class = "card-text", "12")
                )
            )
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
          # Gráfico 1: Situação do Imóvel
          column(6,  # A primeira coluna ocupa metade da largura
                 h3("Situação do Imóvel"),
                 plotlyOutput("grafico_situacao_imovel")
          ),
          # Gráfico 2: Possui Computadores
          column(6,  # A segunda coluna ocupa a outra metade da largura
                 h3("Possui Computadores"),
                 plotlyOutput("grafico_computadores")
          )
        ),
        
        fluidRow(
          # Gráfico 3: Possui Impressoras
          column(6,
                 h3("Possui Impressoras"),
                 plotlyOutput("grafico_impressoras")
          ),
          # Gráfico 4: Possui Extintor
          column(6,
                 h3("Possui Extintor"),
                 plotlyOutput("grafico_extintor")
          )
        ),
        
        fluidRow(
          # Gráfico 5: Possui Ar
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

# Define Server
server <- function(input, output, session) {
  
  # Número de municípios
  municipios <- c("Ananindeua", "Belém", "Castanhal", "Bragança", 
                  "Cametá", "Dom Eliseu", "Eldorado", "Itaituba", 
                  "Jacundá", "Marabá", "Parauapebas", "Santarém")
  
  output$num_municipios <- renderText({
    length(municipios) # Conta o número de municípios
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

# Run App
shinyApp(ui, server)
