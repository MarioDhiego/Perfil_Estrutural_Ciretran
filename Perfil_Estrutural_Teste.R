

#-------------------------------------------------------------------------------
# PACOTES
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
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
library(memoise)  # Pacote para caching
#-------------------------------------------------------------------------------
# Carregar Dados
#setwd("C:/Users/usuario/Documents/Perfil_Estrutural_Cretran")
dados <- read_excel("C:/Users/mario.valente/Documents/github_2024/Perfil_Estrutural_Ciretran-main/Banco_Pesquisa_Ciretran.xlsx")

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
    geom_text(
      stat = "count",
      aes(label = scales::percent(..count.. / sum(..count..))),
      position = position_stack(vjust = 0.5),
      color = "white"
    ) +
    labs(title = title, x = "", y = "Nº de Entrevistados") +
    theme_minimal()
}

#-------------------------------------------------------------------------------
# UI

ui <- dashboardPage(
  title = "Dashboard",
  skin = "blue",
  dashboardHeader(
    title = "Perfil Estatístico das Ciretran's",
    titleWidth = 390,
    tags$li(
      class = "dropdown",
      style = "margin-right: 15px; display: inline-block;",
      a(
        href = "https://www.facebook.com/detranPARA",
        class = "fa fa-facebook fa-lg",
        target = "_blank",
        title = "Facebook",
        style = "color: #3b5998; transition: color 0.3s;"
      ),
      tags$style(HTML(".fa-facebook:hover {color: #8b9dc3;}"))
    ),
    tags$li(
      class = "dropdown",
      style = "margin-right: 15px; display: inline-block;",
      a(
        href = "https://www.instagram.com/detranpa_",
        class = "fa fa-instagram",
        target = "_blank",
        title = "InstaGram",
        style = "color: #e1306c; transition: color 0.3s;"
      ),
      tags$style(HTML(".fa-instagram:hover {color: #fd1d1d;}"))
    ),
    tags$li(
      class = "dropdown",
      style = "margin-right: 15px; display: inline-block;",
      a(
        href = "https://twitter.com/DETRAN_PA",
        class = "fa fa-twitter",
        target = "_blank",
        title = "Twitter",
        style = "color: #1da1f2; transition: color 0.3s;"
      ),
      tags$style(HTML(".fa-twitter:hover {color: #0d95e8;}"))
    ),
    tags$li(
      class = "dropdown",
      style = "margin-right: 15px; display: inline-block;",
      a(
        href = "https://github.com/MarioDhiego",
        icon("github"),
        "Suporte",
        target = "_blank",
        title = "Suporte",
        style = "color: #333; transition: color 0.3s;"
      ),
      tags$style(HTML(".fa-github:hover {color: #6e6e6e;}"))
    )
  ),
  dashboardSidebar(
    tags$img(
      src = "detran1.jpeg",
      width = 230,
      height = 150
    ),
    sidebarMenu(
      menuItem(
        "CIRETRAN'S",
        tabName = "anuario",
        icon = icon("address-card"),
        menuSubItem("SOBRE CIRETRAN", tabName = "sobre1", icon =
                      icon("book")),
        menuSubItem(
          "VÍDEO INSTITUCIONAL",
          tabName = "video1",
          icon = icon("video")
        )
      ),
      menuItem(
        "CLASSIFICAÇÃO",
        tabName = "catCiretran",
        icon = icon("book"),
        menuSubItem("TIPO A", tabName = "tipoA", icon = icon("book")),
        menuSubItem("TIPO B", tabName = "tipoB", icon = icon("book"))
      ),
      menuItem(
        "RECURSOS HUMANOS",
        tabName = "socio1",
        icon = icon("book")
      ),
      menuItem(
        "EQUIPAMENTOS",
        tabName = "equipamento1",
        icon = icon("wrench")
      ),
      menuItem(
        "ESTRUTURAL",
        tabName = "estrutura1",
        icon = icon("building")
      ),
      menuItem("SERVIÇOS", tabName = "servico1", icon = icon("tasks")),
      menuItem("VISITA TÉCNICA", tabName = "visita1", icon = icon("book")),
      selectInput(
        "municipio",
        "MUNICÍPIOS:",
        choices = unique(dados$Municípios),
        selected = unique(dados$Municípios)[56]
      ),
      selectInput(
        "regiao",
        "REGIÃO DE INTEGRAÇÃO:",
        choices = unique(dados$`Região Integração`),
        selected = unique(dados$`Região Integração`)[1]
      ),
      selectInput(
        "tipo_ciretran",
        "TIPO DE CIRETRAN:",
        choices = unique(dados$`Tipo Ciretran`),
        selected = unique(dados$`Tipo Ciretran`)[1]
      ),
      selectInput(
        "situacao_imovel",
        "SITUAÇÃO DO IMÓVEL:",
        choices = unique(dados$`Situação do Imóvel`),
        selected = unique(dados$`Situação do Imóvel`)[1]
      ),
      actionButton("reset_button", "Reiniciar Filtros", class = "btn-success")
    )
  ),
  dashboardBody(
    tags$style(
      ".btn-success { background-color: #004c99; color: white; border: none; }"
    ),
    
    tabItems(
      tabItem(
        tabName = "sobre1",
        tabBox(
          id = "t1",
          width = 12,
          tabPanel(
            "CIRETRAN'S",
            icon = icon("address-card"),
            fluidRow(
              column(
                width = 8,
                position = "left",
                solidHeader = TRUE,
                tags$img(
                  id = "foto1",
                  src = "ciretran.jpg",
                  controls = "controls",
                  width = 700,
                  height = 550
                ),
                tags$br(),
                tags$a("Photo by Asdecom"),
                align = "left"
              ),
              column(
                width = 4,
                tags$br(),
                tags$p(
                  style = "text-align:justify;font-si24pt",
                  strong(
                    "As Circunscrições Regionais de Trânsito (CIRETRAN’s) são unidades administrativas do DETRAN-PA, sediadas nos Municípios do interior do Estado, com competência para desenvolver ações de planejamento, controle, execução, fiscalização e avaliação das atividades relacionadas ao cadastro de veículos, ao processo de habilitação de condutores, operação, fiscalização, engenharia e educação de trânsito, no âmbito de sua circunscrição, previstas no CTB."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si24pt",
                  strong(
                    "As Circunscrições Regionais de Trânsito são classificadas nas categorias A e B, e suas classificações e implantações devem ser aprovadas pelo CONADM e homologadas por ato do Chefe do Poder Executivo. Conforme dito anteriormente, as Circunscrições Regionais de Trânsito -- CIRETRAN's possuem duas classificações, determinadas como tipo A e tipo B. "
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si24pt",
                  strong(
                    "As CIRETRANS tipo A realizam os mesmos serviços da Sede-Belém, já as CIRETRANS tipo B, com exceção de Canaã dos Carajás e Santa Izabel do Pará, não realizam exames de legislação e prático de trânsito, conforme disposto na Lei nº 7.594/2011, e possuem duas classificações de acordo com a Lei nº 7.594/2011 artigos 17 E 18:"
                  )
                )
              )
            )
          ),
          #===============================================================================#
          tabPanel("ATIVIDADES", icon = icon("hospital"), fluidRow(
            column(
              width = 4,
              position = "center",
              solidHeader = TRUE,
              tags$br(),
              tags$p(
                style = "text-align:justify;font-si20pt",
                strong(
                  "As CIRETRANS e suas delegacias são interligadas com o DETRAN-PA nos municípios onde está instalada, em todo o interior do estado, tendo a responsabilidade principal de exigir e impor a obediência às leis de trânsito e seu devido cumprimento, representando o DETRAN nas cidades onde não existe a delegacia desse órgão."
                )
              ),
              tags$br(),
              tags$p(
                style = "text-align:justify;font-si20pt",
                strong(
                  "Estando interligado com o DETRAN-PA, as CIRETRAN’S prestam todos os serviços necessários aos usuários de veículos de sua cidade, atende também toda a população procurando soluções e facilitando as necessidades dos condutores e proprietários de veículos, sem haver a necessidade de procurar delegacias de outras cidades ou mesmo precisar ir para longe solucionar problemas:"
                )
              ),
            ),
            column(
              width = 4,
              position = "center",
              solidHeader = TRUE,
              tags$br("ATIVIDADES"),
              tags$br(),
              tags$p(
                style = "text-align:justify;font-si20pt",
                strong("1) Consultas sobre infrações recebidas;"),
                tags$br(),
                strong("2) Consulta sobre pontos perdidos na CNH;"),
                tags$br(),
                strong("3) Informações constantes do arquivo sobre os veículos;"),
                tags$br(),
                strong("4) Imposto sobre Propriedade de Veículos Automotores (IPVA);"),
                tags$br(),
                strong("5) Licenciamento de veículos;"),
                tags$br(),
                strong("6) Registro de veículos automotores;"),
                tags$br(),
                strong("7) Seguro Obrigatório (DPVAT);"),
                tags$br(),
                strong("8) Emissão de CNH;"),
                tags$br(),
                strong("9) Apreensão de CNH;"),
                tags$br(),
                strong("10) Apreensão de veículos;"),
                tags$br(),
                strong("11) Liberação de Documentos e de Veículos;")
              )
            )
          )
          ),
          tabPanel("MATERIAL E MÉTODOS",
                   icon = icon("book"),
                   fluidRow(
                     column(
                       width = 4,
                       position = "center",
                       tags$br(),
                       h3("OBJETIVO GERAL", align = "center"),
                       tags$br(),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong(
                           "Traçar o Perfil Estatístico das Agências Regionais do DETRAN-PA."
                         )
                       ),
                       tags$br(),
                       h3("OBJETIVOS ESPECÍFICOS", align = "center"),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong(" ")
                       ),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong("1) Levantar o quantitativo de Recursos Humanos;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("2) Levantar o quantitativo e Tipos de Equipamentos ;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("3) Levantar os Tipos de Serviços Oferecidos na Ciretran's;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("4) Levantar as Características Estruturantes das Cietran's.")
                       ),
                       tags$p(style = "text-align: justify;font-si20pt")
                     ),
                     column(
                       width = 4,
                       position = "center",
                       tags$br(),
                       h3("ETAPAS OPERACIONAIS", align = "center"),
                       tags$br(),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong("1) Visita Técnica as CIRETRAN'S do Tipo A e B;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("2) Reunião de Alinhamento com o Gerente da CIRETRAN;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("3) Aplicação do Questionário;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("5) Levantamento das Prestadoras de Serviços;")
                       ),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("6) Estudo Observacional;")
                       ),
                       tags$br(),
                       h3("MÉTRICA", align = "center"),
                       tags$br(),
                       tags$p(
                         style = "text-align:justify;font-si20pt",
                         strong(
                           "Para a coleta dos dados foi utilizado um instrumento semiestruturado composto por 57 itens que versam sobre as Atividades Desenvolvidas nas Agências de Trânsito. A estrutura do questionário contém quatro subescalas, que medem Características Socioeconômicas, Tipos de Equipamentos, Níveis de Serviços e Dimensões Estruturantes."
                         )
                       )
                     )
                   )
          ),

          tabPanel("RECURSO COMPUTACIONAL",
                   icon = icon("computer"),
                   fluidRow(
                     column(
                       width = 4,
                       position = "center",
                       tags$br(),
                       h3("SOFTWARE", align = "center"),
                       tags$br(),
                       solidHeader = TRUE,
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong(
                           "Para Criação do Painel em Formato Web com Dasboard Interativos, foi Desenvolvido um script em Linguagem de Programação R-PROJECT Versão 4.4.1, no formato de Projeto de Software Livre de Código Aberto (open source), ou seja, pode ser utilizado sem custos de licença (R DEVELOPMENT CORE TEAM, 2024)"
                         )
                       ),
                       tags$br(),
                       tags$img(
                         id = "foto2",
                         src = "R.jpg",
                         controls = "controls",
                         width = 180, height = 150
                       ),
                       tags$br(),
                       tags$a("Software R",
                              href = "https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe"
                       ),
                       tags$br(),
                     ),
                     column(
                       width = 4,
                       position = "center", solidHeader = TRUE,
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$p(
                         style = "text-align: justify;font-si20pt",
                         strong("Foi utilizado um Ambiente de Desenvolvmento Integrado (IDE) Chamado Rstudio Versão 1.4.1.7, utilizando um Processo de Extração-Transformação-Carga(ETL) com uso de Várias bibliotecas (library), para o Ambiente Windows")
                       ),
                       tags$br(),
                       tags$br(),
                       tags$img(
                         id = "foto3",
                         src = "RStudio.png",
                         controls = "controls",
                         width = 190, height = 170
                       ),
                       tags$br(),
                       tags$a("RStudio",
                              href = "https://download1.rstudio.org/electron/windows/RStudio-2023.09.1-494.exe"
                       ),
                       tags$br(),
                     )
                   )
          ),
          tabPanel(
            "CRÉDITOS",
            icon = icon("phone"),
            fluidRow(
              column(
                width = 4,
                position = "center",
                solidHeader = TRUE,
                tags$br(),
                h3("DIREITOS AUTORAIS", align = "center"),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong("DEPARTAMENTO DE TRÂNSITO DO ESTADO DO PARÁ")
                ),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong("RENATA MIRELA COELHO")
                ),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong("AVENIDA: AUGUSTO MONTENEGRO KM 03 S/N")
                ),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong("CEP: 66635-918 - PARQUE VERDE - BELÉM - PARÁ")
                ),
                tags$a("https://www.detran.pa.gov.br",
                       href = "https://www.detran.pa.gov.br"
                ),
                tags$br(),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "Esta publicação deve ser citada como: Departamento de Trânsito do Estado do Pará (DETRAN-PA), Programa de Sustentatabilidade Ambiental Por Todo Pará, 2025 (LC/PUB.2025/1-P), Belém, 2025."
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "A autorização para a reprodução total ou parcial deste trabalho deve ser solicitada ao Departamento de Trânsito do Estado do Pará, Gerência de Treinamento, getren@detran.pa.gov.br. Os Estados membros das Nações Unidas e suas instituições governamentais podem reproduzir este trabalho sem autorização prévia. Solicita-se apenas que mencionem a fonte e informem ao DETRAN-PA de tal reprodução."
                  )
                ),
                tags$br(),
              ),
              column(
                width = 4,
                position = "center",
                solidHeader = TRUE,
                tags$br(),
                leafletOutput("mapa"),
              )
            )
          ),
          tabPanel(
            "RESPONSÁVEL TÉCNICO",
            fluidRow(
              column(
                width = 5,
                position = "center",
                tags$br(),
                h3("EQUIPE TÉCNICA", align = "center"),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  tags$br(),
                  strong(
                    "Projeto Perfil Estatístico das CIRETRAN'S, Desenvolvido na Coordenadoria do Núcleo de Planejamento, sob a tutela da Gerência de Estatística (Sr. Kleber Salim).
",
                    tags$br(),
                    tags$a("", href = "kleber.salim@detran.pa.gov.br")
                  ),
                  tags$br(),
                  strong(
                    "O Projeto é Executado sob a Supervisão Técnica do Servidor, Analista de Trânsito, Sr:",
                    tags$a("Mário Diego Valente (Estatístico)", href = "mario.valente@detran.pa.gov.br")
                  )
                ),
                tags$br(),
                tags$p(
                  style = "text-align: justify;font-si20pt",
                  strong(
                    "Reclamações, sugestões, críticas e elogios relacionados ao Projeto do DETRAN-PA podem ser registrados na Gerência de Estatística de Trânsito."
                  )
                )
              )
            )
          ),
          
          
          
          
        )
      ),
      tabItem(
        tabName = "tipoA",
        tabPanel("TIPOS DE CIRETRAN'S", icon = icon("address-card"), fluidRow(
          box(
            width = 12,
            title = tags$div("CIRETRAN DO TIPO A", style = "text-align: center"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            headerBorder = TRUE,
            div(
              style = "text-align: center;",
              class = "elemente",
              DiagrammeROutput("ciretrantipoA")
            )
          )
        ))
      ),
      tabItem(
        tabName = "tipoB",
        tabPanel("TIPOS DE CIRETRAN'S", icon = icon("address-card"), fluidRow(
          box(
            width = 12,
            title = "CIRETRAN DO TIPO B",
            style = "text-align: center",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            headerBorder = TRUE,
            div(
              style = "text-align: center;",
              class = "elemente",
              DiagrammeROutput("ciretrantipoB")
            )
          )
        ))
      ),
      
      tabItem(tabName = "socio1", 
              fluidRow(
                valueBoxOutput("valuebox_servidores", width = 2),
                valueBoxOutput("valuebox_comissionado", width = 2),
                valueBoxOutput("valuebox_analista", width = 2),
                valueBoxOutput("valuebox_agentes", width = 2),
                valueBoxOutput("valuebox_vistoriador", width = 2),
                valueBoxOutput("valuebox_assistente", width = 2)
              ),
              
        fluidRow(
        tabBox(
          title = "",
          width = 12,
          tabPanel("Vistoriador", fluidRow(
            column(
              width = 7,
              box(
                title = "Vistoriador de Trânsito",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput("grafico_vistoriador") %>% withSpinner(color = "#17a2b8")
              )
            ), column(
              width = 5,
              box(
                title = "Tabela Vistoriador",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                DTOutput("tabela_vistoriador")
              )
            )
          )),
          tabPanel("AFT", fluidRow(
            column(
              width = 7,
              # 50% da largura da tela
              box(
                title = "Agente de Fiscalização de Trânsito",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput("grafico_aft")  %>% withSpinner(color = "#17a2b8")
              )
            ), column(
              width = 5,
              # 50% da largura da tela
              box(
                title = "AFT X Região de Integração",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                DTOutput("tabela_aft")
              )
            )
          )),
          tabPanel("Auxiliar", fluidRow(
            column(
              width = 7,
              # 50% da largura da tela
              box(
                title = "Auxiliar de Trânsito",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput("grafico_auxiliar")  %>% withSpinner(color = "#17a2b8")
              )
            ), column(
              width = 5,
              # 50% da largura da tela
              box(
                title = "Auxiliar X Região de Integração",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                DTOutput("tabela_auxiliar")
              )
            )
          )),
          tabPanel("Assistente", fluidRow(
            column(
              width = 7,
              # 50% da largura da tela
              box(
                title = "Assistente de Trânsito",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput("grafico_assistente")  %>% withSpinner(color = "#17a2b8")
              )
            ), column(
              width = 5,
              # 50% da largura da tela
              box(
                title = "Auxiliar X Região de Integração",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                DTOutput("tabela_assistente")
              )
            )
            
            
          )),
        )
      )),
      tabItem(
        tabName = "equipamento1",
        tabPanel(
          "Escala Likert de Equipamentos",
          icon = icon("address-card"),
          fluidRow(
            box(
              width = 12,
              title = "Perfil de Equipamentos",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              headerBorder = TRUE,
              tags$div(
                style = "display: flex; justify-content: center; align-items: center;",
                plotlyOutput("likertPlot1", width = "auto", height = "auto")
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "servico1",
        tabPanel(
          "Escala Likert de Serviços",
          icon = icon("address-card"),
          fluidRow(
            box(
              width = 12,
              title = "Perfil de Serviços",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              headerBorder = TRUE,
              tags$div(
                style = "display: flex; justify-content: center; align-items: center;",
                plotlyOutput("likertPlot2", width = "auto", height = "auto")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "estrutura1",
        tabPanel(
          "Escala Likert Estrutural",
          icon = icon("address-card"),
          fluidRow(
            box(
              width = 12,
              title = "Perfil Estrtutural",
              status = "primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              headerBorder = TRUE,
              tags$div(
                style = "display: flex; justify-content: center; align-items: center;",
                plotlyOutput("likertPlot3", width = "auto", height = "auto")
              )
            )
          )
        )
      )
    )
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
  
  
  #-------------------------------------------------------------------------------#
  output$ciretrantipoA <- renderDiagrammeR({
    DiagrammeR::DiagrammeR(
      "
      graph TB
      A[DETRAN-PA]-->B[CIRETRAN A]
      B-->C[LEI Nº7594/2011]
      B-->D[LEI Nº432/2019]
      C-->E[SANTARÉM]
      E-->F[CASTANHAL]
      F-->G[MARABÁ]
      G-->H[ABAETETUBA]
      C-->I[ALTAMIRA]
      I-->J[CAPANEMA]
      J-->K[PARAGOMINAS]
      K-->L[TUCURUÍ]
      C-->M[REDENÇÃO]
      M-->N[ITAITUBA]
      N-->O[PARAUAPEBAS]
      O-->P[BREVES]
      D-->Q[BRAGANÇA]
      Q-->R[SÃO FÉLIX DO XINGU]"
    )
  })
  
  
  #-------------------------------------------------------------------------------#
  output$ciretrantipoB <- renderDiagrammeR({
    mermaid(
      "graph TB
  A[LEI Nº7594/2011]-->B[DETRAN-PA]
  B-->C[CIRETRAN TIPO B]
  C-->D[SOURE]
  D-->E[ALENQUER]
  E-->F[ALMEIRIM/M.DOURADO]
  F-->G[MONTE ALEGRE]
  G-->H[ÓBIDOS]
  C-->I[ORIXIMINÁ]
  I-->J[IGUARAPÉ-AÇÚ]
  J-->K[SÃO MIGUEL]
  K-->L[SANTA LUZIA]
  L-->M[TOMÉ-AÇÚ]
  C-->N[ITUPIRANGA]
  N-->O[JACUNDÁ]
  O-->P[RONDON]
  P-->Q[SÃO GERALDO]
  Q-->R[BARCARENA]
  C-->S[IGARAPÉ-MIRI]
  S-->T[MEDICILÂNDIA]
  T-->U[URUARÁ]
  U-->V[CAPITÃO POÇO]
  V-->W[OURILÂNDIA DO NORTE]
  C-->X[DOM ELISEU]
  X-->Y[MÃE DO RIO]
  Y-->Z[NOVO REPARTIMENTO]
  Z-->A1[CONCEIÇÃO DO ARAGUAIA]
  A1-->A2[SANTANA DO ARAGUAIA]
  C-->A3[TUCUMÃ]
  A3-->A4[NOVO PROGRESSO]
  A4-->A5[CANÃA DOS CARAJÁS]
  A5-->A6[CURIONÓPOLIS]
  A6-->A7[RURÓPOLIS]
   C-->A8[ANANINDEUA]
   A8-->A9[CAMETÁ]
   A9-->A10[VIGIA]
   A10-->A11[SALINÓPOLIS]
   A11-->A12[TAILÂNDIA]
   C--> A13[SANTA ISABEL]
   A13--> A14[ELDORADO DOS CARAJÁS]
  ",
      width = 1000,
      align = "center"
    )
  })
  #-------------------------------------------------------------------------------#
  
  #------------------------------------------------------------------------------#
  # ValueBox de Servidores
  output$valuebox_servidores <- renderValueBox({
    total_servidores <- sum(dados_filtrados()$Servidores, na.rm = TRUE)
    valueBox(
      formatC(total_servidores, format = "d", big.mark = ","),
      "SERVIDORES",
      icon = icon("user-friends"),
      color = "aqua"
    )
  })
  #------------------------------------------------------------------------------#
  # ValueBox de Agentes
  output$valuebox_agentes <- renderValueBox({
    total_agentes <- sum(dados_filtrados()$N_Agentes, na.rm = TRUE)
    valueBox(
      formatC(total_agentes, format = "d", big.mark = ","),
      "AGENTES DE TRÂNSITO",
      icon = icon("traffic-light"),
      color = "aqua"
    )
  })
  #------------------------------------------------------------------------------#
  # ValueBox de Vistoriador
  output$valuebox_vistoriador <- renderValueBox({
    total_vistoriador <- sum(dados_filtrados()$N_Vistoriador, na.rm = TRUE)
    valueBox(
      formatC(
        total_vistoriador,
        format = "d",
        big.mark = ","
      ),
      "VISTORIADOR",
      icon = icon("clipboard-check"),
      #clipboard #wrench
      color = "aqua"
    )
  })
  #------------------------------------------------------------------------------#
  # ValueBox de Assistente
  output$valuebox_assistente <- renderValueBox({
    total_assistente <- sum(dados_filtrados()$N_Assistente, na.rm = TRUE)
    valueBox(
      formatC(total_assistente, format = "d", big.mark = ","),
      "ASSISTENTE DE TRÂNSITO",
      icon = icon("user-tie"),
      #user-tie
      color = "aqua"
    )
  })
  #------------------------------------------------------------------------------#
  # ValueBox de Analista
  output$valuebox_analista <- renderValueBox({
    total_analista <- sum(dados_filtrados()$N_Analista, na.rm = TRUE)
    valueBox(
      formatC(total_analista, format = "d", big.mark = ","),
      "ANALISTA DE TRÂNSITO",
      icon = icon("user-cog"),
      color = "aqua"
    )
  })
  #------------------------------------------------------------------------------#
  # ValueBox de Comissionado
  output$valuebox_comissionado <- renderValueBox({
    total_comissionado <- sum(dados_filtrados()$N_Comissionado, na.rm = TRUE)
    valueBox(
      formatC(
        total_comissionado,
        format = "d",
        big.mark = ","
      ),
      "CARGO COMISSIONADO",
      icon = icon("cogs"),
      color = "aqua"
    )
  })
  
  #------------------------------------------------------------------------------#
  # Botão de Reset
  observeEvent(input$reset_button, {
    updateSelectInput(session, "municipio", selected = unique(dados$Municípios)[56])
    updateSelectInput(session, "regiao", selected = unique(dados$`Região Integração`)[1])
    updateSelectInput(session,
                      "tipo_ciretran",
                      selected = unique(dados$`Tipo Ciretran`)[1])
    updateSelectInput(session,
                      "situacao_imovel",
                      selected = unique(dados$`Situação do Imóvel`)[1])
  })
  #------------------------------------------------------------------------------#
  # Gráfico de Vistoriador
  
  output$grafico_vistoriador <- renderPlotly({
    if ("Vistoriador" %in% colnames(dados)) {
      vistoriador_count <- dados %>%
        filter(!is.na(Vistoriador)) %>%  # Filtrando os NAs
        group_by(Vistoriador) %>%
        summarise(contagem = n()) %>%
        mutate(percentual = contagem / sum(contagem) * 100)
      
      vistoriador_count <- vistoriador_count %>%
        mutate(Vistoriador = forcats::fct_relevel(Vistoriador, "Sim", after = 0))
      
      p1 <- ggplot(vistoriador_count,
                   aes(x = Vistoriador, y = contagem, fill = Vistoriador)) +
        geom_bar(stat = "identity", , show.legend = FALSE) +
        geom_text(aes(label = paste0(round(percentual, 1), "%")),
                  position = position_stack(vjust = 0.5),
                  color = "black") +
        labs(title = "", x = "Vistoriador Fixo", y = "N° de Ciretrans") +
        theme_minimal()
      
      plotly_p1 <- ggplotly(p1) %>%
        layout(dragmode = "zoom")
      
      return(plotly_p1)
    }
  })
  #------------------------------------------------------------------------------#
  # Tabela Vistoriador por Região Integração (Corrigindo totais)
  
  output$tabela_vistoriador <- renderDT({
    # Excluir os valores NA para Vistoriador
    vistoriador_table <- dados %>%
      filter(!is.na(Vistoriador)) %>%  # Filtrando os NAs
      group_by(`Região Integração`, Vistoriador) %>%
      summarise(contagem = n(), .groups = 'drop')
    
    # Criar tabela de contingência (pivotar os dados)
    vistoriador_table <- vistoriador_table %>%
      pivot_wider(
        names_from = Vistoriador,
        values_from = contagem,
        values_fill = list(contagem = 0)
      )
    
    # Adicionar totais das linhas (por região)
    vistoriador_table <- vistoriador_table %>%
      rowwise() %>%
      mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
      ungroup() # remover o agrupamento
    
    # Agora, adicionar a linha "Total Geral" no final (soma de todas as regiões)
    total_geral <- vistoriador_table %>%
      summarise(across(c(2, 3, 4), sum, na.rm = TRUE)) %>%
      mutate(`Região Integração` = "Total Geral") %>%
      select(`Região Integração`, everything()) # reorganiza para que "Total Geral" fique na primeira coluna
    
    # Adiciona a linha "Total Geral" na tabela
    vistoriador_table <- bind_rows(vistoriador_table, total_geral)
    
    # Renderizar a tabela com DT
    DT::datatable(vistoriador_table, options = list(pageLength = 13)
                  #,caption = "Tabela Geral de Vistoriador por Região Integração"
                  )
  })
    
    
    
    
    
    #------------------------------------------------------------------------------#
    # Gráfico Agente de Trânsito
    output$grafico_aft <- renderPlotly({
      if ("AFT" %in% colnames(dados)) {
        aft_count <- dados %>%
          filter(!is.na(AFT)) %>%  # Filtrando os NAs
          group_by(AFT) %>%
          summarise(contagem = n())
        
        aft_count <- aft_count %>%
          mutate(percentual = contagem / sum(contagem) * 100) %>%
          mutate(AFT = forcats::fct_relevel(AFT, "Sim", after = 0))
        
        p2 <- ggplot(aft_count, aes(x = AFT, y = contagem, fill = AFT)) +
          geom_bar(stat = "identity", show.legend = FALSE) +
          geom_text(aes(label = paste0(round(percentual, 1), "%")),
                    position = position_stack(vjust = 0.5),
                    color = "black") +
          labs(title = "", x = "Agentes de Trânsito Fixo", y = "N° de Ciretrans") +
          theme_minimal()
        
        ggplotly(p2)
      }
    })
    
    output$tabela_aft <- renderDT({
      # Excluir os valores NA para Vistoriador
      aft_table <- dados %>%
        filter(!is.na(AFT)) %>%  # Filtrando os NAs
        group_by(`Região Integração`, AFT) %>%
        summarise(contagem = n(), .groups = 'drop')
      
      # Criar tabela de contingência (pivotar os dados)
      aft_table <- aft_table %>%
        pivot_wider(
          names_from = AFT,
          values_from = contagem,
          values_fill = list(contagem = 0)
        )
      
      # Adicionar totais das linhas (por região)
      aft_table <- aft_table %>%
        rowwise() %>%
        mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
        ungroup() # remover o agrupamento
      
      # Agora, adicionar a linha "Total Geral" no final (soma de todas as regiões)
      total_geral <- aft_table %>%
        summarise(across(c(2, 3, 4), sum, na.rm = TRUE)) %>%
        mutate(`Região Integração` = "Total Geral") %>%
        select(`Região Integração`, everything()) # reorganiza para que "Total Geral" fique na primeira coluna
      
      # Adiciona a linha "Total Geral" na tabela
      aft_table <- bind_rows(aft_table, total_geral)
      
      # Renderizar a tabela com DT
      DT::datatable(aft_table, options = list(pageLength = 13)
                    #,caption = "Tabela Geral de Vistoriador por Região Integração"
                    )
    })
      
      
      
      #------------------------------------------------------------------------------#
      # Gráfico Auxiliar
      output$grafico_auxiliar <- renderPlotly({
        if ("Auxiliar" %in% colnames(dados)) {
          auxiliar_count <- dados %>%
            filter(!is.na(Auxiliar)) %>%  # Excluir valores NA da variável AFT
            group_by(Auxiliar) %>%
            summarise(contagem = n())
          
          auxiliar_count <- auxiliar_count %>%
            mutate(percentual = contagem / sum(contagem) * 100) %>%
            mutate(Auxiliar = forcats::fct_relevel(Auxiliar, "Sim", after = 0))
          
          p3 <- ggplot(auxiliar_count,
                       aes(x = Auxiliar, y = contagem, fill = Auxiliar)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label = paste0(round(percentual, 1), "%")),
                      position = position_stack(vjust = 0.5),
                      color = "black") +
            labs(title = "", x = "", y = "N° de Ciretrans") +
            theme_minimal()
          
          ggplotly(p3)
        }
      })
      
      output$tabela_auxiliar <- renderDT({
        auxiliar_table <- dados %>%
          filter(!is.na(Auxiliar)) %>%  # Excluir valores NA da variável Auxiliar
          group_by(`Região Integração`, Auxiliar) %>%
          summarise(contagem = n(), .groups = 'drop')
        
        # Criar tabela de contingência (pivotar os dados)
        auxiliar_table <- auxiliar_table %>%
          pivot_wider(
            names_from = Auxiliar,
            values_from = contagem,
            values_fill = list(contagem = 0)
          )
        
        # Adicionar totais das linhas (por região)
        auxiliar_table <- auxiliar_table %>%
          rowwise() %>%
          mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
          ungroup() # Remover agrupamento
        
        # Agora, adicionar a linha "Total Geral" no final (soma de todas as regiões)
        total_geral_auxiliar <- auxiliar_table %>%
          summarise(across(c(2, 3, 4), sum, na.rm = TRUE)) %>%
          mutate(`Região Integração` = "Total Geral") %>%
          select(`Região Integração`, everything()) # Reorganizar para que "Total Geral" fique na primeira coluna
        
        # Adiciona a linha "Total Geral" na tabela
        auxiliar_table <- bind_rows(auxiliar_table, total_geral_auxiliar)
        
        # Renderizar a tabela com DT
        DT::datatable(auxiliar_table, options = list(pageLength = 13)
                      #, caption = "Tabela Geral de Auxiliar por Região Integração"
                      )
      })
        
        
        #------------------------------------------------------------------------------#
        
        #------------------------------------------------------------------------------#
        # Gráfico Assistente
        output$grafico_assistente <- renderPlotly({
          if ("Assistente" %in% colnames(dados)) {
            assistente_count <- dados %>%
              filter(!is.na(Assistente)) %>%  # Excluir valores NA da variável Assistente
              group_by(Assistente) %>%
              summarise(contagem = n())
            
            assistente_count <- assistente_count %>%
              mutate(percentual = contagem / sum(contagem) * 100) %>%
              mutate(Assistente = forcats::fct_relevel(Assistente, "Sim", after = 0))
            p4 <- ggplot(assistente_count,
                         aes(x = Assistente, y = contagem, fill = Assistente)) +
              geom_bar(stat = "identity") +
              geom_text(aes(label = paste0(round(percentual, 1), "%")),
                        position = position_stack(vjust = 0.5),
                        color = "black") +
              labs(title = "", x = "", y = "N° de Ciretrans") +
              theme_minimal()
            
            ggplotly(p4)
          }
        })
        
        output$tabela_assistente <- renderDT({
          assistente_table <- dados %>%
            filter(!is.na(Assistente)) %>%  # Excluir valores NA da variável Assistente
            group_by(`Região Integração`, Assistente) %>%
            summarise(contagem = n(), .groups = 'drop')
          
          # Criar tabela de contingência (pivotar os dados)
          assistente_table <- assistente_table %>%
            pivot_wider(
              names_from = Assistente,
              values_from = contagem,
              values_fill = list(contagem = 0)
            )
          
          # Adicionar totais das linhas (por região)
          assistente_table <- assistente_table %>%
            rowwise() %>%
            mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
            ungroup() # Remover agrupamento
          
          # Agora, adicionar a linha "Total Geral" no final (soma de todas as regiões)
          total_geral_assistente <- assistente_table %>%
            summarise(across(c(2, 3, 4), sum, na.rm = TRUE)) %>%
            mutate(`Região Integração` = "Total Geral") %>%
            select(`Região Integração`, everything()) # Reorganizar para que "Total Geral" fique na primeira coluna
          
          # Adiciona a linha "Total Geral" na tabela
          assistente_table <- bind_rows(assistente_table, total_geral_assistente)
          
          # Renderizar a tabela com DT
          DT::datatable(assistente_table, options = list(pageLength = 13)
                        #, caption = "Tabela Geral de Assistente por Região Integração"
                        )
        })
          #------------------------------------------------------------------------------#
          
          
          #-------------------------------------------------------------------------------
          # Escala Likert
          
  output$likertPlot1 <- renderPlotly({
  Dados_Clima <- readxl::read_excel("C:/Users/mario.valente/Documents/github_2024/Perfil_Estrutural_Ciretran-main/Dados_Clima.xls")
            dados_filtrados <- Dados_Clima
            dados_filtrados[, 1:14] <- lapply(
              dados_filtrados[, 1:14],
              factor,
              levels = 1:3,
              labels = c("Sim", "Não", "Não Sei Informar"),
              ordered = TRUE
            )
            
            # Carregar a tabela com os nomes das colunas
            nomes <- read_excel("Dados_Clima.xls", sheet = 3)
            colnames(dados_filtrados)[1:14] <- nomes$Nomes
            
            # Gerar o gráfico da escala Likert
            dados_grafico <- likert(as.data.frame(dados_filtrados[1:14]))
            
            # Paleta de cores para o gráfico
            paleta <- brewer.pal(n = 5, "RdBu")
            paleta[3] <- "lightblue"
            
            # Criar o Gráfico Likert
            graficolikert1 <- likert.bar.plot(
              dados_grafico,
              strip = TRUE,
              strip.left = TRUE,
              ReferenceZero = 3,
              wrap = 25,
              centered = TRUE,
              text.size = 4,
              hjust = 0.5,
              legend = "Escala Likert",
              legend.position = "right",
              # top
              auto.key = list(columns = 1, reverse.rows = TRUE),
              ordered = TRUE
            ) +
              labs(x = "", y = "FREQUÊNCIA (%)") +
              scale_fill_manual(values = paleta) +
              guides(fill = guide_legend(title = "Escala Likert")) +
              theme_bw(base_size = 11) +
              theme(
                axis.text.y = element_text(size = 11),
                axis.text.x = element_text(size = 11),
                panel.grid = element_blank(),
                plot.background = element_rect(fill = "white"),
                plot.title = element_text(hjust = 0.5)  # Centraliza o título
              )
            # Obter as dimensões da janela do navegador
            largura <- session$clientData$output_likertPlot1_width
            altura <- session$clientData$output_likertPlot1_height
            
            ggplotly(graficolikert1) %>%
              layout(
                width = ifelse(is.null(largura), 800, largura),
                # Largura dinâmica
                height = ifelse(is.null(altura), 750, altura),
                # Altura dinâmica
                margin = list(
                  l = 60,
                  r = 80,
                  t = 50,
                  b = 100
                )   # Ajuste das margens internas
              )
          })
          
          
          
          output$likertPlot2 <- renderPlotly({
            Dados_Clima <- readxl::read_excel(
              "C:/Users/mario.valente/Documents/github_2024/Perfil_Estrutural_Ciretran-main/Dados_Clima.xls"
            )
            dados_filtrados <- Dados_Clima
            dados_filtrados[, 15:5] <- lapply(
              dados_filtrados[, 15:25],
              factor,
              levels = 1:3,
              labels = c("Sim", "Não", "Não Sei Informar"),
              ordered = TRUE
            )
            
            # Carregar a tabela com os nomes das colunas
            nomes2 <- read_excel("Dados_Clima.xls", sheet = 4)
            colnames(dados_filtrados)[15:25] <- nomes2$Nomes2
            
            # Gerar o gráfico da escala Likert
            dados_grafico <- likert(as.data.frame(dados_filtrados[15:25]))
            
            # Paleta de cores para o gráfico
            paleta <- brewer.pal(n = 5, "RdBu")
            paleta[3] <- "lightblue"
            
            # Criar o Gráfico Likert
            graficolikert2 <- likert.bar.plot(
              dados_grafico,
              strip = TRUE,
              strip.left = TRUE,
              ReferenceZero = 3,
              wrap = 25,
              centered = TRUE,
              text.size = 4,
              hjust = 0.5,
              legend = "Escala Likert",
              legend.position = "right",
              # top
              auto.key = list(columns = 1, reverse.rows = TRUE),
              ordered = TRUE
            ) +
              labs(x = "", y = "FREQUÊNCIA (%)") +
              scale_fill_manual(values = paleta) +
              guides(fill = guide_legend(title = "Escala Likert")) +
              theme_bw(base_size = 11) +
              theme(
                axis.text.y = element_text(size = 11),
                axis.text.x = element_text(size = 11),
                panel.grid = element_blank(),
                plot.background = element_rect(fill = "white"),
                plot.title = element_text(hjust = 0.5)  # Centraliza o título
              )
            # Obter as dimensões da janela do navegador
            largura <- session$clientData$output_likertPlot2_width
            altura <- session$clientData$output_likertPlot2_height
            
            ggplotly(graficolikert2) %>%
              layout(
                width = ifelse(is.null(largura), 700, largura),
                # Largura dinâmica
                height = ifelse(is.null(altura), 700, altura),
                # Altura dinâmica
                margin = list(
                  l = 60,
                  r = 80,
                  t = 50,
                  b = 100
                )   # Ajuste das margens internas
              )
          })
          
          
          output$likertPlot3 <- renderPlotly({
            Dados_Clima <- readxl::read_excel(
              "C:/Users/mario.valente/Documents/github_2024/Perfil_Estrutural_Ciretran-main/Dados_Clima.xls"
            )
            dados_filtrados <- Dados_Clima
            dados_filtrados[, 26:39] <- lapply(
              dados_filtrados[, 26:39],
              factor,
              levels = 1:3,
              labels = c("Sim", "Não", "Não Sei Informar"),
              ordered = TRUE
            )
            
            # Carregar a tabela com os nomes das colunas
            nomes3 <- read_excel("Dados_Clima.xls", sheet = 5)
            colnames(dados_filtrados)[26:39] <- nomes3$Nomes3
            
            # Gerar o gráfico da escala Likert
            dados_grafico <- likert(as.data.frame(dados_filtrados[26:39]))
            
            # Paleta de cores para o gráfico
            paleta <- brewer.pal(n = 5, "RdBu")
            paleta[3] <- "lightblue"
            
            # Criar o Gráfico Likert
            graficolikert3 <- likert.bar.plot(
              dados_grafico,
              strip = TRUE,
              strip.left = TRUE,
              ReferenceZero = 3,
              wrap = 25,
              centered = TRUE,
              text.size = 4,
              hjust = 0.5,
              legend = "Escala Likert",
              legend.position = "right",
              # top
              auto.key = list(columns = 1, reverse.rows = TRUE),
              ordered = TRUE
            ) +
              labs(x = "", y = "FREQUÊNCIA (%)") +
              scale_fill_manual(values = paleta) +
              guides(fill = guide_legend(title = "Escala Likert")) +
              theme_bw(base_size = 11) +
              theme(
                axis.text.y = element_text(size = 11),
                axis.text.x = element_text(size = 11),
                panel.grid = element_blank(),
                plot.background = element_rect(fill = "white"),
                plot.title = element_text(hjust = 0.5)  # Centraliza o título
              )
            # Obter as dimensões da janela do navegador
            largura <- session$clientData$output_likertPlot3_width
            altura <- session$clientData$output_likertPlot3_height
            
            ggplotly(graficolikert3) %>%
              layout(
                width = ifelse(is.null(largura), 700, largura),
                # Largura dinâmica
                height = ifelse(is.null(altura), 700, altura),
                # Altura dinâmica
                margin = list(
                  l = 60,
                  r = 80,
                  t = 50,
                  b = 100
                )   # Ajuste das margens internas
              )
          })
          
          
          
          
          
          
          
          
          
          
          
          
}

#-------------------------------------------------------------------------------
# APP

shinyApp(ui, server)
