
{
# Librerías
library(quantmod) # Histórico de acciones de Yahoo Finance
library(TTR) # Calcula el retorno diario
library(ggplot2) # Gráficos
library(RColorBrewer) # Colores gráfico
library(PortfolioAnalytics) # Frontera eficiente 
library(dplyr) # Para transformación datos
library(matrixStats) # Para colSds
library(plotly) # Gráficos
library(tidyr) # Funciones como gather
library(lubridate) # Dates
library(shinydashboard) # Dashboard
library(shinyWidgets) 
library(readxl) # Leer excel
library(DT) # Tablas
library(alphavantager) # API datos financieros
library(jsonlite) # Para leer archivo JSON sobre noticias
library(rvest) # Web scrapping 
library(ROI) # Optimización
library(ROI.plugin.glpk)
library(PerformanceAnalytics)
    
# Nota: Poner API Key de Alphavantager en líneas 1021 y 1037

# Paleta de colores
colores <- c("#003560", "#006DA9", "#249AC5", "#9BC4DD", "#E79E93", "#D5644A", "#AB2C00", "#611300", "#A0522D", "#D2691E", "#8B4513" )

# Tabla de empresa - ticker
ruta_archivo <- "~/ticker_empresa.xlsx" # Cambiar ruta
datos_excel <- read_excel(ruta_archivo)

# Tickers a elegir
s <- datos_excel[[2]]
    
# Creo una función para crear una cartera
obtener_cartera <- function(tickers, start_date, end_date) {
    # Descargar datos de Yahoo Finance
    getSymbols(tickers, from = start_date, to = end_date, src = "yahoo")
    
    # Calcular rendimientos diarios
    returns <- list()
    for (ticker in tickers) {
        returns[[ticker]] <- dailyReturn(Cl(get(ticker)))
    }
    
    # Combinar todos los rendimientos en un solo marco de datos
    returns_df <- do.call(merge, returns)
    
    # Eliminar NA y poner nombres a las columnas
    returns_df <- na.omit(returns_df)
    colnames(returns_df) <- tickers
    
    # Dividir los datos en conjuntos de train y test (70% train, 30% test)
    train_size <- floor(0.7 * nrow(returns_df))
    train_data <- returns_df[1:train_size, ]
    test_data <- returns_df[(train_size + 1):nrow(returns_df), ]
    
    # Retornar los datos de entrenamiento y prueba
    return(list(train = train_data, test = test_data))
}

# Información empleada en el dashboard
sectores_sp500 <- data.frame(
    Sector = c("Tecnología de la Información", "Salud", "Finanzas", "Consumo Discrecional", 
               "Industriales", "Servicios de Comunicación", "Consumo Básico", "Energía", "Servicios Públicos",
               "Materiales", "Inmobiliario"),
    Peso_Sector = c(25.7, 15.8, 11.7, 9.8, 8.7, 7.3, 7.2, 5.2, 3.2, 2.7, 2.7)
)
}

# UI es la interfaz del usuario, dentro de la Interfaz de Usuario podemos tener: 
# funciones de Layout, controles de input, controles de output

# Título de la pagina y Side Bar
{header <-  dashboardHeader(title = div("Morganne De Witte"))
    
# Elementos de la barra lateral
sidebar <- dashboardSidebar(sidebarMenu(
        menuItem(tabName = 'aplicacion', text = '   Gestor de Carteras', icon=icon("calculator"))
    ))}

# Elemtnos cuerpo
body <- dashboardBody(

    # Esto hace que las cajas tengan un margen
    tags$head(tags$style(HTML("
             .box-custom {
               margin-top: 20px;
             }"))),
    
    # Pestañas
    tabItems(
    tabItem(tabName = "aplicacion",
            tabsetPanel(tabPanel("Análisis Técnico y Fundamental",
                                 
                                 fluidRow(box(width=12,
                                                  collapsible = TRUE,
                                                  title = "INFORMACIÓN ÍNDICE STANDARD & POOR’S 500",
                                                  status = "primary",
                                                  solidHeader = TRUE,
                                              
                                              box(width=4,
                                                  title = "Representación del mercado del S&P500 en 2022",
                                                  plotlyOutput("sectores_sp")
                                                  
                                              ),
                                              box(width=4,
                                                  title= "Rentabilidad anual en tiempo real",
                                                  height = '460px',
                                                  plotlyOutput("info_sector2")
                                                  
                                              ),
                                              box(width=4,
                                                  dataTableOutput("ticker_empresa")))),
                                 

                                 fluidRow(box(width=12,
                                              collapsible = TRUE,
                                              title = "ANÁLISIS FUNDAMENTAL",
                                              status = "primary",
                                              solidHeader = TRUE,
                                              
                                              box(width = 4,
                                                  title = "Overview",
                                                  subtitle = "",
                                                  pickerInput(
                                                      inputId = "activos_over",
                                                      choices = s,
                                                      multiple = FALSE,
                                                      options = list(
                                                          `live-search` = TRUE)),
                                                  dataTableOutput("overview")),
                                              
                                              box(width = 8,
                                                  title = "Noticias y Análisis de Sentimiento",
                                                  fluidRow(
                                                      column(width = 4,
                                                             pickerInput(
                                                                 inputId = "activos_noticias",
                                                                 choices = s,
                                                                 multiple = FALSE,
                                                                 options = list(`live-search` = TRUE)
                                                             )
                                                      ),
                                                      column(width = 4,
                                                             pickerInput(
                                                                 inputId = "topics",
                                                                 choices = c("blockchain", "earnings", "ipo", "mergers_and_acquisitions", "financial_markets", "economy_fiscal", "economy_monetary", "economy_macro", "energy_transportation", "finance", "life_sciences", "manufacturing", "real_estate", "retail_wholesale", "technology"),
                                                                 multiple = FALSE,
                                                                 options = list(`live-search` = TRUE)
                                                             )
                                                      ),
                                                      column(width = 4,
                                                             pickerInput(
                                                                 inputId = "sort",
                                                                 choices = c("LATEST", "EARLIEST", "RELEVANCE"),
                                                                 multiple = FALSE,
                                                                 options = list(`live-search` = TRUE)
                                                             )
                                                      )
                                                  ),
                                                  dataTableOutput("news_tickers")
                                              
                                              
                                              ))),
                                 
                                 fluidRow(box(width=12,
                                              collapsible = TRUE,
                                              title = "ANÁLISIS TÉCNICO",
                                              status = "primary",
                                              solidHeader = TRUE,
                                              box(
                                                 title = "Seleccione empresa a evaluar",
                                                 class = "box-custom",
                                                 height = '140px',
                                                 pickerInput(
                                                     inputId = "activos",
                                                     choices = s,
                                                     multiple = FALSE,
                                                     
                                                     options = list(
                                                         `live-search` = TRUE)) ),
                                            
                                             box(
                                                 title = "Seleccionar fechas",
                                                 class = "box-custom",
                                                height = '140px',
                                                dateRangeInput("fechas", label = NULL, 
                                                               start = Sys.Date()-30, end = Sys.Date())),
                                             
                                        fluidRow(
                                            box(title = "Gráfico de velas japonesas de ticker seleccionado",
                                                class = "box-custom",
                                                #solidHeader = TRUE,
                                                #status = "primary", 
                                                plotOutput("plot_activos")),
                                           box(title = "Gráfico de velas japonesas de S&P 500",
                                               class = "box-custom",
                                               #status = "primary", 
                                               #solidHeader = TRUE,
                                                plotOutput("plot_SP500")))
                                    
                                ))),
                        
                        tabPanel("Selección de Cartera",
                                 
                                 fluidRow( 
                                     box(solidHeader = TRUE,
                                         status = "primary",
                                         title = "Seleccione activos para su cartera",
                                         pickerInput(
                                             inputId = "activos2",
                                             label = "Seleccionar tickers", 
                                             choices = s,
                                             multiple = TRUE,
                                             options = list(
                                                 `live-search` = TRUE)
                                         )),
                                        
                                     box(title = "Seleccionar fechas", solidHeader = TRUE,
                                         status = "primary",
                                         dateRangeInput("fechas2", label = NULL, 
                                                        start = "2004-08-19", end = Sys.Date())),
                                     class = "box-custom"),
                                 
                                 fluidRow( 
                                     box(title = "Distribución inicial de la cartera", 
                                         status = "primary",plotlyOutput("pie_chart")),
                                     box(title = "Matriz varianza - covarianza", 
                                         status = "primary",plotlyOutput("varcov")),
                                     box(title = "Rendimientos y volatilidades medios", 
                                         status = "primary", dataTableOutput("tabla")),
                                     box(title = "Matriz de Correlaciones", 
                                         status = "primary",plotlyOutput("corr"))
                                 )),
                                 
                        tabPanel("Optimización de Carteras",
                                 
                                 fluidRow(
                                     box(solidHeader = TRUE,
                                         status = "primary",
                                         title = "Frontera Eficiente",
                                         collapsible = TRUE, height = '500px',
                                         plotOutput("FronteraEficiente", height = "400px")),
                                    
                                     box(solidHeader = TRUE,
                                     status = "primary",
                                     title = "Cartera Máximo Ratio de Sharpe",
                                     collapsible = TRUE,
                                     tabsetPanel(
                                         tabPanel("Gráfico Riesgo-Rentabilidad",
                                                  plotOutput("RR_SR")),
                                         tabPanel("Pesos",
                                                  plotlyOutput("MaxSharpe")))),
                                     
                                     box(solidHeader = TRUE,
                                         status = "primary",
                                         title = "Cartera Mínimo Riesgo",
                                         collapsible = TRUE,
                                         box(status = "primary",
                                             title = "Seleccione una rentabilidad objetivo",
                                             width = 12,  # Cambiar el ancho según sea necesario
                                             radioGroupButtons(inputId = "Return",
                                                               label = "Target Return", 
                                                               choices = c(0.0004, 0.0006, 0.001)),
                                             tabsetPanel(
                                                 tabPanel("Riesgo-Rentabilidad",
                                                          plotOutput("RR_MR")),
                                                 tabPanel("Pesos MV",
                                                          plotlyOutput("MinVarRiesgo")),
                                                 tabPanel("Riesgo-Rentabilidad MVG",
                                                          plotOutput("RR_MRG")),
                                                 tabPanel("Pesos MVG",
                                                          plotlyOutput("MinVar"))))),
                                         
                                     box(solidHeader = TRUE,
                                         status = "primary",
                                         title = "Cartera Máxima Rentabilidad",
                                         height = '670px',
                                         collapsible = TRUE,
                                         box(status = "primary",
                                             width = 12,  
                                             tabsetPanel(
                                                 tabPanel("Pesos MR",
                                                          plotlyOutput("MaxReturn4Risk", height = "500px")),
                                                 tabPanel("Riesgo-Rentabilidad MR Global",
                                                          plotOutput("RR_MaxRG")),
                                                 tabPanel("Pesos MR Global",
                                                          plotlyOutput("MaxReturn"))))),
                                    
                                     
                                     box(solidHeader = TRUE,
                                         status = "primary",
                                         title = "Cartera Rebalanceada",
                                         collapsible = TRUE,
                                         tabsetPanel(
                                             tabPanel("Evolución Trim Pesos",
                                                      plotlyOutput("Evol_Pesos")),
                                             tabPanel("Retornos Trim Acumulados",
                                                      plotlyOutput("Ret_Trim")),
                                             tabPanel("Retornos Anuales Acumulados",
                                                      plotlyOutput("Ret_Anuales")))))),
                        
                        tabPanel("Resultados",
                                 fluidRow(
                                     infoBoxOutput("Retorno_Anualizado"),
                                     infoBoxOutput("Riesgo"),
                                     infoBoxOutput("Ratio_Sharpe")),
                                 fluidRow(box(solidHeader = FALSE,
                                              status = "primary",
                                              plotOutput("grafico_final", height = "510"),
                                              height = "540px"),
                                     box(solidHeader = FALSE,
                                                status = "primary",
                                                title = "Seleccione cartera",
                                                pickerInput(
                                                    inputId = "Cartera",
                                                    choices = c("Portfolio Naive",
                                                                "Portfolio Máxima Rentabilidad GLB", 
                                                                "Portfolio Máxima Rentabilidad",
                                                                "Portfolio Rebalanceado",
                                                                "Portfolio Tangente",
                                                                "Portfolio Mínimo Riesgo Global",
                                                                "Portfolio Mínimo Riesgo",
                                                                "SP500"),
                                                    multiple = FALSE,
                                                    options = list())),
                                     
                                            box(solidHeader = FALSE,
                                                         status = "primary",
                                                         title = "Métricas principales",
                                                         dataTableOutput("Principales_metricas")))
                                 ))
                        )))

# Juntamos todos los elementos (UI)
{ui <- dashboardPage(
    header,
    sidebar,
    body,
    skin = "black"
)}

# Server es cómo se calculan y presentan los outputs en función de los inputs 

server <- function(input, output) {
    
    # Para almacenar objetos
    stored_outputs <- new.env()
    
    output$info_sector2 <- renderPlotly({
        
        url <- "https://finance.yahoo.com/sectors/"
        
        # Leer la página web
        pagina <- read_html(url)
        
        # Extraer la tabla de sectores
        tabla_sectores <- html_table(html_nodes(pagina, "table"), fill = TRUE)[[1]]
        nombres_columnas <- c("Sectores", "Peso", "Rentabilidad diaria", "Rentabilidad anual")
        tabla_sectores <- setNames(tabla_sectores, nombres_columnas)
        tabla_sectores$`Rentabilidad anual` <- as.numeric(gsub("%", "", as.character(tabla_sectores$`Rentabilidad anual`)))
        tabla_sectores$`Rentabilidad diaria` <- as.numeric(gsub("%", "", as.character(tabla_sectores$`Rentabilidad diaria`)))
        
        # Crear el treemap interactivo con Plotly
        pop <- ggplot(tabla_sectores, aes(x = reorder(Sectores, `Rentabilidad anual`), y = `Rentabilidad anual`, fill = `Rentabilidad anual` > 0)) +
            geom_col() +
            scale_fill_manual(values = c("lightgrey", "#249AC5")) +
            coord_flip() +
            theme_minimal() +
            labs(title = NULL,
                 x = NULL,
                 y = NULL,
                 fill = NULL) +
            theme(legend.position = "none")
        
        ggplotly(pop)%>% layout(title=FALSE,
                                showlegend = FALSE, 
                                font = list(size = 0),
                                margin = list(l = 0, r = 0, t = 0, b = 0),
                                width = 400,  # Anchura del gráfico
                                height = 400)  # Altura del gráfico
        
        
        
    })

    output$sectores_sp <- renderPlotly({
        
        pie_chart <- plot_ly(sectores_sp500, labels = ~Sector, values = ~Peso_Sector, type = 'pie',
                             textinfo = 'label+percent', hoverinfo = 'text',
                             marker = list(colors = colores),
                             text = ~paste(Sector, ": ", Peso_Sector, "%"))
        
        # Configurar el diseño del gráfico
        pie_chart <- pie_chart %>% layout(showlegend = FALSE, 
                                          font = list(size = 9),
                                          margin = list(l = 95, r = 80, t = 0, b = 10),
                                          width = 400,  # Anchura del gráfico
                                          height = 400)  # Altura del gráfico
        
        # Devolver el gráfico
        return(pie_chart)
    })
    
    output$ticker_empresa <- renderDataTable({
        datatable(datos_excel, options = list(searching = TRUE, paging = TRUE, pageLength = 8), rownames = FALSE)
    })
    
    output$plot_SP500 <- renderPlot({
        
        req(input$fechas)
        
        # Descargar datos del índice S&P 500 (símbolo "^GSPC" en Yahoo Finance)
        GSPC <- getSymbols("^GSPC", from = input$fechas[1], to = input$fechas[2], src="yahoo", auto.assign = FALSE)
        
        # Extraer precios de cierre del índice S&P 500
        sp500_prices <- Cl(GSPC)
        
        # Calcular los rendimientos diarios del índice S&P 500
        sp500_returns_daily <- dailyReturn(sp500_prices)
        
        gspc <- getSymbols("^GSPC", from = input$fechas[1], to = input$fechas[2], src="yahoo", auto.assign = FALSE)
        
        chartSeries(gspc,
                    theme = chartTheme("white", dn.col = "red", bg.col = "white"),
                    name = "SP500",
                    )
    
        
        })
    
    output$plot_activos <- renderPlot({
        req(input$activos, input$fechas)
    
        datos <- getSymbols(input$activos, from = input$fechas[1], to = input$fechas[2], src="yahoo", auto.assign = FALSE)
        
        chartSeries(datos,
                    theme = chartTheme("white", dn.col = "red", bg.col = "white"),
                    name = input$activos)
    })
    
    output$pie_chart <- renderPlotly({
        
        activos_seleccionados <- (req(input$activos2))
        lista_activos <- as.list(activos_seleccionados)
        
        # Calcular el peso igual para cada activo seleccionado
        peso_individual <- 1/length(lista_activos)
        
        # Crear un data frame con los activos y sus pesos
        data <- data.frame(
            Activos = activos_seleccionados,
            Peso = peso_individual
        )
        
        datos_reorganizado <- spread(data, key = Activos, value = Peso)
        
        # Generar el gráfico de pie
        pie_chart <- plot_ly(data, labels = ~Activos, values = ~Peso, type = 'pie', hole = 0.6,
                             textinfo = 'label+percent', 
                             hoverinfo = 'text',
                             marker = list(colors = colores),
                             text = ~paste(Activos, ": ", round(Peso*100, 2), "%"))
        
        # Configurar el diseño del gráfico
        pie_chart <- pie_chart %>% layout(showlegend = TRUE)
        
        # Devolver el gráfico
        return(pie_chart)
        

    })
    
    output$tabla <- renderDataTable({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])

        train_data <- datos_juntos$train

        # Descargar datos del índice S&P 500 (símbolo "^GSPC" en Yahoo Finance)
        GSPC <- getSymbols("^GSPC", from = input$fechas2[1], to = input$fechas2[2], src="yahoo", auto.assign = FALSE)
        
        # Extraer precios de cierre del índice S&P 500
        sp500_prices <- Cl(GSPC)
        
        # Calcular los rendimientos diarios del índice S&P 500
        sp500_returns_daily <- dailyReturn(sp500_prices)
        
        # Eliminar NA (si los hay)
        sp500_returns_daily <- na.omit(sp500_returns_daily)
        stored_outputs$sp500_returns_stored <- sp500_returns_daily
        
        # Calcular rendimiento promedio y volatilidad
        RendimientoPromedio <- c(mean(sp500_returns_daily), colMeans(train_data))
        Volatilidad <- c(sd(sp500_returns_daily), colSds(train_data))
        
        # Crear un dataframe con los resultados
        Cuadro <- data.frame(rbind(RendimientoPromedio, Volatilidad))
        rownames(Cuadro) <- c("Rendimiento Promedio", "Volatilidad")
        
        
        # Cambiar los nombres de las columnas
        colnames(Cuadro) <- c("SP500", colnames(train_data))
        
        # Multiplicar por 100
        Cuadro <- round(Cuadro * 100,2)
        
        datatable(Cuadro, options = list(searching = TRUE, paging = TRUE, pageLength = 8, scrollX = TRUE), rownames = TRUE)
        
    })
    
    output$varcov <- renderPlotly({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data <- datos_juntos$train
        
        Cov <- cov(train_data)*100
        plot_ly(x= colnames(train_data), y = colnames(train_data), z = Cov, type = "heatmap")
    
})
    
    output$corr <- renderPlotly({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data <- datos_juntos$train
        
        corr <- cor(train_data) * 100
        plot_ly(x= colnames(train_data), y = colnames(train_data), z = corr, type = "heatmap", 
                text = ~paste(round(corr, 2), "%"), textinfo = 'z')
        
    })
    
    # Optimización Máximo Ratio de Sharpe
    output$MaxSharpe <- renderPlotly({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data_1 <- datos_juntos$train
        
        # Lo almaceno para resultados
        
        test_data <- datos_juntos$test
        train_data <- datos_juntos$train
        
        stored_outputs$test_data_almacenado <- test_data
        stored_outputs$train_data_almacenado <- train_data

        base_case <- portfolio.spec(assets = colnames(train_data_1))
        base_case <- add.constraint(portfolio = base_case, type = "full_investment")
        base_case <- add.constraint(portfolio = base_case, type = "long_only")
        base_case <- add.objective(portfolio = base_case, type = "return", name = "mean")
        base_case <- add.objective(portfolio = base_case, type = "risk", name = "StdDev")
        
        portfolio_opt1 <- optimize.portfolio(R = train_data_1,
                                             portfolio = base_case,
                                             optimize_method = "ROI", 
                                             trace = T,
                                             maxSR= TRUE, rf = 0.000128)
        

        # Obtener los pesos del portafolio eficiente
        pesos <- extractWeights(portfolio_opt1)
        
        pesos_tan <- pmax(pesos, 0)
        
        
        # Los almaceno para utilizarlo en la parte de resultados
        
        stored_outputs$pesos_tan_test <- pesos_tan
        
        # Filtramos pesos
        valores_filtrados <- pesos_tan[pesos_tan != 0]
        
        # Establecer el diseño del gráfico
        plot_ly(labels = names(valores_filtrados), values = valores_filtrados, type = 'pie', hole = 0.6,
                       text = ~paste(names(valores_filtrados)),
                       marker = list(colors = colores),
                       hoverinfo = "text")

        })
    
    output$Evol_Pesos <- renderPlotly({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data <- datos_juntos$train
        
        # Primero peso inicial igual
        n <- ncol(train_data)
        equal_weights <- rep(1/n, n) 
        
        # Creamos cartera con pesos iguales
        EqualWeights <- Return.portfolio(
            R= train_data, 
            weights= equal_weights, 
            rebalance_on ="quarters", 
            verbose= TRUE)
        
        # Obtener los pesos finales de la cartera
        pesos_finales <- data.frame(EqualWeights$EOP.Weight)
        
        #Lo convertimos de un objeto XTS a un tibble para graficar la evolución de los pesos en el medio de las fechas de rebalanceo
        
        pesos_finales <-tibble::rownames_to_column(pesos_finales, "Fecha")
        pesos_finales <- gather(pesos_finales, Acciones, Pesos, -Fecha)
        
        pesos_finales$Fecha <- as.Date(pesos_finales$Fecha)
        
        p <- pesos_finales %>%
            plot_ly(x = ~as.Date(Fecha), y = ~Pesos*100, color = ~Acciones, type = 'scatter', mode = 'lines') %>%
            layout(title = "Evolución de los Pesos de las Acciones (Rebalanceo Trimestral)",
                   xaxis = list(title = "Fecha"),
                   yaxis = list(title = "Pesos de las Acciones en el Portafolio"))
        
        print(p)
        
    })
    
    output$Ret_Trim <- renderPlotly({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data <- datos_juntos$train
        
        # Primero peso inicial igual
        n <- ncol(train_data)
        equal_weights <- rep(1/n, n) 
        
        # Creamos cartera con pesos iguales
        EqualWeights <- Return.portfolio(
            R= train_data, 
            weights= equal_weights, 
            rebalance_on ="quarters", 
            verbose= TRUE)
        
        ContribucionAccion <- data.frame(EqualWeights$contribution) 
        ContribucionAccion <- tibble::rownames_to_column(ContribucionAccion, "Fecha")
        ContribucionAccion$Ano <- year(ContribucionAccion$Fecha) 
        ContribucionAccion$Trimestre <- quarter(ContribucionAccion$Fecha, with_year = TRUE)
        
        Retornos_EqualWeights <- as.data.frame(EqualWeights$returns)
        Retornos <- tibble::rownames_to_column(Retornos_EqualWeights, "Fecha")
        
        ContribucionyRetornos <- left_join(Retornos, ContribucionAccion, by="Fecha")
        df_transformado <- ContribucionyRetornos %>%
            pivot_longer(cols = -c(Fecha, Ano, Trimestre), names_to = "Acciones", values_to = "Pesos")
        
        df_filtrado <- subset(df_transformado, Acciones != "portfolio.returns")
        
        # Calcular la contribución acumulada de los pesos por Acciones y Trimestre
        ContribucionAcumuladaTrimestral <- df_filtrado %>%
            group_by(Acciones, Trimestre) %>%
            summarise(ContribucionAcumulada = sum(Pesos))
        
        
        # Gráfico
        plot_ly(ContribucionAcumuladaTrimestral, 
                type = "bar", 
                x = as.factor(ContribucionAcumuladaTrimestral$Trimestre), 
                y = ContribucionAcumuladaTrimestral$ContribucionAcumulada * 100, 
                color = as.factor(ContribucionAcumuladaTrimestral$Acciones),
                hoverinfo = "text",
                hovertext =~paste0(Acciones, ": ", round(ContribucionAcumulada*100, 2))) %>%
            layout(title = "Retornos Trimestrales Acumulados dada las Contribuciones",
                   xaxis = list(title = "Fecha"),
                   yaxis = list(title = "Retornos Trimestrales Acumulados"),
                   barmode = "stack",
                   bargap = 0.05) 
        
    })
    
    output$Ret_Anuales <- renderPlotly({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data <- datos_juntos$train
        
        # Primero peso inicial igual
        n <- ncol(train_data)
        equal_weights <- rep(1/n, n) 
        
        # Creamos cartera con pesos iguales
        EqualWeights <- Return.portfolio(
            R= train_data, 
            weights= equal_weights, 
            rebalance_on ="quarters", 
            verbose= TRUE)
        
        ContribucionAccion <- data.frame(EqualWeights$contribution) 
        ContribucionAccion <- tibble::rownames_to_column(ContribucionAccion, "Fecha")
        ContribucionAccion$Ano <- year(ContribucionAccion$Fecha) 
        ContribucionAccion$Trimestre <- quarter(ContribucionAccion$Fecha, with_year = TRUE)
        
        Retornos_EqualWeights <- as.data.frame(EqualWeights$returns)
        Retornos <- tibble::rownames_to_column(Retornos_EqualWeights, "Fecha")
        
        ContribucionyRetornos <- left_join(Retornos, ContribucionAccion, by="Fecha")
        df_transformado <- ContribucionyRetornos %>%
            pivot_longer(cols = -c(Fecha, Ano, Trimestre), names_to = "Acciones", values_to = "Pesos")
        
        df_filtrado <- subset(df_transformado, Acciones != "portfolio.returns")
        
        
        ContribucionAcumuladaAnual <- df_filtrado %>%
            group_by(Acciones, Ano) %>%
            summarise(ContribucionAcumulada = sum(Pesos))
        
        p <-  plot_ly(ContribucionAcumuladaAnual, 
                type = "bar", 
                x = as.factor(ContribucionAcumuladaAnual$Ano), 
                y = ContribucionAcumuladaAnual$ContribucionAcumulada * 100, 
                color = as.factor(ContribucionAcumuladaAnual$Acciones),
                hoverinfo = "text",
                hovertext =~paste0(ContribucionAcumuladaAnual$Acciones, ": ", round(ContribucionAcumulada*100, 2))) %>%
                layout(title = "Retornos Anuales Acumulados dada las Contribuciones de las Acciones",
                       xaxis = list(title = "Fecha"),
                       yaxis = list(title = "Retornos Anuales Acumulados"),
                       barmode = "stack",
                       bargap = 0.05) 
        
        print(p)
    })
    
    # Mínima Riesgo Global
    output$MinVar <- renderPlotly({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data_2 <- datos_juntos$train
        
        portfolio6 <- portfolio.spec(assets = colnames(train_data_2))
        portfolio6 <- add.constraint(portfolio = portfolio6, type = "full_investment")
        portfolio6 <- add.constraint(portfolio = portfolio6,
                                     type="weight_sum",
                                     max_sum=1, min_sum=1)
        portfolio6 <- add.objective(portfolio = portfolio6, type = "risk", name = "sd")
        
        portfolio_opt6 <- optimize.portfolio(R = train_data_2,
                                             portfolio = portfolio6,
                                             optimize_method = "ROI", # Sino no se cumple la restricción
                                             trace = T)
        
        # Pesos
        pesos_1 <- extractWeights(portfolio_opt6)
        
        pesos_risk <- pmax(pesos_1, 0)
        
        # Los almaceno para utilizarlo en la parte de resultados
        
        stored_outputs$pesos_risk_test2 <- pesos_risk

        # Filtramos pesos
        valores_filtrados2 <- pesos_risk[pesos_risk != 0]
        
        # Establecer el diseño del gráfico
        plot_ly(labels = names(valores_filtrados2), values = valores_filtrados2, type = 'pie', hole = 0.6,
                       text = ~paste(names(valores_filtrados2)),
                     marker = list(colors = colores),
                       hoverinfo = "text")
        
    })
    
    # Mínimo Riesgo para un nivel determinado de rentabilidad
    output$MinVarRiesgo <-  renderPlotly({
        
        req(input$fechas2, input$activos2,input$Return)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data_3 <- datos_juntos$train
        
        portfolio7 <- portfolio.spec(assets = colnames(train_data_3))
        portfolio7 <- add.constraint(portfolio = portfolio7,
                                     type="full_investment")
        portfolio7 <- add.constraint(portfolio = portfolio7,
                                     type="long_only")
        
        target_return <- as.numeric(input$Return)
        
        portfolio7 <- add.objective(portfolio=portfolio7, type="return", name="mean", target = target_return)
        portfolio7 <- add.objective(portfolio=portfolio7, type="risk", name="sd")
        
        portfolio_opt7 <- optimize.portfolio(R = train_data_3,
                                             portfolio = portfolio7,
                                             optimize_method = "ROI",
                                             trace = T)
        
        # Pesos
        pesos_2 <- extractWeights(portfolio_opt7)
        
        pesos_risk2 <- pmax(pesos_2, 0)
        
        # Los almaceno para utilizarlo en la parte de resultados
        
        stored_outputs$pesos_risk_test2 <- pesos_risk2
        
        # Filtramos pesos
        valores_filtrados3 <- pesos_risk2[pesos_risk2 != 0]
        
        # Establecer el diseño del gráfico
        plot_ly(labels = names(valores_filtrados3), values = valores_filtrados3, type = 'pie', hole = 0.6,
                text = ~paste(names(valores_filtrados3)),
                marker = list(colors = colores),
                hoverinfo = "text")
    })
    
    # Máxima rentabilidad global
    output$MaxReturn <- renderPlotly({
        
        req(input$fechas2, input$activos2,input$Return)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data_4 <- datos_juntos$train
        
        portfolio4 <- portfolio.spec(assets = colnames(train_data_4))
        portfolio4 <- add.constraint(portfolio = portfolio4, type = "full_investment")
        portfolio4 <- add.constraint(portfolio = portfolio4, type = "long_only")
        portfolio4 <- add.objective(portfolio = portfolio4, type = "return", name = "mean")
        
        portfolio_opt4 <- optimize.portfolio(R = train_data_4,
                                             portfolio = portfolio4,
                                             optimize_method = "ROI", # Sino no se cumple la restricción
                                             trace = T)
        
        # Pesos
        pesos_maxretglobal <- extractWeights(portfolio_opt4)
        
        # Los almaceno para utilizarlo en la parte de resultados
        
        stored_outputs$pesos_maxretglob_test <- pesos_maxretglobal
        
        # Filtramos pesos
        valores_filtrados4 <- pesos_maxretglobal[pesos_maxretglobal != 0]
        
        # Establecer el diseño del gráfico
        plot_ly(labels = names(valores_filtrados4), values = valores_filtrados4, type = 'pie', hole = 0.6,
                text = ~paste(names(valores_filtrados4)),
                marker = list(colors = colores),
                hoverinfo = "text")
        
    })
    
    # Máxima rentabilidad para un nivel determinado de riesgo
    output$MaxReturn4Risk <- renderPlotly({
        
        req(input$fechas2, input$activos2)
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        train_data_5 <- datos_juntos$train
        
        portfolio40 <- portfolio.spec(assets = colnames(train_data_5))
        portfolio40 <- add.constraint(portfolio = portfolio40, type = "full_investment")
        portfolio40 <- add.constraint(portfolio = portfolio40, type = "long_only")
        portfolio40 <- add.objective(portfolio=portfolio40, type="return", name="mean")
        
        portfolio40 <- add.objective(portfolio = portfolio40,
                                    type="risk_budget_objective",
                                    name="CVaR", 
                                    max_prisk = 0.3,
                                    arguments=list(p=0.95,clean="boudt"))
        
        
        
        portfolio_opt40 <- optimize.portfolio(R = train_data_5,
                                             portfolio = portfolio40,
                                             optimize_method = "ROI", 
                                             trace = T)
        
        # Pesos
        pesos_maxret2 <- extractWeights(portfolio_opt40)
        
        # Los almaceno para utilizarlo en la parte de resultados
        
        stored_outputs$pesos_maxret_test2 <- pesos_maxret2
        
        # Filtramos pesos
        valores_filtrados40 <- pesos_maxret2[pesos_maxret2 != 0]
        
        # Establecer el diseño del gráfico
        plot_ly(labels = names(valores_filtrados40), values = valores_filtrados40, type = 'pie', hole = 0.6,
                text = ~paste(names(valores_filtrados40)),
                marker = list(colors = colores),
                hoverinfo = "text")
        
    })
    
    # Gráficos Riesgo rentabilidad
    
    output$RR_SR <- renderPlot({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data_6 <- datos_juntos$train

        base_case <- portfolio.spec(assets = colnames(train_data_6))
        base_case <- add.constraint(portfolio = base_case, type = "full_investment")
        base_case <- add.constraint(portfolio = base_case, type = "long_only")
        base_case <- add.objective(portfolio = base_case, type = "return", name = "mean")
        base_case <- add.objective(portfolio = base_case, type = "risk", name = "StdDev")
        
        portfolio_opt1 <- optimize.portfolio(R = train_data_6,
                                             portfolio = base_case,
                                             optimize_method = "ROI", # Sino no se cumple la restricción
                                             trace = T,
                                             maxSR= TRUE)
        chart.RiskReward(portfolio_opt1,
                         risk.col="sd",
                         return.col = "mean",
                         chart.assets = TRUE,
                         rp=T)
    })
    
    output$RR_MaxRG <- renderPlot({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data_8 <- datos_juntos$train
        
        portfolio4 <- portfolio.spec(assets = colnames(train_data_8))
        portfolio4 <- add.constraint(portfolio = portfolio4, type = "full_investment")
        portfolio4 <- add.constraint(portfolio = portfolio4, type = "long_only")
        portfolio4 <- add.objective(portfolio = portfolio4, type = "return", name = "mean")
        
        portfolio_opt4 <- optimize.portfolio(R = train_data_8,
                                             portfolio = portfolio4,
                                             optimize_method = "ROI", # Sino no se cumple la restricción
                                             trace = T)
        
        
        chart.RiskReward(portfolio_opt4,
                         risk.col="sd",
                         return.col = "mean",
                         chart.assets = TRUE,
                         rp=T)
    })
    
    output$RR_MR <- renderPlot({
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data_9 <- datos_juntos$train
        
        portfolio7 <- portfolio.spec(assets = colnames(train_data_9))
        portfolio7 <- add.constraint(portfolio = portfolio7,
                                     type="full_investment")
        portfolio7 <- add.constraint(portfolio = portfolio7,
                                     type="long_only")
        
        target_return <- as.numeric(input$Return)
        
        portfolio7 <- add.objective(portfolio=portfolio7, type="return", name="mean", target = target_return)
        portfolio7 <- add.objective(portfolio=portfolio7, type="risk", name="sd")
        
        portfolio_opt7 <- optimize.portfolio(R = train_data_9,
                                             portfolio = portfolio7,
                                             optimize_method = "ROI",
                                             trace = T)
        
        chart.RiskReward(portfolio_opt7,
                         risk.col="sd",
                         return.col = "mean",
                         chart.assets = TRUE,
                         rp=T)
    })
    
    output$RR_MRG <- renderPlot({
        
        req(input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data_10 <- datos_juntos$train
        
        portfolio6 <- portfolio.spec(assets = colnames(train_data_10))
        portfolio6 <- add.constraint(portfolio = portfolio6, type = "full_investment")
        portfolio6 <- add.constraint(portfolio = portfolio6, type = "long_only")
        
        portfolio6 <- add.objective(portfolio = portfolio6, type = "risk", name = "sd")
        
        portfolio_opt6 <- optimize.portfolio(R = train_data_10,
                                             portfolio = portfolio6,
                                             optimize_method = "ROI", # Sino no se cumple la restricción
                                             trace = T)
        
        chart.RiskReward(portfolio_opt6,
                         risk.col="sd",
                         return.col = "mean",
                         chart.assets = TRUE,
                         rp=T)
        
    })
    
    output$overview <- renderDataTable({
        
        api_key <- "" # Poner API Key
        av_api_key(api_key)
        
        av_data <- av_get(symbol = input$activos_over, av_fun = "OVERVIEW")
        av_data_df <- cbind(av_data$rank_group, av_data$value)
        colnames(av_data_df) <- c("Información", "Valor")
        
        datatable(av_data_df, options = list(searching = TRUE, paging = TRUE, pageLength = 5))
        
    })
    
    output$news_tickers <- renderDataTable({
        
        
        # URL del JSON
        # Construir el enlace utilizando los valores de los inputs
        url <- paste0("https://www.alphavantage.co/query?function=NEWS_SENTIMENT&tickers=", input$activos_noticias, "&limit=10&apikey=***********&topics=", input$topics, "&sort=", input$sort) # *********** Cambiar por API Key
        
        # Url de prueba
        #url <- "https://www.alphavantage.co/query?function=NEWS_SENTIMENT&tickers=AAPL&apikey=demo"
        
        # Lee el JSON desde la URL
        data <- jsonlite::fromJSON(url)
        
        # Extrae la información relevante
        feed <- data$feed
        title <- feed$title #OK
        url <- feed$url #OK
        summary <- feed$summary #OK
        topics <- sapply(feed$topics, function(x) paste(x$topic, collapse = ", ")) #OK
        overall_sentiment <- unlist(feed$overall_sentiment_label) #OK
        ticker_sentiments <- list()
        
        # Iterar sobre los datos de ticker para cada noticia
        for (i in 1:length(feed$ticker_sentiment)) {
            # Extraer el ticker y el sentimiento label para la noticia actual
            ticker <- feed$ticker_sentiment[[i]]$ticker
            sentiment_label <- feed$ticker_sentiment[[i]]$ticker_sentiment_label
            
            # Combinar los tickers y sentimientos en una cadena de texto
            combined <- paste(ticker, sentiment_label, sep = ": ")
            
            # Agregar la cadena combinada a la lista de resultados
            ticker_sentiments[[i]] <- combined
        }
        
        resultados_ticker_sent <- lapply(ticker_sentiments, function(sublista) {
            paste(sublista, collapse = ", ")})
        
        # Crea un data frame con la información
        
        title <- unlist(title)
        url <- unlist(url)
        summary <- unlist(summary)
        topics <- unlist(topics)
        overall_sentiment <- unlist(overall_sentiment)
        ticker_sentiment_labels <- unlist(resultados_ticker_sent)
        
        suma <- cbind(title, summary, topics, overall_sentiment, ticker_sentiment_labels, url)
        df <- as.data.frame(suma)
        num_columns <- ncol(df)
        
        datatable(df, options = list(searching = TRUE, paging = TRUE, pageLength = 1, scrollX = TRUE, autoWidth = TRUE,
                                     columnDefs = list(list(width = '200px', targets = c(0,1,2)))),
                  rownames = FALSE, filter = 'top') %>%
            formatStyle(names(df), textAlign = 'justify')
                
        
    })
    
    output$FronteraEficiente <- renderPlot({
        
        req(input$Return, input$fechas2, input$activos2)
        
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        
        train_data <- datos_juntos$train
        
        base_case <- portfolio.spec(assets = colnames(train_data))
        base_case <- add.constraint(portfolio = base_case, type = "full_investment")
        base_case <- add.constraint(portfolio = base_case, type = "long_only")
        base_case <- add.objective(portfolio = base_case, type = "return", name = "mean")
        base_case <- add.objective(portfolio = base_case, type = "risk", name = "StdDev")
        
        portfolio_opt1 <- optimize.portfolio(R = train_data,
                                             portfolio = base_case,
                                             optimize_method = "ROI", # Sino no se cumple la restricción
                                             trace = T,
                                             maxSR= TRUE, rf = 0.000128)
        
        ef_1 <- create.EfficientFrontier(train_data,base_case, type="mean-StdDev", n.portfolios=100)
        chart.EfficientFrontier(ef_1, match.col="StdDev", pch=18, col="lightblue", rf=  0.000128) 
    })
    
    output$Retorno_Anualizado <- renderInfoBox({
        
        # No se me almancena asi que hago aqui todos y los guardo en el dataframe
        
        req(input$activos2, input$fechas2, input$activos2,input$Return, input$Cartera)
        datos_juntos <- obtener_cartera(input$activos2, input$fechas2[1], input$fechas2[2])
        test_data <- datos_juntos$test
        train_data <- datos_juntos$train
        ultimo_dia <- index(test_data)[length(index(test_data))]
        nuevo_dia <- as.Date(ultimo_dia) + 1
        
        
        # Portfolio Tangente
        base_case <- portfolio.spec(assets = colnames(train_data))
        base_case <- add.constraint(portfolio = base_case, type = "full_investment")
        base_case <- add.constraint(portfolio = base_case, type = "long_only")
        base_case <- add.objective(portfolio = base_case, type = "return", name = "mean")
        base_case <- add.objective(portfolio = base_case, type = "risk", name = "StdDev")
        portfolio_opt1 <- optimize.portfolio(R = train_data,
                                             portfolio = base_case,
                                             optimize_method = "ROI", 
                                             trace = T,
                                             maxSR= TRUE, rf = 0.000128)
        
        pesos <- extractWeights(portfolio_opt1)
        pesos_tan <- pmax(pesos, 0)
        
        # Portfolio Mínimo Riesgo GLobal
        portfolio6 <- portfolio.spec(assets = colnames(train_data))
        portfolio6 <- add.constraint(portfolio = portfolio6, type = "full_investment")
        portfolio6 <- add.constraint(portfolio = portfolio6, type="long_only")
        portfolio6 <- add.objective(portfolio = portfolio6, type = "risk", name = "sd")
        portfolio_opt6 <- optimize.portfolio(R = train_data,
                                             portfolio = portfolio6,
                                             optimize_method = "ROI", # Sino no se cumple la restricción
                                             trace = T)
        pesos_1 <- extractWeights(portfolio_opt6)
        pesos_risk <- pmax(pesos_1, 0)
        
        # Portfolio Mínimo Riesgo para un Target Return
        target_return <- input$Return
        portfolio7 <- portfolio.spec(assets = colnames(train_data))
        portfolio7 <- add.constraint(portfolio = portfolio7,
                                     type="full_investment")
        portfolio7 <- add.constraint(portfolio = portfolio7,
                                     type="long_only")
        target_return <- as.numeric(input$Return)
        portfolio7 <- add.objective(portfolio=portfolio7, type="return", name="mean", target = target_return)
        portfolio7 <- add.objective(portfolio=portfolio7, type="risk", name="sd")
        portfolio_opt7 <- optimize.portfolio(R = train_data,
                                             portfolio = portfolio7,
                                             optimize_method = "ROI",
                                             trace = T)
        pesos_2 <- extractWeights(portfolio_opt7)
        pesos_risk2 <- pmax(pesos_2, 0)
        
        
        # Portfolio Máxima Rentabilidad Global
        portfolio4 <- portfolio.spec(assets = colnames(train_data))
        portfolio4 <- add.constraint(portfolio = portfolio4, type = "full_investment")
        portfolio4 <- add.constraint(portfolio = portfolio4, type = "long_only")
        portfolio4 <- add.objective(portfolio = portfolio4, type = "return", name = "mean")
        portfolio_opt4 <- optimize.portfolio(R = train_data,
                                             portfolio = portfolio4,
                                             optimize_method = "ROI", # Sino no se cumple la restricción
                                             trace = T)
        pesos_maxretglobal <- extractWeights(portfolio_opt4)
        
        # Máxima Rentabilidad con Restricción de Riesgo

        portfolio40 <- portfolio.spec(assets = colnames(train_data))
        portfolio40 <- add.constraint(portfolio = portfolio40, type = "full_investment")
        portfolio40 <- add.constraint(portfolio = portfolio40, type = "long_only")
        portfolio40 <- add.objective(portfolio=portfolio40, type="return", name="mean")
        portfolio40 <- add.objective(portfolio = portfolio40,
                                     type="risk_budget_objective",
                                     name="CVaR", 
                                     max_prisk = 0.3,
                                     arguments=list(p=0.95,clean="boudt"))
        
        portfolio_opt40 <- optimize.portfolio(R = train_data,
                                              portfolio = portfolio40,
                                              optimize_method = "ROI", 
                                              trace = T)
        pesos_maxret2 <- extractWeights(portfolio_opt40)
        
        # S&P 500
        getSymbols("^GSPC", from = index(test_data)[1], to = nuevo_dia, src = "yahoo", auto.assign = TRUE)
        sp500_prices <- Cl(GSPC)
        sp500_returns_daily <- dailyReturn(sp500_prices)
        stored_outputs$sp500_returns <- sp500_returns_daily
        
        # Portfolio Rebalanceado
        n <- ncol(test_data)
        equal_weights <- rep(1/n, n) 
        EqualWeights <- Return.portfolio(
            R= test_data, 
            weights= equal_weights, 
            rebalance_on ="quarters", 
            verbose= TRUE)
        
        
        # Portfolio Naive
        portfolio_opt2 <- equal.weight(R = test_data,
                                       portfolio = base_case,
                                       optimize_method = "ROI")
        
        naive_w <- extractWeights(portfolio_opt2)
        
        # Calculamos retornos 
        retornos_tangentes <- test_data %*% pesos_tan
        retornos_minvar <- test_data %*% pesos_risk2
        retornos_maxret <- test_data %*% pesos_maxret2
        retornos_minvarglb <- test_data %*% pesos_risk
        retornos_maxretglb <- test_data %*% pesos_maxretglobal
        retornos_naive <- test_data%*%naive_w

        fechas_resultados <- index(test_data)
        df_retornos <- data.frame(Fecha = fechas_resultados, 
                                  "Retornos Equiponderados" = retornos_naive,
                                  "Retornos Máxima Rentabilidad Global" = retornos_maxretglb,
                                  "Retornos Máxima Rentabilidad" = retornos_maxret,
                                  "Retornos Rebalanceados" = EqualWeights$returns,
                                  "Retornos Tangentes" = retornos_tangentes, 
                                  "Retornos Mínimo Riesgo Global" = retornos_minvarglb,
                                  "Retornos Mínimo Riesgo" = retornos_minvar,
                                  "Retornos SP500" = sp500_returns_daily)

        colnames(df_retornos) <- c("Fecha", 
                           "Portfolio Naive",
                           "Portfolio Máxima Rentabilidad GLB", 
                           "Portfolio Máxima Rentabilidad",
                           "Portfolio Rebalanceado",
                           "Portfolio Tangente",
                           "Portfolio Mínimo Riesgo Global",
                           "Portfolio Mínimo Riesgo",
                           "SP500")

        df_retornos$Fecha <- as.Date(df_retornos$Fecha)
        df_retornos <- df_retornos[order(df_retornos$Fecha), ]
        
        df_retornos_xts <- xts(df_retornos[, c("Portfolio Naive",
                                               "Portfolio Máxima Rentabilidad GLB", 
                                               "Portfolio Máxima Rentabilidad",
                                               "Portfolio Rebalanceado",
                                               "Portfolio Tangente",
                                               "Portfolio Mínimo Riesgo Global",
                                               "Portfolio Mínimo Riesgo",
                                               "SP500")], order.by =df_retornos$Fecha)
        
        stored_outputs$df_ret <- df_retornos_xts
        tabla <- table.AnnualizedReturns(df_retornos_xts, Rf = .000128/365, geometric=FALSE)
        
        # Almacenamos la tabla para el resto de infoBox
        stored_outputs$tabla_resultados <- tabla
        
        # Esto es lo que voy cambiando de caja en caja
        resultado <- round(tabla["Annualized Return", input$Cartera]*100,2)

        valueBox(paste0(resultado, "%"),
                 "Retorno Anualizado", paste0(25 + input$count, "%"), icon = icon("money-bill-trend-up",style= "color:#ffffff",lib = "font-awesome"),
                 color = "navy"
        )
        
    })
    
    output$Riesgo <- renderInfoBox({
        
        tabla <- stored_outputs$tabla_resultados
        
        # Requesteamos el input del tipo de cartera
        
        req(input$Cartera)
        
        # Esto es lo que voy cambiando de caja en caja
        resultado <- round(tabla["Annualized Std Dev", input$Cartera]*100,2)
        
        
        valueBox(paste0(resultado, "%"),
            "Riesgo Anualizado", paste0(25 + input$count, "%"), icon = icon("triangle-exclamation", style="color: #ffffff",lib= "font-awesome"),
            color = "navy"
        )
    })
    
    output$Ratio_Sharpe <- renderInfoBox({
        
        tabla <- stored_outputs$tabla_resultados
        
        # Requesteamos el input del tipo de cartera
        
        req(input$Cartera)
        
        # Esto es lo que voy cambiando de caja en caja
        resultado <- round(tabla["Annualized Sharpe (Rf=0.01%)", input$Cartera],2)
        
        
        valueBox((resultado),
            "Ratio de Sharpe Anualizado", paste0(25 + input$count, "%"), icon = icon("file-lines",  style="color: #ffffff",lib = "font-awesome"),
            color = "navy"
        )
    })
    
    output$Principales_metricas <- renderDataTable({
        
        tabla <- stored_outputs$tabla_resultados
        datatable(tabla, options = list(scrollX = TRUE))
    })
    
    output$grafico_final <- renderPlot ({
        
        tabla <- stored_outputs$df_ret
        
        chart <- charts.PerformanceSummary(tabla, main = "Performance Carteras", cex.legend = 1.1, geometric=FALSE, Rf=.000128/365)
        print(chart)
    })
        
}

shinyApp(ui, server)
