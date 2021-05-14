library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(forecast)
library(shinythemes)
library(plotly)

# Descargamos los datos abiertos del covid de Chile
data <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19_std.csv", encoding = "UTF-8")

# Agrupamos por comuna para calcular los casos nuevos
# El dataset viene con casos acumulados, por lo que hay que crear una nueva variable para obtener ese dato
data_agrup <- data %>% 
    group_by(Comuna) %>% 
    mutate(Nuevos = Casos.confirmados - lag(Casos.confirmados)) %>% 
    ungroup()

# Ahora agrupamos por región los datos y calculemos las medias móviles
agrup <- data_agrup %>% 
    na.omit() %>% 
    group_by(Region, Comuna, Poblacion, Fecha) %>% 
    summarise(Nuevos_acum = sum(Casos.confirmados)) %>% 
    mutate(Nuevos = Nuevos_acum - lag(Nuevos_acum),
           Fecha = ymd(Fecha),
           Incidencia = round(Nuevos / Poblacion * 100000, 1), 
           MM_simple = round(rollmean(Incidencia, k = 7, fill = NA), 1), # Media móvil con una ventana de 7 días
           MM_pond = round(ma(Incidencia, order = 4, centre = TRUE), 1)
           )

# Capturamos la fecha máxima que tiene datos del dataset para incluirlo en el plot
last_date <- max(data_agrup$Fecha)


# Define UI ----

ui <- fluidPage(theme = shinytheme("cerulean"),
    
    navbarPage("Visor de casos nuevos COVID-19, Chile",  
    
    #titlePanel("Visor de casos nuevos COVID-19, Chile"),
                
    tabPanel("Comunal"),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "comuna", 
                        label = strong("Elige las comunas"),
                        selected = "Alhue",
                        choices = sort(unique(agrup$Comuna), decreasing = FALSE),
                        multiple = TRUE),
            
            helpText("Este visor te permite ver la evolución de una comuna en particular, respecto de los casos nuevos diarios,
                     o elegir varias para compararlas."),
            br(),
            helpText(paste("Datos actualizados al", last_date)),
            br(),
            br(),
            strong("Nota:"),
            p("En este dashboard podrás encontrar datos sobre los casos nuevos de COVID-19 
              informados por el MINSAL y el Ministerio de Ciencias en su GitHub. Podrás tener una gráfca de 
              la evolución de los casos a lo largo del tiempo y también tendrás acceso a los datos
              tabulados."),
            p("Le he agregado la media móvil simple semanal centrada (MM_simple) para suavizar la curva. Además, se muestra
              la media móvil ponderada (MM_pond) en la tabla adjunta para cada comuna."),
            
            br(),
            a("Enlace al GitHub Min. de Ciencias", href = "https://github.com/MinCiencia/Datos-COVID19"),
            br(),
            br(),
            p("Elaborado por Paulo Villarroel"),
            a("Revisa el GitHub de este proyecto", href = "https://github.com/paulovillarroel/visor_casos")
            
            ),
        
        mainPanel(
            
            tabsetPanel(
                id = "tabset",
                
                tabPanel("Gráfico", 
                         br(),
                         "Gráfico de evolución de incidencia de casos diarios", 
                         br(),
                         br(),
                plotlyOutput("plot_comuna")),
                
                tabPanel("Datos", 
                         br(),
                         "Tabla de datos",
                         br(),
                         br(),
                DT::dataTableOutput("table"),
                downloadButton(outputId = "download_data", label = "Descargar datos"))
                )
            )
        )
        
    )

)
    

# Define server logic ----
server <- function(input, output, session) {
    
    output$plot_comuna <- renderPlotly({
        
        # Gráfico en blanco si no hay selección de comuna
        if (is.null(input$comuna)) {
            
            ggplotly(p <- ggplot() +
                         theme_bw())
            
        } 
        
        else {
            
            p1 <- agrup %>% filter(Comuna == input$comuna) %>%
                ggplot() +
                geom_path(aes(Fecha, MM_simple, color = Comuna), size = 1) +
                geom_line(aes(Fecha, Incidencia, color = Comuna), size = 0.6, alpha = 0.4, linetype = "dotted") +
                theme_bw() +
                labs(title = " ",
                     x = "",
                     y = "Casos por 100 mil habts.") +
                theme(plot.title = element_text(size = 18),
                      plot.subtitle = element_text(size = 12),
                      axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10)
                )
            
            
            return(p1) 
        }

        
    })
    
    
    output$panel <- renderText({
        paste("Panel: ", input$tabset)
    })
    
    
    output$table <- DT::renderDataTable({
        
        tabla <- agrup %>% filter(Comuna == input$comuna)
        DT::datatable(tabla, options = list(orderClasses = FALSE))
            
    })
    
    

    output$download_data <- downloadHandler(
        
        filename = function() {
            
            paste0("casos_comunas_", Sys.Date(), ".csv")
            
        },
        
        content = function(file) {
            
            tabla <- agrup %>% filter(Comuna == input$comuna)
        
            write.csv(tabla, file, row.names = FALSE)
        }
    )
    
    
}


# Run the app ----
shinyApp(ui = ui, server = server)