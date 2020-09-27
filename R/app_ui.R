#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
Sys.setenv(TZ = "America/Sao_Paulo")
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  library(shinydashboard)
  tagList(
    dashboardPage(
      dashboardHeader(title = "Monitor do IPCA"),
      dashboardSidebar(disable = TRUE),
      dashboardBody(
        tags$head(tags$style(HTML(
          "* { font-family: Gadugi; }"))),
        dashboardthemes::shinyDashboardThemes(
          theme = "flat_red"
        ),
        box(width = 5, collapsible = FALSE,
            fluidRow(style = "background-color: white;",
                     column(4,
                            selectInput(inputId = "anomes",label = "IPCA Mensal",
                                        multiple = FALSE, selectize = FALSE,
                                        choices = unique(ipca_mes_grupo$anomes),
                                        selected = max(ipca_mes_grupo$anomes))
                     )
            ),
            fluidRow(style = "background-color: white;",
                     plotly::plotlyOutput(outputId = "plot2")),
            fluidRow(style = "background-color: white;",
                     column(8,
                            dateRangeInput(inputId = "range", label = "Intervalo do Histórico",
                                           start = as.Date("2013-01-01"), end = Sys.Date()-1,
                                           min = as.Date("2013-01-01"), max = Sys.Date()-1,
                                           language = "pt-BR",format = "yyyy/mm/dd")
                     )
            ),
            fluidRow(plotly::plotlyOutput(outputId = "plot3"))
        ),
        box(width = 7, collapsible = FALSE,
            fluidRow(style = "background-color: white;", width = 4,
                     column(2,
                            radioButtons("acum", label = "Nível de agregação",
                                         choices = list("Var. Mensal" = "ipca_mensal",
                                                        "Acum. 12m" = "ipca_acum"),
                                         selected = "ipca_mensal")
                     ),
                     column(1,
                            radioButtons("eixo_y", label = "Eixos Y",
                                         choices = list("Livres" = "free_y", "Fixos" = "fixed"),
                                         selected = "free_y")
                     ),
                     column(3,
                            selectInput(inputId = "years", label = "Anos selecionados",
                                        multiple = TRUE, selectize = TRUE,
                                        choices = factor(unique(year(ipca_mes_grupo$dt))),
                                        selected = c("2018","2019","2020"))
                     ),
                     column(6,
                            selectInput(inputId = "grupos", label = "Grupos do IPCA",
                                        multiple = TRUE, selectize = TRUE,
                                        choices = unique(ipca_mes_grupo$grupo),
                                        selected = sample(unique(ipca_mes_grupo$grupo),6))
                     )
            ),
            fluidRow(plotOutput(outputId = "plot1", height = 700),
                     tableOutput("text")),
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'monitor.ipca'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

