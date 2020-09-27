#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
    library(tidyr)
    library(plotly)
    source("R/load_data.R",encoding = "UTF-8")
    output$text <- renderText({"Este painel utiliza os dados do IBGE através da API SidraR. Tanto o dashboard quanto os dados são públicos e de uso gratuito"})
    
    # loadfonts(device = "win")
    custom_font <- "Gadugi"
    
    ipca_p1 <- reactive({
        ipca_mes_grupo %>%
            filter(grupo %in% input$grupos) %>%
            filter(as.character(year(dt)) %in% input$years) %>%
            mutate(ano = as.factor(year(dt)),
                   mes = factor(month(dt), levels = unique(month(dt)))
            ) %>%
            pivot_longer(c("ipca_mensal", "ipca_acum"),
                         names_to = "ipca",
                         values_to = "value_ipca") %>%
            filter(ipca == input$acum)
    })
    
    output$plot1 <- renderPlot({
        ipca_p1() %>%
            ggplot(aes(x = mes, y = value_ipca, group = ano, color = ano)) +
            geom_line(size = 1.5) +
            geom_hline(yintercept = 0, color = "black", linetype = "dotted", size = 1.25) +
            facet_wrap(~grupo, scales = input$eixo_y) +
            labs(title = "Var. % IPCA por Grupo") +
            xlab("Mês") + ylab("") +
            scale_y_continuous(labels = scales::percent_format(scale = 1,accuracy = 0.1),
                               breaks = scales::pretty_breaks(3)) +
            scale_color_tq() +
            theme_minimal(base_family = custom_font,
                          base_size = 16) +
            theme(plot.title = element_text(hjust = 0.5, colour = "black"),
                  legend.title = element_blank(),
                  legend.position = "top",
                  legend.text = element_text(face = "bold"),
                  strip.text = element_text(face = "bold"),)
    })
    
    output$plot2 <- plotly::renderPlotly({
        font <- list(
            family = custom_font,
            size = 15,
            color = "white"
        )
        label <- list(
            bgcolor = "#232F34",
            bordercolor = "transparent",
            font = font
        )
        
        ggplotly(
            ipca_mes_grupo %>%
                filter(anomes == input$anomes) %>%
                ungroup() %>%
                add_row(grupo = "TOTAL",ipca_mensal = as.numeric(last_ipca[1]), dt = max(.$dt), ipca_acum = NA) %>%
                mutate(Grupo = reorder(grupo, ipca_mensal, mean),
                       IPCA = round(ipca_mensal,2)) %>%
                
                ggplot(aes(x = Grupo,
                           y = IPCA,
                           fill = factor(ifelse(grupo == "TOTAL" , "#2C3E50", "#E31A1C")))) +
                geom_bar(stat = "identity") +
                ylab("% a.m") + xlab("") + labs(title = paste0("Inflação por Grupo - ",input$anomes)) +
                coord_flip() +
                scale_fill_manual(values = c("#E31A1C", "#2C3E50" )) +
                scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
                theme_minimal(base_family = custom_font,
                              base_size = 16)+
                theme(legend.position = "none",
                      plot.title = element_text(colour = "black"),
                      axis.text.x = element_text(colour = "black", size = 10),
                      axis.text.y = element_text(face = "bold", colour = "black"),
                      axis.title.x = element_text(colour = "black"))
            , tooltip = c("y", "x")) %>%
            style(hoverlabel = label) %>%
            layout(font = font, autosize = TRUE) %>%
            config(displayModeBar = FALSE)
    })
    
    output$plot3 <- plotly::renderPlotly({
        font <- list(
            family = custom_font,
            size = 15,
            color = "white"
        )
        label <- list(
            bgcolor = "#232F34",
            bordercolor = "transparent",
            font = font
        )
        
        plot3 <- ipca_mes %>%
            rename(mensal = ipca_mensal, acum = ipca_acum) %>%
            gather(key = "ipca_tipo", value = "valor", -dt) %>%
            filter(dt >= input$range[1] & dt <= input$range[2]) %>%
            mutate(Data = dt,
                   IPCA = round(valor/100,4)) %>%
            
            ggplot(aes(x = Data, y = IPCA, color = ipca_tipo, group=1,
                       text = paste0("IPCA: ", IPCA*100,
                                     "\nData: ", Data))) +
            geom_line(size = 1.5) +
            facet_wrap(. ~ ipca_tipo, nrow = 2,
                       scales = "free_y",
                       labeller = labeller(ipca_tipo = c(acum = "IPCA Acum. (12m)", mensal = "IPCA Mensal"))) +
            geom_hline(data = data.frame(yint=0, ipca_tipo="mensal"),
                       aes(yintercept = yint),
                       color = "black",
                       linetype = "dashed") +
            xlab("") + ylab("") +
            scale_x_date(date_labels = "%Y",
                         date_breaks = "1 year") +
            scale_y_continuous(n.breaks = 6,
                               labels = scales::percent_format(scale = 100)) +
            tidyquant::scale_color_tq() +
            theme_minimal(base_family = custom_font,
                          base_size = 16) +
            theme(legend.position = "none",
                  # title = element_text(face = "bold"),
                  plot.title = element_text(colour = "black"),
                  strip.text.x = element_text(face = "bold", size = 16),
                  axis.text.x = element_text(colour = "black"),
                  axis.text.y = element_text(colour = "black"))
        
        ggplotly(plot3, tooltip = c("text")) %>%
            style(hoverlabel = label) %>%
            layout(font = font, autosize = TRUE) %>%
            config(displayModeBar = FALSE)
    })
}
