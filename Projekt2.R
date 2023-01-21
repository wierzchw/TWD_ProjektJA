library(shinycssloaders)
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(plotly)
library(vistime)
library(lubridate)
library(stringr)
WW_wykres2 <- read.csv("wykres2_WW", sep=",", encoding = "UTF-8")
ST_wykres2 <- read.csv("wykres2_ST", sep=",", encoding = "UTF-8")
MM_wykres2 <- read.csv("wykres2_MM", sep=",", encoding = "UTF-8")

MM_wykres3 <- read.csv("wykres_3_MM", sep=",", encoding = "UTF-8")
WW_wykres3 <- read.csv("wykres_3_WW", sep=",", encoding = "UTF-8")
ST_wykres3 <- read.csv("wykres_3_ST", sep=",", encoding = "UTF-8")

ST <- read.csv("wykres_1_5_ST", sep=",", encoding = "UTF-8")
WW <- read.csv("wykres_1_5_WW", sep=",", encoding = "UTF-8")
MM <- read.csv("wykres_1_5_MM", sep=",", encoding = "UTF-8")

WW2 <- read.csv("proba", sep=",", encoding = "UTF-8")



# Define UI for application that draws a histogram
ui1 <- fluidPage(
  
  # Application title
  titlePanel("Czas na komputerze"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    fluidRow(
      column(3, dateRangeInput('dateRange',
                     label = 'Zakres dni',
                     start = "2022-12-12", end = "2023-01-08",
                     min = "2022-12-12", max = "2023-01-08"
                     )
             ),
      column(3, checkboxGroupInput("urzadzenie", "Urządzenie:",
                         c("telefon" = "telefon",
                           "komputer" = "komputer"),
                          selected=c("telefon", "komputer"),inline = TRUE)
             ),
      column(5, checkboxGroupInput("dni_tygodnia", "Dni tygodnia:",
                                   c("poniedziałek" = "poniedziałek",
                                     "wtorek" = "wtorek",
                                     "środa" = "środa",
                                     "czwartek" = "czwartek",
                                     "piątek" = "piątek",
                                     "sobota" = "sobota",
                                     "niedziela" = "niedziela"),selected=c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"),inline = TRUE)
      ),
      width = 12
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(12, splitLayout(cellWidths = c(1570),shinycssloaders::withSpinner(plotlyOutput("plot1"),
                                                                                        type = getOption("spinner.type", default = 1),
                                                                                        color = getOption("spinner.color", default = "#2c3e50"),
                                                                                        size = getOption("spinner.size", default = 1))),
                      column(12,splitLayout(cellWidths = c(1700), shinycssloaders::withSpinner(plotlyOutput("plot2"),
                                                                                               type = getOption("spinner.type", default = 1),
                                                                                               color = getOption("spinner.color", default = "#2c3e50"),
                                                                                               size = getOption("spinner.size", default = 1)))))
               
               )
      )
    
  )
)

ui2 <- fluidPage(
  
  # Application title
  titlePanel("Top aplikacje"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    fluidRow(
      column(3, dateRangeInput('dateRange2',
                               label = 'Zakres dni',
                               start = "2022-12-12", end = "2023-01-08",
                               min = "2022-12-12", max = "2023-01-08")
      ),
      column(3, checkboxGroupInput("urzadzenie2", "Urządzenie:",
                                   c("telefon" = "telefon",
                                     "komputer" = "komputer"),
                                   selected=c("telefon", "komputer"),inline = TRUE)
      ),
      column(5, checkboxGroupInput("dni_tygodnia2", "Dni tygodnia:",
                                   c("poniedziałek" = 1,
                                     "wtorek" = 2,
                                     "środa" = 3,
                                     "czwartek" = 4,
                                     "piątek" = 5,
                                     "sobota" = 6,
                                     "niedziela" = 7),selected=c(1,2,3,4,5,6,7),inline = TRUE)
      ),
      column(7, sliderInput("godziny",
                            "Zakres godzin:",
                            min = 0,
                            max = 23,
                            value = c(0,23))
      ),
      width = 12
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(column(12, splitLayout(cellWidths = c(1600), shinycssloaders::withSpinner(plotlyOutput("plot3"),
                                                                                         type = getOption("spinner.type", default = 1),
                                                                                         color = getOption("spinner.color", default = "#2c3e50"),
                                                                                         size = getOption("spinner.size", default = 1))),
                      column(12,splitLayout(cellWidths = c(1700), shinycssloaders::withSpinner(plotlyOutput("plot4"),
                                                                                               type = getOption("spinner.type", default = 1),
                                                                                               color = getOption("spinner.color", default = "#2c3e50"),
                                                                                               size = getOption("spinner.size", default = 1)))))
               
      )
    )
  )
)

ui3 <- fluidPage(
  
  # Application title
  titlePanel("Oś czasu"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    fluidRow(
      column(3, dateInput('day', 
                          label = 'Dzień',
                          value = "2022-12-12",
                          min = "2022-12-12", max = "2023-01-08")
      ),
      column(3, checkboxGroupInput("urzadzenie3", "Urządzenie:",
                                   c("telefon" = "tel",
                                     "komputer" = "komp"),
                                   selected=c("tel", "komp"),inline = TRUE)
      ),
      width = 12
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c(553, 553, 553), shinycssloaders::withSpinner(plotlyOutput("plot7"),
                                                                                type = getOption("spinner.type", default = 1),
                                                                                color = getOption("spinner.color", default = "#2c3e50"),
                                                                                size = getOption("spinner.size", default = 1)), 
                    shinycssloaders::withSpinner(plotlyOutput("plot8"),
                                                 type = getOption("spinner.type", default = 1),
                                                 color = getOption("spinner.color", default = "#2c3e50"),
                                                 size = getOption("spinner.size", default = 1)),
                    shinycssloaders::withSpinner(plotlyOutput("plot9"),
                                                 type = getOption("spinner.type", default = 1),
                                                 color = getOption("spinner.color", default = "#2c3e50"),
                                                 size = getOption("spinner.size", default = 1)))
      )
    )
  )
)



# Define server logic required to daw a histogram
server <- function(input, output, session) {
 
  
  output$plot1 <- renderPlotly({
    zaczytanie <-function(df){
      df <- df  %>% 
        filter(day >= input$dateRange[1]  & day <= input$dateRange[2])%>%
        filter(week_day %in% input$dni_tygodnia )%>%
        filter(type %in% input$urzadzenie) %>%
        select(-c(category))
      
      czas <- df %>%
        summarise(days = as.Date(input$dateRange[2]) - as.Date(input$dateRange[1]) + 1)
      
      df <- df %>% 
        group_by(hour) %>%
        summarise(duration = as.integer(sum(duration)/as.integer(czas$days)))
      return(df)
    }
    
    dodanie_zer <- function(df, i) {
      czyZmiana <- df %>% 
        filter(hour == i)
      
      if (nrow(czyZmiana) != 1){
        df <- rbind(df ,data.frame(hour = i, duration = 0))
      }
      return(df)
    }
    
    wykres <- function(df, show) {
      df <- df %>%
        arrange(desc(hour))
      plt <- plot_ly(df, x = ~hour,
                     y = ~duration,
                     type = "scatter",
                     mode = "lines+markers",
                     marker = list(size = 10, color = "#2c3e50"),
                     line = list(color = "#2c3e50"),
                     showlegend = show,
                     hoverinfo = 'text',
                     text = ~paste('</br> Czas łączny',
                                   '</br> Godzina: ', hour, ":00 - ", hour ,":59",
                                   '</br> Średnio minut: ', duration)
      )
      return(plt)
    }
    
    wykresST <- zaczytanie(ST)
    wykresWW <- zaczytanie(WW)
    wykresMM <- zaczytanie(MM)
    
    for (i in 0:23){
      wykresST <- dodanie_zer(wykresST, i)
      wykresWW <- dodanie_zer(wykresWW, i)
      wykresMM <- dodanie_zer(wykresMM, i)
    }
    
    p1 <- wykres(wykresWW, T)
    p2 <- wykres(wykresST, F)
    p3 <- wykres(wykresMM, F)
    
    subplot(p1,p2,p3,nrows = 1, shareX = TRUE, shareY = TRUE)  %>%
      layout(xaxis = list(title = ""),
             xaxis2 = list(title = ""),
             xaxis3 = list(title = ""),
             yaxis = list( title = "średnia liczba minut"
             )) %>%
      layout(
        annotations = list(
          list(
            x = 0.16,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 1",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.85,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )))
  })
  
  output$plot2 <- renderPlotly({
    
    
    zaczytanie <-function(df){
      df <- df  %>% 
        filter(day >= input$dateRange[1]  & day <= input$dateRange[2])%>%
        filter(week_day %in% input$dni_tygodnia )%>%
        filter(type %in% input$urzadzenie)
      
      czas <- df %>%
        summarise(days = as.Date(input$dateRange[2]) - as.Date(input$dateRange[1]) + 1)
      
      df <- df %>% 
        group_by(category, hour) %>%
        summarise(duration = as.integer(sum(duration)/as.integer(czas$days)))
      return(df)
    }
    
    dodanie_zer <- function(df, i) {
      czyZmiana <- df %>% 
        filter(hour == i)
      
      if (nrow(czyZmiana) != 3){
        if (nrow(czyZmiana %>% filter(category == "praca")) == 0){
          df <- rbind(df ,data.frame(hour = i,  category = "praca", duration = 0))
        }
        if (nrow(czyZmiana %>% filter(category == "rozrywka")) == 0){
          df <- rbind(df ,data.frame(hour = i,  category = "rozrywka", duration = 0))
        }
        if (nrow(czyZmiana %>% filter(category == "inne")) == 0){
          df <- rbind(df ,data.frame(hour = i,  category = "inne", duration = 0))
        }
      }
      return(df)
    }
    
    wykres <- function(df, show) {
      df <- df %>%
        arrange(desc(hour))
      plt <- plot_ly(df, x = ~hour,
                     y = ~duration,
                     type = "scatter",
                     mode = "lines+markers",
                     marker = list(size = 10),
                     color = ~category,
                     showlegend = show,
                     hoverinfo = 'text',
                     text = ~paste('</br> Aktywność: ', category,
                                   '</br> Godzina: ', hour, ":00 - ", hour ,":59",
                                   '</br> Średnio minut: ', duration)
      )
      return(plt)
    }
    
    
    
    wykresST <- zaczytanie(ST)
    wykresWW <- zaczytanie(WW)
    wykresMM <- zaczytanie(MM)
    
    for (i in 0:23){
      wykresST <- dodanie_zer(wykresST, i)
      wykresWW <- dodanie_zer(wykresWW, i)
      wykresMM <- dodanie_zer(wykresMM, i)
    }
    
    p1 <- wykres(wykresWW, T)
    p2 <- wykres(wykresST, F)
    p3 <- wykres(wykresMM, F)
    
    subplot(p1,p2,p3,nrows = 1, shareX = TRUE, shareY = TRUE)  %>%
      layout(xaxis = list(title = "Godzina"),
             xaxis2 = list(title = "Godzina"),
             xaxis3 = list(title = "Godzina"),
             yaxis = list( title = "Średnia liczba minut"
             )) %>%
      layout(
        annotations = list(
          list(
            x = 0.16,
            y = 1,
            font = list(size = 20),
            text = "",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.85,
            y = 1,
            font = list(size = 20),
            text = "",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )))
  })
  
  output$plot3 <- renderPlotly({
    wykres <- function(df){
  df <- df %>%
      filter(date >= input$dateRange2[1]  & date <= input$dateRange2[2])%>%
      filter(hour >= input$godziny[1]  & hour <= input$godziny[2])%>%
      filter(week_day %in% input$dni_tygodnia2 )%>%
      filter(typ %in% input$urzadzenie2) %>%
      group_by(app) %>%
      summarise(godziny = sum(duration)/3600)  %>%
      mutate(app = fct_reorder(app, godziny, .desc = TRUE) ) %>%
      arrange(desc(godziny)) %>%
      head(8)
    
  plot <- plot_ly(df ,x = ~app,
                  y = ~godziny,
                  type = "bar",
                  name=" ",
                  marker = list(color = "#2c3e50"),
                  hovertemplate = paste('%{x}', '<br>Liczba godzin: %{y:.2f}<br>')
  )
  plot
    return(plot)
    }
    
    p1 <- wykres(WW_wykres2)
    p2 <- wykres(ST_wykres2)
    p3 <- wykres(MM_wykres2)
    
      
    subplot(p1,p2,p3,nrows = 1, shareX = TRUE, shareY = TRUE)  %>%
      layout(xaxis = list(title = "Aplikacje", tickangle = 35),
             xaxis2 = list(title = "Aplikacje", tickangle = 35),
             xaxis3 = list(title = "Aplikacje", tickangle = 35),
             yaxis = list( title = "Liczba godzin"
             ),showlegend = F) %>%
      layout(
        annotations = list(
          list(
            x = 0.16,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 1",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.5,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 2",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          ),
          list(
            x = 0.85,
            y = 1,
            font = list(size = 20),
            text = "DANONEK 3",
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "bottom",
            showarrow = FALSE
          )))
    })
  output$plot4 <- renderPlotly({
    df2 <- WW_wykres2 %>%
      filter(date >= input$dateRange2[1]  & date <= input$dateRange2[2])%>%
      filter(hour >= input$godziny[1]  & hour <= input$godziny[2])%>%
      filter(week_day %in% input$dni_tygodnia2 )%>%
      filter(typ %in% "komputer") %>%
      group_by(app) %>%
      summarise(godziny = sum(duration)/3600)  %>%
      mutate(app = fct_reorder(app, godziny, .desc = TRUE) ) %>%
      arrange(desc(godziny)) %>%
      head(8)
    
    df <- WW2 %>%
      filter(day >= input$dateRange2[1]  & day <= input$dateRange2[2])%>%
      filter(hour >= input$godziny[1]  & hour <= input$godziny[2])%>%
      filter(week_day %in% input$dni_tygodnia2 )%>%
      filter(type %in% input$urzadzenie2) %>%
      filter(app %in% df2$app) %>%
      group_by(app) %>%
      mutate(sum = sum(duration)) %>%
      group_by(app, st) %>%
      mutate(sum_g = sum(duration)) %>%
      mutate(percent = as.integer (sum_g)/as.integer (sum)) %>%
      summarise(percent = mean(percent)*100, duration = mean(duration)*100) %>%
      select(app, percent, st) %>% inner_join(df2, by = "app") %>%
      ungroup()%>%
      mutate(app = fct_reorder(app, godziny, .desc = TRUE) ) %>%
      arrange(desc(godziny))
      
    
    
    
    
    fig <- plot_ly(df, x = ~app, y = ~percent, type = 'bar', color = ~ st,
                   colors = c('n-afk' = '#CC1480', 'afk' = '#E1C8B4')) %>% 
      layout(yaxis = list(title = 'Count'), barmode = 'stack')
  fig
    })
    
    
  output$plot7 <- renderPlotly({
      df <- WW_wykres3 %>%
        filter(day == input$day, type %in% input$urzadzenie3)
      
      if (nrow(df) == 0){
        df <- df %>% select(c(app,start, end, color, tooltip))
        
        start <- as.POSIXct(paste(input$day, "00:00:01", sep=" "))
        end <- as.POSIXct(paste(input$day, "23:59:59", sep=" "))
        
        df <- rbind(df,data.frame(app = " ", 
                                 start = start,
                                 end = end,
                                 color = "transparent",
                                 tooltip = "")
                    ) %>% 
          mutate(tooltip = paste(app,"\n", "Od", format(start, "%H:%M:%S"), "do", format(end, "%H:%M:%S")))
      }
      
      plot<- df %>% vistime(
        col.event = "app",
        col.start = "start",
        col.end = "end",
        col.group = "app",
        show_labels = F
      ) %>%
        layout(
          xaxis = list(
            type = "date",
            tickformat = "%H:00"
          )
        )
      
      plot
    })
    
    output$plot8 <- renderPlotly({
      df <- ST_wykres3 %>%
        filter(day == input$day, type %in% input$urzadzenie3)
      
      if (nrow(df) == 0){
        df <- df %>% select(c(app,start, end, color, tooltip))
        
        start <- as.POSIXct(paste(input$day, "00:00:01", sep=" "))
        end <- as.POSIXct(paste(input$day, "23:59:59", sep=" "))
        
        df <- rbind(df,data.frame(app = " ", 
                                  start = start,
                                  end = end,
                                  color = "transparent",
                                  tooltip = "")
        ) %>% 
          mutate(tooltip = paste(app,"\n", "Od", format(start, "%H:%M:%S"), "do", format(end, "%H:%M:%S")))
      }
      
      plot<- df %>% vistime(
        col.event = "app",
        col.start = "start",
        col.end = "end",
        col.group = "app",
        show_labels = F
      ) %>%
        layout(
          xaxis = list(
            type = "date",
            tickformat = "%H:00"
          )
        )
      
      plot
    })
    output$plot9 <- renderPlotly({
      df <- MM_wykres3 %>%
        filter(day == input$day, type %in% input$urzadzenie3)
      
      if (nrow(df) == 0){
        df <- df %>% select(c(app,start, end, color, tooltip))
        
        start <- as.POSIXct(paste(input$day, "00:00:01", sep=" "))
        end <- as.POSIXct(paste(input$day, "23:59:59", sep=" "))
        
        df <- rbind(df,data.frame(app = " ", 
                                  start = start,
                                  end = end,
                                  color = "transparent",
                                  tooltip = "")
        ) %>% 
          mutate(tooltip = paste(app,"\n", "Od", format(start, "%H:%M:%S"), "do", format(end, "%H:%M:%S")))
      }
      
      plot<- df %>% vistime(
        col.event = "app",
        col.start = "start",
        col.end = "end",
        col.group = "app",
        show_labels = F
      ) %>%
        layout(
          xaxis = list(
            type = "date",
            tickformat = "%H:00"
          )
        )
      
      plot
    })
}
 
  
app_ui <- navbarPage(
  title = "Projekt JA",
  tabPanel("Czas na komputerze", ui1),
  tabPanel("Top aplikacjie", ui2),
  tabPanel("Oś czasu", ui3),
  theme = bslib::bs_theme(bootswatch = "flatly")
)

# Run the application 
runApp(shinyApp(app_ui, server))
