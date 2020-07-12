library(shiny)
library(mqtt)
library(hrbrthemes)
library(tidyverse)

ui <- fluidPage(
    plotOutput('plot1'),
    plotOutput('plot2'),
    plotOutput('plot3')
)

server <- function(input, output) {
    get_temps <- function(n) {
        
        i <- 0
        max_recs <- n
        readings <- vector("character", max_recs)
        
        temp_cb <- function(id, topic, payload, qos, retain) {
            i <<- i + 1
            readings[i] <<- c(payload %>% readBin( "character")%>% jsonlite::fromJSON(),list(time= nanotime::nanotime(Sys.time())) )  %>% 
                jsonlite::toJSON(auto_unbox =TRUE)
            ifelse(i==max_recs,"quit" , "go") %>% return()
        }
        
        mqtt::topic_subscribe(host= "192.168.0.17" , port = 1883L, topic = "accelo1" , 
                              message_callback = temp_cb ,
                              connection_callback = mqtt::mqtt_silent_callback)
        
        readings %>% 
            purrr::map( jsonlite::fromJSON) %>% 
            purrr::map(magrittr::extract,c("x","y","z","time")) %>%
            purrr::map(unlist) %>%
            purrr::map_df(as.list) %>%   mutate(x = (x %>% as.numeric()+ 4600 ) / 100,
                                                y = (y %>% as.numeric()- 14800) / 100,
                                                z = (z %>% as.numeric()+ 7900 ) / 100 )
        
        
    }
    
    values <- reactiveValues(x = NULL, y = NULL, z = NULL, time = NULL)
    
    observeEvent(invalidateLater(1), {
        new_response <- get_temps(1)
        if (length(new_response) != 0) {
            values$x <- c(values$x,new_response$x)
            values$y <- c(values$y,new_response$y)
            values$z <- c(values$z,new_response$z)
            values$time <- c(values$time,new_response$time)
        }
    }, ignoreNULL = FALSE)
    
    output$plot1 <- renderPlot({
         data.frame(time = values$time, x = values$x) %>% 
         ggplot( aes(x = time, y=x)) +
            geom_line() 
    })
    
    output$plot2 <- renderPlot({
        data.frame(time= values$time, y = values$y) %>% 
         ggplot( aes(x = time, y=y )) +
            geom_line() +
            geom_point() 
    })
    
    
    output$plot3 <- renderPlot({
        data.frame(time = values$time, z = values$z) %>% 
        ggplot( aes(x = time, y=z)) +
            geom_line() +
            geom_point() 
    })
    
}

shinyApp(ui = ui, server = server)