#
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html
#
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(lubridate)
library(tpltheme)
library(ggtext)
library(ggalt)
library(ggridges)
library(scales)
library(viridis)
library(forcats)

shinyApp(
    ui = tagList(
        navbarPage(
            "Design Contest",
            theme = shinytheme("cosmo"),
            tabPanel("Interactive Dumbell Plot",
                     sidebarPanel(
                         h4("Select the Year to see the data"),
                         sliderInput("year1", "Year :", min=1980, max=2019, value=1980, 
                                     animate =
                                         animationOptions(interval=1000,loop=TRUE))
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Dumbell Plot",
                                      plotOutput("dumbellPlot")
                             ),
                             tabPanel("Data",
                                      h4("Please find the data below"),
                                      DT::dataTableOutput("table1")
                                      )
                         )
                     )
            ),
            tabPanel("Interactive Distribution",
                     sidebarPanel(
                         h4("Select the Year to see the corresponding distribution"),
                         sliderInput("year2", "Year :", min=1980, max=2019, value=1985, 
                                     animate =
                                         animationOptions(interval=2000,loop=TRUE))
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Ggridges",
                                      plotOutput("ridgePlot")
                             ),
                             tabPanel("Data",
                                      h4("Please find the data below"),
                                      DT::dataTableOutput("table2")
                             )
                         )
                     )
            )
        )
    ),
    
    server = function(input, output) {
        
        ipf_lifts <- read_csv("data/ipf_lifts.csv")
        ipf_lifts1 <- ipf_lifts %>% 
            mutate(year = lubridate::year(date))
        ipf_lifts_reshape <- ipf_lifts1 %>% 
            tidyr::pivot_longer(cols = c("best3squat_kg", "best3bench_kg", "best3deadlift_kg"), names_to = "lift") %>% 
            select(name, sex, year, lift, value)
        ipf_lifts_maxes <- ipf_lifts_reshape %>% 
            group_by(year, sex, lift) %>% 
            top_n(1, value) %>% 
            ungroup %>% 
            distinct(year, lift, value, .keep_all = TRUE)
        max_pivot <- ipf_lifts_maxes %>% 
            spread(sex, value)
        male_lifts <- max_pivot %>% 
            select(-name) %>% 
            filter(!is.na(M)) %>% 
            group_by(year, lift) %>% 
            summarise(male = mean(M))
        
        female_lifts <- max_pivot %>% 
            select(-name) %>% 
            filter(!is.na(`F`)) %>% 
            group_by(year, lift) %>% 
            summarise(female = mean(`F`))
        max_lifts <- merge(male_lifts, female_lifts)
        max_lifts_final <- max_lifts %>% 
            group_by(year, lift) %>% 
            mutate(diff = male - female)
        max_lift_final_year <- reactive({filter(max_lifts_final,year==input$year1)})
        
        output$dumbellPlot <- renderPlot({
            ggplot(max_lift_final_year()) + 
                ggalt::geom_dumbbell(aes(y = lift,
                                  x = female, xend = male),
                              colour = "grey", size = 5,
                              colour_x = "#D6604C", colour_xend = "#395B74") +
                labs(y = element_blank(),
                     x = "Top Lift Recorded (kg)",
                     title =  "How <span style='color:#D6604C'>Women</span> and <span style='color:#395B74'>Men</span> Differ in Top Lifts") +
                theme(plot.title = element_markdown(lineheight = 1.1, size = 20),
                      plot.subtitle = element_text(size = 15)) +
                scale_y_discrete(labels = c("Bench", "Deadlift", "Squat")) +
                drop_axis(axis = "y") +
                geom_text(aes(x = female, y = lift, label = paste(female, "kg")),
                          color = "#D6604C", size = 4, vjust = -2) +
                geom_text(aes(x = male, y = lift, label = paste(male, "kg")),
                          color = "#395B74", size = 4, vjust = -2) +
                geom_rect(aes(xmin=430, xmax=470, ymin=-Inf, ymax=Inf), fill="grey80") +
                geom_text(aes(label=diff, y=lift, x=450), fontface="bold", size=4) +
                geom_text(aes(x=450, y=3, label="Difference"),
                          color="grey20", size=4, vjust=-3, fontface="bold")
        })
        
        output$table1 <- DT::renderDataTable(DT::datatable(max_lifts_final))
        
        ipf_lifts2 <- ipf_lifts %>%
            mutate(year = lubridate::year(date)) %>%
            mutate(age_class = fct_rev(as.factor(age_class))) %>%
            filter(age_class != '5-12') %>%
            filter(age_class != '80-999') %>%
            drop_na(c(age_class,best3squat_kg))
            
        ipf_lifts_year <- reactive({filter(ipf_lifts2,year==input$year2)})
        
        output$ridgePlot <- renderPlot({
            ggplot(ipf_lifts_year(),aes(x=best3squat_kg,y=age_class,fill=factor(..quantile..))) +
                stat_density_ridges(geom="density_ridges_gradient",calc_ecdf = TRUE,
                                    quantiles = 4,quantile_lines = TRUE,
                                    na.rm = TRUE) +
                scale_fill_viridis(discrete = TRUE,name="Quartiles") +
                scale_x_continuous(limits = c(0,510),expand = c(0,0),labels=unit_format(unit="Kg")) +
                theme_bw() +
                labs(
                    title = "Distribution of Weight lifted in Squat for different Age Groups",
                    x = "",
                    y = "Age Classification"
                )
        })
            
        output$table2 <- DT::renderDataTable(DT::datatable(ipf_lifts2))
        
        output$table3 <- renderTable({
            head(cars, 10)
        })
    }
)