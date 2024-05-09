library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(lubridate)
library(DT)


';l'

# Load my dataset
marathon_data <- read_csv("world_marathon_majors.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  # titlePanel("Major World Marathon Leaders"),
  
  # Sidebar with a slider input for number of bins 
  #sidebarLayout(
   # sidebarPanel(
      # selectInput("marathon", "Race Location", choices = c(marathon_data$marathon, "NONE")),
      # selectInput("year", "Year", choices = c(marathon_data$year, "NONE")),
      #actionButton("submit", "Submit")  # Add submit button
      
   # ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Race Location-Year",
        selectInput("marathon", "Race Location", choices = c(marathon_data$marathon, "NONE")),
        selectInput("year", "Year", choices = c(marathon_data$year, "NONE")),
        actionButton("submit", "Submit"),  # Add submit button
      # Output for top male finisher
      tableOutput("top_male_finisher"),
      # Output for top female finisher
      tableOutput("top_female_finisher"),
      # Output for box plot
      plotOutput("raceplot"),
      DTOutput("topmale"),
      DTOutput("topfemale"),
      DTOutput("race_winners_table"),
      DTOutput("female_winners"),
      DTOutput("male_winners")
    ),
    
    ###
    # This is for my second tab 
    tabPanel("Country-Gender",
             selectizeInput("country", "Country", choices = c(marathon_data$country, "NONE"), multiple = TRUE),
             selectizeInput("marathon", "Race Location", choices = c(marathon_data$marathon, "NONE"), multiple = TRUE),
             selectInput("gender", "Gender", choices = c( "Male", "Female", "NONE")),
             actionButton("submit2", "Submit"),
             tableOutput("printtable"),
             plotOutput("dot_plot")
             )
  )
    )
)


server <- function(input, output) {
  
  # Observe the click event of the submit button
  observeEvent(input$submit, 
               {
                 # Update outputs based on user selections
                 if (input$marathon != "NONE" & input$year != "NONE") {
                   filtered_data <- reactive({
                     filter(marathon_data, marathon == input$marathon & year == input$year)
                   })
                   
                   # Retrieve top male finishers
                   top_male_finisher <- reactive({
                     top_male <- head(arrange(filter(filtered_data(), gender == "Male"),time), 1) |>
                       select(winner, time, country)
                     return(top_male)
                   })
                   
                   # Output top male finishers
                   output$topmale <- renderDT({
                     top_male <- top_male_finisher()
                     top_male$time <- as.POSIXct(top_male$time, origin = "1970-01-01", tz = "UTC")
                     top_male$time <- format(top_male$time, "%H:%M:%S")
                     top_male <- top_male[, c("winner", "time", "country")] # Select relevant columns
                     DT::datatable(
                       top_male,
                       caption = "Top Male Finisher",
                       options = list(dom = 't', paging = FALSE)
                     )
                   })
                   
                   # Retrieve top female finisher
                   top_female_finisher <- reactive({
                     top_female <- head(arrange(filter(filtered_data(), gender == "Female"),time), 1) |>
                       select(winner, time, country)
                     return(top_female)
                   })
                   
                   # Output top female finisher
                   output$topfemale <- renderDT({
                     top_female <- top_female_finisher()
                     top_female$time <- as.POSIXct(top_female$time, origin = "1970-01-01", tz = "UTC")
                     top_female$time <- format(top_female$time, "%H:%M:%S")
                     top_female <- top_female[, c("winner", "time", "country")] # Select relevant columns
                     DT::datatable(
                       top_female,
                       caption = "Top Female Finisher",
                       options = list(dom = 't', paging = FALSE)
                     )
                   })
                   
                   # Create box plot of top finishers' times by gender
                   output$raceplot <- renderPlot({
                     male_time <- (top_male_finisher()$time)
                     female_time <- (top_female_finisher()$time)
                     
                     data <- data.frame(
                       Gender = c("Male", "Female"),
                       Time = c(male_time, female_time)
                     )
                     
                     ggplot(data, aes(x = Gender, y = Time, fill = Gender)) +
                       geom_bar(stat = "identity") +
                       labs(title = "Top Finisher's Gender Comparison",
                            x = "Gender",
                            y = "Finish Time") +
                       theme_minimal()
                   })
                 } 
                 
                 else if (input$marathon == "NONE" & input$year != "NONE") {
                   # Check if only year is provided
                   # observeEvent(input$year, {
                   # Bar plot of men vs women for all 6 races of the specified year
                   output$raceplot <- renderPlot({
                     data <- marathon_data |>
                       filter(year == input$year)
                     
                     ggplot(data, aes(x = marathon, y= time, fill = gender)) +
                       geom_col(position = "dodge") +
                       labs(title = paste("Men vs Women for All Races in", input$year),
                            x = "Race",
                            y = "Time",
                            fill = "Gender") +
                       theme_minimal()
                   })
                   
                   # Tables for each race with the male and female winner in them
                   output$race_winners_table <- renderDT({
                     race_winners <- lapply(unique(marathon_data$marathon), function(race) {
                       filtered_winners <- filter(marathon_data, marathon == race & year == input$year)
                       male_winner <- head(arrange(filter(filtered_winners, gender == "Male"), time), 1)
                       female_winner <- head(arrange(filter(filtered_winners, gender == "Female"), time), 1)
                       data.frame(
                         Race = race,
                         "Male Winner" = ifelse(is.null(male_winner), NA, male_winner$winner),
                         "Female Winner" = ifelse(is.null(female_winner), NA, female_winner$winner)
                       )
                     })
                     do.call(rbind, race_winners)
                    
                   })
                 }
                 
                 else if (input$year == "NONE" & input$marathon != "NONE") {
                   # Check if only race location is provided
                   observeEvent(input$marathon, {
                     # Create line plot of males vs females over the years
                     output$raceplot <- renderPlot({
                       data2 <- marathon_data %>%
                         filter(marathon == input$marathon)
                       
                       ggplot(data2, aes(x = year, y = time, color = gender)) +
                         geom_line() +
                         labs(title = paste("Males vs Females Over the Years for", input$marathon),
                              x = "Year",
                              y = "Finish Time",
                              color = "Gender") +
                         theme_minimal()
                     })
                     
                     #  Table of all female winners over the years
                     output$female_winners <- renderDT({
                       female_winners <- marathon_data %>%
                          filter(marathon == input$marathon & gender == "Female")
                       datatable(
                        female_winners, 
                        caption = "Female Winners",
                        options = list(dom = 't', pageLength = 10)
                       )
                      })
                     
                      # Table of all male winners over the years
                      output$male_winners <- renderDT({
                        male_winners <- marathon_data %>%
                          filter(marathon == input$marathon & gender == "Male")
                        datatable(
                        male_winners, 
                        caption = "Male Winners", 
                        options = list(dom = 't', pageLength = 10)
                        )
                      })
                   })
                 } 
                 
               })
  ###
  ###
  ###
  # This is code for my second tab 
 observeEvent(input$submit2, 
              {
                output$printtable = renderTable(marathon_data)
    if (input$gender == "NONE") {           
      # Check if only country is inputted 
      if (input$country != "NONE" && length(input$country) == 1 && input$marathon == "NONE") {
       # Filter data based on the country that was selected
         filtered_data <- marathon_data |>
          filter(country == input$country)
        # output$printtable = renderTable(filtered_data)
         
         # Group data by year and find top runner for each year
         top_runners_each_year <- filtered_data |>
           group_by(year) |>
           slice_min(order_by = time, n = 1) |>
           ungroup()
        # output$printtable = renderTable(top_runners_each_year)
         
         # Create dot plot of the top runners from each year
         output$dot_plot <- renderPlot({
           hist(rnorm(100))
           # ggplot(top_runners_each_year, aes(x = year, y = time)) +
           #   geom_point() +
           #   labs(title = paste("Top Runners from", input$country, "from Each Year"),
           #        x = "Year",
           #        y = "Finish Time") +
           #   theme_minimal()
         })
         
      }
    
        
    #    # Check if it is more than one country 
    #   else if (input$country != "NONE" && length(input$country) > 1) {
    #     
    #     # Filter dataset based on selected countries
    #     filtered_data <- marathon_data |>
    #       filter(country %in% input$country, gender == input$gender)
    #     
    #     # Group data by year and country
    #     top_runners_each_year <- filtered_data |>
    #       group_by(year, country) |>
    #       slice_min(order_by = time, n = 1) |>
    #       ungroup()  
    #  
    #     
    #     # Create dot plot of the top runners from each year, faceted by country
    #     output$dot_plot <- renderPlot({
    #       ggplot(top_runners_each_year, aes(x = year, y = time)) +
    #         geom_point() +
    #         facet_wrap(~country) +
    #         labs(title = paste("Top Runners from Each Year (Faceted by Country)"),
    #              x = "Year",
    #              y = "Finish Time") +
    #         theme_minimal()
    #     })
    #   }
    # }
    #              
    #   # Check if only race location is inputted
    #   if (input$marathon != "NONE" && length(input$marathon) == 1 && input$country == "NONE") {
    #     
    #     # Filter dataset based on selected race location
    #     filtered_data <- marathon_data |>
    #       filter(marathon == input$marathon, gender == input$gender)
    #     
    #     # Group data by year and find top runner for each year
    #     top_runners_each_year <- filtered_data |>
    #       group_by(year) |>
    #       slice_min(order_by = time, n = 1) |>
    #       ungroup()
    #     
    #     # Create line plot of the top runners from each year
    #     output$dot_plot <- renderPlot({
    #       ggplot(top_runners_each_year, aes(x = year, y = time)) +
    #         geom_line(size = 1.5) +
    #         labs(title = paste("Top Runners from", input$marathon, "from Each Year"),
    #              x = "Year",
    #              y = "Finish Time") +
    #         theme_minimal()
    #     })
    #     
    #   } 
    #   # Check if there is more than one race location 
    #   else if (input$races != "NONE" && length(input$races) > 1) {
    #     
    #     # Filter dataset based on selected race locations
    #     filtered_data <- marathon_data |>
    #       filter(marathon %in% input$marathon, gender == input$gender)
    #  
    #    
    #     # Group data by year and race location
    #     top_runners_each_year <- filtered_data |>
    #       group_by(year, marathon) |>
    #       slice_min(order_by = time, n = 1) |>
    #       ungroup()
    #     
    #     # Create line plot of the top runners from each year, faceted by race location
    #     output$dot_plot <- renderPlot({
    #       ggplot(top_runners_each_year, aes(x = year, y = time)) +
    #         geom_line(size = 1.5) +
    #         facet_wrap(~marathon) +
    #         labs(title = "Top Runners from Each Year (Faceted by Race Location)",
    #              x = "Year",
    #              y = "Finish Time") +
    #         theme_minimal()
    #       
    #     })
    #   }
    #   
    #   # Check if only gender was inputted 
    #   if (input$gender != "NONE" && input$country == "NONE" && input$race == "NONE") {
    #     
    #     # Filter dataset based on selected gender
    #     filtered_data <- marathon_data |>
    #       filter(gender == input$gender)
    #     
    #     # Group data by year and find top runner for each year
    #     top_runners_each_year <- filtered_data |>
    #       group_by(year) |>
    #       slice_min(order_by = time, n = 1) |>
    #       ungroup()
    #     
    #     
    #     # Create line plot of the top times for the specified gender over the years
    #     output$dot_plot <- renderPlot({
    #       ggplot(top_runners_each_year, aes(x = year, y = time)) +
    #         geom_line(size = 1.5) +
    #         labs(title = paste("Top Times for", input$gender, "Over the Years"),
    #              x = "Year",
    #              y = "Finish Time") +
    #         theme_minimal()
    #     })
    #   }
    #              
    #   # Check if only country and gender is inputted 
    #   if (input$gender != "NONE" && input$country != "NONE" && input$race == "NONE") {
    #     
    #     # Filter dataset based on selected country and gender
    #     filtered_data <- marathon_data %>%
    #       filter(country %in% input$country, gender == input$gender)
    #     
    #     
    #   # Check if there is only one country 
    #     if (length(input$country) == 1) {
    #       # Create a dot plot for the single selected country and gender
    #       output$dot_plot <- renderPlot({
    #         ggplot(filtered_data, aes(x = year, y = time, color = marathon)) +
    #           geom_point() +
    #           labs(title = paste("Top Runners from", input$country, "(", input$gender, ") from Each Year"),
    #                x = "Year",
    #                y = "Finish Time",
    #                color = "Marathon") +
    #           theme_minimal()
    #       })
    #     }
    #     
    #   # Check is there is more than one country 
    #     else {
    #       # Create a dot plot with facet_wrap by the selected countries and gender
    #       output$dot_plot <- renderPlot({
    #         ggplot(filtered_data, aes(x = year, y = time, color = marathon)) +
    #           geom_point() +
    #           labs(title = paste("Top Runners from Each Year by Country (", input$gender, ")"),
    #                x = "Year",
    #                y = "Finish Time",
    #                color = "Marathon") +
    #           facet_wrap(~country) +
    #           theme_minimal()
    #       })
    #     }
    #   }
    #              
    #              
    #   # Check if only gender and race are inputted 
    #   if (input$gender != "NONE" && input$country == "NONE" && input$marathon != "NONE") {
    #     
    #   # Check if there is one race 
    #     if (length(input$marathon) == 1) {
    #       # Filter dataset based on user input of gender and race
    #       filtered_data <- marathon_data |>
    #         filter(gender == input$gender, marathon == input$marathon)
    #       
    #       # Create line plot of the specified gender running the specified race
    #       output$dot_plot <- renderPlot({
    #         ggplot(filtered_data, aes(x = year, y = time)) +
    #           geom_line() +
    #           labs(title = paste("Top Times for", input$gender, "in", input$marathon, "Over the Years"),
    #                x = "Year",
    #                y = "Finish Time") +
    #           theme_minimal()
    #       })
    #     } 
    #     
    #   # Check if there is more than one race inputted
    #     else {
    #       # Filter dataset based on user input of gender and races
    #       filtered_data <- marathon_data |>
    #         filter(gender == input$gender, marathon %in% input$marathon)
    #       
    #       # Create line plot of the specified gender running the specified races
    #       output$dot_plot <- renderPlot({
    #         ggplot(filtered_data, aes(x = year, y = time, color = marathon)) +
    #           geom_line() +
    #           labs(title = paste("Top Times for", input$gender, "in Selected Races Over the Years"),
    #                x = "Year",
    #                y = "Finish Time",
    #                color = "Race Location") +
    #           theme_minimal() +
    #           facet_wrap(~marathon)
    #       })
    #     }
    #   }
    #              
    #              
    #   # Check if country and race were inputted     
    #   if (input$gender == "NONE" && input$country != "NONE" && input$marathon != "NONE") {
    #   # Check if both only had one input 
    #     if (length(input$country) == 1 && length(input$marathon) == 1) {
    #       
    #       # Filter dataset based on user input of country and race
    #       filtered_data <- marathon_data |>
    #         filter(country == input$country, marathon == input$marathon)
    #       
    #       # Create dot plot of the winners from that specific country and specific race
    #       output$dot_plot <- renderPlot({
    #         ggplot(filtered_data, aes(x = year, y = time)) +
    #           geom_point() +
    #           labs(title = paste("Winners from", input$country, "in", input$marathon),
    #                x = "Year",
    #                y = "Finish Time") +
    #           theme_minimal()
    #       })
    #     }
    #     
    #   # Check if only one of them is 1 - whatever is 1 then facet_wrap using the other 
    #     else if (length(input$country) == 1 || length(input$marathon) == 1) {
    #       if (length(input$country) == 1) {
    #         # Facet_wrap by race
    #         filtered_data <- marathon_data |>
    #           filter(country == input$country, marathon %in% input$marathon)
    #         
    #         output$dot_plot <- renderPlot({
    #           ggplot(filtered_data, aes(x = year, y = time)) +
    #             geom_point() +
    #             labs(title = paste("Winners from", input$country, "in Selected Races"),
    #                  x = "Year",
    #                  y = "Finish Time") +
    #             theme_minimal() +
    #             facet_wrap(~marathon)
    #         })
    #       }
    #     }
    #       else {
    #         # Facet_wrap by country
    #         filtered_data <- marathon_data |>
    #           filter(country %in% input$country, marathon == input$marathon)
    #         
    #         output$dot_plot <- renderPlot({
    #           ggplot(filtered_data, aes(x = year, y = time)) +
    #             geom_point() +
    #             labs(title = paste("Winners from Selected Countries in", input$marathon),
    #                  x = "Year",
    #                  y = "Finish Time") +
    #             theme_minimal() +
    #             facet_wrap(~country)
    #         })
    #       }
    #     } 
    #     
    #   # Check if both are great than one -- ask which one the user wants to facet wrap with 
    #     else if (length(input$country) > 1 && length(input$marathon) > 1) {
    #       showModal(modalDialog(
    #         title = "Facet wrap selection",
    #         actionButton("facet_by_country", "Facet by Country"),
    #         actionButton("facet_by_race", "Facet by Race")
    #       ))
    #       
    #       observeEvent(input$facet_by_country, {
    #         removeModal()
    #         # Facet_wrap by country was selected
    #         filtered_data <- marathon_data |>
    #           filter(country %in% input$country, marathon %in% input$marathon)
    #         
    #         output$dot_plot <- renderPlot({
    #           ggplot(filtered_data, aes(x = year, y = time)) +
    #             geom_point() +
    #             labs(title = paste("Winners from Selected Countries and Races"),
    #                  x = "Year",
    #                  y = "Finish Time") +
    #             theme_minimal() +
    #             facet_wrap(~country)
    #         })
    #       })
    #       
    #       observeEvent(input$facet_by_race, {
    #         removeModal()
    #         # Facet_wrap by race was selected 
    #         filtered_data <- marathon_data |>
    #           filter(country %in% input$country, marathon %in% input$marathon)
    #         
    #         output$dot_plot <- renderPlot({
    #           ggplot(filtered_data, aes(x = year, y = time)) +
    #             geom_point() +
    #             labs(title = paste("Winners from Selected Countries and Races"),
    #                  x = "Year",
    #                  y = "Finish Time") +
    #             theme_minimal() +
    #             facet_wrap(~marathon)
    #         })
    #       })
    #     }
    #   
    #              
    #   # Check if country, race, and location are inputted
    #   if (input$country != "NONE" && input$marathon != "NONE" && input$gender != "NONE") {
    #   # Ask what the user wants to facet_wrap with            
    #     # Ask the user what they want to facet_wrap with
    #     showModal(modalDialog(
    #       title = "Facet wrap selection",
    #       selectInput("facet_selection", "Facet by:", choices = c("Country", "Race")),
    #       footer = actionButton("facet_submit", "Submit")
    #     ))
    #     
    #     observeEvent(input$facet_submit, {
    #       removeModal()
    #       if (input$facet_selection == "Country") {
    #         # Facet by country
    #         filtered_data <- marathon_data %>%
    #           filter(country == input$country, marathon == input$marathon, gender == input$gender)
    #         
    #         output$dot_plot <- renderPlot({
    #           ggplot(filtered_data, aes(x = year, y = time, color = marathon)) +
    #             geom_point() +
    #             labs(title = paste("Winners from", input$country, "in", input$marathon, "by", input$gender),
    #                  x = "Year",
    #                  y = "Finish Time",
    #                  color = "Race Location") +
    #             theme_minimal() +
    #             facet_wrap(~country)
    #         })
    #       }
    #       
    #       else if (input$facet_selection == "Race") {
    #         # Facet by race
    #         filtered_data <- marathon_data %>%
    #           filter(country == input$country, marathon == input$marathon, gender == input$gender)
    #         
    #         output$dot_plot <- renderPlot({
    #           ggplot(filtered_data, aes(x = year, y = time, color = country)) +
    #             geom_point() +
    #             labs(title = paste("Winners from", input$country, "in", input$marathon, "by", input$gender),
    #                  x = "Year",
    #                  y = "Finish Time",
    #                  color = "Country") +
    #             theme_minimal() +
    #             facet_wrap(~marathon)
    #         })
    #       }
    #     })
    #   }
                 
               }
})
}


# Run the application 
shinyApp(ui = ui, server = server)

