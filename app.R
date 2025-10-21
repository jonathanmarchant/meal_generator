#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)

source("meals.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Marchant Menu Maker"),

    #     same_meat_max = same_meat_max,
    # same_starch_max = same_starch_max,
    # same_special_max = same_special_max,
    # max_long = max_long,
    # max_medlong = max_medlong
    sidebarLayout(
        sidebarPanel(
            sliderInput("meals",
                        "Number of meals:",
                        min = 1,
                        max = 7,
                        value = 4,
                        step = 1),
            sliderInput("same_meat_max",
                        "Maximum number with same meat:",
                        min = 1,
                        max = 7,
                        value = 2,
                        step = 1),
            sliderInput("same_starch_max",
                        "Maximum number with same starch:",
                        min = 1,
                        max = 7,
                        value = 2,
                        step = 1),
            sliderInput("same_special_max",
                        "Maximum number of burgers/sandwiches:",
                        min = 0,
                        max = 7,
                        value = 1,
                        step = 1),
            sliderInput("max_long",
                        "Maximum proportion of long meals:",
                        min = 0,
                        max = 1,
                        value = 0.35,
                        step = 0.05),
            sliderInput("max_medlong",
                        "Maximum proportion of medium-long meals:",
                        min = 0,
                        max = 1,
                        value = 0.5,
                        step = 0.05),
            selectizeInput("mandatory", "Required meals", meals$meal_name, multiple = TRUE),
            actionButton("go", "Generate menu")
        ),

        mainPanel(
           card(
             card_header(tags$h2("Menu")),
             tableOutput("menu"),
             uiOutput("ingreds_button")
             ),
           card(
             card_header(tags$h2("Ingredients")),
             tableOutput("ingredients")
           )
        )
    )
)

server <- function(input, output) {
  
  chosen_menu <- reactive({
    find_menu(meals,
              meals_required = input$meals,
              same_meat_max = input$same_meat_max,
              same_starch_max = input$same_starch_max,
              same_special_max = input$same_special_max,
              max_long = input$max_long,
              max_medlong = input$max_medlong,
              must_include = input$mandatory) %>% 
      select(meal_id, meal_name, timing_min, prepped_in_5) %>% 
      mutate(portions = 2L) %>% 
      arrange(meal_id)
  }) %>% 
    bindEvent(input$go)
  
  output$menu <- renderTable({
    chosen_menu()
  })
  
  output$ingredients <- renderTable({
    chosen_menu() %>% 
      select(meal_id, portions) %>%
      inner_join(ingredients, by="meal_id") %>%
      arrange(category, ingredient_name) %>%
      mutate(description = str_c(amount_for_2*(portions/2), units, " ", ingredient_name),
             meal_id = as.integer(meal_id)) %>%
      select(description, category, meal_id)
  }) %>% 
    bindEvent(input$ingreds)
  
  output$ingreds_button <- renderUI({
    actionButton("ingreds", "List ingredients")
  }) %>% 
    bindEvent(input$go)
}

# Run the application 
shinyApp(ui = ui, server = server)
