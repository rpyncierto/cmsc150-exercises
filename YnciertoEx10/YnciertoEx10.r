# importing exers 8 and 9
source("YnciertoEx08.r")
source("YnciertoEx09.r")

# importing libraries
library(shiny)
library(shinydashboard)
library(shinyMatrix)
library(shinyjs)

# UI side
ui <- dashboardPage(
  dashboardHeader(
    title = "Numerical Methods"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Quadratic Spline Interpolation",
        tabName = "qsi"
      ),
      menuItem(
        "Simplex Method",
        tabName = "simplex"
      )
    )
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tabItems(
      # UI for Quadratic Spline Interpolation
      tabItem(
        tabName = "qsi",
        fluidPage(
          fluidRow( # Refresh Button
            column(
              width = 12,
              align = "right",
              actionButton(
                "reset",
                "Refresh"
              )
            )
          ),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          br(),
          column(
            width = 12,
            offset = 5,
            fluidRow( # user input for x and y values as well as the estimate input 
              column(
                width = 5,
                textInput( # user input for x-values
                  "x",
                  "x-values",
                  placeholder = "1,2,3",
                  width = "445"
                ),
                textInput( # user input for y-values
                  "y",
                  "y-values",
                  placeholder = "4,5,6",
                  width = "445"
                ),
                numericInput( # user input for x
                  "estimate",
                  "Estimate:",
                  0,
                  width = "445"
                )
              )
            ),
            fluidRow( # solve button
              column(
                align = "center",
                width = 3,
                actionButton(
                  inputId = "solve",
                  label = "Solve"
                )
              )
            ),
            fluidRow( # quadratic spline interpolation functions
              br(),
              column(
                width = 5,
                h5(strong("Functions per Interval:")),
                htmlOutput("functions")
              )
            ),
            fluidRow( # estimated f(x) of input x
              column(
                width = 3,
                h5(strong("Estimated f(x):")),
                verbatimTextOutput("resultQSI")
              )
            )
          )
        )
      ),
      
      
      # UI for Simplex Method
      tabItem(
        tabName = "simplex",
        fluidPage(
          fluidRow( # refresh button
            column(
              width = 12,
              align = "right",
              actionButton(
                "clear",
                "Refresh"
              )
            )
          ),
          br(),
          br(),
          column(
            width = 12,
            offset = 4,
            fluidRow( # method drop down either maximization or minimization
              column(
                width = 4,
                selectInput(
                  "method",
                  "Method",
                  choices = c(
                    "Minimization",
                    "Maximization"
                  ),
                  width = "650"
                )
              )
            ),
            fluidRow( # checkbox if the user wants to solve a generic minimization/maximization or transportation problem
              column(
                width = 4,
                checkboxInput(
                  inputId = "problem",
                  label = "Transportation Problem"
                )
              )
            ),
            fluidRow( # user input for variables
              column(
                width = 2,
                numericInput(
                  "variables",
                  "Variables:",
                  value = 2,
                  min = 2
                )
              ),
              column( # user input for constraints
                width = 2,
                numericInput(
                  "constraints",
                  "Constraints:",
                  value = 2,
                  min = 2
                )
              )
            ),
            fluidRow( # generate matrix button
              column(
                width = 4,
                align = "center",
                actionButton(
                  inputId = "generate",
                  label = "Generate"
                )
              )
            ),
            fluidRow( # minimized/maximized output or the Z value
              column(
                width = 4,
                br(),
                h5(strong("Z:")),
                verbatimTextOutput("result_simplex")
              )
            )
          ),
          column(
            width = 12,
            fluidRow(
              br(),
              br(),
              div(id = "tableau"), # displaying of tableau
              div(id = "solveButton"), # inserting solve button
              br(),
              br(),
              div(id = "tableauHeader"), # displaying final tableau header
              div(id = "finalTableau"), # displaying the final tableau
              br(),
              div(id = "solutionHeader"), # displaying solution header
              div(id = "basicSolution"), # displaying the solution sets
              br(),
              div(id = "problemHeader"), # displaying the problem header
              div(id = "shippingNum") # displaying the shipping number per plant to each warehouses
            )
          )
        )
      )
    )
  )
)


# function that converts a string into numeric list (used in quadratic spline interpolation to convert string input of the user into a numeric list)
converter <- function(string) {
  my_list <- strsplit(string, ",")[[1]]
  numeric <- as.numeric(my_list)
  return (numeric)
}

# Server side
server <- function(input, output, session) {
  
  my_reactive_values <- reactiveValues() # variables automatically update when their dependencies change
  
  observeEvent( # solve button has been clicked
    input$solve, {
      
      # conversion from string to numeric list
      x_values <- converter(input$x)
      y_values <- converter(input$y)
      
      # condition statements before actually evaluating
      if(length(y_values) != 0) { #
        if(length(x_values) != 0) {
          if(length(y_values) >= 3) {
            if(length(x_values) >= 3) {
              if(length(x_values) == length(y_values)) {
                poly <- poly.qsi(data <- list(x_values, y_values), input$estimate)
                output$resultQSI <- renderText(poly$y)
                output$functions <- renderUI({
                  text_function = "<br/>"
                  for (i in 1:length(poly$qsi.fxns)) { # storing all functions in one string with their corresponding interval number
                    text_function <- paste(text_function, "Interval ", i, ":", sep = "")
                    text_function <- paste(text_function, "<br/>function(x) ", deparse(poly$qsi.fxns[[i]])[2], sep = "" )
                    text_function <- paste(text_function, "", sep = "<br/><br/>")
                  }
                  HTML(paste(text_function, sep = "<br/>"))
                })
              } else {
                output$resultQSI <- renderText("Please fill the same number of data points.")
              }
            } else {
              output$resultQSI <- renderText("Please fill at least 3 data points.")
            }
          } else {
            output$resultQSI <- renderText("Please fill at least 3 data points.")
          }
        } else {
          output$resultQSI <- renderText("Please fill the x and y values.")
        }
      } else {
        output$resultQSI <- renderText("Please fill the x and y values.")
      }
    }
  )
  
  observeEvent( # generate button has been clicked
    input$generate, {
      
      shinyjs::disable("generate") # disable the button
      
      # get number of rows and columns + 1 for Objective Function and RHS
      rows <- (input$constraints) + 1
      cols <- (input$variables) + 1
      
      
      if(input$problem){# if problem is checked
        rows <- 8 + 1
        cols <- 15 + 1
      }
      
      row_name <- character(rows)
      col_name <- character(cols)
      
      # assigning names in rows
      for (i in 1:(rows - 1)) {
        row_name[i] <- paste0("Constraint ", i)
      }
      row_name[rows] <- "Objective Function"
      
      # assigning names for columns
      for (i in 1:(cols - 1)) {
        col_name[i] <- paste0("x", i)
      }
      col_name[cols] <- "RHS"
      
      # creation of matrix
      m <- matrix(0, rows, cols, dimnames = list(row_name, col_name))
      insertUI( # displaying matrix in the UI
        immediate = TRUE, selector = "#tableau", where = "beforeEnd",
        matrixInput(
          "matrix", value = m, class = "numeric"
        )
      )
      
      insertUI( # displaying solve button
        immediate = TRUE, selector = "#solveButton", where = "beforeEnd",
        actionButton("compute", "Solve")
      )
    }
  )
  
  
  observeEvent( # compute button has been clicked
    input$compute, {
      
      if (all(input$matrix == 0)) {
        output$result_simplex <- renderText("Please fill in the matrix")
      } else {
      
        shinyjs::disable("compute")
        # initialization
        isMax = FALSE
        problem = FALSE
        
        if(!input$method == "Minimization") {
          isMax = TRUE
        }
        
        result <- createTableau(input$matrix, isMax, input$problem) # perform the method
        output$result_simplex <- renderText(result$opt.val) # output the result (optimum value)
        
        insertUI( # display final tableau header
          immediate = TRUE, selector = "#tableauHeader", where = "beforeEnd",
          h5(strong("Final Tableau:"))
        )
        
        insertUI( # display the final tableau
          immediate = TRUE, selector = "#finalTableau", where = "beforeEnd",
          output$resulting_tableau <- renderTable(result$final.tableau, rownames = TRUE, colnames = TRUE, width = "auto", spacing = "l")
        )
        
        insertUI( # display the solution set header
          immediate = TRUE, selector = "#solutionHeader", where = "beforeEnd",
          h5(strong("Solution Set:"))
        )
        
        insertUI(# display the solution set matrix
          immediate = TRUE, selector = "#basicSolution", where = "beforeEnd",
          output$basic_solution <- renderTable(result$basic.solution, rownames = TRUE, colnames = TRUE, width = "auto",spacing = "l")
        )
        
        if(input$problem) { # if transportation problem has been checked
          insertUI( # insert problem headers
            immediate = TRUE, selector = "#problemHeader", where = "beforeEnd",
            h5(strong("Shipping Numbers: "))
          )
          
          insertUI( # insert matrix of shipping numbers
            immediate = TRUE, selector = "#shippingNum", where = "beforeEnd",
            output$shipping_num <- renderTable(result$shipping.num, rownames = TRUE, width = "auto", colnames = TRUE, spacing = "l")
          )
        }
      }
    }
  )
  
  observeEvent( # is transportation problem has been clicked
    input$problem, {
      if(input$problem) {
        # disable the following input methods
        shinyjs::disable("constraints")
        shinyjs::disable("variables")
        shinyjs::disable("method")
      } else {
        shinyjs::enable("constraints")
        shinyjs::enable("variables")
        shinyjs::enable("method")
      }
    }
  )
  
  observeEvent( # refresh button has been clicked (QSI)
    input$clear, {
    refresh()
  })
  
  observeEvent( # refresh button has been clicked (Simplex)
    input$reset, {
    refresh()
  })
}

# run the shiny app
shinyApp(ui, server)