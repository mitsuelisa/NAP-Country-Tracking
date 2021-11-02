#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(collapsibleTree)
library(gsheet)
library(plotly)
require(colorspace)

# Read the public Google sheet with Data and make it into a tibble
Tracking_Data <- gsheet2tbl('docs.google.com/spreadsheets/d/13TR6sd-J33bwztz0dWuCLOOgxQGpxOHN/edit#gid=1965485765')
td_2 <- unite(Tracking_Data, "Element_together", Element_letter:Element_description, sep = " - ", remove = FALSE, na.rm = FALSE)
Regions <- unite(td_2, "Measure_together", c(Measure_code, Measure_description), sep = " - ", remove = FALSE, na.rm = FALSE)
Regions$Details[is.na(Regions$Details)] = "No Details"


#New dataframe only with the variables I need
Regions_Filtered <- Regions %>%
  select(
    Region,Country,Date_of_entry_to_DB, Category, Element_together, Element_letter, Measure_code, Measure_together, Measure_global_number, Details)
Regions_Filtered <- rename(Regions_Filtered, Year = Date_of_entry_to_DB, Global.Measure = Measure_global_number, Measure = Measure_code, Element = Element_letter, Measure.Description = Measure_together, Element.Description = Element_together)
Regions_Filtered <- mutate(Regions_Filtered, Measures.Count = 1)

# Define UI for application that draws a collapsible tree
ui <- fluidPage(
  
  # Application title
  titlePanel("NAP Country Tracking Tool"),
  
  # Sidebar with a select input for the root node
  sidebarLayout(
    sidebarPanel(
      tags$b("The NAP Process has 4 Elements:"),
      tags$ul(
        tags$li("A - Laying the ground work and addressing gaps"), 
        tags$li("B - Preparatory elements"), 
        tags$li("C - Implementation strategies"),
        tags$li("D - Reporting monitoring and review")
      ),
      tags$p("Each element has certain 'Measures' that show what steps a country has taken towards the NAP process."),
      
      tags$p("Click on a node to open or collapse more information. You can also change the design of the tree by changing the paremeters below."),
      
      tags$hr(),
      
      tags$h6("Add, reorder or delete parameters to modify the tree hierarchy"),
      
      selectInput(
        "hierarchy", "Tree hierarchy",
        choices = c(
          "Region", "Category", "Country", "Year", "Element", "Measure", "Details", "Element.Description", "Measure.Description"
        ),
        selected = c("Region", "Category", "Country", "Year", "Element", "Measure"),
        multiple = TRUE
      ),
      
      tags$h6("Global.Measure stands for how advanced a measure is. For example, D2 Measures are further in the NAP process than A4 measures."),
      tags$h6("Measures.Count simply counts how many measures have been taken regardless of how further they are in the process"),
      
      selectInput(
        "fill", "Node color",
        choices = c("Global.Measure", "Measures.Count"),
        selected = "Global.Measure"
      ),
      
      
      HTML('<div class="gradientlegend" alt="The colors of the node are a red-green gradient. The red represent the most Measures taken while the color green represents the least Measures taken." style="width: auto;
  height: 20px;
  background-image: 
    linear-gradient(
      90deg,
      #d33f6a,
      #e9b62d,
      #e4dd6b,
      #e2e6bd);"></div>'),
      
      HTML('<table style="border-collapse: collapse; border: none; width: 100%;">
        <tr style="border: none;"> 
            <td style="border: none;">More
            </td>
            <td style="border: none; text-align:right;">Less
            </td>
        </tr>
    </table>'),
      
      tags$br(),
      
      tags$b("The node you most recently clicked"),
      verbatimTextOutput("str"),
      tags$br(),
      
      tags$b("More Details"),
      verbatimTextOutput("TextDetails")
      
    ),
    
    # Show a tree diagram with the selected root node
    mainPanel(
      collapsibleTreeOutput("plot", height = "800px")
    )
  )
)

# Define server logic required to draw a collapsible tree diagram
server <- function(input, output) {
  output$plot <- renderCollapsibleTree({
    collapsibleTreeSummary(
      Regions_Filtered,
      hierarchy = input$hierarchy,
      inputId = "node",
      root = input$fill,
      attribute = input$fill,
      zoomable = FALSE,
    )
  })
  
  output$str <- renderPrint(str(input$node))
  output$TextDetails <- renderPrint(Regions_Filtered$Details)
}

# Run the application
shinyApp(ui = ui, server = server)
