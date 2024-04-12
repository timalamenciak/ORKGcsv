# This script and shiny app will allow for limited data entry
# for ORKG with standardized text responses. It spits out a
# spreadsheet appropriately formatted for inclusion in ORKG
# There should be five fields:
# location - WGS84 coordinates only
# ecosystem type - IUCN typology - biome_iucn.csv
# method - method.csv
# degradation type - degradation.csv
# focal species - GBIF species list? maybe taxize package
# Workaround for multiple values: use a semicolon (;) to separate entries. 
#  - Use Excel to split the column based on that.


library(shiny)
library(dplyr)
library(tidygeocoder)
library(mapview)
library(raster)
library(rcrossref) #for DOI validation

setwd("C:/Users/Tim Alamenciak/Documents/TReK")

#These load the pre-populated CSVs for limiting the inputs.
method = read.csv("method.csv")
degradation = read.csv("degradation.csv")
iucn = read.csv("biome_iucn.csv")

#This sets up the dataframe that will contain the outputs
outframe = data.frame(DOI=character(),
                      coords=character(),
                      site_city=character(),
                      site_province=character(),
                      site_country=character(),
                      iucn_realm=character(),
                      iucn_biome=character(),
                      iucn_efg=character(),
                      method=character(),
                      degradation=character(),
                      species=character())

#This function adds a row to the outframe.
addline <- function(DOI, coords, site_city, site_province, site_country, iucn_realm, iucn_biome, iucn_efg,
                    method, degradation, species){
  outframe[nrow(outframe) + 1,] <<- c(DOI, coords, site_city, site_province, site_country, iucn_realm, iucn_biome, iucn_efg,
                                     method, degradation, species)
}

#This is the UI for the shiny app
ui <- fluidPage(
  titlePanel("ORKG CSV generator"),
  textInput("doi", "What's the DOI?"),
  fluidRow(
    column(4,
           sidebarLayout(
             sidebarPanel(width = 12, id="location", 
                          h5("Location details"),
                            textInput("site_coords", label = "Coordinates (WGS84)"),
                            textInput("site_city", label = "Research site city"),
                            textInput("site_province", label = "Research site province"),
                            textInput("site_country", label = "Research site country")
             ),
             mainPanel(width = 0)
           )
    ),
    column(4, sidebarLayout(
      sidebarPanel(width = 12, id="ecosystem",
                   h5("Ecosystem type"),
                    selectInput("realm", label = "Realm", choices = unique(iucn$realm), selected = NULL),
                    selectInput("biome", label = "Biome", choices = iucn$biome),
                    selectInput("efg", label = "Ecosystem Functional Group", choices = iucn$efg)),
      mainPanel( width = 0)
    )),
    column(4, sidebarLayout(
      sidebarPanel(width = 12, id="info",
                   h5("Article title:"),
                   textOutput("title")),
      mainPanel( width = 0)
    ))
  ),
  
  textInput("species", label = "Main species (scientific name)"),
  selectInput("method", label = "Method", choices = method$name),
  selectInput("degradation", label = "Degradation type", choices = degradation$name),
  actionButton("save", "Save entry"),
  actionButton("write", "Write to CSV"),
  
    
  tableOutput("table")
)

server <- function(input, output, session) {

  realm <- reactive({
    filter(iucn, realm == input$realm)
  })
  observeEvent(realm(), {
    choices <- unique(realm()$biome)
    updateSelectInput(inputId = "biome", choices = choices)
  })
  
  biome <- reactive({
    req(input$biome)
    filter(realm(), biome == input$biome)
  })
  observeEvent(biome(), { 
    choices <- unique(biome()$efg)
    updateSelectInput(inputId = "efg", choices = choices)
  })
  
  output$title <- renderText({
    article <- cr_works(dois = input$doi)
    paste(article$data$title)
  }) 
  
  site_coords <- reactive({
    #Take the coordinates and run it through the reverse geocoder to get city, province and country
    #This is also where the ecosystem suggestions should be calculated.
  })
  
  observeEvent(input$save, {
    addline(input$doi, 
            input$site_coords, 
            input$site_city,
            input$site_province,
            input$site_country,
            input$realm, 
            input$biome, 
            input$efg, 
            input$method, 
            input$degradation,
            input$species)
    
    output$table <- renderTable(outframe, width="100%")
    
  })
  
  observeEvent(input$write, {
    write.csv(outframe, "ORKG.csv", col.names = c("paper:doi", "site coordinates", "site city",
                                                  "site province", "site_country", "IUCN realm",
                                                  "IUCN biome", "IUCN EFG", "Restoration method", 
                                                  "Degradation type", "Focal species"))
  })
}


shinyApp(ui, server)
