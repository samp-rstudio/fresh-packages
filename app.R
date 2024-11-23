library(shiny)
library(bslib)
library(DT)
library(utils)
library(rsconnect)
library(gh)
library(base64enc)
library(duckdb)
library(reticulate)

ui <- page_sidebar(
  title = "Installed R Packages",
  sidebar = sidebar(
    selectInput("sort_by", "Sort by",
                choices = c("Package Name" = "Package", "Version"),
                selected = "Package"
    ),
    checkboxInput("reverse", "Reverse order", FALSE),
    card(
      actionButton("update_btn", "Update All Packages", 
                   class = "btn-primary btn-lg",
                   width = "100%")
    ),
    card(
      actionButton("diff_btn", "Compare manifest.json", 
                   class = "btn-primary btn-lg",
                   width = "100%")
    ),
    card(
      actionButton("push_btn", "Push to GitHub", 
                   class = "btn-primary btn-lg",
                   width = "100%")
    )
  ),
  
  card(
    card_header("Update Status"),
    textOutput("status_text")
  ),
  
  card(
    card_header("Diff Status"),
    verbatimTextOutput("diff_text")
  ),
  
  card(
    card_header("Push Status"),
    textOutput("push_text")
  ),
  
  card(
    card_header("Installed Packages and Versions"),
    DT::dataTableOutput("packages_table")
  )
)

server <- function(input, output, session) {
  lib_path <- "/cloud/lib"

  # Function to push manifest to GitHub
  push_manifest_to_github <- function() {
    # Read the manifest file
    manifest_content <- readLines("manifest.json", warn = FALSE)
    manifest_content <- paste(manifest_content, collapse = "\n")
    
    # Encode content in base64
    content_base64 <- base64encode(charToRaw(manifest_content))
    
    # Get current commit SHA to use as parent
    repo_info <- gh("/repos/samp-rstudio/fresh-packages/contents/manifest.json")
    
    # Create or update file in GitHub
    gh("PUT /repos/samp-rstudio/fresh-packages/contents/manifest.json",
       message = "Update manifest.json",
       content = content_base64,
       sha = if(length(repo_info) > 0) repo_info$sha else NULL)
    
    print("Updated https://github.com/samp-rstudio/fresh-packages/contents/manifest.json")
  }

  output$push_text <- renderText({
    if (input$push_btn == 0) {
      "Click the button 'Push to GitHub' to save manifest.json to GitHub"
    } else {
      # Push to GitHub
      push_manifest_to_github()
    }
  })
  
  output$diff_text <- renderText({
    if (input$diff_btn == 0) {
      "Click the button 'Compare manifest.json' to check if the manifest changed."
    } else {
      o <- system("diff manifest.prev manifest.json", intern=TRUE)
      paste(o, collapse="\n")
    }
  })
  
  # Status message output
  output$status_text <- renderText({
    if (input$update_btn == 0) {
      "Click the button'Update All Packages' to start updating packages."
    } else {
      # Create a progress object
      progress <- Progress$new(session, min = 0, max = 1)
      progress$set(message = "Updating packages...", value = 0.3)
      
      # Capture the update.packages() output
      update_result <- capture.output({
        update.packages(lib.loc = lib_path, repos="https://packagemanager.posit.co/cran/__linux__/jammy/latest
", ask = FALSE, checkBuilt = TRUE)
      })
      
      # save old manifest
      system("cp manifest.json manifest.prev", intern=TRUE)
      
      # Create manifest file
      rsconnect::writeManifest(appDir = ".", appFiles = c("app.R", "requirements.txt"))
      
      progress$set(value = 1)
      progress$close()

      # Return status message
      paste("Package update completed at", format(Sys.time(), "%H:%M:%S"), 
            "\nCheck the R console for detailed information.")
    }
  })
  
  # Get installed packages data
  packages_data <- reactive({
    pkg_data <- as.data.frame(installed.packages()[, c("Package", "Version")])
    
    # Sort based on user selection
    if (input$reverse) {
      pkg_data[order(pkg_data[[input$sort_by]], decreasing = TRUE), ]
    } else {
      pkg_data[order(pkg_data[[input$sort_by]]), ]
    }
  })
  
  # Render the data table
  output$packages_table <- DT::renderDataTable({
    DT::datatable(
      packages_data(),
      options = list(
        pageLength = 15,
        searchHighlight = TRUE
      ),
      filter = "top"
    )
  })
}

shinyApp(ui, server)
