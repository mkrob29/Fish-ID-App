# Fish ID Database Entry APP

library(readxl)
library(openxlsx)
library(shiny)
library(shinyWidgets)
library(shinyMobile)
library(shinyFiles)
library(dplyr)
library(shinyjqui)

setwd("C:/Users/Volunteer/OneDrive - North Island A Luxury Collection/Desktop/Fish ID")
source("lib/helpers.R")


ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Background for Setup tab */
      .setup-page {
        background-image: url('North-Island-Conservation-tutrle-hatchlings-2048x1365.jpg');
        background-size: cover;
        background-position: center;
        min-height: 100vh;
      }
      /* Semi-transparent panels so background is visible */
      .setup-panel {
        background-color: rgba(255,255,255,0.8);
        padding: 20px;
        border-radius: 10px;
      }
      .mySidebar .form-group {
    margin-bottom: 5px !important;
  }
  .mySidebar .shiny-input-container {
    margin-bottom: 5px !important;
  }
  /* Main panel styling */
    .myMainpanel {
      height: 70vh;          /* full viewport height */
    }
    "))
  ),
  
  titlePanel("North Island Fish ID Database Entry"),
  
  tabsetPanel(id = "mainTabs",   # give the tabset an id
              tabPanel("Setup",
                       div(class = "setup-page",
                           sidebarLayout(
                             sidebarPanel(class = "setup-panel",
                                          shinyDirButton("folder", "Choose Folder", "Select working directory"),
                                          textInput("day", "Day", value = format(Sys.Date(), "%d")),
                                          textInput("month", "Month", value = format(Sys.Date(), "%m")),
                                          textInput("year", "Year", value = format(Sys.Date(), "%Y")),
                                          textInput("spot", "Snorkel/Dive site", value = ""),
                                          actionButton("confirm_setup", "Confirm Setup", class = "btn-success")
                             ),
                             mainPanel(class = "setup-panel",
                                       h3("Setup Preview"),
                                       verbatimTextOutput("setup_preview")
                             )
                           )
                       )
              ),
              
              tabPanel("Add Fish Sighting",
                       sidebarLayout(
                         sidebarPanel(
                           uiOutput("dynamic_inputs"),
                           hr(),
                           actionButton("save_entry", "Save Entry", class = "btn-primary"),
                           class = "mySidebar",
                           style = "flex:0 0 300px; height:100vh;"
                         ),
                         mainPanel(
                           h3("Image Viewer"),
                           uiOutput("image_display"),
                           div(
                             style = "margin-top:5px;",
                             actionButton("prev_img", "⟵ Previous"),
                             actionButton("next_img", "Next ⟶")
                           ),
                           style = "flex:1; height:100vh;"
                         )
                       )
              ),
              tabPanel("Credits",
                       fluidRow(
                         column(12,
                                div(class = "setup-panel",
                                    h2("Credits"),
                                    tags$hr(),
                                    tags$p(strong("Version:"), " 1.0 - Coconut Man"),
                                    tags$p(strong("Author:"), "Marcus Krobath"),
                                    tags$p(strong("Contact:"), "mk29@gmx.at"),
                                    tags$br(),
                                    tags$p("This application was developed to support North Island conservation efforts in November 2025"),
                                    tags$p("Important notice: do not move original excel files, as data download will not work"),
                                    tags$p("If you want to add new species to the master databse, please add it in the corresponding excel-file"),
                                    tags$br(),
                                    tags$p("If you read this you have to eat a coconut for me :)")
                                )
                         )
                       )
              )
  )
)

server <- function(input, output, session) {
  
  # sightings_path <- "C:/Users/Volunteer/OneDrive - North Island A Luxury Collection/Documents - NI-Environment Center/VOLUNTEERS/Database/Coral Reef monitoring/Fish ID database_V2.xlsx"
  new_db_path <- "C:/Users/Volunteer/OneDrive - North Island A Luxury Collection/Documents - NI-Environment Center/VOLUNTEERS/Database/Coral Reef monitoring/Fish_ID_App.xlsx"
  master_path <- "C:/Users/Volunteer/OneDrive - North Island A Luxury Collection/Documents - NI-Environment Center/VOLUNTEERS/Database/Coral Reef monitoring/Fish ID database_V3.xlsx"
  
  # db which lists all sightings
  initial_db <- read_excel(
    new_db_path,
    sheet = "DATABASE",
    col_types = c("numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))
  
  sightings_db <- reactiveVal(initial_db)
  autofill_vals <- reactiveVal(list(Common_Name = NULL, Fish_Group = NULL))
  current_file_val <- reactiveVal("")
  
  # master db which lists species and genus names etc
  master_db <- read_excel(
    master_path,
    sheet = "Species Master List")
  master_db[] <- lapply(master_db, function(x)
    if (is.character(x)) gsub("_", " ", x) else x)
  master_db[] <- lapply(master_db, function(x)
    if (is.character(x)) trimws(gsub("\\s+", " ", x)) else x)
  
  # auto complete sources
  ac_fish_group <- sort(unique(master_db$Group))
  ac_species <- sort(unique(master_db$`Species scientific name`))
  ac_common_name <- sort(unique(master_db$`Common name`))
  
  # Setup Page Logic
  volumes <- c(
    DataDir = "D:/1. Wildlife ACT MARINE database",
    UserDir = "C:/Users/Volunteer"
  )
  
  shinyDirChoose(input, "folder", roots = volumes, session = session)
  
  setup_vals <- reactiveValues(
    dir = NULL,
    day = format(Sys.Date(), "%d"),
    month = format(Sys.Date(), "%m"),
    year = format(Sys.Date(), "%Y"),
    spot = ""
  )
  
  # Gate to build Add Sighting UI only after data is loaded
  loaded <- reactiveVal(FALSE)
  
  observeEvent(input$confirm_setup, {
    setup_vals$dir <- parseDirPath(volumes, input$folder)
    setup_vals$day <- input$day
    setup_vals$month <- input$month
    setup_vals$year <- input$year
    setup_vals$spot <- input$spot
    
    # mark data as loaded
    loaded(TRUE)
    
    # Switch to Add Fish Sighting tab
    updateTabsetPanel(session, "mainTabs", selected = "Add Fish Sighting")
  })
  
  # store image list and index
  image_vals <- reactiveValues(
    files = NULL,
    index = 1
  )
  
  # when setup is confirmed, list images in chosen folder
  observeEvent(input$confirm_setup, {
    
    # create a temporary folder inside www
    temp_dir <- file.path("www", "temp_images")
    if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
    
    # clear out old files
    old_files <- list.files(temp_dir, full.names = TRUE)
    if (length(old_files) > 0) file.remove(old_files)
    
    # copy images from chosen folder into www/temp_images
    image_files <- list.files(setup_vals$dir,
                              pattern = "\\.(jpg|jpeg|png|gif|JPG|PNG)$",
                              full.names = TRUE)
    if (length(image_files) > 0) {
      file.copy(image_files, temp_dir, overwrite = TRUE)
      
      # now build relative paths for Shiny
      rel_paths <- file.path("temp_images", basename(image_files))
      
      image_vals$files <- rel_paths
      image_vals$index <- 1
    } else {
      image_vals$files <- NULL
    }
    
    # switch to Add Fish Sighting tab
    updateTabsetPanel(session, "mainTabs", selected = "Add Fish Sighting")
  })
  
  # navigation
  observeEvent(input$next_img, {
    req(image_vals$files)
    image_vals$index <- image_vals$index %% length(image_vals$files) + 1
  })
  
  observeEvent(input$prev_img, {
    req(image_vals$files)
    image_vals$index <- (image_vals$index - 2) %% length(image_vals$files) + 1
  })
  
  # display current image
  output$image_display <- renderUI({
    req(image_vals$files)
    shinyjqui::jqui_resizable(
      tags$img(
        src   = image_vals$files[image_vals$index],
        style = "max-height:90vh; max-width:100%; height:auto; width:auto; object-fit:contain;"
      )
    )
  })
  
  output$setup_preview <- renderPrint({
    list(
      Directory = setup_vals$dir,
      Day = setup_vals$day,
      Month = setup_vals$month,
      Year = setup_vals$year,
      Spot = setup_vals$spot
    )
  })
  
  # Build Add Fish Sighting inputs after load
  output$dynamic_inputs <- renderUI({
    req(loaded())
    req(!is.null(sightings_db()))
    
    lapply(names(sightings_db()), function(col){
      col_class <- class(sightings_db()[[col]])[1]
      
      # Special autocomplete fields
      if (col == "Fish Group") {
        return(
          selectizeInput(
            "Fish_Group", "Fish Group",
            choices = c("", ac_fish_group),
            options = list(placeholder = "Start typing..."),
            selected = NULL
          )
        )
      }
      
      if (col == "Species") {
        return(
          selectizeInput(
            "Species", "Species",
            choices = c("", ac_species),
            options = list(placeholder = "Start typing..."),
            selected = NULL
          )
        )
      }
      
      if (col == "Common Name") {
        return(
          selectizeInput(
            "Common_Name", "Common Name",
            choices = c("", ac_common_name),
            options = list(placeholder = "Start typing..."),
            selected = NULL
          )
        )
      }
      
      # Inline defaults for Day / Month / Year / Spot
      if (col == "Day") {
        return(textInput("Day", "Day", value = setup_vals$day))
      }
      if (col == "Month") {
        return(textInput("Month", "Month", value = setup_vals$month))
      }
      if (col == "Year") {
        return(textInput("Year", "Year", value = setup_vals$year))
      }
      if (col == "Snorkel/Dive site") {
        return(textInput("Snorkel/Dive site", "Snorkel/Dive site", value = setup_vals$spot))
      }
      if (col == "Reference Folder") {
        return(textInput("Reference Folder", "Reference Folder", value = setup_vals$dir))
      }
      if (col == "Reference picture") {
        current_file <- if (!is.null(image_vals$files)) basename(image_vals$files[image_vals$index]) else ""
        current_file_val(current_file)   # store globally
        return(textInput("Reference_Picture", "Reference Picture", value = current_file))
      }
      
      # Default field generation
      if (col_class %in% c("character", "logical")) {
        textInput(col, label = col, value = "")
      } else if (col_class %in% c("numeric", "integer")) {
        numericInput(col, label = col, value = NA)
      } else if (col_class == "factor") {
        selectInput(col, label = col, choices = levels(sightings_db()[[col]]))
      } else if (col_class == "Date") {
        dateInput(col, label = col, value = Sys.Date())
      } else {
        textInput(col, label = col, value = "")
      }
    })
  })
  
  
  # Auto-fill observers using master_db
  observeEvent(input$Species, ignoreInit = TRUE, {
    req(loaded(), master_db, input$Species)
    species_filtered <- master_db %>%
      dplyr::filter(`Species scientific name` == input$Species)
    
    if (nrow(species_filtered) > 0) {
      if (is.null(input$Common_Name) || input$Common_Name == "") {
        updateSelectizeInput(session, "Common_Name",
                             selected = species_filtered$`Common name`[1],
                             choices = sort(unique(species_filtered$`Common name`)),
                             server = TRUE)
      }
      if (is.null(input$Fish_Group) || input$Fish_Group == "") {
        updateSelectizeInput(session, "Fish_Group",
                             selected = species_filtered$Group[1],
                             choices = sort(unique(species_filtered$Group)),
                             server = TRUE)
      }
    }
  })
  
  observeEvent(input$Common_Name, ignoreInit = TRUE, {
    req(loaded(), master_db, input$Common_Name)
    species_filtered <- master_db %>%
      dplyr::filter(`Common name` == input$Common_Name)
    
    if (is.null(input$Species) || input$Species == "") {
      updateSelectizeInput(session, "Species",
                           selected = species_filtered$`Species scientific name`[1],
                           choices = sort(unique(species_filtered$`Species scientific name`)),
                           server = TRUE)
    }
    if (nrow(species_filtered) > 0) {
      if (is.null(input$Fish_Group) || input$Fish_Group == "") {
        updateSelectizeInput(session, "Fish_Group",
                             selected = species_filtered$Group[1],
                             choices = sort(unique(species_filtered$Group)),
                             server = TRUE)
      }
      # store autofill values
      autofill_vals(list(
        Common_Name = species_filtered$`Common name`[1],
        Fish_Group  = species_filtered$Group[1]
      ))
    }
  })
  
  observeEvent(input$Fish_Group, ignoreInit = TRUE, {
    req(loaded(), master_db, input$Fish_Group)
    species_filtered <- master_db %>%
      dplyr::filter(Group == input$Fish_Group)
    
    species_choices <- sort(unique(species_filtered$`Species scientific name`))
    common_choices <- sort(unique(species_filtered$`Common name`))
    
    if (is.null(input$Species) || input$Species == "") {
      updateSelectizeInput(session, "Species",
                           choices = species_choices,
                           selected = "",
                           server = TRUE)
    }
    if (is.null(input$Common_Name) || input$Common_Name == "") {
      updateSelectizeInput(session, "Common_Name",
                           choices = common_choices,
                           selected = "",
                           server = TRUE)
    }
    # store autofill values
    autofill_vals(list(
      Common_Name = species_filtered$`Common name`[1],
      Fish_Group  = species_filtered$Group[1]
    ))
  })
  
  # Preview new row
  new_row <- reactive({
    vals <- lapply(names(sightings_db()), function(col) {
      val <- input[[col]]
      if (is.null(val)) NA else val
    })
    df <- as.data.frame(t(vals), stringsAsFactors = FALSE)
    colnames(df) <- names(sightings_db())
    df
  })
  
  output$preview <- renderTable(new_row())
  
  # Save entry
  observeEvent(input$save_entry, {
    req(loaded(), sightings_db())
    
    # rename image if needed
    temp_dir <- file.path("www", "temp_images")
    
    # get the current filename from image_vals
    old_name <- basename(image_vals$files[image_vals$index])
    new_name <- input$Reference_Picture
    
    # build absolute paths for temp folder
    old_temp_path <- file.path(temp_dir, old_name)
    new_temp_path <- file.path(temp_dir, new_name)
    
    if (!is.null(new_name) && new_name != "" && new_name != old_name) {
      
      # original path in source folder
      old_orig_path <- file.path(setup_vals$dir, old_name)
      new_orig_path <- file.path(setup_vals$dir, new_name)
      
      # check if new name already exists in either location
      if (file.exists(new_temp_path) || file.exists(new_orig_path)) {
        showNotification(
          paste("Filename", new_name, "already exists! Please choose another name."),
          type = "error"
        )
        return()   # stop here, skip the rest of observeEvent
      } else {
        # rename in temp folder
        success_temp <- file.rename(old_temp_path, new_temp_path)
        # rename in original folder
        success_orig <- file.rename(old_orig_path, new_orig_path)
        
        if (success_temp && success_orig) {
          image_vals$files[image_vals$index] <- file.path("temp_images", new_name)
          showNotification(paste("Image renamed to", new_name, "!"), type = "message")
        } else {
          showNotification("Failed to rename image!", type = "error")
          return()   # also stop here if rename failed
        }
      }
    }
    
    new_row_df <- new_row()
    
    af <- autofill_vals()
    if (is.null(new_row_df$`Common Name`) || new_row_df$`Common Name` == "" || is.na(new_row_df$`Common Name`)) {
      new_row_df$`Common Name` <- af$Common_Name
    }
    if (is.null(new_row_df$`Fish Group`) || new_row_df$`Fish Group` == ""|| is.na(new_row_df$`Fish Group`)) {
      new_row_df$`Fish Group` <- af$Fish_Group
    }
    if (is.null(new_row_df$`Reference picture`) ||
        new_row_df$`Reference picture` == "" ||
        is.na(new_row_df$`Reference picture`)) {
      
      # if user typed something, use that
      if (!is.null(input$Reference_Picture) && input$Reference_Picture != "") {
        new_row_df$`Reference picture` <- input$Reference_Picture
      } else {
        # otherwise fall back to the current file name
        new_row_df$`Reference picture` <- current_file_val()
      }
    }
    print(new_row_df)
    
    # align types
    for (col in names(sightings_db())) {
      if (is.numeric(sightings_db()[[col]])) {
        new_row_df[[col]] <- as.numeric(new_row_df[[col]])
      } else {
        new_row_df[[col]] <- as.character(new_row_df[[col]])
      }
    }
    
    updated <- dplyr::bind_rows(sightings_db(), new_row_df)
    # reapply underscores
    cols_to_fix <- c("Common Name", "Fish Group", "Species")
    
    for (col in cols_to_fix) {
      if (col %in% names(updated)) {
        updated[[col]] <- gsub(" ", "_", trimws(gsub("\\s+", " ", updated[[col]])))
      }
    }
    sightings_db(updated)   # update reactiveVal
    
    save_sightings(updated, new_db_path)
    showNotification("New entry saved to Excel!", type = "message")
    
    # automatically go to next image
    image_vals$index <- image_vals$index %% length(image_vals$files) + 1
  })
}

shinyApp(ui, server)










