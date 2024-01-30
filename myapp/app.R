

#---------------------------------------------------------------------------------------------------
#-- Shiny app to create a searchable/filterable resources list for the UBC BCCDC I-EDI Committee ---
#-- Code author: Jennifer Ferris, Freddy Francis  
#-- 
#-- last edited: Jan 30, 20234 
#-- by: Freddy
#---------------------------------------------------------------------------------------------------

library(googledrive)
library(googlesheets4)
library(dplyr)
library(stringr)
library(shiny)
library(shinyWidgets)
library(bslib)
#library(DT)
library(reactable)
library(reactablefmtr)
library(shinyfilter)
library(shinyBS)

#### required to build shinylive (only once when setting up)
#install.packages("pak")
#pak::pak("posit-dev/r-shinylive")
#pak::pak("rstudio/httpuv")



#-- first-time use: 
#-- you will need to authenticate your Google Drive access so that R can import data from a Google Sheet
#-- you can authenticate with the two lines of commented-out code below, see more instructions at:  https://googlesheets4.tidyverse.org/articles/drive-and-sheets.html

drive_auth()
gs4_auth(token = drive_token())


#-- styling ---

BCCDC_theme <- bs_theme(
  version = 5, 
  bg = "#fff",
  fg = "#004B8C",
  primary = "#004B8C",
  secondary = "#5DA5DA",
  info = "#5DA5DA",
  base_font = "Arial",
  heading_font = "Arial"
)

linebreaks <- function(n){HTML(strrep(br(), n))}

#--- load data from Google Sheet -----
resource_drive <- drive_get("Resource_list_shiny_app")

gs4_get(resource_drive)

resource_list <- read_sheet(resource_drive, sheet = 1) %>%
  arrange(Category, Resource) 

categories <- c("All Categories", strsplit(resource_list$Category, ", ") %>% 
  unlist() %>% 
  unique())

# category_des <- as.data.frame(categories) %>% 
#   dplyr::mutate(description=c("Complete list",
#                               "Training resources for workplace bullying and harrasment",
#                               "Student specific resources",
#                               "Equity and diversity in workplace related resources",
#                               "Idigeneity and cultural safety", 
#                               "Applicable to research process",
#                               "MH",
#                               "SV",
#                               "Hybrid work"))
# 
# description <- category_des$description

#-- Shiny app ----
#-- helpful blog post about creating Shiny tables: https://clarewest.github.io/blog/post/making-tables-shiny/

####Creating tooltips for radiobuttons
### User @K.Rohde from stack overflow https://stackoverflow.com/questions/36132204/reactive-radiobuttons-with-tooltipbs-in-shiny

radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('dispose');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
}

my_css <-
  ".btn-group, .btn-group-vertical {
    column-count: 2;
  }
  
  .btn-group-toggle {
  width:200px;
  }

  .radiobtn { 
    width:200px;
  }"

#-- UI -----

ui <- fluidPage(theme = BCCDC_theme,
                linebreaks(1),
                titlePanel("UBC-BCCDC Indigeneity, equity, diversity and inclusion (I-EDI) Resource List"),
                linebreaks(1),
                mainPanel(width = 12,
                          
                          #-- header text --
                          h4("Welcome! This is a curated list of resources to support I-EDI for staff, faculty, and students involved with research at BCCDC."),
                          h6("You can browse resources by their categories by using the buttons at the top of the page."), 
                          h6("You can search for resources by type, intended audience, and description by using the filters in the list."),
                          h5("If you would like to provide feedback about this page, or suggest another resource to be added, please use this form."),
                          
                          linebreaks(1),
                          tags$head(tags$style(HTML(my_css))),
                          #-- buttons to select resource category
                          radioButtons(
                            inputId = "category",
                            label = "Resource Categories",
                            choices = categories,
                            width = '100%',
                            inline = T
                           ),
                          radioTooltip(id = "category",
                                       choice = "All Categories",
                                       title = "Complete list",
                                       placement = "right",
                                       trigger = "hover"),
                          radioTooltip(id = "category",
                                       choice = "Bullying and Harassment Prevention",
                                       title = "Resources aimed at preventing or responding to bullying or harassment and encouraging respect in the workplace",
                                       placement = "right",
                                       trigger = "hover"),
                          radioTooltip(id = "category",
                                       choice = "Student-Specific",
                                       title = "Resources that are only accessible to students, such as student grants or student counselling services, or policies specifically impacting students, such as how to request exam accommodations",
                                       placement = "right",
                                       trigger = "hover"),
                          radioTooltip(id = "category",
                                       choice = "Equity Diversity and Inclusion",
                                       title = "Resources supportive of different groups of individuals, including people of different races, ethnicities, religions, abilities, genders, and sexual orientations having equitable opportunities in workplaces and in research",
                                       placement = "right",
                                       trigger = "hover"),
                          radioTooltip(id = "category",
                                       choice = "Indigenous Engagement and Decolonization",
                                       title = "Resources that support learning & implementation of practices that remove or undo colonial elements and the addition or redoing of Indigenous elements into research and public health practice, moving beyond tokenistic gestures of recognition or inclusion to meaningfully change practices and structures" ,
                                       placement = "right",
                                       trigger = "hover"),
                          radioTooltip(id = "category",
                                       choice = "Research Support" ,
                                       title = "Resources to support research such as grant databases, data collection standards and tools, or research ethics frameworks",
                                       placement = "right",
                                       trigger = "hover"),
                          radioTooltip(id = "category",
                                       choice = "Mental Health",
                                       title = "Resources to support or enhance mental wellness among staff or students such as services, education tools, training resources or webinars",
                                       placement = "right",
                                       trigger = "hover"),
                          radioTooltip(id = "category",
                                       choice = "Sexual Violence Prevention",
                                       title = "Resources aimed at preventing or responding to all forms of sexual violence (including physical, emotional, psychological, and cyber violence) both in the workplace and in communities",
                                       placement = "right",
                                       trigger = "hover"),
                          radioTooltip(id = "category",
                                       choice = "Accessibility",
                                       title = "Resources to support people with physical accessibility needs and ensure workplaces and learning environments are accessible to all, such as accessible restroom policies, as well as resources to support staff or students who have other accessibility needs such as exam accommodation policies and learning advisors",
                                       placement = "right",
                                       trigger = "hover"),
                          
                          #- check box for resource audience
                          awesomeCheckboxGroup(
                            inputId = "audience",
                            label = "I'm looking for resources for:", 
                            choices = c("Faculty", "Staff", "Students"),
                            selected = "",
                            inline = TRUE),

                          #- table output
                          fluidRow(style = 'margin-left: 5%; margin-right: 5%;',
                                   column(width = 12, align = "center",
                                          reactableOutput("mytable")
                                          )
                                   )
            ) #- mainpanel
  ) #- fluidpage


#-- Server ------

server <- function(input, output, session) {
  
  
  #- filter resource list to only resources with selected category and audience type
  filtered_list <- reactive({
    
    #filt_list <- resource_list
    
    if(input$category != "All Categories"){
      resource_list <- resource_list %>% 
        filter(str_detect(Category, regex(input$category, ignore_case = T)))
    }
    
    if(length(input$audience) == 1){
      resource_list <- resource_list %>%
        filter(str_detect(`Target Audience`, regex(input$audience, ignore_case = T)))
      } 
    if(length(input$audience) == 2){
        resource_list <- resource_list %>%
          filter(str_detect(`Target Audience`, regex(input$audience[1], ignore_case = T))) %>%
          filter(str_detect(`Target Audience`, regex(input$audience[2], ignore_case = T)))
    }
    if(length(input$audience) == 3){
      resource_list <- resource_list %>%
        filter(str_detect(`Target Audience`, regex(input$audience[1], ignore_case = T))) %>%
        filter(str_detect(`Target Audience`, regex(input$audience[2], ignore_case = T))) %>%
        filter(str_detect(`Target Audience`, regex(input$audience[3], ignore_case = T))) 
    }
    
    resource_list 
  })

  #- resource data table
  output$mytable <- renderReactable({
    reactable(filtered_list()[,c(1, 3:4, 6:8)], 
              filterable = T,
              searchable = F,
              theme = clean(),
              
              #- settings for columns in table
              columns = list(
                'Resource Type' = colDef(
                  align = "center", minWidth = 75,
                  
                  #- function to filter column by dropdown select menu
                  filterInput = function(values, name) {
                    tags$select(
                      # Set to undefined to clear the filter
                      onchange = sprintf("Reactable.setFilter('type-select', '%s', event.target.value || undefined)", name),
                      # "All" has an empty value to clear the filter, and is the default option
                      tags$option(value = "", "All"),
                      lapply(unique(values), tags$option),
                      "aria-label" = sprintf("Filter %s", name),
                      style = "width: 100%; height: 28px;"
                    )}
                  ), 
                
                'Resource Creator' = colDef(
                  align = "center", minWidth = 75), 
                
                'Target Audience' = colDef(
                  align = "center", minWidth = 75),
                
                'Available To' = colDef(
                  align = "center", minWidth = 75,
                  filterInput = function(values, name) { #- dropdown column filter
                    tags$select(
                      onchange = sprintf("Reactable.setFilter('type-select', '%s', event.target.value || undefined)", name),
                      lapply(unique(values), tags$option),
                      "aria-label" = sprintf("Filter %s", name),
                      style = "width: 100%; height: 28px;"
                    )}
                  ), 
                
                'Description' = colDef(
                  minWidth = 200), 
                Resource = colDef(minWidth = 150, 
                                  cell = function(Resource) {
                                    url <- resource_list[resource_list$Resource == Resource, "URL"]
                                    htmltools::tags$a(href = as.character(url), target = "_blank", as.character(Resource))
                                    })
                ),
              
              defaultPageSize = 15,
              elementId = c("type-select")) 
  }) #- reactable 
  } #-server


#-- Run -----
shinyApp(ui = ui, server = server)



















#-- modifying manual spreadsheet ----
# resource_edit <- readxl::read_xlsx("data/resource_list.xlsx")
# 
# resource_edit_2 <- resource_edit %>%
#   group_by(Resource, across(c(URL, `Resource Type`:Description))) %>%
#   summarise(Category = paste(Category, collapse = ", ")) %>%
#   relocate(Category, .after = `Resource Creator`)
# 
# write.csv(resource_edit_2, "data/resource_list_longform.csv", row.names = F)
