###########################################
###########################################
#**************** ui.R *******************#
###########################################
###########################################

library(shiny)
library(shinythemes)
library(shinyBS)
library(shinyjs)
library(shinydashboard)
library(gdata)
library(rsconnect)
#library(markdown)
require(markdown)
#require(rmarkdown)

#install.packages("rmarkdown", repos = "https://cran.revolutionanalytics.com")
header <- dashboardHeader(title = "Insulin Recommendation Portal", titleWidth = 350, 
                          
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "2 unread chat",
                                         icon("comments-o")
                                       )), 
                          
                          dropdownMenu(type = "tasks", badgeStatus = "danger",
                                       taskItem(value = 100, color = "green",
                                                "Count carbs"
                                       ),
                                       taskItem(value = 65, color = "red",
                                                "Exercise and health events"
                                       ),
                                       taskItem(value = 25, color = "blue",
                                                "Appointment with Dr. Joe"
                                       )),
                          
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Team - IRP",
                                         message = "Membership 2016.",
                                         time = "2016-10-10"
                                       ),
                                       messageItem(
                                         from = "New User",
                                         message = "Getting started.",
                                         icon = icon("question"),
                                         time = "22:00"
                                       ),
                                       messageItem(
                                         from = "Help",
                                         message = "Search for topics.",
                                         icon = icon("info-circle")
                                       ))
)

sidebar <- dashboardSidebar(width = 350,
                            sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                                              label = "Search..."),
                            sidebarMenu(menuItem("IRP - MED", tabName = "hp", icon = icon("user-md"),
                                                 menuSubItem("Calculate an Insulin-to-Carb Ratio", tabName = "icratio"),
                                                 menuSubItem("Basal Insulin Calculator", tabName = "bicphy"),
                                                 menuSubItem("Evidence Based Medicine", tabName = "ebm"),
                                                 menuSubItem("Pull Patient Record", tabName = "ppd")
                            ),
                            menuItem("IRP - PatientOne", tabName = "pat", icon = icon("users"),
                                     menuSubItem("Basal Insulin Calculator", tabName = "bicpat"),
                                     menuSubItem("Patient Data", tabName = "pd")
                            ),
                            menuItem("API Integration", tabName = "api", icon = icon("medkit")),
                            menuItem("Chat", tabName = "chat", icon = icon("comment"))
                            )
)

body <- dashboardBody(
  
  tabItems(
    tabItem("icratio",
            fluidRow(align="center",
                     
                     bsAlert("remember_compute")
                     
            ),
            fluidRow(
              #shinythemes::themeSelector(),
              
              box(align = "center",
                  title = h4("Step 1: Enter Preprandial Glucose in mg/dL", align = "center"), height = 200, width = 4, solidHeader = TRUE, status = "primary",
                  sliderInput("slider1", "Target Range: 80 - 130 mg/dL", min = 0, max = 350, value = 125, step=10, timeFormat = TRUE)
              ),
              box(align = "center",
                  title = h4("Step 2: Count Carbohydrates in grams", align = "center"), height = 200, width = 4, solidHeader = TRUE, status = "primary",
                  numericInput("carbs", "", 35, min = 1, max = 200)
              ),
              box(align = "center",
                  title = h4("Step 3: Enter Postprandial Glucose in mg/dL", align = "center"), height = 200, width = 4, solidHeader = TRUE, status = "primary",
                  sliderInput("slider2", "Target Range: <180 mg/dL", min = 0, max = 350, value = 200, step=10, timeFormat = TRUE)
              )
            ),
            
            fluidRow(align = "center",
                     br(),
                     
                     tags$button(id = "goButton", type = "button", class = "btn action-button btn-primary", "Compute!") # color selection here
            ),
            
            br(),
            br(),
            
            box(align = "center",
                title = h4("Observation", align = "center"), height = 150, width = 12, solidHeader = TRUE, status = "primary",
                htmlOutput("spike"),
                tags$head(tags$style("#spike{color: grey;
                                     font-size: 25px;
                                     font-style: italic;
                                     }"
                         )
                )
                
                ),
            box(align = "center",
                title = h4("Insulin-to-Carb (I:C) Ratio: Carbohydrates in grams covered by 1.0 U of Insulin", align = "center"), 
                height = 200, width = 12, solidHeader = TRUE, status = "danger",
                includeMarkdown("frontPage.md"),
                numericInput("insy", "", 25, min = 1, max = 100, width = 85)
                
            )
            
                ),
    
    tabItem("bicphy",
            
            fluidRow(
              #shinythemes::themeSelector(),
              box(align = "center",
                  title = h4("Step 1: Enter Preprandial Glucose in mg/dL", align = "center"), height = 200, width = 3, solidHeader = TRUE, status = "primary",
                  sliderInput("slider3", "Target Range: 80 - 130 mg/dL", min = 0, max = 350, value = 125, step=10, timeFormat = TRUE)
              ),
              box(align = "center",
                  title = h4("Step 2: Count Carbohydrates in grams", align = "center"), height = 200, width = 3, solidHeader = TRUE, status = "primary",
                  numericInput("carbs1", "", 35, min = 1, max = 200)
              ),
              box(align = "center",
                  title = h4("Step 3: Blood Glucose Spike Ratio (1g : __ mg/dL)", align = "center"), height = 200, width = 3, solidHeader = TRUE, status = "primary",
                  numericInput("gs", "", 4, min = 1, max = 200)
              ),
              box(align = "center",
                  title = h4("Step 4: Insulin:Carbohydrates Ratio (1U : __ g)", align = "center"), height = 200, width = 3, solidHeader = TRUE, status = "primary",
                  numericInput("ic1", "", 25, min = 1, max = 200)
              ),
              
              fluidRow(align = "center",
                       br(),
                      
                       tags$button(id = "goButton1", type = "button", class = "btn action-button btn-primary", "Go!") # color selection here
              ),
              
              br(),
              br(),
              
              
              box(align = "center",
                  title = h4("Recommendation", align = "center"), height = 150, width = 12, solidHeader = TRUE, status = "success",
                  htmlOutput("basalphy"),
                  tags$head(tags$style("#basalphy{color: grey;
                                       font-size: 25px;
                                       font-style: italic;
                                       }"
                         )
                  )
                  
                  )
              
                  )
              ), 
    
    tabItem("ppd",
            
            fluidPage(
              titlePanel(""),
              sidebarLayout(
                sidebarPanel(
                  fileInput('file1', 'Choose File',
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv')),
                  tags$hr(),
                  checkboxInput('header', 'Header', TRUE),
                  radioButtons('sep', 'Separator',
                               c(Comma=',',
                                 Semicolon=';',
                                 Tab='\t'),
                               ','),
                  radioButtons('quote', 'Quote',
                               c(None='',
                                 'Double Quote'='"',
                                 'Single Quote'="'"),
                               '"')
                ),
                mainPanel(
                  tableOutput('contents')
                )
              )
            )
    ), 
    
    tabItem("bicpat",
            
            fluidRow(
              #shinythemes::themeSelector(),
              box(align = "center",
                  title = h4("Step 1: Enter Preprandial Glucose in mg/dL", align = "center"), height = 200, width = 6, solidHeader = TRUE, status = "primary",
                  sliderInput("slider4", "Target Range: 80 - 130 mg/dL", min = 0, max = 350, value = 125, step=10, timeFormat = TRUE)
              ),
              box(align = "center",
                  title = h4("Step 2: Count Carbohydrates in grams", align = "center"), height = 200, width = 6, solidHeader = TRUE, status = "primary",
                  numericInput("carbs2", "", 35, min = 1, max = 200)
              ),
              box(align = "center",
                  title = h4("Carb Counter", align = "center"), height = 300, width = 12, solidHeader = TRUE, status = "primary",
                  a(img(src="carb1.png"), href="https://www.myfitnesspal.com/"),
                  a(img(src="carb3.png"), href="http://www.calorieking.com/")
              ),
              
              br(),
              
              fluidRow(align = "center",
                       br(),
                       
                       tags$button(id = "goButton2", type = "button", class = "btn action-button btn-primary", "Run!") # color selection here
              ),
              
              br(),
              br(),
              
              
              box(align = "center",
                  title = h4("Recommendation", align = "center"), height = 150, width = 12, solidHeader = TRUE, status = "success",
                  htmlOutput("basalpat"),
                  tags$head(tags$style("#basalpat{color: grey;
                                       font-size: 25px;
                                       font-style: italic;
                                       }"
                         )
                  )
                  
                  )
              
                  )
              ),
    
    tabItem("pd",
            
            fluidRow(align="center",
                     textInput("name", "Name", ""),
                     numericInput("age", "Age", "", 10, min = 1, max = 100),
                     numericInput("weight", "Weight in Kg", "", 100, min = 1, max = 300),
                     selectInput("insulin type", "Insulin Type",
                                 list("Rapid-acting insulin"= c("Insulin glulisine (Apidra)", "Insulin lispro (Humalog)", "Insulin aspart (NovoLog)"),
                                      "Short-acting insulin"= c("Humulin R", "Novolin R"), 
                                      "Intermediate-acting insulin"= c("NPH (N)", ""), 
                                      "Long-acting insulin"= c("Insulin detemir (Levemir)","Insulin glargine (Lantus)"))),
                     numericInput("insulin dose", "Daily Insulin Dose (U)", "", 10, min = 1, max = 80),
                     tags$button(id = "goButton", type = "button", class = "btn action-button btn-primary", "Submit")
            )
    ), 
    
    tabItem("ebm",
            
            fluidRow(align="center",
                     br(),
                     br(),
                     a(img(src="ebm1.png"), href="https://www.guideline.gov/"),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     a(img(src="ebm2.png"), href="https://www.nice.org.uk/")
            )
    ),
    
    tabItem("chat",
            fluidRow(align="center",
                     bootstrapPage(
                       # We'll add some custom CSS styling -- totally optional
                       includeCSS("shinychat.css"),
                       
                       # And custom JavaScript -- just to send a message when a user hits "enter"
                       # and automatically scroll the chat window for us. Totally optional.
                       includeScript("sendOnEnter.js"),
                       
                       div(
                         # Setup custom Bootstrap elements here to define a new layout
                         class = "container-fluid", 
                         div(class = "row-fluid",
                             # Set the page title
                             tags$head(tags$title("Chat with your doctor!")),
                             
                             # Create the header
                             div(class="span6", style="padding: 10px 0px;",
                                 h1("Chat with your doctor!")
                             ), div(class="span6", id="play-nice",
                                    centerText("IP addresses are logged.")
                             )
                             
                         ),
                         # The main panel
                         div(
                           class = "row-fluid", 
                           mainPanel(
                             # Create a spot for a dynamic UI containing the chat contents.
                             uiOutput("chat"),
                             
                             # Create the bottom bar to allow users to chat.
                             fluidRow(
                               div(class="span10",
                                   textInput("entry", "")
                               ),
                               div(class="span2 center",
                                   actionButton("send", "Send")
                               )
                             )
                           ),
                           # The right sidebar
                           sidebarPanel(
                             # Let the user define his/her own ID
                             textInput("user", "Your User ID:", value=""),
                             selectInput("connect with", "Connect with:",c("Dr. Joe", "Molly", "Dr. Carson")),
                             tags$hr(),
                             # Create a spot for a dynamic UI containing the list of users.
                             uiOutput("userList"),
                             tags$hr()
                             )
                         )
                       )
                     )
            )
    )
  )
)

dashboardPage(header, sidebar, body, skin = "black")




