###########################################
###########################################
#************** server.R *****************#
###########################################
###########################################

# Globally define a place where all users can share some reactive data.

vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.

if (file.exists("chat.Rds")){
  
  vars$chat <- readRDS("chat.Rds")
  
} else {
  
  vars$chat <- " "
  
}

#' Get the prefix for the line to be added to the chat window. Usually a newline character unless it's the first line.

linePrefix <- function(){
  
  if (is.null(isolate(vars$chat))){
    
    return("")
  }
  
  return("<br />")
}

server<- function(input, output, session) {
  
  createAlert(session, "remember_compute", "exampleAlert", 
              content = centerText("Please remember to click 'Compute!' below after making any changes here."), append = TRUE)
  
  observeEvent(input$goButton, {
    
    output$spike <- renderText({ paste("1g of carb spikes patient's plasma glucose by","<font color=\"8a46ff\"><b>", 
                                     format(round((input$slider2 - input$slider1)/input$carbs, 2), nsmall = 2), paste("mg/dL"),"</b></font>")  })
 
  })
  
  observeEvent(input$goButton1, {
    
    output$basalphy <- renderText({ paste("Basal insulin dose recommended","<font color=\"8a46ff\"><b>", 
                                          format(round((input$slider3 + (input$carbs1 * input$gs))/input$ic1, 2), nsmall = 2), paste("mg/dL"),"</b></font>")  })
  })
  
  observeEvent(input$goButton2, {
    
    output$basalpat <- renderText({ paste("Basal insulin dose recommended","<font color=\"8a46ff\"><b>", 
                                          format(round((input$slider4 + (input$carbs2 * input$gs))/input$ic1, 2), nsmall = 2), paste("mg/dL"),"</b></font>")  })
  })
  
  # Create a spot for reactive variables specific to this particular session
  
  sessionVars <- reactiveValues(username = "")
  
  # Track whether or not this session has been initialized. We'll use this to assign a username to unininitialized sessions.
  
  init <- FALSE
  
  # When a session is ended, remove the user and note that they left the room. 
  
  session$onSessionEnded(function() {
    
    isolate({
      
      vars$users <- vars$users[vars$users != sessionVars$username]
      
    })
  })
  
  # Observer to handle changes to the username
  
  observe({
    
    # We want a reactive dependency on this variable, so we'll just list it here.
    input$user
    
    if (!init){
      
      # Seed initial username
      
      sessionVars$username <- paste0("Duke")
      init <<- TRUE
    } else {
      
      # A previous username was already given
      
      isolate({
        
        if (input$user == sessionVars$username || input$user == ""){
          
          # No change. Just return.
          return()
        }
        
        # Updating username      
        # First, remove the old one
        
        vars$users <- vars$users[vars$users != sessionVars$username]
        
        # Now update with the new one
        
        sessionVars$username <- input$user
        
      })
    }
    # Add this user to the global list of users
    
    isolate(vars$users <- c(vars$users, sessionVars$username))
    
  })
  
  # Keep the username updated with whatever sanitized/assigned username we have
  
  observe({
    
    updateTextInput(session, "user", 
                    value=sessionVars$username)  
    
  })
  
  # Keep the list of connected users updated
  # Listen for input$send changes (i.e. when the button is clicked)
  
  observe({
    
    if(input$send < 1){
      
      # The code must be initializing, b/c the button hasn't been clicked yet.
      return()
    }
    
    isolate({
      
      # Add the current entry to the chat log.
      vars$chat <<- c(vars$chat, 
                      paste0(linePrefix(),
                             tags$span(class="username",
                                       tags$abbr(title=Sys.time(), sessionVars$username)
                             ),
                             ": ",
                             tagList(input$entry)))
    })
    
    # Clear out the text entry field.
    updateTextInput(session, "entry", value="")
    
  })
  
  # Dynamically create the UI for the chat window.
  
  output$chat <- renderUI({
    
    if (length(vars$chat) > 500){
      
      # Too long, use only the most recent 500 lines
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
    }
    # Save the chat object so we can restore it later if needed.
    saveRDS(vars$chat, "chat.Rds")
    
    # Pass the chat log through as HTML
    HTML(vars$chat)
  })

}

