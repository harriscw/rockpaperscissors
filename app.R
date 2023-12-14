library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  ###
  # Landing page
  ###
  conditionalPanel("input.start==0",
                   fluidRow(
                     column(width = 12,align="center",
                       h2("Room 7 Rock Paper Scissors Battle"),
                       img(src = "logo.jpg")
                     )
                   ),
                   fluidRow(
                     column(width=12,align="center",
                     actionButton("start", label = "Start") 
                     )
                   )
  ),
  
  ###
  # Battle Page
  ###
  
  conditionalPanel("input.start>0",
                   fluidRow(
                     column(width = 5,align="center",
                        h1(textOutput("player1")),
                        uiOutput("player1_display")
                     ),
                     column(width = 2,align="center",
                            uiOutput("vs")
                     ),
                     column(width = 5,align="center",
                        h1(textOutput("player2")),
                        uiOutput("player2_display")
                     )
                   ),
                   fluidRow(
                     conditionalPanel("output.show_window == true",
                       column(width = 5,align="center",uiOutput("player1_throw_display")
                              ),
                       column(width = 2,align="center",
                                                h1(textOutput("thewinner"))),
                       column(width = 5,align="center",uiOutput("player2_throw_display")
                              )
                     )
                   ),
                   hr(),
                   fluidRow(
                     column(width = 12,align="center",
                            actionButton("newplayers", label = "New Players"),
                            actionButton("battle", label = "Battle"),
                            br(),
                            hr(),
                            h3("Scoreboard"),
                            verbatimTextOutput("winner_table")
                     )
                   )
                   
  )

  

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  vals=reactiveValues(thelist=list("winners"=c()))
  theplayers=list.files("www/players")
  
  
  
  
  observeEvent(input$newplayers, {
    
    output$show_window=reactive({FALSE})
    outputOptions(output, "show_window", suspendWhenHidden = FALSE)
    
    
    # select players
    player1=sample(theplayers,1)
    player2=sample(theplayers[theplayers != player1],1)
    
    
    vals$thelist[["player1"]]={gsub("\\.jpg","",player1)}
    vals$thelist[["player2"]]={gsub("\\.jpg","",player2)}
    
    # display player names
    output$player1=renderText({gsub("\\.jpg","",player1)})
    output$player2=renderText({gsub("\\.jpg","",player2)})
    
    # display player images
    output$player1_display = renderUI({
      div(
        img(src = paste0("players/",player1),height="30%", width="30%")
      )
    })
    
    output$player2_display = renderUI({
      div(
        img(src = paste0("players/",player2),height="30%", width="30%")
      )
    })
    
    output$vs=renderUI({
      h1("VS.")
    })
    
  })
  
  
  observeEvent(input$battle, {
    
    output$show_window=reactive({TRUE})
    # outputOptions(output, "show_window", suspendWhenHidden = FALSE)
    
    if(input$newplayers>0){
  
    # do some throws
      if(runif(1)<.05){
        player1_throw="socks"
      }else{
        player1_throw=sample(c("paper","rock","scissors"),1)
      }
      
      if(runif(1)<.05){
        player2_throw="socks"
      }else{
        player2_throw=sample(c("paper","rock","scissors"),1)
      }
    
    # display it
    output$player1_throw_display=renderUI({
      div(
        fluidRow(
        img(src = paste0(player1_throw,".png"),height="30%", width="30%"),
        fluidRow(h3(paste(toupper(player1_throw),"!")))
        )
      )
    })
    
    output$player2_throw_display=renderUI({
      div(
        fluidRow(
          img(src = paste0(player2_throw,".png"),height="30%", width="30%"),
          fluidRow(h3(paste(toupper(player2_throw),"!")))
        )
      )
    })
    
    # get results
    if(player1_throw==player2_throw){
      winner="Tie"
    }else if(player1_throw=="socks"){
      winner="Player 1"
    }else if(player2_throw=="socks"){
      winner="Player 2"
    }else if(player1_throw=="rock" & player2_throw=="scissors"){
      winner="Player 1"
    }else if(player1_throw=="rock" & player2_throw=="paper"){
      winner="Player 2"
    }else if(player1_throw=="paper" & player2_throw=="scissors"){
      winner="Player 2"
    }else if(player1_throw=="paper" & player2_throw=="rock"){
      winner="Player 1"
    }else if(player1_throw=="scissors" & player2_throw=="paper"){
      winner="Player 1"
    }else if(player1_throw=="scissors" & player2_throw=="rock"){
      winner="Player 2"
    }
    
    if(winner=="Player 1"){
      res = paste(vals$thelist[["player1"]],"wins!")
      vals$thelist$winners=c(vals$thelist$winners,vals$thelist[["player1"]])
    }else if(winner=="Player 2"){
      res = paste(vals$thelist[["player2"]],"wins!")
      vals$thelist$winners=c(vals$thelist$winners,vals$thelist[["player2"]])
    }else{
      "Tie!"
    }
    
    output$winner_table=renderPrint({
      if(length(vals$thelist$winners)>0){
        sort(table(vals$thelist$winners),decreasing = TRUE)
      }
    })
    
    output$thewinner=renderText({

      
      if(winner=="Player 1"){
        paste(toupper(vals$thelist[["player1"]]),"wins!")
      }else if(winner=="Player 2"){
        paste(toupper(vals$thelist[["player2"]]),"wins!")
      }else{
        "Tie!"
      }
    })
    
    }#end of if newplayers>0
    
  })

  


}

# Run the application 
shinyApp(ui = ui, server = server)
