# Trivia Module ----
# Trivia game outputs and observers

output$trivia_question <- renderText({
  req(trivia_game_state$current_question)
  trivia_game_state$current_question$question
})

output$trivia_answer <- renderUI({
  req(trivia_game_state$current_question)
  selectizeInput(
    'trivia_user_answer',
    "Your answer: ",
    unique(dfs$PLAYER)
  )
})

output$trivia_leaderboard <- renderDT({
  tibble(still = "UNDER CONSTRUCTION")
})

# Trivia observers
observeEvent(input$trivia_submit, {
  req(input$trivia_user_answer, trivia_game_state$current_question) 
  
  correct <- trivia_game_state$current_question$correct_answer
  if (input$trivia_user_answer == correct && !trivia_game_state$game_over) {
    trivia_game_state$points <- trivia_game_state$points + trivia_game_state$current_question$difficulty
    trivia_game_state$current_question <- NULL
    trivia_game_state$current_question <- trivia_next_question()
    output$trivia_result <- renderText({
      paste("Correct! You have", trivia_game_state$points, "points.")
    })
  } else {
    trivia_game_state$game_over <- TRUE
  }
})

observeEvent(trivia_game_state$game_over, {
  if (trivia_game_state$game_over) {
    hide("trivia_submit")
    show("trivia_restart")
    output$trivia_result <- renderText({
      paste("You lose! The answer was...", 
            trivia_game_state$current_question$correct_answer,
            "Your final score:", trivia_game_state$points)
    })
  }
})

observeEvent(input$trivia_restart, {
  trivia_game_state$points <- 0
  trivia_game_state$game_over <- FALSE
  trivia_game_state$current_question <- NULL
  trivia_game_state$current_question <- trivia_next_question()
output$trivia_result <- renderText({""})
  hide("trivia_restart")
  show("trivia_submit")
})
