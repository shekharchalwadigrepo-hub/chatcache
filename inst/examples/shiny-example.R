library(shiny)
library(shinychat)
library(ellmer)
library(chatcache)

ui <- bslib::page_fluid(
  titlePanel("chatcache example"),
  chat_ui("chat")
)

server <- function(input, output, session) {
  chat <- chat_openai(
    system_prompt = "You are a helpful assistant. Be concise."
  )

  observeEvent(input$chat_user_input, {
    cached_shinychat_respond(chat, input$chat_user_input, "chat")
  })
}

shinyApp(ui, server)
