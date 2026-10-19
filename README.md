# chatcache

Persistent SQLite cache for user prompts and LLM responses in applications built with [ellmer](https://github.com/tidyverse/ellmer) and [shinychat](https://github.com/posit-dev/shinychat).

## Installation

```r
# Once on CRAN
install.packages("chatcache")

# Development version
# pak::pak("shekharchalwadigrepo-hub/chatcache")
```

## Quick start

```r
library(ellmer)
library(chatcache)

# Optional: explicitly initialize (auto-initializes on first use)
chatcache_init()

chat <- chat_openai(system_prompt = "Be concise.")

# First call hits the API and stores the result
answer <- cached_chat(chat, "What is the capital of France?")

# Subsequent identical calls return instantly from cache
answer <- cached_chat(chat, "What is the capital of France?")

# Inspect the cache
chatcache_list()
```

## Usage with shinychat

```r
library(shiny)
library(shinychat)
library(ellmer)
library(chatcache)

ui <- bslib::page_fluid(
  chat_ui("chat")
)

server <- function(input, output, session) {
  chat <- chat_openai(system_prompt = "You are a helpful assistant.")

  observeEvent(input$chat_user_input, {
    cached_shinychat_respond(chat, input$chat_user_input, "chat")
  })
}

shinyApp(ui, server)
```

## Features

- Persistent SQLite storage (survives R and Shiny restarts)
- Cache key incorporates prompt, system prompt, model, and conversation history
- Shared cache across users of the same application
- Hit counting and listing of cached interactions
- WAL mode enabled for better concurrent access
- Simple drop-in functions for ellmer and shinychat

## Cache location

By default the database is stored in:

```r
tools::R_user_dir("chatcache", which = "cache")
```

You can override this with `chatcache_init(cache_dir = "...")`.

## License

MIT
