#' chatcache: Persistent Cache for ellmer and shinychat
#'
#' Provides a persistent SQLite-based cache for user prompts and corresponding
#' LLM responses. Designed for applications built with the \pkg{ellmer} and
#' \pkg{shinychat} packages.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang %||%
#' @importFrom cli cli_inform cli_abort
#' @importFrom digest digest
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom DBI dbConnect dbExistsTable dbExecute dbGetQuery
#' @importFrom RSQLite SQLite
## usethis namespace: end
NULL

#' Initialize the chat cache
#'
#' Creates or connects to the SQLite database used to store prompts and responses.
#'
#' @param cache_dir Character string. Directory in which to store the cache
#'   database. Defaults to the user cache directory via `tools::R_user_dir()`.
#'
#' @return An invisible DBI connection object.
#'
#' @examples
#' \dontrun{
#' chatcache_init()
#' chatcache_init(cache_dir = tempdir())
#' }
#'
#' @export
chatcache_init <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) {
    cache_dir <- tools::R_user_dir("chatcache", which = "cache")
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  db_path <- file.path(cache_dir, "chatcache.sqlite")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

  # Enable WAL mode for better concurrent access
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")

  if (!DBI::dbExistsTable(con, "interactions")) {
    DBI::dbExecute(con, "
      CREATE TABLE interactions (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        cache_key TEXT UNIQUE,
        prompt TEXT,
        system_prompt TEXT,
        model TEXT,
        provider TEXT,
        response TEXT,
        metadata TEXT,
        created_at TEXT,
        updated_at TEXT,
        hit_count INTEGER DEFAULT 1
      )
    ")
  }

  options(chatcache.con = con)
  invisible(con)
}

#' Generate a cache key
#'
#' Creates a stable SHA-256 hash from the prompt and relevant context.
#'
#' @param prompt Character. The user prompt.
#' @param system_prompt Character or `NULL`. The system prompt.
#' @param model Character or `NULL`. The model name.
#' @param history Character vector or `NULL`. Simplified conversation history.
#'
#' @return A character string (SHA-256 hash).
#'
#' @export
make_cache_key <- function(prompt, system_prompt = NULL, model = NULL, history = NULL) {
  parts <- c(
    as.character(prompt),
    as.character(system_prompt %||% ""),
    as.character(model %||% ""),
    if (!is.null(history)) paste(history, collapse = "||") else ""
  )
  digest::digest(paste(parts, collapse = "|||"), algo = "sha256")
}

#' Retrieve a cached response
#'
#' Looks up a previously stored response by cache key and increments the hit count.
#'
#' @param cache_key Character. The key returned by [make_cache_key()].
#' @param con Optional DBI connection. Uses the global connection if `NULL`.
#'
#' @return A list with elements `response`, `metadata`, and `hit_count`, or `NULL`
#'   if the key is not found.
#'
#' @export
chatcache_get <- function(cache_key, con = NULL) {
  con <- con %||% getOption("chatcache.con")
  if (is.null(con)) {
    chatcache_init()
    con <- getOption("chatcache.con")
  }

  res <- DBI::dbGetQuery(
    con,
    "SELECT response, metadata, hit_count FROM interactions WHERE cache_key = ?",
    params = list(cache_key)
  )

  if (nrow(res) == 0) return(NULL)

  DBI::dbExecute(
    con,
    "UPDATE interactions SET hit_count = hit_count + 1, updated_at = datetime('now') WHERE cache_key = ?",
    params = list(cache_key)
  )

  list(
    response = res$response[1],
    metadata = if (!is.na(res$metadata[1]) && nzchar(res$metadata[1])) {
      jsonlite::fromJSON(res$metadata[1])
    } else {
      NULL
    },
    hit_count = res$hit_count[1] + 1L
  )
}

#' Store a response in the cache
#'
#' Inserts or replaces a prompt-response pair in the cache.
#'
#' @param cache_key Character. Cache key.
#' @param prompt Character. Original user prompt.
#' @param response Character. LLM response text.
#' @param system_prompt Character or `NULL`.
#' @param model Character or `NULL`.
#' @param provider Character or `NULL`.
#' @param metadata List or `NULL`. Additional metadata to store as JSON.
#' @param con Optional DBI connection.
#'
#' @return Invisible `TRUE`.
#'
#' @export
chatcache_set <- function(cache_key, prompt, response,
                          system_prompt = NULL, model = NULL, provider = NULL,
                          metadata = NULL, con = NULL) {
  con <- con %||% getOption("chatcache.con")
  if (is.null(con)) {
    chatcache_init()
    con <- getOption("chatcache.con")
  }

  meta_json <- if (!is.null(metadata)) {
    jsonlite::toJSON(metadata, auto_unbox = TRUE)
  } else {
    NA_character_
  }

  DBI::dbExecute(con, "
    INSERT OR REPLACE INTO interactions
    (cache_key, prompt, system_prompt, model, provider, response, metadata, created_at, updated_at, hit_count)
    VALUES (?, ?, ?, ?, ?, ?, ?, datetime('now'), datetime('now'), 1)
  ", params = list(
    cache_key, prompt, system_prompt, model, provider, response, meta_json
  ))

  invisible(TRUE)
}

#' Cached ellmer chat call
#'
#' Checks the cache before calling an ellmer Chat object. On a miss, the model
#' is queried and the result is stored.
#'
#' @param chat An ellmer Chat object.
#' @param prompt Character. User prompt.
#' @param ... Additional arguments passed to `chat$chat()`.
#' @param use_history Logical. Whether to include conversation history in the
#'   cache key. Default is `TRUE`.
#'
#' @return Character string containing the response.
#'
#' @examples
#' \dontrun{
#' library(ellmer)
#' chat <- chat_openai(system_prompt = "Be concise.")
#' answer <- cached_chat(chat, "What is the capital of France?")
#' }
#'
#' @export
cached_chat <- function(chat, prompt, ..., use_history = TRUE) {
  if (!inherits(chat, "Chat")) {
    cli::cli_abort("{.arg chat} must be an ellmer Chat object.")
  }

  system_prompt <- chat$get_system_prompt()
  model <- chat$get_model()
  provider <- class(chat)[1]

  history_txt <- NULL
  if (isTRUE(use_history)) {
    history <- tryCatch(chat$get_turns(), error = function(e) NULL)
    if (!is.null(history) && length(history) > 0) {
      history_txt <- tryCatch({
        vapply(history, function(turn) {
          role <- tryCatch(turn@role, error = function(e) "unknown")
          text <- tryCatch({
            contents <- turn@contents
            paste(vapply(contents, function(c) {
              if (inherits(c, "ContentText")) c@text else "[non-text]"
            }, character(1)), collapse = " ")
          }, error = function(e) as.character(turn))
          paste0(role, ": ", text)
        }, character(1))
      }, error = function(e) NULL)
    }
  }

  key <- make_cache_key(prompt, system_prompt, model, history_txt)

  cached <- chatcache_get(key)
  if (!is.null(cached)) {
    cli::cli_inform("Cache hit (hits: {cached$hit_count})")
    return(cached$response)
  }

  resp_text <- chat$chat(prompt, ...)
  if (!is.character(resp_text)) {
    resp_text <- as.character(resp_text)
  }

  chatcache_set(
    cache_key = key,
    prompt = prompt,
    response = resp_text,
    system_prompt = system_prompt,
    model = model,
    provider = provider
  )

  resp_text
}

#' List cached interactions
#'
#' Returns a data frame of recent cached prompt-response pairs.
#'
#' @param limit Integer. Maximum number of rows to return. Default is 100.
#' @param con Optional DBI connection.
#'
#' @return A data frame.
#'
#' @export
chatcache_list <- function(limit = 100, con = NULL) {
  con <- con %||% getOption("chatcache.con")
  if (is.null(con)) {
    chatcache_init()
    con <- getOption("chatcache.con")
  }

  DBI::dbGetQuery(con, "
    SELECT id,
           substr(prompt, 1, 80) AS prompt_preview,
           substr(response, 1, 80) AS response_preview,
           model, provider, hit_count, created_at
    FROM interactions
    ORDER BY updated_at DESC
    LIMIT ?
  ", params = list(as.integer(limit)))
}

#' Clear the cache
#'
#' Deletes all stored interactions.
#'
#' @param confirm Logical. If `TRUE` (default) and running interactively, asks
#'   for confirmation.
#'
#' @return Invisible logical indicating whether the cache was cleared.
#'
#' @export
chatcache_clear <- function(confirm = TRUE) {
  if (isTRUE(confirm) && !interactive()) {
    cli::cli_abort("Set {.code confirm = FALSE} to clear the cache non-interactively.")
  }
  if (isTRUE(confirm)) {
    ans <- readline("Clear all cached interactions? (yes/no): ")
    if (tolower(ans) != "yes") return(invisible(FALSE))
  }

  con <- getOption("chatcache.con")
  if (!is.null(con)) {
    DBI::dbExecute(con, "DELETE FROM interactions")
  }
  cli::cli_inform("Cache cleared.")
  invisible(TRUE)
}

#' Cached response helper for shinychat
#'
#' Convenience wrapper that retrieves or generates a response and appends it
#' to a shinychat UI.
#'
#' @param chat An ellmer Chat object.
#' @param user_input Character. The user message (typically `input$chat_user_input`).
#' @param chat_id Character. The shinychat UI id. Default is `"chat"`.
#'
#' @return Invisible character response.
#'
#' @export
cached_shinychat_respond <- function(chat, user_input, chat_id = "chat") {
  if (!requireNamespace("shinychat", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg shinychat} is required for this function.")
  }

  resp <- cached_chat(chat, user_input)
  shinychat::chat_append(chat_id, resp)
  invisible(resp)
}
