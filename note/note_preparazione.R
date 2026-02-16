# scarica note da TODOIST

# recupera token
TODOIST_TOKEN <- Sys.getenv("TODOIST_API_TOKEN")

# funzione per scaricare le info
todoist_req <- function(endpoint, query = NULL) {
  request(paste0("https://api.todoist.com/api/v1/", endpoint)) |>
    req_headers(
      Authorization = paste("Bearer", TODOIST_TOKEN)
    ) |>
    req_url_query(!!!query) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

# funzione per il parsing
get_lista_note <- function(
    project_names = c("Casa", "Trasferta", "Acquisti da fare")
    , emojis = emoji(c("house", "airplane", "shopping_cart"))
){
  tasks <- todoist_req("tasks")$results
  
  projects <- todoist_req("projects")$results
  
  note <- projects %>% 
    filter(name %in% project_names) %>% 
    select(id, name, child_order) %>%
    rename(project_order = child_order) %>% 
    inner_join(tasks, by = c("id" = "project_id")) %>% 
    select(name, content, priority, contains("order"))
  
  note$emoji <- NA
  for(p in project_names){
    e <- emojis[which(project_names == p)]
    note[which(note$name == p), "emoji"] <- e
  }
  note %>% 
    mutate(text = paste(emoji, content))
}

# funzione per il rendering delle note
render_lista_note <- function(lista = lista_note){
  lapply(unique(lista_note$priority), function(p){
    lista %>% 
      filter(priority == p) %>% 
      arrange(project_order, child_order) %>% 
      select(text) %>% 
      unlist() %>% 
      toString()
  })
}

# preparazione nel formato finale
lista_note <- get_lista_note() 
lista_note <- render_lista_note()
