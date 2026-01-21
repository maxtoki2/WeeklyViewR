# install.packages("httr2")  # if needed
library(httr2)
library(emojifont)

# Your Todoist API token
TODOIST_TOKEN <- Sys.getenv("TODOIST_API_TOKEN")
# or set directly (less secure):
# TODOIST_TOKEN <- "your_api_token_here"

todoist_req <- function(endpoint, query = NULL) {
  request(paste0("https://api.todoist.com/rest/v2/", endpoint)) |>
    req_headers(
      Authorization = paste("Bearer", TODOIST_TOKEN)
    ) |>
    req_url_query(!!!query) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}


tasks <- todoist_req("tasks")

tasks[1,]

projects <- todoist_req("projects")

note <- projects %>% 
  # filter(id == 2365788352) %>% 
  filter(!(substr(name, 1, 5) %in% c("Inbox", "Getti"))) %>% 
  select(id, name) %>% 
  inner_join(tasks, by = c("id" = "project_id")) %>% 
  select(name, content, priority, order)

search_emoji("plane")
emoji("house")
emoji("shopping_cart")
emoji("airplane")

# functions
get_lista_note <- function(
    project_names = c("Casa", "Trasferta", "Acquisti da fare")
    , emojis = emoji(c("house", "airplane", "shopping_cart"))
){
  tasks <- todoist_req("tasks")
  
  projects <- todoist_req("projects")
  
  note <- projects %>% 
    filter(name %in% project_names) %>% 
    select(id, name) %>% 
    inner_join(tasks, by = c("id" = "project_id")) %>% 
    select(name, content, priority, order)
  
  # lapply(project_names, function(p))
    note
}

get_lista_note()
