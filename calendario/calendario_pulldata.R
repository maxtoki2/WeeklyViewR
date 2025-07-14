############
# calendario
############
print("calendario")
source("calendario/calendario_functions.R")
calendario_completo <- get_calendars(param_calendari)
calendario <- calendario_completo %>% 
  filter(data %in% periodo) #%>% 
  # group_by(data) %>% 
  # mutate(col_group = row_number()) #%>% 
  # ungroup() %>% 
  # assign_cell()
