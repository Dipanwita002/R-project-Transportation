library(dplyr)
library(tidyr)
library(leaflet)
leaflet(data = city_cu) %>% addTiles() %>%
  addMarkers(~longitude, ~latitude, popup = ~city, label = ~city)
library(geosphere)


city_coords <- city_cu %>% select(longitude, latitude)

distance_matrix <- as.matrix(
  distm(city_coords, fun = distHaversine)
)/1000 #convert metres to kilometres

rownames(distance_matrix) <- city_cu$city
colnames(distance_matrix) <- city_cu$city

################################

install.packages("ompr")
devtools::install_github("dirkschumacher/ompr.roi") #or cran version higher than 0.8.0.9

install.packages("ROI.plugin.glpk")
#######################

library(ompr)
#specify the dimensions of the distance matrix
n <- length(city_cu$id)

#create a distance extraction function
dist_fun <- function(i, j) {
  vapply(seq_along(i), function(k) distance_matrix[i[k], j[k]], numeric(1L))
}

#######################

model <- MILPModel() %>%
  # we create a variable that is 1 iff we travel from city i to j
  add_variable(x[i, j], i = 1:n, j = 1:n, 
               type = "integer", lb = 0, ub = 1) %>%
  
  # a helper variable for the MTZ formulation of the tsp
  add_variable(u[i], i = 1:n, lb = 1, ub = n) %>% 
  
  # minimize travel distance
  set_objective(sum_expr(colwise(dist_fun(i, j)) * x[i, j], i = 1:n, j = 1:n), "min") %>%
  
  # you cannot go to the same city
  set_bounds(x[i, i], ub = 0, i = 1:n) %>%
  
  # leave each city
  add_constraint(sum_expr(x[i, j], j = 1:n) == 1, i = 1:n) %>%
  
  # visit each city
  add_constraint(sum_expr(x[i, j], i = 1:n) == 1, j = 1:n) %>%
  
  # ensure no subtours (arc constraints)
  add_constraint(u[i] >= 2, i = 2:n) %>% 
  add_constraint(u[i] - u[j] + 1 <= (n - 1) * (1 - x[i, j]), i = 2:n, j = 2:n)

model


library(ompr.roi)
library(ROI.plugin.glpk)
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))
result_val <- round(objective_value(result), 2)                     
result_val

####################
solution <- get_solution(result, x[i, j]) %>% 
  filter(value > 0)

paths <- select(solution, i, j) %>% 
  rename(from = i, to = j) %>% 
  mutate(trip_id = row_number()) %>% 
  inner_join(city_cu, by = c("from" = "id"))

paths_leaflet <- paths[1,]
paths_row <- paths[1,]

for (i in 1:n) {
  paths_row <- paths %>% filter(from == paths_row$to[1])
  
  paths_leaflet <- rbind(paths_leaflet, paths_row)
}

leaflet() %>% 
  addTiles() %>%
  addMarkers(data = city_cu, ~longitude, ~latitude, popup = ~city, label = ~city) %>% 
  addPolylines(data = paths_leaflet, ~longitude, ~latitude, weight = 2)
