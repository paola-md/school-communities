#=============================
# AUXILIARY FUNCTIONS
#=============================

#-----------------------------
# Load functions
#-----------------------------
instalar <- function(paquete) {
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    #install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

paquetes <- c('igraph', "tidyverse", "ggraph", "tidygraph", "GeoRange")


lapply(paquetes, instalar);
theme_set(theme_minimal())

#-----------------------------
# Build graph
#-----------------------------
get_relations <- function(df){
  u_cct_o <-df %>% distinct(cct_o, .keep_all = TRUE) %>% 
    dplyr::select(cct_o, latitud_o, longitud_o) %>% 
    rename(lat = latitud_o, lon = longitud_o, name = cct_o)
  
  u_cct_d <- df %>% distinct(cct_d, .keep_all = TRUE) %>% 
    dplyr::select(cct_d, latitud_d, longitud_d) %>% 
    rename(lat = latitud_d, lon = longitud_d, name = cct_d)
  
  nodos <- rbind(u_cct_d, u_cct_o) %>% 
    distinct(name, .keep_all = TRUE)
  
  relations <- df %>%
    filter(distancia<30) %>% 
    filter(numDest >1) %>% 
    mutate(proporcion = numDest/numSalen) %>%  # Tipo page-rank. numSalen 
    mutate(cct_d_u = ifelse(cct_d < cct_o, cct_d, cct_o)) %>%
    mutate(cct_o_u = ifelse(cct_d < cct_o, cct_o, cct_d)) %>%
    group_by(cct_d_u, cct_o_u) %>%
    dplyr::summarise(total_cambian = sum(numSalen),
                    flujo = sum(numDest),
                    weight = flujo /total_cambian) %>%
    ungroup() %>% 
    rename(cct_d = cct_d_u, cct_o = cct_o_u) %>% 
    rename(name = cct_d) %>% 
    left_join(nodos, by= c("name")) %>% 
    rename( cct_d =name , latitud_d = lat, longitud_d = lon) %>% 
    rename(name = cct_o) %>% 
    left_join(nodos, by= c("name")) %>% 
    rename( cct_o =name , latitud_o = lat, longitud_o = lon)
  
  return(relations)
}

get_nodos <- function(select_relations){
  # Crear nodos 
  u_cct_o <-select_relations%>% distinct(cct_o, .keep_all = TRUE) %>% 
    dplyr::select(cct_o, latitud_o, longitud_o) %>% 
    rename(lat = latitud_o, lon = longitud_o, name = cct_o)
  
  u_cct_d <- select_relations %>% distinct(cct_d, .keep_all = TRUE) %>% 
    dplyr::select(cct_d, latitud_d, longitud_d) %>% 
    rename(lat = latitud_d, lon = longitud_d, name = cct_d)
  
  select_nodos<- rbind(u_cct_d, u_cct_o) %>% distinct(name, .keep_all = TRUE)
  
  return(select_nodos)
}


get_select_nodos <- function(select_relations,current_group){
  # Crear nodos 
  u_cct_o <-select_relations%>% distinct(cct_o, .keep_all = TRUE) %>% 
    dplyr::select(cct_o, latitud_o, longitud_o) %>% 
    rename(lat = latitud_o, lon = longitud_o, name = cct_o)
  
  u_cct_d <- select_relations %>% distinct(cct_d, .keep_all = TRUE) %>% 
    dplyr::select(cct_d, latitud_d, longitud_d) %>% 
    rename(lat = latitud_d, lon = longitud_d, name = cct_d)
  
  select_nodos<- rbind(u_cct_d, u_cct_o) %>% distinct(name, .keep_all = TRUE)
  
  select_nodos$grupo <- current_group 
  return(select_nodos)
}



get_select_relations <- function(nodos, current_group){
  select_nodos <- nodos %>% filter(grupo == current_group)
  
  # Encontrar las relaciones entre los nodos del mismo grupo
  select_relations_o <- relations %>% rename(name = cct_o) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_o = name)
  
  select_relations_od <- select_relations_o  %>% rename(name = cct_d) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_d = name) 
  
  select_relations_d <- relations  %>% rename(name = cct_d) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_d = name)
  
  select_relations_do <- select_relations_d  %>% rename(name = cct_o) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_o = name) 
  
  select_relations <- rbind(select_relations_do, select_relations_od) %>% 
    distinct(cct_o, cct_d, .keep_all = TRUE)
  
  return(select_relations)
}


get_select_relations_com <- function(relations, select_nodos){

  # Encontrar las relaciones entre los nodos del mismo grupo
  select_relations_o <- relations %>% rename(name = cct_o) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_o = name)
  
  select_relations_od <- select_relations_o  %>% rename(name = cct_d) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_d = name) 
  
  select_relations_d <- relations  %>% rename(name = cct_d) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_d = name)
  
  select_relations_do <- select_relations_d  %>% rename(name = cct_o) %>% 
    inner_join(select_nodos, by = c("name")) %>% 
    rename(cct_o = name) 
  
  select_relations <- rbind(select_relations_do, select_relations_od) %>% 
    distinct(cct_o, cct_d, .keep_all = TRUE)
  
  return(select_relations)
}

get_select_nodos <- function(select_relations,current_group){
  # Crear nodos 
  u_cct_o <-select_relations%>% distinct(cct_o, .keep_all = TRUE) %>% 
    dplyr::select(cct_o, latitud_o, longitud_o) %>% 
    rename(lat = latitud_o, lon = longitud_o, name = cct_o)
  
  u_cct_d <- select_relations %>% distinct(cct_d, .keep_all = TRUE) %>% 
    dplyr::select(cct_d, latitud_d, longitud_d) %>% 
    rename(lat = latitud_d, lon = longitud_d, name = cct_d)
  
  select_nodos<- rbind(u_cct_d, u_cct_o) %>% distinct(name, .keep_all = TRUE)
  
  select_nodos$grupo <- current_group 
  return(select_nodos)
}



#-----------------------------
# Community Functions
#-----------------------------
get_community_stats <- function(fc){
  sizes_fc <- sizes(fc)
  stats <- data.frame(
    algoritm =   algorithm(fc),
    modularity = modularity(fc),
    num_groups =   length(fc) ,
    mean_size = mean(sizes_fc),
    median_size = median(sizes_fc),
    max_size = max(sizes_fc),
    min_size= min(sizes_fc)
  )
  return(stats)
}

save_map <- function(select_nodos,algorithm, current_group,num_plot=10){
  top_groups <- select_nodos %>% 
    group_by(sub_grupo) %>% 
    summarise(
      n = n()
    ) %>% arrange(desc(n)) %>% 
    top_n(num_plot) %>% 
    dplyr::select(sub_grupo)
  
  plot_group <- select_nodos %>% 
    filter(!is.na(lat)) %>% 
    inner_join(top_groups)
  
  g <- qmplot(lon, lat, data = plot_group,
              colour = as.factor(sub_grupo), 
              size = I(1), 
              mapcolor = "bw")
  
  file_name <- str_c("./../../results/school_clusters/maps/",
                     algorithm, "_group_",current_group,".png")
  ggsave(filename = file_name, g)
}

save_subgroups <- function(fc, select_nodos,algorithm,current_group){
  fccommunity<- membership(fc)
  
  select_nodos$sub_grupo <- 0
  tam <- nrow(select_nodos)
  for (i in 1:tam){
    escuela <- select_nodos$name[i] 
    select_nodos$sub_grupo[i] <- fccommunity[escuela][[1]]
  }
  
  file_name <- str_c("./../../results/school_clusters/groups/select_nodos/",
                     algorithm, "_group_",current_group,".csv")
  sub_df <- select_nodos %>% 
    dplyr::select(name, grupo, sub_grupo)
  write_csv(sub_df,file_name)
  return(select_nodos)
}



#-----------------------------
# Subgroup stats
#-----------------------------
get_stats_sub_group <- function(current_sub_group, select_nodos){
  sub_group<- select_nodos %>% filter(sub_grupo == current_sub_group)
  
  distance_matrix <- dist(sub_group%>% as.matrix(),
                          method = 'euclidean')%>% as.matrix()
  
  distance_matrix[lower.tri(distance_matrix , diag = TRUE)] <-NA
  
  #Area envolvente convexa
  convex_hull <- CHullAreaEarth(sub_group$lon,  sub_group$lat)
  
  # Estadisticas de la escuela
  num_schools <- nrow(sub_group)
  sub_group <- sub_group %>% 
    mutate(
      privada = ifelse(substr(name, 3,3) == "P", 1, 0)
    )
  
  porcentaje_priv <- sum(sub_group$privada)/num_schools
  
  resp <- data.frame(group =current_sub_group,
                     mean_dist = mean(distance_matrix, na.rm =TRUE), 
                     max_dist =  max(distance_matrix,na.rm =TRUE),
                     min_dist =   min(distance_matrix ,na.rm =TRUE), 
                     median_dist = median(distance_matrix,na.rm =TRUE),
                     convex_hull =  convex_hull,
                     num_elem_subgroup = num_schools,
                     priv =  porcentaje_priv)
  
  return(resp)
}

#-----------------------------
# Subgroup stats
#-----------------------------
get_stats_group <- function(select_nodos, algorithm, current_group) {
  sub_results <- data.frame(group = -1,
                            mean_dist = -1,
                            max_dist = -1,
                            min_dist = -1,
                            median_dist = -1,
                            convex_hull  = -1,
                            num_elem_subgroup = -1,
                            priv = -1)
  
  list_groups <- unique(select_nodos$sub_grupo) %>% sort()
  tam <- length(list_groups) 
  for (i in 1:tam){
    curr_group <- list_groups[[i]] 
    r <- get_stats_sub_group(curr_group,select_nodos)
    sub_results <- rbind(sub_results, r)
  }
  
  sub_results <- sub_results %>% filter(priv != -1)
  file_name <- str_c("./../../results/school_clusters/groups/group_stats/",
                     algorithm, "_group_",current_group,".csv")
  
  write.csv(round(sub_results,2),file_name)
  
}

#-----------------------------
# Network stats
#-----------------------------

get_centrality_stats <- function(school_network,current_group){
  # https://rdrr.io/cran/tidygraph/man/centrality.html
  school_network  <- school_network %>%  as_tbl_graph %>% 
    tidygraph::activate(nodes) %>% 
    mutate(
      alpha = centrality_alpha(weights = weight),
      authority = centrality_authority(weights = weight),
      betweenness = centrality_betweenness(weights = weight),
      eigen = centrality_eigen(weights = weight),
      hub = centrality_hub(weights = weight),
      pagerank = centrality_pagerank(weights = weight),
      subgraph = centrality_subgraph(),
      degree = centrality_degree()
    )
  
  resumen_central <- school_network %>% as_tibble()
  file_name <- str_c("./../../results/school_clusters/groups/centrality_stats/",
                     "group_",current_group,".csv")
  
  write.csv(resumen_central,file_name)
  
  
}

#-----------------------------
# Comparison algoritms
#-----------------------------

compare_clustering_algorithms <- function(fc, select_nodos,current_group){
  algorithm <- str_replace(algorithm(fc), " ", "_")
  select_nodos <- save_subgroups(fc, select_nodos,
                                 algorithm, current_group)
  
  #save_map(select_nodos,algorithm, current_group)
  get_stats_group(select_nodos, algorithm, current_group)
  return(get_community_stats(fc)) 
}


get_new_nodes <- function(algorithm, current_group){
  algorithm <- str_replace(algorithm, " ", "_")
  file_name <- str_c("./../../results/school_clusters/groups/select_nodos/",
                     algorithm, "_group_",current_group,".csv")
  new_nodos <- read.csv(file_name)
  return(new_nodos)
}