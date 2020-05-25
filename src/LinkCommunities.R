library(tidyverse)
library(linkcomm)


setwd("~/Documents/Maestria/SegundoSemestre/MetodosAnaliticos/ProyectoFinal")
agregado <- read_csv('data/agregado_dist_sec.csv') 

relations <- agregado %>% 
  mutate(cct_d_u = ifelse(cct_d < cct_o, cct_d, cct_o)) %>%
  mutate(cct_o_u = ifelse(cct_d < cct_o, cct_o, cct_d)) %>%
  group_by(cct_d_u, cct_o_u) %>%
  dplyr::summarise(weight = sum(numDest)/sum(numSalen)) %>%
  ungroup() %>% 
  rename(cct_d = cct_d_u, cct_o = cct_o_u) %>% 
  left_join(agregado, by = c("cct_d", "cct_o")) %>% 
  mutate(estado_d = substr(cct_d, 1,2),
         estado_o = substr(cct_o, 1,2)) %>% 
  filter(estado_o == "09" | estado_d == "09") 

relations %>% group_by(estado_o) %>% summarise(num = n()) %>% arrange(desc(num))

h <- data.frame(relations[,1:3])


link_communities_results <- getLinkCommunities(h,hcmethod = "centroid")
link_communities_results




new_comunities<- link_communities_results$nodeclusters

cluster_by_edges<- link_communities_results$edges

new_comunities<-cluster_by_edges %>% 
  mutate(node1 = as.character(node1),
         cluster = as.character(cluster)) %>% 
  rename(node = node1) %>% 
  group_by(node, cluster) %>% 
  summarise(num1 = n()) %>% 
  full_join(cluster_by_edges %>% 
              mutate(node2 = as.character(node2),
                     cluster = as.character(cluster)) %>% 
              rename(node = node2) %>% 
              group_by(node, cluster) %>% 
              summarise(num2 = n())) %>% 
  replace_na(list("num1"=0, "num2"=0)) %>% 
  ungroup() %>% 
  mutate(Total = num1 + num2) %>% 
  group_by(node) %>% 
  filter(Total == max(Total)) %>% 
  select(node,cluster) %>% 
  distinct() 

df_com_prev <- read_csv('data/communities.csv')
df_com <- df_com_prev %>% 
  left_join(new_comunities, by = c("name"="node")) %>% 
  filter(!is.na(cluster)) %>% 
  select(-id_community) %>% 
  rename(id_community = cluster) %>% 
  distinct() %>% 
  mutate(id_community = as.numeric(id_community)) %>% 
  group_by(name) %>%
  filter(id_community == min(id_community)) %>%
  ungroup()


relations <- get_relations(df_relations)

top_groups <- nodos %>% 
  group_by(buff_num) %>% 
  summarise(
    n = n()
  ) %>% arrange(desc(n)) %>% 
  select(buff_num,n)


df_com <- df_com %>%   mutate(
  estado = substr(name, 1,2)
)



nodos <- df_com %>% 
  filter(estado=="09") %>% 
  distinct()

relations <- get_relations(df_relations)
select_relations <- get_select_relations_com(relations, nodos) 
select_nodos <- get_nodos(select_relations)
select_nodos <- select_nodos %>% left_join(df_com, by= c("name"))

school_network <- graph_from_data_frame(select_relations, directed=FALSE, vertices=select_nodos)
is_weighted(school_network)

members <- as.double(select_nodos$id_community)
nam <- as.character(select_nodos$name)
comms <- list(membership=members, vcount = vcount(school_network), 
              name = nam,algorithm="by.hand" )
class(comms)<- "communities"
modularity(school_network, membership(comms))


top_comunities <- df_com %>%
  group_by(id_community) %>% 
  summarise(num = n()) %>% 
  arrange(desc(num)) %>% 
  head(10)

plot_group <- df_com %>% 
  filter(id_community %in% top_comunities$id_community ) %>% 
  filter(estado =="09")

library(ggmap)
qmplot(longitud, latitud, data = plot_group,
       colour = as.factor(id_community), 
       size = I(2),
       mapcolor = "bw")


qmplot(longitud, latitud, data = plot_group,
       colour = as.factor(id_community), 
       size = I(2),
       mapcolor = "bw")


#Modularidad con greedy 
library("igraph")

current_group<-1

fc <- cluster_fast_greedy(school_network)
algorithm <- str_replace(algorithm(fc), " ", "_")
select_nodos <- save_subgroups(fc, select_nodos,
                               algorithm, current_group)

select_nodos$id_community <- fc$membership

members <- as.double(select_nodos$id_community)
nam <- as.character(select_nodos$name)
comms <- list(membership=members, vcount = vcount(school_network), 
              name = nam,algorithm="by.hand" )
class(comms)<- "communities"
modularity(school_network, membership(comms))

#save_map(select_nodos,algorithm, current_group)
get_stats_group(select_nodos, algorithm, current_group)
get_community_stats(fc) 
