library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(networkD3)


project_info <- data_frame(dept = c("2013-2014 Cohort", rep(c("PSYC","DRAM","PHYS","ENGR","CHEE","CIVL","ECE","ENPH","GEOE", "MECH", "MINE", "MTHE"),each=4)),
                    project_year = c(1, rep(c(1:4), 12))) %>% 
  mutate(node = row_number()-1)


table <- student_info %>% 
  filter(course != 103) %>% 
  mutate(dept = ifelse(subject == "APSC" & semester %in% c(1,2), "ENGR", ifelse(subject == "APSC" & semester %in% c(3,4,5,6,7,8), str_sub(plan, 1, 4), as.character(subject))),
         project_year = ifelse(semester %in% c(1,2), 1, ifelse(semester %in% c(3,4), 2, ifelse(semester %in% c(5,6), 3, 4)))) %>% 
  filter(consent>1, project_year < 4) %>% 
  mutate(dept = ifelse(dept %in% c("CSCI","CMPE","ELEC"), "ECE", ifelse(dept %in% c("CHEE","ENCH"), "CHEE", ifelse(dept %in% c("MTHE","MATH"), "MTHE", dept)))) %>% 
  group_by(project_year, dept) %>% 
  tally %>% 
  ungroup %>% 
  complete(project_year,nesting(dept), fill = list(n=0)) %>% 
  rbind(c(1,"2013-2014 Cohort",1)) %>% 
  mutate(project_year = as.numeric(project_year)) %>% 
  left_join(project_info,table, by = c("project_year","dept")) %>% 
  filter(n>0, !is.na(n)) %>% 
  mutate(grouping = ifelse(dept %in% c("ENGR","CHEE","CIVL","ECE","ENPH","GEOE", "MECH", "MINE", "MTHE"), "ENGR", dept)) %>% 
  arrange(node) %>% 
  mutate(node = row_number()-1)
 
links <- data_frame(source = c(0,1,2,0,4,0,6,0,8,8,8,8,8,8,8,8),
                    target = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                    value = c(1119,187,92,105,42,150,18,633,98,80,89,65,39,130,46,33)) %>% 
  mutate(grouping = table$grouping[table$node %in% .$target]) %>% 
  as.data.frame()

nodes <- c("2013-2014 Cohort","PSYC 100","PSYC 203","PSYC 301","DRAM 100","DRAM 200","PHYS 107","PHYS 239","APSC 100","APSC 200 CHEE","APSC 200 CIVL","APSC 200 ECE","APSC 200 ENPH","APSC 200 GEOE","APSC 200 MECH","APSC 200 MINE","APSC 200 MTHE") %>% 
  as.data.frame() %>% 
  rename(.,name = `.`) %>% 
  mutate(grouping = ifelse(grepl("APSC", name), "ENGR", ifelse(grepl("Cohort", name), as.character(name), str_sub(name, 1,4))))

# colors <- viridis::viridis(4) %>% 
#   str_sub(1,7) 
# 
# ColourScale <- paste0('d3.scale.ordinal().range([',paste(dQuote(colors), collapse=","), ']);')

ColourScale <- 'd3.scale.ordinal().range(["#440154","#31688E","#35B779","#FDE725"])'

## Using networkd3
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  NodeGroup = NULL,
  LinkGroup = "grouping",
  fontSize = 14,
  nodeWidth = 50,
  colourScale = JS(ColourScale)
)


## Using Riverplot
library(riverplot)


river_nodes <- table %>%
  select(node, project_year) %>%
  bind_cols(data_frame(
    labels = c(
      "2013-2014 Cohort",
      "PSYC 100",
      "PSYC 203",
      "PSYC 301",
      "DRAM 100",
      "DRAM 200",
      "PHYS 107",
      "PHYS 239",
      "APSC 100",
      "APSC 200 CHEE",
      "APSC 200 CIVL",
      "APSC 200 ECE",
      "APSC 200 ENPH",
      "APSC 200 GEOE",
      "APSC 200 MECH",
      "APSC 200 MINE",
      "APSC 200 MTHE"
    )
  )) %>%
  set_names(c("ID", "x", "labels")) %>%
  mutate(
    ID = ID + 1,
    x = ifelse(ID == 1, x, x + 1),
    nodestyle = "regular",
    col = ifelse(
      grepl("PSYC", labels),
      viridis(4)[1],
      ifelse(
        grepl("DRAM", labels),
        viridis(4)[2],
        ifelse(
          grepl("PHYS", labels),
          viridis(4)[3],
          ifelse(grepl("Cohort", labels), "grey80", viridis(4)[4])
        )
      )
    ),
    srt = "0",
    textcol = "white"
  ) %>%
  as.data.frame()


river_links <- data_frame(N1= c(0,1,2,0,4,0,6,0,8,8,8,8,8,8,8,8),
                                  N2 = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16),
                                  Value = c(1119,187,92,105,42,150,18,633,98,80,89,65,39,130,46,33)) %>% 
  mutate_each(funs(.+1),-Value) %>%
  mutate(edgestyle = "sin",
         edgecol = "col",
         lty = 1,
          col = ifelse(N2 %in% c(2:4),
            viridis(4)[1],
            ifelse(N2 %in% c(5:6),
              viridis(4)[2],
              ifelse(
                N2 %in% c(7:8),
                viridis(4)[3],
                ifelse(N2 %in% c(9:17), viridis(4)[4], "grey80"))))) %>% 
  as.data.frame()
  


makeRiver(river_nodes,river_links) -> r

  riverplot(r)

