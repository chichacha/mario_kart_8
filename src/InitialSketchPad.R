
library(tidyverse)
library(rvest)
library(htmltab)

# get datasets from website

url <- "~/Downloads/mario.htm"

db1 <- url %>% htmltab(1, rm_nodata_cols=F) %>% janitor::clean_names() %>% rename(name=1)
db2 <- url %>% htmltab(2,rm_nodata_cols=F) %>% janitor::clean_names() %>% rename(name=1)
db3 <- url %>% htmltab(3,rm_nodata_cols=F) %>% janitor::clean_names() %>% rename(name=1) 
db4 <- url %>% htmltab(4,rm_nodata_cols=F) %>% janitor::clean_names() %>% rename(name=1)

character_db <- db1 %>% mutate_at(2:8, as.numeric) 
kart_db <- db2 %>% mutate_at(-1, as.numeric)
tire_db <- db3 %>% mutate_at(-1, as.numeric)
glider_db <- db4 %>% mutate_at(-1, as.numeric)

char_db <-character_db %>% 
  mutate(name = str_replace(name,"Mario/Luigi","Mario, Baby Luigi")) %>%
  mutate(name = str_split(name,",")) %>%
  unnest(name) %>%
  mutate(name=str_trim(name))

character_db %>% write_csv("data/character_db.csv")
kart_db %>% write_csv("data/kart_db.csv")
tire_db %>% write_csv("data/tire_db.csv")
glider_db %>% write_csv("data/glider_db.csv")
char_db %>% write_csv("data/char_db.csv")


character_db <- read_csv("data/character_db.csv")

library(rvest)
library(tidyverse)
url2 <- "~/Downloads/mario_char.html"
pre_url <- "https://www.mariowiki.com"
tmp <-url2 %>% read_html() %>%
  html_nodes("a")
tmp2 <- url2 %>% read_html() %>%
  html_nodes("img")

mario_char_df <- tibble(
  url = tmp %>% html_attr("href"),
  char_name = tmp %>% html_attr("title"),
  char_alt = tmp2 %>% html_attr("alt"),
  src = tmp2 %>% html_attr("src")
) %>%
  mutate(url = str_c(pre_url,url),
         src = str_c(pre_url,src))

mario_char_df <-mario_char_df %>% select(char_name, url, src) %>% unique()

char_db <- read_csv("data/char_db.csv")
char_db <-char_db %>%
  left_join(mario_char_df %>% select(name=char_name,url,src))

char_db <- char_db %>%
  mutate(src2=str_replace(src,"32px","64px"))


char_mat <-char_db %>% select(1:8)
char_mat <-column_to_rownames(char_mat,"name")

hc_res <- hclust(dist(char_mat), method="average")
plot(hc_res)
grp <-cutree(hc_res,k=10)


tmp <- tibble(
  label=names(grp),
  grp = grp
)

char_db <- char_db %>% 
  left_join(tmp %>% select(name=label,grp))


g <- as_tbl_graph(hc_res)  %>% left_join(tmp)


library(ggraph)
library(tidygraph)

g %>%
  ggraph("dendrogram") +
  geom_edge_bend() +
  geom_node_point(aes(filter=leaf, color=factor(grp))) +
  geom_node_text(aes(label=str_c(" -",label), filter=leaf, color=factor(grp)),
                 angle=-90, hjust=0) +
  expand_limits(y=-1) 


library(magick) 

gl <- create_layout(g, layout="kk")
gl %>% filter(leaf) %>% ggplot(aes(x=x,y=y)) +
  geom_text(aes(label=label),angle=45)


test <-char_db %>% arrange(total) %>%
  group_by(grp) %>% ## how can i make this variable?
  summarise(img_list=list(src2),
            name_list=list(name)) %>%
  mutate(char_img = map2(img_list,name_list,~image_read(.x) %>% 
                           image_background(color="white") %>%
                           image_border(color="white") %>%
                           image_annotate(text =.y,"south") %>% image_append()))

test$char_img %>%
  image_join() %>%
  image_append(stack=T) %>%
  image_background(color="white") %>%
  image_write("output/Character_Grouping.png")


?image_border

