# install.packages("rtweet")
library(rtweet)
library(tidyverse)

### search_tweets
tw_covid19 <- search_tweets(q = "covid19",n = 100)
tw_covid19 %>% View()
### get_timeline
usuario = "RafaelNadal"
tl_nadal <- get_timeline(user = usuario, n = 100)
### get_followers
usuario <- "rnadalacademy"
fl_nadalacademy <- get_followers(user = usuario, n = 67500)
fl_nadalacademy %>% View()
### get_friends
fr_nadalacademy <- get_friends(user = usuario, n = 205)
fr_nadalacademy %>% View()
### get_favorites
usuario = "RafaelNadal"
fv_nadal <- get_favorites(usuario, n = 1000)
fv_nadal %>% View()
### users_data
us_data_nadal <- users_data(fv_nadal)
us_data_nadal %>% View()

#### Analisis de datos
### Usuarios que más publican
tw_covid19 %>% glimpse()
tw_covid19_es <- tw_covid19 %>% filter(lang == "es")
tw_covid19_es %>% 
  count(screen_name) %>% 
  arrange(desc(n))

#tidying - ordenar
dat_top5 <- fv_nadal %>% 
  count(screen_name) %>%
  arrange(desc(n)) %>% 
  head(5)
#mapping
p  <- ggplot(data = dat_top5,
             mapping= aes(x= reorder(screen_name,n),
                          y=n))
#geom
p <- p + geom_bar(stat = "identity",
                  fill = "red3")
#labels
p <- p + labs(title  = "Top-5 de usuarios favoritos de Rafa",
              y = "# de 'me gusta'",
              x = "Usuarios de twitter")
#dibujar
p + theme_classic()


### Numero de tweets favoritos por año
install.packages("lubridate")
library(lubridate)
library(tidyverse)
fv_nadal_fecha <- fv_nadal %>%
  mutate(fecha = ymd_hms(created_at)) %>%
  mutate(fecha = format(fecha, "%Y")) %>%
  select(created_at, fecha) %>%
  count(fecha)

p <- ggplot(data = fv_nadal_fecha,
            mapping = aes(x = fecha,
                          y = n,
                          group = 1))

p <- p + geom_line()

p <- p + labs(title = "Favoritos por año",
              x = "Años",
              y = "# de Favoritos",
              subtitle = "Rafael Nadal en Twitter")

p
### Hashtags más comúnes
tw_futbol <- search_tweets("fútbol",n=18000,  
                           lang = "es",
                           include_rts = FALSE)
#install.packages("tidyr")
#install.packages("tidytext")
library(tidyr)
library(tidytext)
library(tidyverse)
tw_futbol %>% 
  View()

tw_hashtags_n <- tw_futbol %>% 
  unnest(hashtags) %>%
  filter(!is.na(hashtags)) %>% 
  mutate(hashtags = toupper(hashtags)) %>%
  count(hashtags) %>%
  arrange(desc(n)) %>%
  head(20)
  
p <- ggplot(data = tw_hashtags_n,
            mapping = aes(x = reorder(hashtags,n),
                          y =  n))

p <- p + geom_bar(stat = "identity")
p

p <- p + coord_flip()
p

p <- p + labs(title = "Hashtags más utilizadas",
              x = "Hashtags",
              y = "# de menciones")
p
###
#wordcloud
tw_hashtags_wordcloud <- tw_futbol %>% 
  unnest(hashtags) %>%
  filter(!is.na(hashtags)) %>% 
  mutate(hashtags = toupper(hashtags)) %>%
  count(hashtags) %>%
  arrange(desc(n))

#install.packages("wordcloud") 
library(wordcloud)

wordcloud(words = tw_hashtags_wordcloud$hashtags,
          freq = tw_hashtags_wordcloud$n,
          colors = brewer.pal(9,"RdBu"))

#####
# perfil de los usuarios
library(lubridate)

us_data_nadal_anio <- us_data_nadal %>% 
  distinct(user_id, account_created_at) %>%
  select(account_created_at) %>%
  mutate(anio = account_created_at %>% format("%Y")) %>%
  count(anio) 

p <- ggplot(data = us_data_nadal_anio,
            mapping = aes(x = anio,
                          y = n))
p <- p + 
  geom_bar(stat = "identity") +
  labs(title = "Cuentas creadas por año",
       x = "Año",
       y = "# de cuentas creadas")
p

### AMigos y seguidores
library(rtweet)
usuario <- "rogerfederer"
fr_federer <- get_friends(user = usuario, n = 100)

install.packages("VennDiagram")
library(VennDiagram)

venn.diagram(
  x = list(nadal=fr_nadalacademy$user_id,
           federer=fr_federer$user_id),
  filename = "amigos_fed_nad.png",
  fill = c("red3", "orange"),
  alpha = 0.50,
  cex = 2.5)

### criando redes

tw_futbol %>% 
  unnest(hashtags) %>%
  filter(!is.na(hashtags)) %>%
  select(screen_name, hashtags) %>%
  rename(Source=screen_name, Target=hashtags)%>% 
  write.csv("red_usuario_hashtag.csv",
            row.names=FALSE)


tw_futbol %>% 
  filter(!is.na(reply_to_screen_name))%>%
  select(reply_to_screen_name,screen_name) %>%
  rename(Source=reply_to_screen_name,
         Target=screen_name)%>%
  write.csv("red_usuario_usuario.csv",
            row.names=FALSE)

