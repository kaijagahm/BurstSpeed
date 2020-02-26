load("data/outputs/allmorpho.Rda")
library(tidyverse)

allmorpho$clutch <- as.character(allmorpho$clutch)

allmorpho <- allmorpho %>%
  mutate(indiv = case_when(treat == "WILD" ~ clutch,
                           TRUE ~ indiv)) 
allmorpho$clutch[allmorpho$treat == "WILD"] <- NA

allmorpho %>% filter(!is.na(treat)) %>% ggplot(aes(x = clutch, y = mass/gs, col = treat))+
  geom_boxplot()+
  facet_wrap(~pond)



test <- allmorpho %>% 
  filter(treat %in% c("HIGH", "LOW")) %>% 
  group_by(pond, clutch) %>% 
  summarize(sizediff = size[1] - size[2],
            massdiff = mass[1] - mass[2])
test %>% ggplot(aes(x = clutch, y = massdiff))+
  geom_point()+
  facet_wrap(~pond)
