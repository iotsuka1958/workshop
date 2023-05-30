## bayestestRパッケージを利用してSimpson's Pardoxのダミーデータを作成
###############################################################
library(bayestestR)
library(tidyverse)
library(ggthemes)
######################
set.seed(2)
data_temp <- simulate_simpson(n = 100, groups = 4, r = 0.5, group_prefix = "school_") |> 
  mutate(V2 = (V2 + 9) * 10, V1 = .5 * V1 + 1) |> 
  rename("study_time" = V1, "grade" = V2, "school" = Group) |> 
  mutate(study_time = study_time*.7)
#####################
ggplot(data_temp, aes(x = study_time, y = grade)) +
  geom_point(aes(color = school)) +
  geom_smooth(aes(color = school), method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "学習時間(h)", y = "正答率")
#####################
piyopiyo <- function(){
  ggplot(data_temp, aes(x = study_time, y = grade)) +
    geom_point(alpha = .7) +
    labs(x = "学習時間(h)", y = "正答率")
}
#####################
piyopiyo() +
  geom_smooth(method = "lm", se = FALSE)
#####################
piyopiyo() +
  aes(colour = school)
#####################
piyopiyo() +
  aes(colour = school) +
  scale_color_few(guide = "none") +
  facet_wrap(~school)
#####################
piyopiyo() +
  aes(colour = school) +
  scale_color_few(guide = "none") +
  facet_wrap(~school) +
  geom_smooth(method = "lm", se = FALSE)
#####################
piyopiyo() +
  aes(colour = school) +
  scale_color_few(guide = "none") +
  geom_smooth(method = "lm", se = FALSE)

###########################################
# 以下はデータirisをベースにSimpson's paradoxのダミーデータを作成したコード
###########################################
df_base <- iris %>%
  janitor::clean_names() %>%
  as_tibble() %>% 
  filter(species == "versicolor") %>% 
  rename("time" = sepal_length,
         "point" = petal_length,
         "school" = species)

df_D <- df_base %>% 
  mutate(time = time - 3, point = point + .4, school = "school_D")

df_C <- df_D %>% 
  mutate(time = time -.25, point = point + .9, school = "school_C")

df_B <- df_C %>% 
  mutate(time = time -.25, point = point + .9, school = "school_B")

df_A <- df_B %>% 
  mutate(time = time -.25, point = point + .9, school = "school_A")


q <- df_A %>% bind_rows(df_B, df_C, df_D) %>% 
  ggplot(aes(time, point * 10)) +
  geom_jitter(width = .8, height = .8, alpha = .7, size = 2) +
#  geom_point(alpha = .75, size = 2) +
  labs(x = "time spent studying", 
       y = "score")

set.seed(1)
q

set.seed(1)
q +
  geom_smooth(method = "lm", se = FALSE)

set.seed(1)
q +
  aes(colour = school) +
  #  scale_color_viridis_d(name = "") 
  ggthemes::scale_colour_few(guide = "none")

set.seed(1)
q +
  aes(colour = school) +
  scale_color_few(guide = "none") +
  facet_wrap(~school)

set.seed(1)
q +
  aes(colour = school) +
  scale_color_few(guide = "none") +
  facet_wrap(~school) +
  geom_smooth(method = "lm", se = FALSE)

set.seed(1)
q +
  geom_smooth(method = "lm", se = FALSE)


