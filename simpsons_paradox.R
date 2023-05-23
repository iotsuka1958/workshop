library(bayestestR)
library(tidyverse)
library(ggthemes)
set.seed(2)
data <- simulate_simpson(n = 100, groups = 4, r = 0.2, group_prefix = "school_") |> 
  mutate(V2 = (V2 + 9) * 10, V1 = .5 * V1 + 1) |> 
  rename("study_time" = V1, "grade" = V2, "school" = Group)

ggplot(data, aes(x = study_time, y = grade)) +
  geom_point(aes(color = school)) +
  geom_smooth(aes(color = school), method = "lm", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "学習時間", y = "正答率")

piyopiyo <- function(){
  ggplot(data, aes(x = study_time, y = grade)) +
    geom_point(alpha = .7) +
    labs(x = "学習時間", y = "正答率")
}

piyopiyo() +
  geom_smooth(method = "lm", se = FALSE)

piyopiyo() +
  aes(colour = school)

piyopiyo() +
  aes(colour = school) +
  scale_color_few(guide = "none") +
  facet_wrap(~school)


piyopiyo() +
  aes(colour = school) +
  scale_color_few(guide = "none") +
  facet_wrap(~school) +
  geom_smooth(method = "lm", se = FALSE)

piyopiyo() +
  aes(colour = school) +
  scale_color_few(guide = "none") +
  geom_smooth(method = "lm", se = FALSE)


