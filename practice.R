# Доступ к датасетам сайта Our World In Data
library(owidR)
# Библиотека для работы с данными (от получения до визуализации)
library(tidyverse)
# Данные по ВВП и населению
library(gapminder)

# Загружаем данные по санитарным условиям
sanitation_data <- owid('sanitation-facilities-coverage')

# Изучаем данные
str(sanitation_data)

# Преобразовываем данные
sanitation_agg <- sanitation_data %>% 
  pivot_longer(cols = -c(entity, code, year), names_to = 'sanitation_level',
               values_to = 'people') %>%
  separate(sanitation_level, 
           into = c('measure', 'level', 'nation_level', 'quantity_measure'),
           sep = ' - ') %>% 
  select(-measure, -nation_level, -quantity_measure) %>%
  mutate(level = factor(
    level, 
    levels = c('Open defecation', 'Unimproved', 
               'Limited (shared)', 'At least basic')
    )) %>% 
  group_by(entity, year) %>% 
  mutate(people = people / sum(people))

# Визуализируем прогресс по санитарным условиям в Индии
ggplot(data = filter(sanitation_agg, entity == 'India'),
       aes(x = year, y = people, col = level))+
  geom_line()

# Изучим взаимосвязь санитарных условий и ВВП
sanitation_agg %>% 
  filter(level == 'Open defecation') %>% 
  inner_join(
    x = .,
    y = gapminder,
    by = c('entity' = 'country', 'year')
  ) %>% 
  ggplot(aes(x = gdpPercap, y = people, col = continent))+
  geom_point()+
  stat_smooth(method = 'lm')+
  scale_x_log10()

# Загрузим данные по грамотности взрослого населения
males <- owid('adult-literacy-male')
females <- owid('adult-literacy-female')

# Преобразуем и визуализируем данные прямо в одном пайпе
males %>% 
  rename(males = `Literacy rate, adult male (% of males ages 15 and above)`) %>% 
  inner_join(
    x = .,
    y = females %>% 
      rename(females = `Literacy rate, adult female (% of females ages 15 and above)`)
  ) %>% 
  inner_join(
    x = .,
    y = gapminder %>%
      select(entity = country, continent) %>% 
      distinct()
  ) %>% 
  pivot_longer(cols = c(males, females), names_to = 'sex', values_to = 'literacy') %>% 
  ggplot(aes(x = year, y = literacy / 100, col = sex))+
  geom_point(alpha = 0.5)+
  stat_smooth(method = 'lm', se = FALSE, lwd = 2)+
  facet_grid(rows = vars(continent))+
  scale_y_continuous(labels = scales::percent_format())+
  scale_color_manual(values = c('red', 'blue'))+
  theme_bw()+
  theme(legend.position = 'none',
        axis.title.x = element_blank())
