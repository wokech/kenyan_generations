# Kenyan Generations - William Okech

### MAKE SURE THE CORRECT DATA TYPE IS IN USE, INT, NUM, CHAR.....

# Load libraries

# install.packages("readr")
install.packages("patchwork")
library(readr)
library(ggplot2)
library(tidyverse)
library(patchwork)

# Load data
kenyan_pop_2019 <- read_csv("C:/Users/willy/OneDrive/Desktop/kenyan_pop_year.csv")
head(kenyan_pop_2019)

#############################################################

# To create the groupings (if necessary)
# gen <- c('Kenyatta II', 'Kibaki', 'Moi', 'Kenyatta I', 'Pre-Independence')
# gen

# range <- c('> 2012', '2003-2012', '1979-2002', '1963-1978', '< 1963')
# range

# gen_desc <- data.frame(rank = 5:1,
#                        gen = gen,
#                        range = range,
#                        stringsAsFactors = FALSE) %>%
# arrange(rank)

#############################################################

# Select the 3 different data types and add necessary columns

# Male

kenyan_pop_2019_male <- kenyan_pop_2019 %>% select(Age, Male)
kenyan_pop_2019_male$type <- 'male'
kenyan_pop_2019_male$ref_year <- '2019'
k_pop_male <- kenyan_pop_2019_male %>% 
  rename(
    age = Age,
    population = Male,
    type = type
  )

k_pop_male$age <- as.integer(k_pop_male$age)
k_pop_male$population <- as.integer(k_pop_male$population)
k_pop_male$ref_year <- as.integer(k_pop_male$ref_year)
k_pop_male$birth_year <- k_pop_male$ref_year - k_pop_male$age

k_pop_male_gen <- k_pop_male %>%
  mutate (gen = case_when (
    birth_year < 1963 ~ 'Pre-Independence',
    birth_year < 1979 & birth_year >= 1963 ~ 'Kenyatta I',
    birth_year < 2003 & birth_year >= 1979 ~ 'Moi',
    birth_year <= 2012 & birth_year >= 2003 ~ 'Kibaki',
    birth_year > 2012 ~ 'Kenyatta II'),
    rank = case_when (
      birth_year < 1963 ~ '1',
      birth_year < 1979 & birth_year >= 1963 ~ '2',
      birth_year < 2003 & birth_year >= 1979 ~ '3',
      birth_year <= 2012 & birth_year >= 2003 ~ '4',
      birth_year > 2012 ~ '5'))

k_pop_male_gen$rank <- as.integer(k_pop_male_gen$rank)

# Female

kenyan_pop_2019_female <- kenyan_pop_2019 %>% select(Age, Female)
kenyan_pop_2019_female$type <- 'female'
kenyan_pop_2019_female$ref_year <- '2019'
k_pop_female <- kenyan_pop_2019_female %>% 
  rename(
    age = Age,
    population = Female,
    type = type
  )

k_pop_female$age <- as.integer(k_pop_female$age)
k_pop_female$population <- as.integer(k_pop_female$population)
k_pop_female$ref_year <- as.integer(k_pop_female$ref_year)
k_pop_female$birth_year <- k_pop_female$ref_year - k_pop_female$age

k_pop_female_gen <- k_pop_female %>%
  mutate (gen = case_when (
    birth_year < 1963 ~ 'Pre-Independence',
    birth_year < 1979 & birth_year >= 1963 ~ 'Kenyatta I',
    birth_year < 2003 & birth_year >= 1979 ~ 'Moi',
    birth_year <= 2012 & birth_year >= 2003 ~ 'Kibaki',
    birth_year > 2012 ~ 'Kenyatta II'),
    rank = case_when (
      birth_year < 1963 ~ '1',
      birth_year < 1979 & birth_year >= 1963 ~ '2',
      birth_year < 2003 & birth_year >= 1979 ~ '3',
      birth_year <= 2012 & birth_year >= 2003 ~ '4',
      birth_year > 2012 ~ '5'))

k_pop_female_gen$rank <- as.integer(k_pop_female_gen$rank)

# Total 

kenyan_pop_2019_total <- kenyan_pop_2019 %>% select(Age, Total)
kenyan_pop_2019_total$type <- 'total'
kenyan_pop_2019_total$ref_year <- '2019' # reference year = 2019

k_pop_total <- kenyan_pop_2019_total %>% 
  rename(
    age = Age,
    population = Total,
    type = type
  )

k_pop_total$age <- as.integer(k_pop_total$age)
k_pop_total$population <- as.integer(k_pop_total$population)
k_pop_total$ref_year <- as.integer(k_pop_total$ref_year)
k_pop_total$birth_year <- k_pop_total$ref_year - k_pop_total$age

k_pop_total_gen <- k_pop_total %>%
  mutate (gen = case_when (
    birth_year < 1963 ~ 'Pre-Independence',
    birth_year < 1979 & birth_year >= 1963 ~ 'Kenyatta I',
    birth_year < 2003 & birth_year >= 1979 ~ 'Moi',
    birth_year <= 2012 & birth_year >= 2003 ~ 'Kibaki',
    birth_year > 2012 ~ 'Kenyatta II'),
    rank = case_when (
      birth_year < 1963 ~ '1',
      birth_year < 1979 & birth_year >= 1963 ~ '2',
      birth_year < 2003 & birth_year >= 1979 ~ '3',
      birth_year <= 2012 & birth_year >= 2003 ~ '4',
      birth_year > 2012 ~ '5'))

k_pop_total_gen$rank <- as.integer(k_pop_total_gen$rank)

# Plot the graphs

# Population by generation
# Male
p1 <- k_pop_male_gen %>%
  group_by(gen, rank) %>%
  summarize(population = sum(population)) %>%
  mutate(lab = round(population/1000000, 1)) %>%
  arrange(rank, gen) %>%
  ggplot(aes(x = reorder(gen, -rank),
             y = population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  geom_text(aes(label = lab), 
            size = 3.5)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab('') + ylab('') +
  coord_flip()+
  ggthemes::scale_fill_stata() +
  theme_minimal() +
  labs(title = 'Population by Kenyan generation (male)',
       caption = 'SOURCE: KNBS 2019')

# Female

p2 <- k_pop_female_gen %>%
  group_by(gen, rank) %>%
  summarize(population = sum(population)) %>%
  mutate(lab = round(population/1000000, 1)) %>%
  arrange(rank, gen) %>%
  ggplot(aes(x = reorder(gen, -rank),
             y = population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  geom_text(aes(label = lab), 
            size = 3.5)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab('') + ylab('') +
  coord_flip()+
  ggthemes::scale_fill_stata() +
  theme_minimal() +
  labs(title = 'Population by Kenyan generation (female)',
       caption = 'SOURCE: KNBS 2019')

# Total

p3 <- k_pop_total_gen %>%
  group_by(gen, rank) %>%
  summarize(population = sum(population)) %>%
  mutate(lab = round(population/1000000, 1)) %>%
  arrange(rank, gen) %>%
  ggplot(aes(x = reorder(gen, -rank),
             y = population, 
             fill = gen)) +
  geom_col(show.legend = FALSE, 
           alpha = 0.75)  +
  geom_text(aes(label = lab), 
            size = 3.5)+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  xlab('') + ylab('') +
  coord_flip()+
  ggthemes::scale_fill_stata() +
  theme_minimal() +
  labs(title = 'Living population born dur (total)',
       caption = 'SOURCE: KNBS 2019')

# Plot patchwork images
p1 / p2 / p3

# Population by single year of age & generation

# Male
gg1 <- k_pop_male_gen %>% 
  group_by(birth_year, age, gen) %>%
  summarize(tot = sum(population)) %>%
  group_by(gen) %>%
  mutate(tot = max(tot)) %>% #For labels below.
  filter(yob %in% c('1919', '1928', '1946', '1965', 
                    '1981', '1997', '2013'))
View(gg)

gen_pops %>%
  ggplot(aes(x = AGE, 
             y = pop, 
             fill = gen)) +
  geom_vline(xintercept = gg$AGE,
             linetype =2, 
             color = 'gray', 
             size = .25)+
  
  geom_col(show.legend = FALSE, 
           alpha = 0.85,
           width = .7)   +
  annotate(geom="text", 
           x = gg$AGE - 4.5, 
           y = gg$tot + 70000, 
           label = gg$gen,
           size = 3.25) +
  xlab('Age')+ 
  ylab('') +
  theme_minimal() +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank()) +
  ggthemes::scale_fill_stata()+
  scale_x_reverse(breaks = rev(gg$AGE)) +
  labs(title = 'American population by single-year age & generation')







theme_set(theme_classic())

g <- ggplot(k_pop_mft, aes(age, population, fill = type))
g + geom_bar(stat="identity", width = 0.5) + 
  labs(title="", 
       subtitle="", 
       caption="") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



k_pop_total

k_pop_total_plot <- k_pop_total %>% select(birth_year, population)
k_pop_total_plot



k_pop_total_gen %>%
  ggplot(aes(x = age, 
             y = population, 
             fill = gen)) + geom_bar(stat='identity')


k_pop_total_gen_1 <- k_pop_total_gen %>% 
  group_by(gen) %>%
  summarise(population = sum(population)) %>%
  mutate(lab = round(population/1000000, 1))
k_pop_total_gen_1

k_pop_total_gen_ranked <- merge(k_pop_total_gen_1, gen_desc, by='gen')
k_pop_total_gen_ranked

ggplot(k_pop_total_gen_1, aes(x=gen,
             y=population, 
             fill = gen)) + 
geom_bar(stat='identity', show.legend = FALSE, 
           alpha = 0.75) +
geom_text(aes(label = lab), 
            size = 3.5)+
  xlab('') + ylab('') +
  coord_flip() + 
  ggthemes::scale_fill_stata() +
  theme_minimal()


         