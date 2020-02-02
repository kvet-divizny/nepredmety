library(tidyverse)
library(forcats)

xml_long <- read_file("prezentator_longterm.xml") %>% 
  str_replace_all("; ", ", ")
longterm_csv <- read_csv2(xml_long, col_names = c("Fakulta", "Pracoviste", "Forma", "Typ", "ISkod", "Obdobi", "UCO", "Semskup", "Rozvrh", "Frekvence", "Limit", "Zapis", "Registrace"))
longterm_csv <- mutate(longterm_csv,
                       Obdobi = factor(Obdobi, levels = c("jaro 2016", "podzim 2016", "jaro 2017", "podzim 2017", "jaro 2018", "podzim 2018", "jaro 2019", "podzim 2019")))

rozpad_uco <- longterm_csv %>% separate_rows(UCO, sep = " - ")

empties <- filter(longterm_csv, Zapis == 0)

semiempties <- empties %>% ungroup() %>%
  group_by(ISkod) %>% 
  summarise(n = n(),
            max = max(Zapis),
            prac = first(Pracoviste)) %>% 
  filter(n >= 4)

superempties <- empties %>% ungroup() %>%
  group_by(ISkod) %>% 
  summarise(n = n(),
            max = max(Zapis),
            prac = first(Pracoviste)) %>% 
  filter(n == 8)


dummy_courses <- filter(longterm_csv, ISkod %in% superempties$ISkod)

ggplot(empties, aes(x = Pracoviste)) +
  geom_bar()

ggplot(longterm_csv, aes(x = Obdobi, y = Zapis)) +
  geom_point(position = "jitter")

empties %>% ungroup() %>% group_by(ISkod) %>% 
  filter(n() == 7) %>%
  ungroup() %>% count(Pracoviste) %>% 
  mutate(n = n/7)

empties %>% ungroup() %>%
  filter(Pracoviste == "ÚKS") %>%
  group_by(ISkod) %>% 
  filter(n() == 7) %>%
  select(ISkod) %>% 
  write_csv("wtf.csv")

longterm_csv %>% group_by(Obdobi) %>%
  summarise(n = n(),
                      mean = mean(Zapis))

zapis_mean <- longterm_csv %>% group_by(ISkod) %>%
  summarise(n = n(),
            mean = mean(Zapis))

whee <- zapis_mean %>% filter(n >=4, mean == 0)


# UCO
rozpad_uco %>% group_by(UCO) %>% 
  summarise(n = n(),
            mean = mean(Zapis),
            koef = n() * mean(Zapis)) %>% 
  arrange(desc(koef))
