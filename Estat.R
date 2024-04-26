library(dplyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)

banco <- read.csv("C:\\Users\\lucas\\OneDrive\\Área de Trabalho\\PROJETO ESTAT\\banco_final.csv")
banco$date_aired <- as.Date(banco$date_aired)
banco <- banco %>%
  mutate(ano = format(date_aired, "%Y"))
banco$ano <- as.integer(banco$ano)
banco$decada <- cut(banco$ano, breaks = c(1960, seq(1970, 2020, by = 10), Inf),
                    labels = c("1960", "1970", "1980", "1990", "2000", "2010", "2020"))

banco <- banco %>%
  rename(tipos = format)


estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

graf <- banco %>%
  mutate(decada = case_when(
    decada %>% str_detect("1960") ~ "1960",
    decada %>% str_detect("1970") ~ "1970",
    decada %>% str_detect("1980") ~ "1980",
    decada %>% str_detect("1990") ~ "1990",
    decada %>% str_detect("2000") ~ "2000",
    decada %>% str_detect("2010") ~ "2010",
    decada %>% str_detect("2020") ~ "2020"
  )) %>% mutate(tipos = case_when(
    tipos %>% str_detect("Serie") ~ "Série",
    tipos %>% str_detect("Movie") ~ "Filme",
    tipos %>% str_detect("CrossOver") ~ "CrossOver",
    
  )) %>%
  group_by(decada,tipos) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

porcentagens <- str_c(graf$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(graf$freq, " (", porcentagens, ")"))
ggplot(graf) +
  aes(
    x = fct_reorder(decada, freq, .desc = TRUE), y = freq,
    fill = tipos, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = 0.9),
    vjust = -0.5, hjust = 0.5,
    size = 2
  ) +
  labs(x = "Decadas", y = "Frequência") +
  theme_estat()

ggsave("resultado1.pdf", width = 158, height = 93, units = "mm")