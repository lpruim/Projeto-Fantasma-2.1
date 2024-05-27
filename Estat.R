library(dplyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(forcats)
library(tibble)
library(tidyr)
#ANALISE 1

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
  group_by(decada,tipos)
  

graf <- banco %>%
  select(decada, season)
porcentagens <- str_c(graf$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(graf$freq, " (", porcentagens, ")"))


print_quadro_resumo <- function(data, var_name, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  var_name <- substitute(var_name)
  data <- data %>%
    summarize(`Média` = round(mean(!!sym(var_name)),2),
              `Desvio Padrão` = round(sd(!!sym(var_name)),2),
              `Variância` = round(var(!!sym(var_name)),2),
              `Mínimo` = round(min(!!sym(var_name)),2),
              `1º Quartil` = round(quantile(!!sym(var_name), probs = .25),2),
              `Mediana` = round(quantile(!!sym(var_name), probs = .5),2),
              `3º Quartil` = round(quantile(!!sym(var_name), probs = .75),2),
              `Máximo` = round(max(!!sym(var_name)),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l |\n", sep=" ")
  for (i in seq(2, col_count))
  {
    numCount <- data[i, -c(1)] %>%
      as.numeric() %>%
      {floor(log10(.)) + 1} %>%
      max()
    latex <- str_c(latex, "\t\t\tS[table-format = ", numCount ,".2]\n", sep="")
  }
  
  
  latex <- str_c(latex, "\t\t\t|}\n\t\\toprule\n\t\t", sep="")
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}



ggplot(graf) +
  aes(x = decada, y = freq, group = tipos, colour = tipos) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Decadas", y = "Frequência") +
  theme_estat()
ggsave("series_grupo.pdf", width = 158, height = 93, units = "mm")
#ANALISE 2 
mean_ratings %>% 
  group_by(season) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = imdb)

mean_ratings <- banco %>%
  select(season, imdb)
mean_ratings <- mean_ratings %>%
  mutate(season = case_when(
    season %>% str_detect("1") ~ "1",
    season %>% str_detect("2") ~ "2",
    season %>% str_detect("3") ~ "3",
    season %>% str_detect("4") ~ "4"))
mean_ratings <- na.omit(mean_ratings)
ggplot(mean_ratings) +
  aes(x = factor(season, levels = c("1", "2", "3", "4")), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporadas", y = "Nota Imdb") +
  theme_estat()


ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")
#ANALISE 3 

novo_banco <- banco %>%
  mutate(setting_terrain = case_when(
    setting_terrain %>% str_detect("Forest") ~ "Flhoresta",
    setting_terrain %>% str_detect("Rural") ~ "Rural",
    setting_terrain %>% str_detect("Urban") ~ "Urbano"
  )) %>%
  mutate(trap_work_first = case_when(
    trap_work_first %>% str_detect("True") ~ "Verdade",
    trap_work_first %>% str_detect("False") ~ "Falso",
  )) %>%
  group_by(setting_terrain, trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

novo_banco <- na.omit(novo_banco)
novo_banco <- novo_banco %>%
  rename(Funcionou = trap_work_first)

porcentagens <- str_c(novo_banco$freq_relativa, "%") %>% str_replace("\\.", ",")
legendas <- str_squish(str_c(novo_banco$freq, " (", porcentagens, ")"))

ggplot(novo_banco) +
  aes(
    x = fct_reorder(setting_terrain, freq, .desc = T), y = freq,
    fill = Funcionou, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Locais", y = "Frequência") +
  theme_estat()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")

#ANALISE 4




ggplot(banco) +
  aes(x = engagement, y = imdb) +
  geom_point(colour = "#A11D21", size = 3, alpha = 0.5) +
  labs(
    x = "Engajamento",
    y = "Nota IMDM"
  ) +
  theme_estat() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8))

ggsave("disp_1uni.pdf", width = 158, height = 93, units = "mm")

banco %>% 
  print_quadro_resumo(var_name = imdb)

correlção <- cor(banco$imdb, banco$engagement)

#ANALISE 5

banco5 <- banco %>%
  mutate(caught_fred = case_when(
    caught_fred %>% str_detect("True") ~ "Fred")) %>% 
  mutate(caught_daphnie = case_when(
    caught_daphnie %>% str_detect("True") ~ "Daphnie")) %>%
  mutate(caught_velma = case_when(
    caught_velma %>% str_detect("True") ~ "Velma")) %>%
  mutate(caught_shaggy = case_when(
    caught_shaggy %>% str_detect("True") ~ "Salsicha")) %>%
  mutate(caught_scooby = case_when(
    caught_scooby %>% str_detect("True") ~ "Scooby")) %>%
  mutate(caught_other = case_when(
    caught_other %>% str_detect("True") ~ "Outros")) %>%
  mutate(caught_not = case_when(
    caught_not %>% str_detect("True") ~ "Ninguém ")) 

banco5 <- banco5 %>%
  select(engagement, caught_fred, caught_daphnie, caught_scooby, caught_shaggy, caught_velma, caught_not, caught_other)


substituicao <- function(coluna1, coluna2, coluna3, coluna4, coluna5, coluna6, coluna7) {

  coluna1 <- ifelse(is.na(coluna1), "", coluna1)
  coluna2 <- ifelse(is.na(coluna2), "", coluna2)
  coluna3 <- ifelse(is.na(coluna3), "", coluna3)
  coluna4 <- ifelse(is.na(coluna4), "", coluna4)
  coluna5 <- ifelse(is.na(coluna5), "", coluna5)
  coluna6 <- ifelse(is.na(coluna6), "", coluna6)
  coluna7 <- ifelse(is.na(coluna7), "", coluna7)
  
  
  resultado <- paste(coluna1, coluna2, coluna3, coluna4, coluna5, coluna6, coluna7)
  return(resultado)
}


banco5$concatenacao_caught <- substituicao(banco5$caught_fred, banco5$caught_daphnie, banco5$caught_scooby, banco5$caught_shaggy, banco5$caught_velma, banco5$caught_not, banco5$caught_other)

banco5 <- separate_rows(banco5, concatenacao_caught, sep = " ")

banco5 <- banco5 %>% mutate(concatenacao_caught = na_if(concatenacao_caught, ""))

banco5 <- subset(banco5, !is.na(concatenacao_caught))


ggplot(banco5) +
  aes(x = reorder(concatenacao_caught, engagement, FUN = median), y = engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagens", y = "Engajamento") +
  theme_estat()
ggsave("box_bi1.pdf", width = 158, height = 93, units = "mm")
banco5%>% 
  group_by(concatenacao_caught) %>% # caso mais de uma categoria
  print_quadro_resumo(var_name = engagement)
