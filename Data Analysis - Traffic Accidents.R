# Installing required packages
install.packages("tidyverse") 
install.packages("ggplot2")   
install.packages("readxl")
install.packages("openxlsx")
install.packages("forcats")
install.packages("maps")
install.packages("geobr")
install.packages("kableExtra")


# Loading the readxl package
library(readxl)
# Reading the Excel file
data <- read_excel("C:/Users/Rafael - Notebook/Desktop/Arquivos/Trabalhos ceub/Trabalho R/acidentes_rodovias_2022.xlsx")





head(data)          # Displays the first few rows of the dataset
summary(data)       # Provides a statistical summary of the variables
str(data)          # Shows information about the columns



#Loading the dplyr package
library(dplyr)


##### Number of Accidents by Weekday Plot

# Calculate the total number of accidents by weekday
counts <- data %>%
  group_by(dia_semana) %>%
  summarize(acidentes = n())

# Define the order of weekdays
dias <- c("domingo", "segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado")
counts$dia_semana <- factor(counts$dia_semana, levels = dias)

# Load the ggplot2 library
library(ggplot2)

# Define the color palette
cores <- c("#6368ff", "#6c78ff", "#747eff", "#7d85ff", "#858cff", "#8d92ff", "#9599ff")

# Create the bar plot
ggplot(counts, aes(x = dia_semana, y = acidentes)) +
  geom_bar(stat = "identity", fill = cores) +
  geom_text(aes(label = acidentes), vjust = -0.5, size = 3, color = "black") +
  labs(x = "Dia da Semana", y = "Quantidade de Acidentes", title = "Quantidade de Acidentes por Dia da Semana") +
  theme_minimal()

#####




##### Graph of the main causes of accidents

# Transform the three types of data into one
data$causa_acidente[data$causa_acidente %in% c("Reação tardia ou ineficiente do condutor", "Ausência de reação do condutor", "Desatenção ao acessar uma via")] <- "Falta de atenção/reação à condução"

# Calculate the total number of accidents for each cause
causas <- table(data$causa_acidente)

# Sort the causes of accidents in descending order
causas <- sort(causas, decreasing = TRUE)

# Select the top 7 causes of accidents
top_causas <- head(causas, 5)

# Create a data frame with the top 7 causes and their totals
df_top <- data.frame(causa = names(top_causas), total = as.numeric(top_causas))

# Reorder the columns according to the total values in descending order
df_top$causa <- factor(df_top$causa, levels = df_top$causa[order(df_top$total, decreasing = TRUE)])

# Create the bar chart
graficoTop <- ggplot(df_top, aes(x = causa, y = total)) +
  geom_bar(stat = "identity", fill = "#4B0082", width = 0.7, position = position_dodge(width = 0.8)) +
  labs(x = "Causa de Acidente", y = "Total de Acidentes", title = "Principais causas de acidentes") +
  theme_minimal()

# Add data labels to the bars
graficoTop <- graficoTop + geom_text(aes(label = total), vjust = -0.5)

# Display the chart
print(graficoTop)

#####




##### Graph of accidents by alcohol consumption

# Filter data by alcohol consumption
dados_alcool <- data %>%
  filter(causa_acidente == "Ingestão de álcool pelo condutor")

# Group data by day of the week and calculate totals for accidents, injuries, serious injuries and deaths
totais_dia_semana <- dados_alcool %>%
  group_by(dia_semana) %>%
  summarise(Acidentes = n(),
            Feridos = sum(feridos),
            `Feridos graves` = sum(feridos_graves),
            Mortos = sum(mortos))

# Sort the days of the week in the correct order
totais_dia_semana$dia_semana <- factor(totais_dia_semana$dia_semana,
                                       levels = c("segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado", "domingo"))

# Create the line chart
grafico_linhas <- ggplot(totais_dia_semana, aes(x = dia_semana, group = 1)) +
  geom_line(aes(y = Acidentes, color = "Acidentes"), size = 1) +
  geom_line(aes(y = Feridos, color = "Feridos"), size = 1) +
  geom_line(aes(y = `Feridos graves`, color = "Feridos graves"), size = 1) +
  geom_line(aes(y = Mortos, color = "Mortos"), size = 1) +
  geom_text(aes(y = Acidentes, label = Acidentes), vjust = -0.5, color = "black", size = 3) +
  geom_text(aes(y = Feridos, label = Feridos), vjust = -0.5, color = "black", size = 3) +
  geom_text(aes(y = `Feridos graves`, label = `Feridos graves`), vjust = -0.5, color = "black", size = 3) +
  geom_text(aes(y = Mortos, label = Mortos), vjust = 1, color = "black", size = 3) +
  scale_color_manual(values = c("Acidentes" = "blue", "Feridos" = "orange", "Feridos graves" = "purple", "Mortos" = "red")) +
  labs(x = "Dia da Semana", y = "Número", title = "Acidentes por ingestão de álcool") +
  theme_minimal() +
  theme(plot.title=element_text(size=14),
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10)) +
  guides(color=guide_legend(title=NULL))

# Display the chart
print(grafico_linhas)

#####





##### Graph of accidents by time of day

# Define colors for each time of day
cores <- c("#FFDAB9","#D8BFD8", "#000080", "#FFFF00")

# Calculate the total number of accidents for each time of day
total_fases <- table(data$fase_dia)

# Create a data frame with the times of day and their totals
df_fases <- data.frame(fase = names(total_fases), total = as.numeric(total_fases))

# Sort the times of day in descending order
df_fases <- df_fases[order(df_fases$total, decreasing = TRUE), ]

# Create the pie chart
grafico_pizza <- ggplot(df_fases, aes(x = "", y = total, fill = fase)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  labs(fill = "Fase do Dia", x = NULL, y = NULL, title = "Acidentes por Fase do Dia") +
  theme_void() +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values=cores)

# Add data labels to the pie chart
grafico_pizza <- grafico_pizza +
  geom_text(aes(label=total, x=1.25, y=cumsum(total)-0.5*total), size=4, color="black")

# Display the chart
print(grafico_pizza)




##### Top Causes of Fatal Accidents Plot

# Group data by cause of accident and calculate the total number of deaths by cause
accidents_deaths <- data %>%
  group_by(causa_acidente) %>%
  summarize(total_mortes = sum(mortos))

# Select the top 5 causes of accidents that resulted in deaths
top_5_causes <- accidents_deaths %>%
  top_n(5, total_mortes)

# Reorder columns according to total values in descending order
top_5_causes$causa_acidente <- factor(top_5_causes$causa_acidente, levels = top_5_causes$causa_acidente[order(top_5_causes$total_mortes, decreasing = TRUE)])

# Create the bar chart
graficoTop <- ggplot(top_5_causes, aes(x = causa_acidente, y = total_mortes)) +
  geom_bar(stat = "identity", fill = "#E74C3C", width = 0.7) +
  geom_text(aes(label = total_mortes), vjust = -0.3) +
  xlab("Causa do Acidente") +
  ylab("Total de Mortes") +
  ggtitle("Principais Causas de Acidentes que Geraram Mortes") +
  theme_minimal()

# Display the chart
print(graficoTop)





##### Graph of number of accidents by state

# Load the geobr package
library(geobr)

# Get geographic data of Brazil
brasil <- read_country(year = 2020)

# Get geographic data of Brazil's regions
regioes <- read_region(year = 2020)

# Calculate accident rate by region
taxas <- data %>%
  group_by(regional) %>%
  summarize(taxa_acidentes = n(),
            longitude = mean(longitude, na.rm = TRUE),
            latitude = mean(latitude, na.rm = TRUE))

# Create the graph
ggplot() +
  geom_sf(data = brasil) +
  geom_sf(data = regioes, fill = NA, color = "black") +
  geom_point(data = taxas, aes(x = longitude, y = latitude, size = taxa_acidentes, fill = taxa_acidentes), alpha = 0.5, shape = 21) +
  scale_fill_gradient(low = "green", high = "red", guide = guide_colorbar(reverse = TRUE)) +
  guides(size = "none") +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Número de Acidentes por Estado") +
  labs(fill = "Número de Acidentes") +
  theme(legend.position = "right")



#####



##### Graph of number of deaths by state

# Get geographic data of Brazil
brasil <- read_country(year = 2020)

# Get geographic data of Brazil's regions
regioes <- read_region(year = 2020)

# Calculate death rate by region
taxas <- data %>%
  group_by(regional) %>%
  summarize(total_mortes = sum(mortos),
            longitude = mean(longitude, na.rm = TRUE),
            latitude = mean(latitude, na.rm = TRUE))

# Create the graph
ggplot() +
  geom_sf(data = brasil) +
  geom_sf(data = regioes, fill = NA, color = "black") +
  geom_point(data = taxas, aes(x = longitude, y = latitude, size = total_mortes, color = total_mortes), alpha = 0.5) +
  scale_color_gradient(low = "green", high = "red", name = "Número de Mortes") +
  scale_size(range = c(1, 5), trans = "sqrt", guide = FALSE) +
  xlab("Longitude") +
  ylab("Latitude") +
  ggtitle("Número de Mortes por Estado")

#####



##### Graph of types of accidents

# Load the tidyverse package
library(tidyverse)

# Select relevant data
dados_relevantes <- data %>%
  select(condicao_metereologica, tipo_acidente)

# Group data by meteorological condition and type of accident
dados_agrupados <- dados_relevantes %>%
  group_by(condicao_metereologica, tipo_acidente) %>%
  summarise(contagem = n())

# Define custom color palette
cores <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999", "#FF00FF", "#00FFFF", "#800080", "#008000", "#FF0000", "#0000FF", "#808080")
ggplot(dados_agrupados, aes(x = condicao_metereologica, y = contagem, fill = tipo_acidente)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cores) +  # Apply custom color palette
  labs(x = "Condição Meteorológica", y = "Contagem", fill = "Tipo de Acidente") +
  theme_minimal()

#####





##### Graph of accidents by month of the year

#load the lubridate package
library(lubridate)

# Select relevant data and create a new column for the month
dados_relevantes <- data %>%
  mutate(mes = month(data_inversa, label = TRUE, abbr = TRUE)) %>%
  group_by(mes) %>%
  summarise(acidentes = n())

# Create the graph
ggplot(dados_relevantes, aes(x = mes, y = acidentes, fill = mes)) +
  geom_col() +
  labs(x = "Mês do Ano", y = "Quantidade de Acidentes", title = "Quantidade de Acidentes por Mês do Ano") +
  geom_text(aes(label = acidentes), position = position_stack(vjust = 0.5), color = "white", size = 3) +
  theme_minimal()

#####





##### Graph of states and death rate

# Calculate the total number of accidents and death rate per accident for each state
dados_estados <- data %>%
  group_by(uf) %>%
  summarise(acidentes = n(),
            taxa_morte = sum(mortos) / n())

# Sort the data by the number of accidents in descending order
dados_estados <- dados_estados %>%
  arrange(desc(acidentes))

# Select the top 5 states with the most accidents
top_estados <- head(dados_estados, 5)

# Create the bar graph
ggplot(top_estados, aes(x = reorder(uf, taxa_morte), y = taxa_morte, fill = uf)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Estado", y = "Taxa de morte por acidente (%)", title = "Os 5 Estados com mais acidentes e maior taxa de morte") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip() +
  geom_text(aes(label = paste(round(taxa_morte * 100, 2), "%\n", acidentes, " acidentes")), position = position_stack(vjust = 0.5), color = "black", size = 4)

#####





##### Total table

# Load the knitr and kableExtra packages
library(knitr)
library(kableExtra)

# Calculate the totals of accidents, injured, seriously injured and deaths
total <- data %>%
  summarise(acidentes = n(),
            feridos = sum(feridos),
            feridos_graves = sum(feridos_graves),
            mortes = sum(mortos))

# Create the formatted table
tabela <- kable(total, format = "html", col.names = c("Total de Acidentes", "Total de Feridos", "Total de Feridos Graves", "Total de Mortes")) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Total de Ocorrências" = 4))

# Display the table
tabela

#####




##### Statistical table of state data

# Group data by state and calculate totals for accidents, injuries, serious injuries, and deaths
dados_agrupados <- data %>%
  group_by(uf) %>%
  summarise(acidentes = n(),
            feridos = sum(feridos),
            feridos_graves = sum(feridos_graves),
            mortes = sum(mortos)) %>%
  ungroup()

# Sort data by number of accidents in descending order
dados_ordenados <- dados_agrupados %>%
  arrange(desc(acidentes))

# Select the top 10 states with the highest number of accidents
top_10_estados <- dados_ordenados %>%
  head(10)

# Create a formatted table
tabela2 <- kable(top_10_estados, format = "html", col.names = c("Estado", "Total de Acidentes", "Total de Feridos", "Total de Feridos Graves", "Total de Mortes")) %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c("Top 10 Estados" = 5)) %>%
  row_spec(0, bold = TRUE, background = "#F2F2F2") %>%
  row_spec(1:10, bold = FALSE) %>%
  column_spec(1, bold = TRUE)

# Display the table
tabela2

#####



##### Alcohol-related Statistical Table

# Filter the data for alcohol intake
dados_alcool <- data %>%
  filter(causa_acidente == "Ingestão de álcool pelo condutor")

# Calculate the total number of accidents, injuries, serious injuries, and fatalities related to alcohol intake
totais_alcool <- dados_alcool %>%
  summarise(Acidentes = n(),
            Feridos = sum(feridos),
            `Feridos graves` = sum(feridos_graves),
            Mortos = sum(mortos))

# Create the HTML table
tabela3 <- kable(totais_alcool, format = "html") %>%
  kable_styling(full_width = FALSE)

# Display the HTML table
tabela3


