#instalando pacotes
if (!require("dplyr")){
  install.packages("dplyr")
} 

library(dplyr)


##importando base de dados
df <- read.csv("/cloud/project/ICD_EXER/violencia_domestica_2023.csv",header = T, sep = ";")


#analisando o DataFrame
View(df)
head(df)
tail(df)
str(df)
summary(df)
tibble(df)
colnames(df)


##corrigindo data serializada pelo Excel/Csv

#criando uma copia do DataFrame2 para não mexer no original
df2 <- df


# 1. Detecta datas serializadas (como "45096")
is_serial <- grepl("^[0-9]{5}$", df$data_fato)


# 2. Aplica ifelse com valores como strings
df3 <- df2 |>
  mutate(
      data_fato = if_else(
      is_serial,
      as.Date(as.numeric(data_fato), origin = "1899-12-30"),
      as.Date(data_fato, format = "%Y-%m-%d %H:%M:%S")
    )
  )



# Confirmando se deu certo kkkkkk
View(df3)
class(df3$data_fato)
summary(df3)
str(df3)



#EDA(analise exploratoria)

dados_uberl <- df3 |>
  filter(municipio_fato == "UBERLANDIA")

View(dados_uberl)
head(dados_uberl)
colnames(dados_uberl)

str(dados_uberl)
#quantidade de crimes: 'data.frame':	1128 obs. 

summary(dados_uberl$tentado_consumado)
#CONSUMADO   TENTADO 
#1111        17




# 1. Agrupamento e contagem
delito <- dados_uberl |>
  group_by(mes, natureza_delito) |>
  count(name = "Total")

# 2. Pega o valor máximo global da coluna Total
maior_total <- max(delito$Total)

# 3. Filtra a(s) linha(s) correspondente(s) ao maior valor
delito |>
  filter(Total == maior_total)



# 1. Agrupa e conta os delitos por mês
delitos_por_mes <- dados_uberl |>
  group_by(mes, natureza_delito) |>
  count(name = "Total") |> 
  filter(natureza_delito == "AMEACA")

# 2. ordenando os valoes
delitos_por_mes |>
  arrange(desc(Total))


#criando coluna dia_fato
dados_uberl$dia_fato <- weekdays(dados_uberl$data_fato)
dados_uberl

# Filtrar só os crimes do tipo AMEACA e contar por dia_fato
ameaca_por_dia <- dados_uberl |>
  filter(natureza_delito == "AMEACA") |>  
  group_by(dia_fato) |>
  count(name = "Total de delitos") |>
  arrange(match(dia_fato, c(
    "domingo", "segunda-feira", "terça-feira",
    "quarta-feira", "quinta-feira", "sexta-feira", "sábado"
  )))

ameaca_por_dia




