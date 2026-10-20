require(openxlsx)            # Leitura de base de dados
require(dplyr)               # Manipulação de base de dados
require(gtsummary)           # Tabelas automáticas
require(gt)                  # Tabelas automáticas
require(rstatix)             # Coeficiente de Cramer
require(ggplot2)             # Gráficos
require(qqplotr)             # Gráficos qqplot
require(DescTools)           # Teste de Levene
require(multcompView)


Dados <- read.xlsx("Nutrição.xlsx")

# linguagem de pontuação em português
# formatação em português (vírgula pra decimais e ponto para milhares)
theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")

DadosQuanti = Dados %>% 
  select(Idade,Peso,Altura, IMC)

tbl_summary(data = DadosQuanti)

tbl_summary(data = DadosQuanti,
            statistic = all_continuous() ~ "{mean} ({sd})")

Dados1 = Dados %>% 
  select(Idade, Peso, Altura, IMC,Refrigerante)

tbl_summary(data = Dados1,
            by = Refrigerante,
            statistic = all_continuous() ~ "{mean} ({sd})")



#diagnosticos de normalidade

#variavel Idade
ggplot(Dados1, aes(x = Idade)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Refrigerante) +
  xlab("Idade") +
  ylab("Densidade") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(Dados1, aes(sample = Idade)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Refrigerante,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))

by(Dados1$Idade, Dados1$Refrigerante, shapiro.test)

#Nota-se que os dados de Idade nas categorias Aumentou, Diminuiu, Não alterou e Não consumo 
#não são normalmente distribuídos conforme os diagnósticos. #Além disso, os desvios observados 
#seguem um padrão reconhecível proveniente da assimetria da distribuição. 

#variavel Peso
ggplot(Dados1, aes(x = Peso)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Refrigerante) +
  xlab("Peso") +
  ylab("Densidade") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(Dados1, aes(sample = Peso)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Refrigerante,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))

by(Dados1$Peso, Dados1$Refrigerante, shapiro.test)

#Nota-se que os dados de Peso nas categorias Aumentou, Diminuiu, Não alterou e Não consumo 
#não são normalmente distribuídos conforme os diagnósticos. #Além disso, os desvios observados 
#seguem um padrão reconhecível proveniente da assimetria da distribuição. 

#variavel Altura
ggplot(Dados1, aes(x = Altura)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Refrigerante) +
  xlab("Altura") +
  ylab("Densidade") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(Dados1, aes(sample = Altura)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Refrigerante,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))

by(Dados1$Altura, Dados1$Refrigerante, shapiro.test)

#Nota-se que os dados de Altura nas categorias Aumentou, Diminuiu, Não alterou e Não consumo 
#não são normalmente distribuídos conforme os diagnósticos. #Além disso, os desvios observados 
#seguem um padrão reconhecível proveniente da assimetria da distribuição. 


#variavel IMC
ggplot(Dados1, aes(x = IMC)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Refrigerante) +
  xlab("IMC") +
  ylab("Densidade") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(Dados1, aes(sample = IMC)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Refrigerante,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))

by(Dados1$IMC, Dados1$Refrigerante, shapiro.test)


#Nota-se que os dados de IMC nas categorias Aumentou, Diminuiu, Não alterou e Não consumo 
#não são normalmente distribuídos conforme os diagnósticos. #Além disso, os desvios observados 
#seguem um padrão reconhecível proveniente da assimetria da distribuição. 


#Como tivemos problemas de normalidade em todos os cenários, vamos testar a homogeneidade 
#com o teste de levene, pois o teste de Bartlet é sensivel à normalidade

LeveneTest(formula = Idade~Refrigerante,data = Dados1)
#p-valor = 2.016e-09 ***

LeveneTest(formula = Peso~Refrigerante,data = Dados1)
#p-valor = 0.002297 **

LeveneTest(formula = Altura~Refrigerante,data = Dados1)
#p-valor = 0.0324 *

LeveneTest(formula = IMC~Refrigerante,data = Dados1)
#p-valor = 0.000548 ***

#há evidências de heterogeneidade das variâncias para as variáveis todas as variáveis (Idade,
#Peso, Altura e IMC).

#Conforme os diagnósticos anteriores, devemos fazer o teste da ANOVA de Welch para todas as variáveis 
#(Idade, Peso, Altura e IMC).

tbl_summary(
  data = Dados1,
  by = Refrigerante,
  statistic = all_continuous() ~ "{mean} ({sd})"
) |>
  add_p(
    test = all_continuous() ~ "oneway.test",
    test.args = all_continuous() ~ list(var.equal = FALSE)
  )

####
#gerar as letras
gerar_letras <- function(variavel, data, alpha = 0.05) {
  
  gh <- games_howell_test(
    formula = as.formula(paste(variavel, "~ Refrigerante")),
    data = data
  )
  
  comparacoes <- paste(
    gh$group1,
    gh$group2,
    sep = "-"
  )
  
  pvalores <- gh$p.adj
  names(pvalores) <- comparacoes
  
  letras <- multcompLetters(
    pvalores,
    threshold = alpha
  )$Letters
  
  return(letras)
}

letras_idade <- gerar_letras("Idade", Dados1)
letras_peso <- gerar_letras("Peso", Dados1)
letras_altura <- gerar_letras("Altura", Dados1)
letras_imc <- gerar_letras("IMC", Dados1)


tabela2 <- tbl_summary(
  data = Dados1,
  by = Refrigerante,
  statistic = all_continuous() ~ "{mean} ({sd})",
  label = list(
    Idade ~ "Idade<sup>W</sup>",
    Peso ~ "Peso<sup>W</sup>",
    Altura ~ "Altura<sup>W</sup>",
    IMC ~ "IMC<sup>W</sup>"
  )
) %>%
  add_p(
    test = all_continuous() ~ "oneway.test",
    test.args = all_continuous() ~ list(var.equal = FALSE),
    pvalue_fun = label_style_pvalue(digits = 3)
  ) %>%
  bold_p(t = 0.05) %>%
  modify_spanning_header(all_stat_cols() ~ "**Refrigerante**") %>%
  modify_header(label ~ "**Variáveis**<sup><em>3</em></sup>") %>%
  bold_labels() %>%
  modify_header(
    all_stat_cols() ~ "**{level}**<br>N = {n}<sup><em>1</em></sup>"
  ) %>%
  modify_header(
    p.value ~ "**Valor-p**<sup><em>2</em></sup>"
  ) %>%
  modify_footnote(everything() ~ NA) %>%
  modify_source_note(
    "<sup><em>1</em></sup> Média (Desvio Padrão)<br>
   <sup><em>2</em></sup> ANOVA de Welch de um fator (não assumindo variâncias iguais)<br>
   <sup><em>3</em></sup> Grupos que compartilham pelo menos uma letra não diferem significativamente pelo teste de Games-Howell (α = 0,05).",
    text_interpret = "html"
  )

letras_lista <- list(
  Idade = letras_idade,
  Peso = letras_peso,
  Altura = letras_altura,
  IMC = letras_imc
)

grupos <- levels(factor(Dados1$Refrigerante))

for (variavel in names(letras_lista)) {
  
  letras <- letras_lista[[variavel]]
  
  linha <- which(
    tabela2$table_body$variable == variavel &
      tabela2$table_body$row_type == "label"
  )
  
  if (length(linha) == 1) {
    
    for (j in seq_along(grupos)) {
      
      grupo <- grupos[j]
      coluna <- paste0("stat_", j)
      
      letra <- letras[grupo]
      
      if (!is.na(letra)) {
        
        tabela2$table_body[[coluna]][linha] <-
          paste0(
            tabela2$table_body[[coluna]][linha],
            "<sup><strong>", letra, "</strong></sup>"
          )
      }
    }
  }
}

tabela2 <- tabela2 %>%
  fmt_markdown(columns = starts_with("stat_")) %>%
  fmt_markdown(columns = label) %>%
  tab_style(
    style = cell_text(
      v_align = "middle"
    ),
    locations = cells_column_labels(
      columns = p.value
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#fef9c3"),
    locations = cells_body(
      rows = variable %in% c("Idade", "Peso", "IMC") &
        row_type == "label"
    )
  ) %>%
  tab_options(
    table.font.size = "20px",
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )

tabela2

letras_idade
letras_peso
letras_altura
letras_imc



#A análise comparou quatro grupos de Refrigerante (Aumentou, Diminuiu, Não alterou e Não consumo) 
#em relação a variáveis quantitativas, utilizando o teste da ANOVA de Welch (W)
#conforme indicado na tabela. 
#Para cada variável, tem-se as seguintes conclusões:
  
#Observa-se que o grupo Satisfeito apresenta maior idade média (45) em comparação ao grupo Neutro/Insatisfeito (38). O valor-p (0,002) indica diferença estatisticamente significativa, sugerindo que indivíduos mais velhos tendem a apresentar maior satisfação.

#A variável Peso também apresenta diferença significativa entre os grupos (valor-p < 0,001). 
#O grupo Aumentou aumentou o peso, em média, (74) do que o grupo Não consome (67), 
#indicando associação entre maior distância percorrida e maior satisfação.

#Não há evidência de diferença entre os grupos (valor-p = 0,589). As médias são semelhantes, indicando que o atraso na partida não parece estar associado à satisfação dos passageiros.

#Também não se observa diferença estatisticamente significativa (valor-p = 0,292). As médias entre os grupos são próximas, sugerindo ausência de associação entre atraso na chegada e satisfação.


#COMPARAÇÕES MÚLTIPLAS
#Entretanto, percebemos que todas as variáveis apresentaram p-valor significativo. 
#Teremos que realizar o teste de comparação múltipla de

games_howell_test(Idade~Refrigerante,
                  data=Dados1)

games_howell_test(
  Peso ~ Refrigerante,
  data = Dados1
)

games_howell_test(
  Altura ~ Refrigerante,
  data = Dados1
)


games_howell_test(
  IMC ~ Refrigerante,
  data = Dados1
)

###Idade
#Aumentou × todos → significativo
#Diminuiu × Não alterou → não significativo
#Diminuiu × Não consumo → não significativo
#Não alterou × Não consumo → não significativo


###Peso
#Aumentou × Diminuiu → não significativo (p = 0.204)
#Aumentou × Não alterou → significativo
#Aumentou × Não consumo → significativo
#Diminuiu × Não alterou → não significativo
#Diminuiu × Não consumo → significativo
#Não alterou × Não consumo → significativo


###Altura
#Todos os p-valores são maiores que 0,05.


###IMC
#Aumentou × Diminuiu → significativo
#Aumentou × Não alterou → significativo
#Aumentou × Não consumo → significativo
#Diminuiu × Não alterou → não significativo
#Diminuiu × Não consumo → significativo
#Não alterou × Não consumo → significativo