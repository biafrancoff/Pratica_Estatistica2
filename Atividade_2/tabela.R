##############Criar Tabela Testes de Independência##############################
# Setup ------------------------------------------------------------------------
require(openxlsx)            # Leitura de base de dados
require(dplyr)               # Manipulação de base de dados
require(gtsummary)           # Tabelas automáticas
require(gt)                  # Tabelas automáticas
require(rstatix)             # Coeficiente de Cramer
require(ggplot2)             # Gráficos
require(qqplotr)             # Gráficos qqplot
require(DescTools)           # Teste de Levene

DadosFatoresRisco <- read.xlsx("DadosFatoresRisco.xlsx")

# linguagem de pontuação em português
# formatação em português (vírgula pra decimais e ponto para milhares)
theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")

# Adequando variáveis qualitativas ---------------------------------------------
DadosFatoresRisco$IMC <- factor(
  DadosFatoresRisco$IMC,
  levels = c("< 25", "25 a 29,9", "30 ou mais"),
  labels = c("Menos de 25", "25 a 29,9", "30 ou mais")
)

DadosFatoresRisco$Tempo_Dormir <- factor(
  DadosFatoresRisco$Tempo_Dormir,
  levels = c(
    "Menos de 7 horas",
    "7 a 9 horas",
    "Mais de 9 horas"
  )
)

DadosFatoresRisco$Saude_Geral <- factor(
  DadosFatoresRisco$Saude_Geral,
  levels = c(
    "Razoável/Ruim",
    "Boa",
    "Excelente/Muito boa"
  )
)

DadosFatoresRisco$Raca <- factor(
  DadosFatoresRisco$Raca,
  levels = c(
    "Branco",
    "Preto",
    "Outros"
  )
)

DadosFatoresRisco$Saude_Mental <- factor(
  DadosFatoresRisco$Saude_Mental,
  levels = c(
    "Nenhum dia",
    "1 a 13 dias",
    "14 dias ou mais"
  )
)

DadosFatoresRisco$Saude_Fisica <- factor(
  DadosFatoresRisco$Saude_Fisica,
  levels = c(
    "Nenhum dia",
    "1 a 13 dias",
    "14 dias ou mais"
  )
)

DadosFR = DadosFatoresRisco %>% select(Asma, 	
                                       IMC,
                                       Cigarro,
                                       Consumo_Alcool,
                                       Saude_Fisica,
                                       Saude_Mental,
                                       Dificuldade_Andar,
                                       Sexo,
                                       Faixa_Idade,
                                       Raca,
                                       Atividade_Fisica,
                                       Saude_Geral,
                                       Tempo_Dormir
)
# Tabelas de contingencia ------------------------------------------------------
tbl_summary(data = DadosFR)

tbl_summary(data = DadosFR,
            by = Asma)

tbl_summary(data = DadosFR,
            by = Asma,
            percent='row')

# Verificação da Matriz Esperada -----------------------------------------------
chisq.test(DadosFR$IMC,DadosFR$Asma)$expected
chisq.test(DadosFR$Cigarro,DadosFR$Asma)$expected
chisq.test(DadosFR$Consumo_Alcool,DadosFR$Asma)$expected #fisher 
chisq.test(DadosFR$Saude_Fisica,DadosFR$Asma)$expected
chisq.test(DadosFR$Saude_Mental,DadosFR$Asma)$expected
chisq.test(DadosFR$Dificuldade_Andar,DadosFR$Asma)$expected
chisq.test(DadosFR$Sexo,DadosFR$Asma)$expected
chisq.test(DadosFR$Faixa_Idade,DadosFR$Asma)$expected
chisq.test(DadosFR$Raca,DadosFR$Asma)$expected #fisher
chisq.test(DadosFR$Atividade_Fisica,DadosFR$Asma)$expected
chisq.test(DadosFR$Saude_Geral,DadosFR$Asma)$expected
chisq.test(DadosFR$Tempo_Dormir,DadosFR$Asma)$expected #fisher


# Tabela com o pvalor ----------------------------------------------------------
tbl_summary(data = DadosFR,
            by = Asma,
            percent = "row")%>%
  add_p()

# Residuos ---------------------------------------------------------------------
# acima de 1,96 ou abaixo de -1,96 na normal padrao sao os caras influentes
chisq.test(DadosFR$Saude_Mental,DadosFR$Asma)$stdres

# Coeficiente de Cramer --------------------------------------------------------

cramer_fun <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  v <- cramer_v(tab)
  tibble::tibble(`**Cramér**` = round(v, 2))
}

# Criando tabela ---------------------------------------------------------------

tabela <- tbl_summary(data = DadosFR,
                      by = Asma,
                      percent = "row",
                      label = list(
                        IMC ~ "IMC<sup>Q</sup>",
                        Cigarro ~ "Cigarro<sup>Q</sup>",
                        Consumo_Alcool ~ "Consumo de Álcool<sup>F</sup>",
                        Saude_Fisica ~"Saúde Física<sup>Q</sup>",
                        Saude_Mental ~ "Saúde Mental<sup>Q</sup>",
                        Dificuldade_Andar ~ "Dificuldade de Andar<sup>Q</sup>",
                        Sexo ~"Sexo<sup>Q</sup>",
                        Faixa_Idade ~"Idade<sup>Q</sup>",
                        Raca ~ "Raça<sup>F</sup>",
                        Atividade_Fisica ~"Atividade Física<sup>Q</sup>",
                        Saude_Geral ~"Saúde Geral<sup>Q</sup>",
                        Tempo_Dormir ~"Tempo de Dormir<sup>F</sup>"
                      )
)%>%
  add_p(pvalue_fun = label_style_pvalue(digits = 3)) %>%
  bold_p(t = 0.05) %>%
  add_stat(fns = everything() ~ cramer_fun)%>%
  modify_spanning_header(all_stat_cols() ~ "**Asma**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)")%>%
  modify_bold(
    columns = stat_1,  # primeira coluna
    rows = (variable == "Saude_Mental" & label == "Nenhum dia")
  )%>%
  modify_bold(
    columns = stat_2,  # segunda coluna
    rows = (variable == "Saude_Mental" & label == "Nenhum dia"))%>%
  modify_footnote(everything() ~ NA)%>%
  as_gt() %>%                   
  fmt_markdown(columns = label) %>%
  gt::tab_options(
    table.font.size = "20px",
    heading.title.font.size = "26px",
    column_labels.font.size = "22px",
    container.height = gt::px(650),
    container.overflow.y = TRUE
  ) %>%
  
  gt::tab_source_note(
    source_note = gt::html(
      "
    <div class='legenda-conteudo'>
      <b>Q</b>: Teste qui-quadrado de Pearson;<br>
      <b>F</b>: Teste exato de Fisher;<br>
      <b>r</b>: Resíduo padronizado;<br>

      <span style='color:#8FB9DE;'>■</span>
      Acima do esperado (r &gt; 1,96)
      &nbsp;&nbsp;&nbsp;

      <span style='color:#B8A4D4;'>■</span>
      Abaixo do esperado (r &lt; -1,96)
      &nbsp;&nbsp;&nbsp;

      <span style='color:#E5C94F;'>■</span>
      Associação significativa (p &lt; 0,05)
    </div>
    "
    )
  ) %>%
  
  gt::tab_style(
    style = gt::cell_text(align = "left"),
    locations = gt::cells_source_notes()
  ) %>%
  
  gt::opt_css(
    css = "
    .gt_table thead {
      position: sticky !important;
      top: -12px !important;
      z-index: 9999 !important;
      background: white !important;
    }

    .gt_table thead th {
      background: white !important;
    }

    .gt_sourcenotes {
      position: sticky !important;
      bottom: 0 !important;
      z-index: 99999 !important;
      background: white !important;
      padding-top: 6px !important;
      padding-bottom: 8px !important;
      box-shadow: 0 30px 0 0 white !important;
    }

    .gt_sourcenotes::before {
      content: '';
      position: absolute !important;
      top: 0 !important;
      left: 0 !important;
      width: 100% !important;
      height: 2px !important;
      background: #4b5563 !important;
      z-index: 100001 !important;
    }

    .gt_sourcenote {
      background: white !important;
    }

    .legenda-conteudo {
      position: relative !important;
      left: -200px !important;
      text-align: left !important;
      white-space: nowrap;
    }
  "
  )

tabela

## Colorindo -------------------------------------------------------------------

tabela <- tabela %>%
  tab_style(
    style = list(
      cell_fill(color = "#fef9c3")
    ),
    locations = cells_body(
      rows = variable %in% c("Saude_Mental") & 
        row_type == "label"
    )
  )

tabela <- tabela %>%
  # Mais casos que o esperado
  tab_style(
    style = cell_fill(color = "#DCEAF7"),
    locations = cells_body(
      columns = stat_1,
      rows = variable == "Saude_Mental" &
        row_type == "level" &
        label == "Nenhum dia"
    )
  ) %>%
  
  # Menos casos que o esperado
  tab_style(
    style = cell_fill(color = "#E8E0F2"),
    locations = cells_body(
      columns = stat_2,
      rows = variable == "Saude_Mental" &
        row_type == "level" &
        label == "Nenhum dia"
    )
  )
