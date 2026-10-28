require(openxlsx)            # Leitura de base de dados
require(dplyr)               # Manipulação de base de dados
require(gtsummary)           # Tabelas automáticas
require(gt)                  # Tabelas automáticas
require(rstatix)             # Coeficiente de Cramer
require(ggplot2)             # Gráficos
require(qqplotr)             # Gráficos qqplot
require(DescTools)           # Teste de Levene

Dados <- read.xlsx("Nutrição.xlsx")

# linguagem de pontuação em português
# formatação em português (vírgula pra decimais e ponto para milhares)
theme_gtsummary_language("pt", big.mark = ".", decimal.mark = ",")


# 1. Seleção inicial da base ---------------------------------------------------
DadosQuali = Dados %>% select(Refrigerante, Genero, Raca_cor, Regiao, Isolamento,
                              Trabalha, Renda_familiar, Escolaridade,
                              Covid, Consulta_Nutricionista, Dificuldade_Financeira,
                              cigarro, Ansiedade, Depressao, Insonia, Enxaqueca)

# Tabelas de contingencia iniciais ---------------------------------------------
tbl_summary(data = DadosQuali)

tbl_summary(data = DadosQuali,
            by = Refrigerante)

tbl_summary(data = DadosQuali,
            by = Refrigerante,
            percent = 'row')

# Verificação da Matriz Esperada (dados brutos) --------------------------------
chisq.test(DadosQuali$Genero, DadosQuali$Refrigerante)$expected     ######
chisq.test(DadosQuali$Raca_cor, DadosQuali$Refrigerante)$expected   ######
chisq.test(DadosQuali$Regiao, DadosQuali$Refrigerante)$expected     #qui-quadrado
chisq.test(DadosQuali$Isolamento, DadosQuali$Refrigerante)$expected #qui-quadrado
chisq.test(DadosQuali$Trabalha, DadosQuali$Refrigerante)$expected   #qui-quadrado
chisq.test(DadosQuali$Renda_familiar, DadosQuali$Refrigerante)$expected #qui-quadrado
chisq.test(DadosQuali$Escolaridade, DadosQuali$Refrigerante)$expected   ######
chisq.test(DadosQuali$Covid, DadosQuali$Refrigerante)$expected       #qui-quadrado
chisq.test(DadosQuali$Consulta_Nutricionista, DadosQuali$Refrigerante)$expected #qui-quadado
chisq.test(DadosQuali$Dificuldade_Financeira, DadosQuali$Refrigerante)$expected #qui-quadrado
chisq.test(DadosQuali$cigarro, DadosQuali$Refrigerante)$expected    #qui-quadrado
chisq.test(DadosQuali$Ansiedade, DadosQuali$Refrigerante)$expected  #qui-quadrado
chisq.test(DadosQuali$Depressao, DadosQuali$Refrigerante)$expected  #qui-quadrado
chisq.test(DadosQuali$Insonia, DadosQuali$Refrigerante)$expected    #qui-quadrado
chisq.test(DadosQuali$Enxaqueca, DadosQuali$Refrigerante)$expected   #qui-quadrado


# 2. Agrupando e tratando as variáveis no DadosQuali ---------------------------

# Agrupando a variavel Genero
DadosQuali$Genero_agrupado <- ifelse(
  DadosQuali$Genero %in% c("Outro", "Prefiro não responder"),
  "Outro/Prefiro não responder",
  as.character(DadosQuali$Genero)
)
DadosQuali$Genero_agrupado <- factor(DadosQuali$Genero_agrupado)
chisq.test(DadosQuali$Genero_agrupado, DadosQuali$Refrigerante)$expected  


# Agrupando a variavel Raca
DadosQuali$Raca_agrupado <- ifelse(
  DadosQuali$Raca_cor %in% c("Amarela", "Indígena", "Outro"),
  "Outros",
  as.character(DadosQuali$Raca_cor)
)
DadosQuali$Raca_agrupado <- factor(DadosQuali$Raca_agrupado)
chisq.test(DadosQuali$Raca_agrupado, DadosQuali$Refrigerante)$expected  


# Agrupando a variavel Escolaridade
DadosQuali$Escolaridade_agrupado <- ifelse(
  DadosQuali$Escolaridade %in% c("Analfabeto", "Ensino Fundamental Completo"),
  "Até Ensino Fundamental Completo",
  as.character(DadosQuali$Escolaridade)
)
DadosQuali$Escolaridade_agrupado <- factor(DadosQuali$Escolaridade_agrupado)
chisq.test(DadosQuali$Escolaridade_agrupado, DadosQuali$Refrigerante)$expected  

# 3. Criando DadosQuali_ com as colunas novas e filtrando os dados residuais ---

DadosQuali_ = DadosQuali %>% 
  select(Refrigerante, Genero_agrupado, Raca_agrupado, Regiao, Isolamento,
         Trabalha, Renda_familiar, Escolaridade_agrupado,
         Covid, Consulta_Nutricionista, Dificuldade_Financeira,
         cigarro, Ansiedade, Depressao, Insonia, Enxaqueca)




# 4. Adequando variáveis qualitativas (Ordenação e Níveis) ---------------------

# Raça
DadosQuali_$Raca_agrupado <- factor(
  DadosQuali_$Raca_agrupado,
  levels = c("Branca", "Parda", "Preta", "Outros", "Prefiro não declarar")
)

# Escolaridade
DadosQuali_$Escolaridade_agrupado <- factor(
  DadosQuali_$Escolaridade_agrupado,
  levels = c("Até Ensino Fundamental Completo","Ensino Médio Completo", "Ensino Superior Completo", "Pós-graduação", "Não desejo informar")
)


# Agora aplica os níveis na ordem crescente
DadosQuali_$Renda_familiar <- factor(
  as.character(DadosQuali_$Renda_familiar),
  levels = c(
    "até R$ 1254,00",
    "entre R$ 1.255 - R$ 8.640",
    "mais de R$ 8.640"
  ),
  labels = c(
    "Até R$ 1.254",
    "Entre R$ 1.255 - R$ 8.640",
    "Mais de R$ 8.640"
  )
)


# 5. Verificação da Matriz Esperada (ajustadas) --------------------------------
chisq.test(DadosQuali_$Genero_agrupado,DadosQuali_$Refrigerante)$expected        #fisher
chisq.test(DadosQuali_$Raca_agrupado, DadosQuali_$Refrigerante)$expected         #qui-quadrado
chisq.test(DadosQuali_$Escolaridade_agrupado, DadosQuali_$Refrigerante)$expected #fisher

# 6. Tabelas de contingencia finais --------------------------------------------
tbl_summary(data = DadosQuali_)

tbl_summary(data = DadosQuali_,
            by = Refrigerante)

tbl_summary(data = DadosQuali_,
            by = Refrigerante,
            percent = 'row')
  


# Tabela com o pvalor ----------------------------------------------------------
tbl_summary(data = DadosQuali_,
            by = Refrigerante,
            percent = "row")%>%
  add_p()

# Residuos ---------------------------------------------------------------------
# acima de 1,96 ou abaixo de -1,96 na normal padrao sao os caras influentes
chisq.test(DadosQuali_$Genero_agrupado,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Isolamento,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Trabalha,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Renda_familiar,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Escolaridade_agrupado,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Consulta_Nutricionista,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Dificuldade_Financeira,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Ansiedade,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Depressao,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Insonia,DadosQuali_$Refrigerante)$stdres
chisq.test(DadosQuali_$Enxaqueca,DadosQuali_$Refrigerante)$stdres


# Coeficiente de Cramer --------------------------------------------------------

cramer_fun <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  v <- cramer_v(tab)
  tibble::tibble(`**Cramér**` = round(v, 2))
}

# Criando tabela ---------------------------------------------------------------
tabela <- tbl_summary(
  data = DadosQuali_,
  by = Refrigerante,
  percent = "row",
  label = list(
    Genero_agrupado ~ "Gênero<sup>F</sup>",
    Raca_agrupado ~ "Raça/Cor<sup>F</sup>",
    Regiao ~ "Região<sup>Q</sup>",
    Isolamento ~ "Isolamento social<sup>Q</sup>",
    Trabalha ~ "Situação de Trabalho<sup>Q</sup>",
    Renda_familiar ~ "Faixa de Renda Familiar<sup>Q</sup>",
    Escolaridade_agrupado ~ "Nível de Escolaridade<sup>F</sup>",
    Covid ~ "Diagnóstico de Covid-19<sup>Q</sup>",
    Consulta_Nutricionista ~ "Consulta com Nutricionista<sup>Q</sup>",
    Dificuldade_Financeira ~ "Dificuldade Financeira<sup>Q</sup>",
    cigarro ~ "Consumo de Cigarro<sup>Q</sup>",
    Ansiedade ~ "Ansiedade na Pandemia<sup>Q</sup>",
    Depressao ~ "Depressão na Pandemia<sup>Q</sup>",
    Insonia ~ "Insônia na Pandemia<sup>Q</sup>",
    Enxaqueca ~ "Enxaqueca na Pandemia<sup>Q</sup>"
  )
) %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test",
      Escolaridade_agrupado ~ "fisher.test",
      Genero_agrupado ~ "fisher.test"
    ),
    test.args = list(
      Escolaridade_agrupado ~ list(simulate.p.value = TRUE, B = 10000),
      Genero_agrupado ~ list(simulate.p.value = TRUE, B = 10000)
    ),
    pvalue_fun = label_style_pvalue(digits = 3)
  ) %>%
  bold_p(t = 0.05) %>%
  add_stat(fns = everything() ~ cramer_fun) %>%
  modify_spanning_header(all_stat_cols() ~ "**Refrigerante**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)") %>%
  modify_footnote(everything() ~ NA) %>%
  as_gt() %>%                    
  fmt_markdown(columns = label) %>%
  
  # Distribui larguras fixas e perfeitamente equilibradas entre as colunas
  gt::cols_width(
    label ~ gt::px(260),
    all_stat_cols() ~ gt::px(125)
  ) %>%
  
  gt::tab_options(
    table.font.size = "20px",
    heading.title.font.size = "26px",
    column_labels.font.size = "22px",
    container.height = gt::px(650),
    container.overflow.y = TRUE,
    data_row.padding = gt::px(10)
  ) %>%
  gt::tab_source_note(
    source_note = gt::html(
      "
      <div class='legenda-modelo'>
        <b>Q</b>: Teste qui-quadrado de Pearson;<br>
        <b>F</b>: Teste exato de Fisher;<br>
        <b>r</b>: Resíduo padronizado;<br>
        <span style='color:#8FB9DE;'>■</span> Acima do esperado (r &gt; 1,96) &nbsp;&nbsp;&nbsp;
        <span style='color:#B8A4D4;'>■</span> Abaixo do esperado (r &lt; -1,96) &nbsp;&nbsp;&nbsp;
        <span style='color:#E5C94F;'>■</span> Associação significativa (p &lt; 0,05)
      </div>
      "
    )
  ) %>%
  gt::tab_style(
    style = list(
      gt::cell_text(align = "left", v_align = "middle"),
      "white-space: nowrap !important;"
    ),
    locations = gt::cells_body(columns = label)
  ) %>%
  gt::tab_style(
    style = list(
      "padding-left: 25px !important;"
    ),
    locations = gt::cells_body(
      columns = label,
      rows = row_type == "level"
    )
  ) %>%
  gt::tab_style(
    style = gt::cell_text(align = "left"),
    locations = gt::cells_source_notes()
  ) %>%
  gt::opt_css(
    css = "
    .gt_table {
      table-layout: fixed !important;
      width: 100% !important;
    }

    /* Cabeçalho fixo no topo */
    .gt_table thead {
      position: sticky !important;
      top: -12px !important;
      z-index: 9999 !important;
      background: white !important;
    }

    .gt_table thead th {
      background: white !important;
    }

    /* Uniformiza todas as colunas de dados (Refrigerante, Valor-p e Cramér) */
    .gt_table th:not(:first-child),
    .gt_table td:not(:first-child) {
      width: 125px !important;
      min-width: 125px !important;
      max-width: 125px !important;
      text-align: center !important;
      padding-left: 8px !important;
      padding-right: 8px !important;
    }

    /* Rodapé fixo com linha preta permanente */
    .gt_sourcenotes {
      position: sticky !important;
      bottom: 0 !important;
      z-index: 99999 !important;
      background: white !important;
      padding: 0 !important;
      text-align: left !important;
      box-shadow: 0 -2px 0 0 #111827 !important; /* Mantém a linha preta sempre visível */
    }

    .gt_sourcenote {
      background: white !important;
      text-align: left !important;
      padding: 8px 12px 10px 16px !important;
      border-top: 2px solid #111827 !important;  /* Linha preta escura sobre a legenda */
    }

    .legenda-modelo {
      text-align: left !important;
      font-size: 16px !important;
      line-height: 1.45 !important;
      color: #111827 !important;
      white-space: nowrap !important;
    }
    "
  )




# 2. Colorindo as Associações e Células Significativas --------------------------

# Destaque Amarelo para Variáveis Significativas (p < 0,05)
tabela <- tbl_summary(data = DadosQuali_,
                      by = Refrigerante,
                      percent = "row",
                      label = list(
                        Genero_agrupado ~ "Gênero<sup>F</sup>",
                        Raca_agrupado ~ "Raça/Cor<sup>Q</sup>",
                        Regiao ~ "Região<sup>Q</sup>",
                        Isolamento ~"Isolamento social<sup>Q</sup>",
                        Trabalha ~ "Situação de Trabalho<sup>Q</sup>",
                        Renda_familiar ~ "Faixa de Renda Familiar<sup>Q</sup>",
                        Escolaridade_agrupado ~"Nível de Escolaridade<sup>F</sup>",
                        Covid ~"Diagnóstico de Covid-19<sup>Q</sup>",
                        Consulta_Nutricionista ~ "Consulta com  Nutricionista<sup>Q</sup>",
                        Dificuldade_Financeira ~"Dificuldade Financeira<sup>Q</sup>",
                        cigarro ~"Consumo de Cigarro<sup>Q</sup>",
                        Ansiedade ~"Ansiedade na Pandemia<sup>Q</sup>",
                        Depressao ~"Depressão na Pandemia<sup>Q</sup>",
                        Insonia ~"Insônia na Pandemia<sup>Q</sup>",
                        Enxaqueca ~"Enxaqueca na Pandemia<sup>Q</sup>"
                      )
) %>%
  add_p(
    test = list(
      all_categorical() ~ "chisq.test",
      Escolaridade_agrupado ~ "fisher.test",
      Genero_agrupado ~ "fisher.test"
    ),
    test.args = list(
      Escolaridade_agrupado ~ list(simulate.p.value = TRUE, B = 10000),
      Genero_agrupado ~ list(simulate.p.value = TRUE, B = 10000)
    ),
    pvalue_fun = label_style_pvalue(digits = 3)
  ) %>%
  bold_p(t = 0.05) %>%
  add_stat(fns = everything() ~ cramer_fun) %>%
  
  modify_spanning_header(all_stat_cols() ~ "**Mudança no consumo de refrigerantes**") %>%
  modify_header(label ~ "**Variáveis**") %>%
  bold_labels() %>%
  modify_header(
    all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)"
  ) %>%
  modify_footnote(everything() ~ NA) %>%
  
  as_gt() %>%
  fmt_markdown(columns = label) %>%
  
  # Largura das colunas
  gt::cols_width(
    label ~ gt::pct(24),
    all_stat_cols() ~ gt::pct(12.67)
  ) %>%
  
  gt::tab_options(
    table.font.size = "20px",
    heading.title.font.size = "26px",
    column_labels.font.size = "22px",
    container.height = gt::px(650),
    container.overflow.y = TRUE,
    data_row.padding = gt::px(10)
  ) %>%
  
  gt::tab_source_note(
    source_note = gt::html(
      "
      <div class='legenda-modelo'>
        <b>Q</b>: Teste qui-quadrado de Pearson;<br>
        <b>F</b>: Teste exato de Fisher;<br>
        <b>r</b>: Resíduo padronizado;<br>
        <span style='color:#8FB9DE;'>■</span> Acima do esperado (r &gt; 1,96) &nbsp;&nbsp;&nbsp;
        <span style='color:#B8A4D4;'>■</span> Abaixo do esperado (r &lt; -1,96) &nbsp;&nbsp;&nbsp;
        <span style='color:#E5C94F;'>■</span> Associação significativa (p &lt; 0,05)
      </div>
      "
    )
  ) %>%
  
  gt::tab_style(
    style = list(
      gt::cell_text(align = "left"),
      "white-space: nowrap !important;"
    ),
    locations = gt::cells_body(columns = label)
  ) %>%
  
  gt::tab_style(
    style = list(
      "padding-left: 25px !important;"
    ),
    locations = gt::cells_body(
      columns = label,
      rows = row_type == "level"
    )
  ) %>%
  
  gt::tab_style(
    style = gt::cell_text(align = "left"),
    locations = gt::cells_source_notes()
  ) %>%
  
  gt::opt_css(
    css = "
    
    /* =====================================================
       TABELA
       ===================================================== */
    
    .gt_table {
      table-layout: fixed !important;
      width: 100% !important;
    }
    
    
   /* =====================================================
   CABEÇALHO FIXO
   ===================================================== */

.tabela-qualitativa .gt_table thead th {
  position: sticky !important;
  background: white !important;
  z-index: 100 !important;
  white-space: nowrap !important;
}

.gt_table thead tr:first-child th {
  top: -10px !important;                               /* <-- 1. PUXOU PARA CIMA */
  z-index: 102 !important;
  box-shadow: 0 -30px 0 0 white !important;           /* <-- 2. CRIA A PAREDE BRANCA NO TOPO */
}

.gt_table thead tr:nth-child(2) th {
  top: 22px !important;                                /* <-- Compensou a subida (32 - 10 = 22px) */
  z-index: 101 !important;
}

.gt_table thead th[rowspan] {
  top: -10px !important;                               /* <-- PUXOU AS COLUNAS LATERAIS JUNTO */
  z-index: 103 !important;
  box-shadow: 0 -30px 0 0 white !important;           /* <-- PAREDE BRANCA NAS LATERAIS */
}
    
  /* =====================================================
   AJUSTE: DESLOCAR AUMENTOU PARA A DIREITA
   ===================================================== */

/* Seleciona exatamente a 1ª célula da 2ª linha do cabeçalho (Aumentou + valor) */
.gt_table thead tr:nth-child(2) th:nth-child(1) {
  text-align: center !important;
  padding-left: 20px !important; /* Move para a direita */
  padding-right: 0px !important;
}

/* Garante que o corpo da coluna continue alinhado normalmente */
.gt_table tbody td:nth-child(2) {
  text-align: center !important;
  transform: none !important;
}
    /* =====================================================
       COLUNA VARIÁVEIS
       ===================================================== */
    
    .gt_table th:first-child,
    .gt_table td:first-child {
      width: 24% !important;
      min-width: 24% !important;
      max-width: 24% !important;
      text-align: left !important;
      padding-right: 15px !important;
    }
    
    
    /* =====================================================
       4 COLUNAS DE REFRIGERANTE
       ===================================================== */
    
    .gt_table th:nth-child(2),
    .gt_table td:nth-child(2),
    .gt_table th:nth-child(3),
    .gt_table td:nth-child(3),
    .gt_table th:nth-child(4),
    .gt_table td:nth-child(4),
    .gt_table th:nth-child(5),
    .gt_table td:nth-child(5) {
      width: 13% !important;
      min-width: 13% !important;
      max-width: 13% !important;
      text-align: center !important;
    }
    .gt_table thead tr:nth-child(2) th {
  white-space: nowrap !important;
  font-size: 20px !important;
}
    
    /* =====================================================
       VALOR-P
       ===================================================== */
    
    .gt_table th:nth-child(6),
    .gt_table td:nth-child(6) {
      width: 12% !important;
      min-width: 12% !important;
      max-width: 12% !important;
      text-align: center !important;
    }
    
    
    /* =====================================================
       CRAMÉR
       ===================================================== */
    
    .gt_table th:nth-child(7),
    .gt_table td:nth-child(7) {
      width: 12% !important;
      min-width: 12% !important;
      max-width: 12% !important;
      text-align: center !important;
    }
    
    
    /* =====================================================
       CÉLULAS DAS COLUNAS NUMÉRICAS
       ===================================================== */
    
    .gt_table th:not(:first-child),
    .gt_table td:not(:first-child) {
      padding-left: 8px !important;
      padding-right: 8px !important;
    }
    
    
    /* =====================================================
       LEGENDA FIXA
       ===================================================== */
    
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
  text-align: left !important;
  padding-left: 0 !important;
}

.gt_sourcenote > * {
  position: relative !important;
  left: -280px !important;
  white-space: nowrap !important;
}
  "
  )


# ============================================================
# 2. COLORINDO AS ASSOCIAÇÕES E CÉLULAS SIGNIFICATIVAS
# ============================================================

# Destaque Amarelo para Variáveis Significativas (p < 0,05)
tabela <- tabela %>%
  tab_style(
    style = list(cell_fill(color = "#fef9c3")),
    locations = cells_body(
      rows = variable %in% c(
        "Genero_agrupado", "Isolamento", "Trabalha", "Renda_familiar",
        "Escolaridade_agrupado", "Consulta_Nutricionista",
        "Dificuldade_Financeira", "Ansiedade", "Depressao", "Insonia",
        "Enxaqueca"
      ) & row_type == "label"
    )
  )

# ============================================================
# ACIMA DO ESPERADO (r > 1,96)
# Apenas TEXTO em Azul Escuro + NEGRITO
# ============================================================

tabela <- tabela %>%
  tab_style(
    style = list(cell_text(color = "#0369a1", weight = "bold")),
    locations = cells_body(
      columns = stat_1,
      rows = (variable == "Genero_agrupado" & label == "Feminino") |
        (variable == "Escolaridade_agrupado" & label == "Ensino Superior Completo") |
        (variable == "Escolaridade_agrupado" & label == "Não desejo informar") |
        (variable == "Consulta_Nutricionista" & label == "Não") |
        (variable == "Ansiedade" & label == "Sim") |
        (variable == "Depressao" & label == "Sim") |
        (variable == "Insonia" & label == "Sim") |
        (variable == "Enxaqueca" & label == "Sim")
    )
  ) %>%
  tab_style(
    style = list(cell_text(color = "#0369a1", weight = "bold")),
    locations = cells_body(
      columns = stat_2,
      rows = (variable == "Genero_agrupado" & label == "Masculino") |
        (variable == "Trabalha" & label == "Não") |
        (variable == "Renda_familiar" & label == "Até R$ 1.254") |
        (variable == "Escolaridade_agrupado" & label == "Ensino Médio Completo") |
        (variable == "Consulta_Nutricionista" & label == "Sim") |
        (variable == "Dificuldade_Financeira" & label == "Sim") 
    )
  ) %>%
  tab_style(
    style = list(cell_text(color = "#0369a1", weight = "bold")),
    locations = cells_body(
      columns = stat_3,
      rows = (variable == "Isolamento" & label == "Não") |
        (variable == "Trabalha" & label == "Sim") |
        (variable == "Escolaridade_agrupado" & label == "Pós-graduação") |
        (variable == "Consulta_Nutricionista" & label == "Não") |
        (variable == "Dificuldade_Financeira" & label == "Não") |
        (variable == "Depressao" & label == "Não") |
        (variable == "Insonia" & label == "Não") |
        (variable == "Enxaqueca" & label == "Não")
    )
  ) %>%
  tab_style(
    style = list(cell_text(color = "#0369a1", weight = "bold")),
    locations = cells_body(
      columns = stat_4,
      rows = (variable == "Genero_agrupado" & label == "Feminino") |
        (variable == "Isolamento" & label == "Sim") |
        (variable == "Escolaridade_agrupado" & label == "Pós-graduação") |
        (variable == "Consulta_Nutricionista" & label == "Sim") |
        (variable == "Dificuldade_Financeira" & label == "Não") |
        (variable == "Ansiedade" & label == "Não") |
        (variable == "Insonia" & label == "Não")
    )
  )

# ============================================================
# ABAIXO DO ESPERADO (r < -1,96)
# Apenas TEXTO em Roxo Escuro + NEGRITO
# ============================================================

tabela <- tabela %>%
  tab_style(
    style = list(cell_text(color = "#6d28d9", weight = "bold")),
    locations = cells_body(
      columns = stat_1,
      rows = (variable == "Genero_agrupado" & label == "Masculino") |
        (variable == "Escolaridade_agrupado" & label == "Pós-graduação") |
        (variable == "Consulta_Nutricionista" & label == "Sim") |
        (variable == "Ansiedade" & label == "Não") |
        (variable == "Depressao" & label == "Não") |
        (variable == "Insonia" & label == "Não") |
        (variable == "Enxaqueca" & label == "Não")
    )
  ) %>%
  tab_style(
    style = list(cell_text(color = "#6d28d9", weight = "bold")),
    locations = cells_body(
      columns = stat_2,
      rows = (variable == "Genero_agrupado" & label == "Feminino") |
        (variable == "Trabalha" & label == "Sim") |
        (variable == "Renda_familiar" & label == "Mais de R$ 8.640") |
        (variable == "Escolaridade_agrupado" & label == "Pós-graduação") |
        (variable == "Consulta_Nutricionista" & label == "Não") |
        (variable == "Dificuldade_Financeira" & label == "Não")
    )
  ) %>%
  tab_style(
    style = list(cell_text(color = "#6d28d9", weight = "bold")),
    locations = cells_body(
      columns = stat_3,
      rows = (variable == "Isolamento" & label == "Sim") |
        (variable == "Trabalha" & label == "Não") |
        (variable == "Escolaridade_agrupado" & label == "Ensino Médio Completo") |
        (variable == "Consulta_Nutricionista" & label == "Sim") |
        (variable == "Dificuldade_Financeira" & label == "Sim") |
        (variable == "Depressao" & label == "Sim") |
        (variable == "Insonia" & label == "Sim") |
        (variable == "Enxaqueca" & label == "Sim")
    )
  ) %>%
  tab_style(
    style = list(cell_text(color = "#6d28d9", weight = "bold")),
    locations = cells_body(
      columns = stat_4,
      rows = (variable == "Genero_agrupado" & label == "Masculino") |
        (variable == "Isolamento" & label == "Não") |
        (variable == "Renda_familiar" & label == "Até R$ 1.254") |
        (variable == "Escolaridade_agrupado" & label == "Ensino Superior Completo") |
        (variable == "Consulta_Nutricionista" & label == "Não") |
        (variable == "Dificuldade_Financeira" & label == "Sim") |
        (variable == "Ansiedade" & label == "Sim") |
        (variable == "Insonia" & label == "Sim") 
    )
  )


tabela
