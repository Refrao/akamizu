# =============================================================================
# waterfall_chart.R
# Gerador de Gráfico Waterfall a partir de Excel → HTML para Outlook
#
# Como usar:
#   1. Instale os pacotes necessários (bloco abaixo, execute UMA VEZ)
#   2. Prepare seu arquivo Excel conforme o formato esperado
#      (ou gere um template de exemplo com create_template_excel())
#   3. Ajuste o bloco CONFIG abaixo
#   4. Execute o script completo (Ctrl+Shift+Enter no RStudio)
#   5. Abra output/waterfall_email.html no Chrome ou Edge
#   6. Ctrl+A → Ctrl+C → Cole no corpo do e-mail no Outlook
# =============================================================================


# ── Instalação de pacotes (execute UMA VEZ, depois pode comentar) ─────────────
# install.packages(c("readxl", "base64enc", "janitor", "writexl"))
# Os demais (dplyr, ggplot2, scales) já estão no tidyverse


# ── Carregamento de pacotes ───────────────────────────────────────────────────
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(base64enc)
library(janitor)

source("waterfall_utils.R")


# ── CONFIG — edite apenas esta seção ─────────────────────────────────────────

# Caminho do arquivo Excel de entrada
# A planilha deve ter uma aba chamada "data" com as colunas:
#   label  : nome da barra (texto)
#   value  : valor numérico (delta ou acumulado, dependendo do 'type')
#   type   : um de: start | increase | decrease | subtotal | total
#   note   : observação por linha (opcional)
EXCEL_PATH <- "waterfall_data_template.xlsx"

# Pasta onde o HTML final será salvo
OUTPUT_DIR <- "output"

# Título e subtítulo do gráfico e do e-mail
CHART_TITLE    <- "Bridge Financeiro - FY2025"
CHART_SUBTITLE <- "Receita Bruta → EBIT"

# Data exibida no e-mail (padrão: hoje)
REPORT_DATE <- format(Sys.Date(), "%d/%m/%Y")

# Símbolo monetário (ex: "R$ ", "$ ", "€ ")
CURRENCY_SYMBOL <- "R$ "

# Sufixo de escala: "" para valores inteiros, "K" se estiver em milhares, "M" em milhões
SCALE_SUFFIX <- ""

# Dimensões do gráfico em pixels
CHART_WIDTH_PX  <- 1200
CHART_HEIGHT_PX <- 650
CHART_DPI       <- 150

# Texto de rodapé do e-mail
FOOTER_NOTE <- "Fonte: Dados financeiros internos. Gerado automaticamente."


# ── PIPELINE ──────────────────────────────────────────────────────────────────

# (Opcional) Gerar template Excel de exemplo pela primeira vez:
# Requer writexl: install.packages("writexl")
# library(writexl)
# create_template_excel(EXCEL_PATH)


# 1. Ler dados do Excel
message("[ 1/6 ] Lendo arquivo: ", EXCEL_PATH)
df_raw <- read_waterfall_excel(EXCEL_PATH)
message("        ", nrow(df_raw), " linha(s) carregada(s).")


# 2. Calcular geometria das barras
message("[ 2/6 ] Calculando geometria do waterfall...")
df_geom <- compute_waterfall_geometry(df_raw)


# 3. Construir gráfico ggplot2
message("[ 3/6 ] Construindo gráfico...")
wf_plot <- build_waterfall_plot(
  df_geom,
  title           = CHART_TITLE,
  subtitle        = CHART_SUBTITLE,
  currency_symbol = CURRENCY_SYMBOL,
  scale_suffix    = SCALE_SUFFIX
)


# 4. Visualizar no RStudio (comente para execução headless / sem interface gráfica)
print(wf_plot)


# 5. Exportar gráfico como imagem base64
message("[ 4/6 ] Exportando imagem como base64...")
data_uri <- export_plot_base64(wf_plot, CHART_WIDTH_PX, CHART_HEIGHT_PX, CHART_DPI)


# 6. Montar HTML para Outlook
message("[ 5/6 ] Montando HTML para Outlook...")
html_content <- build_html_email(
  data_uri        = data_uri,
  df_raw          = df_raw,
  title           = CHART_TITLE,
  subtitle        = CHART_SUBTITLE,
  report_date     = REPORT_DATE,
  footer_note     = FOOTER_NOTE,
  currency_symbol = CURRENCY_SYMBOL
)


# 7. Salvar arquivo HTML
message("[ 6/6 ] Salvando HTML...")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)
output_path <- file.path(OUTPUT_DIR, "waterfall_email.html")
writeLines(html_content, con = output_path, useBytes = FALSE)

message("\n✔  Pronto! Arquivo salvo em: ", normalizePath(output_path))
message("")
message("── Como usar no Outlook ──────────────────────────────────────────────")
message("  1. Abra o arquivo no Chrome ou Edge:")
message("       ", normalizePath(output_path))
message("  2. Pressione Ctrl+A para selecionar tudo")
message("  3. Pressione Ctrl+C para copiar")
message("  4. No Outlook, abra um novo e-mail")
message("  5. Clique no corpo do e-mail e pressione Ctrl+V para colar")
message("  6. O gráfico e a tabela aparecerão diretamente no e-mail")
message("─────────────────────────────────────────────────────────────────────")
