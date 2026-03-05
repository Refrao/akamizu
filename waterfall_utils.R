# =============================================================================
# waterfall_utils.R
# Funções auxiliares para geração de gráfico waterfall a partir de Excel
# Requer: readxl, dplyr, ggplot2, scales, base64enc, janitor, writexl
# =============================================================================


# ── 1. Leitura e validação do Excel ──────────────────────────────────────────

read_waterfall_excel <- function(path, sheet = "data") {
  if (!file.exists(path)) {
    stop("Arquivo não encontrado: ", path)
  }

  df <- readxl::read_excel(path, sheet = sheet)
  df <- janitor::clean_names(df)

  required_cols <- c("label", "value", "type")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Colunas obrigatórias ausentes no Excel: ",
         paste(missing_cols, collapse = ", "),
         "\nColunas encontradas: ", paste(names(df), collapse = ", "))
  }

  df$value <- as.numeric(df$value)
  df$type  <- tolower(trimws(as.character(df$type)))

  valid_types <- c("start", "increase", "decrease", "subtotal", "total")
  invalid_types <- setdiff(unique(df$type), valid_types)
  if (length(invalid_types) > 0) {
    stop("Tipos inválidos na coluna 'type': ", paste(invalid_types, collapse = ", "),
         "\nUse apenas: ", paste(valid_types, collapse = ", "))
  }

  # Adiciona colunas opcionais se ausentes
  if (!"note"  %in% names(df)) df$note  <- NA_character_
  if (!"group" %in% names(df)) df$group <- NA_character_

  df
}


# ── 2. Cálculo da geometria do waterfall ─────────────────────────────────────

compute_waterfall_geometry <- function(df) {
  n <- nrow(df)
  ymin <- numeric(n)
  ymax <- numeric(n)

  running_total <- 0

  for (i in seq_len(n)) {
    tipo  <- df$type[i]
    valor <- df$value[i]

    if (tipo == "start") {
      ymin[i]       <- 0
      ymax[i]       <- valor
      running_total <- valor

    } else if (tipo == "increase") {
      ymin[i]       <- running_total
      ymax[i]       <- running_total + valor
      running_total <- running_total + valor

    } else if (tipo == "decrease") {
      # valor é negativo: a barra "cai" a partir do running_total
      ymax[i]       <- running_total
      ymin[i]       <- running_total + valor
      running_total <- running_total + valor

    } else if (tipo %in% c("subtotal", "total")) {
      # valor é o acumulado explícito; ancora o running_total
      ymin[i]       <- 0
      ymax[i]       <- valor
      running_total <- valor
    }
  }

  color_map <- c(
    start    = "#5B9BD5",
    increase = "#70AD47",
    decrease = "#ED7D31",
    subtotal = "#8EA9C1",
    total    = "#404040"
  )

  abs_max <- max(abs(c(ymin, ymax)), na.rm = TRUE)
  offset  <- abs_max * 0.025

  df$ymin       <- ymin
  df$ymax       <- ymax
  df$fill_color <- unname(color_map[df$type])
  df$label_y    <- ifelse(df$ymax >= df$ymin,
                          df$ymax + offset,
                          df$ymin - offset)
  df$x_pos      <- seq_len(n)

  df
}


# ── 3. Construção do gráfico ggplot2 ─────────────────────────────────────────

build_waterfall_plot <- function(df_geom, title, subtitle,
                                  currency_symbol = "R$ ",
                                  scale_suffix    = "") {

  fmt_value <- function(x) {
    paste0(
      currency_symbol,
      formatC(abs(x), format = "f", digits = 0,
              big.mark = ".", decimal.mark = ","),
      scale_suffix
    )
  }

  # Linhas tracejadas conectoras entre barras delta
  connector_rows <- df_geom[df_geom$type %in% c("increase", "decrease"), ]
  connector_list <- lapply(seq_len(nrow(connector_rows)), function(k) {
    xp       <- connector_rows$x_pos[k]
    row_curr <- df_geom[df_geom$x_pos == xp, ]
    row_prev <- df_geom[df_geom$x_pos == xp - 1, ]
    if (nrow(row_prev) == 0) return(NULL)

    y_conn <- if (row_curr$type == "decrease") row_curr$ymax else row_curr$ymin

    data.frame(
      x    = row_prev$x_pos + 0.43,
      xend = row_curr$x_pos - 0.43,
      y    = y_conn,
      yend = y_conn
    )
  })
  connector_data <- do.call(rbind, connector_list)

  # Cores e rótulos da legenda
  legend_labels <- c("Início", "Aumento", "Redução", "Subtotal", "Total")
  legend_colors <- c("#5B9BD5", "#70AD47", "#ED7D31", "#8EA9C1", "#404040")

  p <- ggplot2::ggplot(df_geom) +
    # Barras
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = x_pos - 0.43,
        xmax = x_pos + 0.43,
        ymin = ymin,
        ymax = ymax,
        fill = fill_color
      )
    ) +
    ggplot2::scale_fill_identity(
      guide  = ggplot2::guide_legend(title = NULL),
      labels = legend_labels,
      breaks = legend_colors
    ) +
    # Linhas conectoras
    {
      if (!is.null(connector_data) && nrow(connector_data) > 0) {
        ggplot2::geom_segment(
          data = connector_data,
          ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          linetype  = "dashed",
          color     = "#AAAAAA",
          linewidth = 0.4
        )
      }
    } +
    # Rótulos de valor
    ggplot2::geom_text(
      ggplot2::aes(
        x     = x_pos,
        y     = label_y,
        label = fmt_value(value)
      ),
      size     = 3.2,
      fontface = "bold",
      color    = "#333333"
    ) +
    # Eixo X: preserva a ordem das linhas do Excel (não ordena alfabeticamente)
    ggplot2::scale_x_continuous(
      breaks = df_geom$x_pos,
      labels = df_geom$label,
      expand = ggplot2::expansion(add = 0.6)
    ) +
    # Eixo Y formatado em R$
    ggplot2::scale_y_continuous(
      labels = function(x) {
        paste0(
          currency_symbol,
          formatC(x, format = "f", digits = 0,
                  big.mark = ".", decimal.mark = ","),
          scale_suffix
        )
      },
      expand = ggplot2::expansion(mult = c(0.05, 0.14))
    ) +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      x        = NULL,
      y        = NULL,
      fill     = NULL
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = 15, color = "#1F3864"),
      plot.subtitle      = ggplot2::element_text(color = "#555555", size = 11),
      axis.text.x        = ggplot2::element_text(angle = 30, hjust = 1, size = 10),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "bottom",
      legend.text        = ggplot2::element_text(size = 9),
      plot.background    = ggplot2::element_rect(fill = "white", color = NA),
      panel.background   = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin        = ggplot2::margin(10, 20, 10, 10)
    )

  p
}


# ── 4. Exportar gráfico como base64 PNG ──────────────────────────────────────

export_plot_base64 <- function(plot, width_px = 1200, height_px = 650, dpi = 150) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  ggplot2::ggsave(
    filename = tmp,
    plot     = plot,
    width    = width_px / dpi,
    height   = height_px / dpi,
    dpi      = dpi,
    bg       = "white"
  )

  raw_bytes <- readBin(tmp, what = "raw", n = file.info(tmp)$size)
  encoded   <- base64enc::base64encode(raw_bytes)
  paste0("data:image/png;base64,", encoded)
}


# ── 5. Montar HTML compatível com Outlook ────────────────────────────────────

build_html_email <- function(data_uri, df_raw, title, subtitle,
                              report_date, footer_note = "",
                              currency_symbol = "R$ ") {

  fmt_brl <- function(x) {
    paste0(
      currency_symbol,
      formatC(abs(x), format = "f", digits = 0,
              big.mark = ".", decimal.mark = ",")
    )
  }

  tipo_label <- function(t) {
    switch(t,
      start    = "Início",
      increase = "Aumento",
      decrease = "Redução",
      subtotal = "Subtotal",
      total    = "Total",
      t
    )
  }

  # Linhas da tabela resumo com estilos inline (Outlook não processa CSS externo)
  rows_html <- paste0(
    sapply(seq_len(nrow(df_raw)), function(i) {
      bg    <- if (i %% 2 == 0) "#F5F7FA" else "#FFFFFF"
      nota  <- ifelse(is.na(df_raw$note[i]), "", df_raw$note[i])
      value_str <- fmt_brl(df_raw$value[i])
      # Valores negativos ficam em vermelho na tabela
      value_color <- if (df_raw$value[i] < 0) "color:#C00000;" else ""

      paste0(
        '<tr style="background-color:', bg, ';">',
        '<td style="padding:6px 10px;border-bottom:1px solid #E0E0E0;',
        'font-family:Calibri,Arial,sans-serif;font-size:12px;">',
        df_raw$label[i], '</td>',
        '<td style="padding:6px 10px;border-bottom:1px solid #E0E0E0;',
        'font-family:Calibri,Arial,sans-serif;font-size:12px;',
        'text-align:right;', value_color, '">',
        value_str, '</td>',
        '<td style="padding:6px 10px;border-bottom:1px solid #E0E0E0;',
        'font-family:Calibri,Arial,sans-serif;font-size:12px;">',
        tipo_label(df_raw$type[i]), '</td>',
        '<td style="padding:6px 10px;border-bottom:1px solid #E0E0E0;',
        'font-family:Calibri,Arial,sans-serif;font-size:12px;color:#777777;">',
        nota, '</td>',
        '</tr>'
      )
    }),
    collapse = "\n"
  )

  # HTML completo e auto-contido (sem JS, sem CSS externo, imagem em base64)
  html <- paste0(
    '<!DOCTYPE html>\n',
    '<html><head><meta charset="UTF-8"></head>\n',
    '<body style="margin:0;padding:20px;font-family:Calibri,Arial,sans-serif;',
    'background-color:#FFFFFF;color:#333333;">\n\n',

    '<h2 style="margin:0 0 4px 0;font-size:18px;font-weight:bold;',
    'font-family:Calibri,Arial,sans-serif;color:#1F3864;">',
    title, '</h2>\n',

    '<p style="margin:0 0 4px 0;font-size:12px;color:#555555;',
    'font-family:Calibri,Arial,sans-serif;">',
    subtitle, '</p>\n',

    '<p style="margin:0 0 16px 0;font-size:11px;color:#999999;',
    'font-family:Calibri,Arial,sans-serif;">',
    'Gerado em: ', report_date, '</p>\n\n',

    # Imagem com largura fixa de 700px — seguro para corpo de e-mail
    '<img src="', data_uri, '"\n',
    '     alt="Grafico Waterfall"\n',
    '     width="700"\n',
    '     style="width:700px;max-width:100%;display:block;border:0;">\n\n',

    '<hr style="border:0;border-top:1px solid #DDDDDD;margin:20px 0;">\n\n',

    '<h3 style="margin:0 0 8px 0;font-size:13px;font-weight:bold;',
    'font-family:Calibri,Arial,sans-serif;color:#1F3864;">Detalhamento</h3>\n\n',

    # Tabela com atributos cellpadding/cellspacing (respeitados pelo Word/Outlook)
    '<table cellpadding="0" cellspacing="0" width="700"\n',
    '       style="border-collapse:collapse;width:700px;',
    'font-family:Calibri,Arial,sans-serif;">\n',
    '  <thead>\n',
    '    <tr style="background-color:#1F3864;">\n',
    '      <th style="padding:8px 10px;text-align:left;font-family:Calibri,Arial,sans-serif;',
    'font-size:12px;color:#FFFFFF;font-weight:bold;">Descricao</th>\n',
    '      <th style="padding:8px 10px;text-align:right;font-family:Calibri,Arial,sans-serif;',
    'font-size:12px;color:#FFFFFF;font-weight:bold;">Valor</th>\n',
    '      <th style="padding:8px 10px;text-align:left;font-family:Calibri,Arial,sans-serif;',
    'font-size:12px;color:#FFFFFF;font-weight:bold;">Tipo</th>\n',
    '      <th style="padding:8px 10px;text-align:left;font-family:Calibri,Arial,sans-serif;',
    'font-size:12px;color:#FFFFFF;font-weight:bold;">Observacao</th>\n',
    '    </tr>\n',
    '  </thead>\n',
    '  <tbody>\n',
    rows_html, '\n',
    '  </tbody>\n',
    '</table>\n\n',

    '<p style="margin:24px 0 0 0;font-size:10px;color:#AAAAAA;',
    'font-family:Calibri,Arial,sans-serif;">',
    footer_note, '</p>\n\n',

    '</body>\n</html>'
  )

  html
}


# ── 6. Gerar template Excel de exemplo ───────────────────────────────────────

#' Cria um arquivo Excel de exemplo com dados de bridge financeiro.
#' Requer o pacote writexl: install.packages("writexl")
#'
#' @param path Caminho do arquivo .xlsx a ser criado
create_template_excel <- function(path = "waterfall_data_template.xlsx") {
  template_data <- data.frame(
    label = c(
      "Receita Bruta",
      "Deducoes",
      "Receita Liquida",
      "CMV",
      "Lucro Bruto",
      "Despesas Operacionais",
      "EBITDA",
      "D&A",
      "EBIT"
    ),
    value = c(
      1000000,
      -150000,
       850000,
      -300000,
       550000,
      -120000,
       430000,
       -30000,
       400000
    ),
    type = c(
      "start",
      "decrease",
      "subtotal",
      "decrease",
      "subtotal",
      "decrease",
      "subtotal",
      "decrease",
      "total"
    ),
    note = c(
      "",
      "Impostos e devolucoes",
      "",
      "Custo dos Produtos Vendidos",
      "",
      "",
      "Lucro antes de juros, impostos e amortizacoes",
      "Depreciacao e Amortizacao",
      ""
    ),
    stringsAsFactors = FALSE
  )

  writexl::write_xlsx(template_data, path)
  message("Template Excel criado em: ", path)
  message("Abra o arquivo, ajuste os dados e execute waterfall_chart.R")
}
