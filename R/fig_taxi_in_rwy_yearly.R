
TXINRWY_YY <- params$txinrwy_yy %>%
  tidyr::pivot_longer(
    cols      = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
    names_to  = "TYPE",
    values_to = "TIME"
  )

TXINRWY_YY <- TXINRWY_YY %>%
  mutate(
    TYPE   = factor(TYPE,
                    levels = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
                    labels = c("Avg. Reference time", "Avg. Additional time")
    ))

TXINRWY_YY <- TXINRWY_YY %>%
  mutate(YEAR = as.character(YEAR))


TXINRWY_YY %>%
  plotly::plot_ly(
    x          = ~YEAR,
    y          = ~TIME,
    type       = "bar",
    showlegend = TRUE,
    color      = ~TYPE
  ) %>%
  layout(
    barmode   = "stack",
    hovermode = "x unified",
    xaxis     = list(title = ""),
    yaxis     = list(
      title       = "Average Taxi-In time per runway [min/dep]",
      titlefont   = list(size = 11),
      hoverformat = ".2f"
    )
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )