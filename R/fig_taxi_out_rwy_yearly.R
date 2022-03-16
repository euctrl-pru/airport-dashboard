
TXOTRWY_YY <- params$txotrwy_yy %>%
  tidyr::pivot_longer(
    cols      = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
    names_to  = "TYPE",
    values_to = "TIME"
  )

TXOTRWY_YY <- TXOTRWY_YY %>%
  mutate(
    TYPE   = factor(TYPE,
                    levels = c("AVG_UNIMP_TIME", "AVG_ADD_TIME"),
                    labels = c("Avg. Reference time", "Avg. Additional time")
    ))

TXOTRWY_YY <- TXOTRWY_YY %>%
  mutate(YEAR = as.character(YEAR))


TXOTRWY_YY %>%
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
      title       = "Average Taxi-out time per runway [min/dep]",
      titlefont   = list(size = 11),
      hoverformat = ".2f"
    )
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )