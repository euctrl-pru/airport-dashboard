
if (nrow(params$atfm) > 0) {
  
  share_atfm <- atfm_pm %>%
    # filter(YEAR == atfm_max_year) %>%
    group_by(REG_REASON, YEAR) %>%
    summarise(
      FLT_ARR_TOT = sum(FLT_ARR_TOT, na.rm = TRUE),
      DLY         = sum(DLY, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(YEAR) %>% 
    mutate(
      DLY_TOT   = sum(DLY, na.rm = TRUE),
      DLY_SHARE = round(DLY / DLY_TOT, 2),
      LABELS    = paste0(REG_REASON, "<br>", DLY_SHARE*100, "%")
    ) %>%
    filter(DLY_SHARE > 0) %>% 
    left_join(data.frame(cbind(REG_REASON=atfm_lbl, Colours=atfm_col))) %>%
    mutate(
      REG_REASON = factor(REG_REASON,
                          levels     = rev(atfm_lbl),
                          labels     = rev(atfm_lbl)
      )) %>% 
    arrange(YEAR, REG_REASON)
  
  filter_years <- share_atfm %>%
    pull(YEAR) %>%
    unique() %>% 
    sort()
  
  atfm_max_year <- atfm_pm %>%
    pull(YEAR) %>%
    max() %>%
    unique()
  
  button_list <- lapply(
    1:length(filter_years),
    function(x) {
      list(
        method = "restyle",
        args = list("transforms[0].value", filter_years[x]),
        label = filter_years[x]
      )
    }
  )
  
  button_type_list <- list(
    buttons   = button_list,
    type      = "buttons",
    bgcolor   = "#c3c1e8",
    active    = length(filter_years) - 1,
    direction = "right",
    xref      = "paper",
    x         = 0.3,
    xanchor   = "left",
    yref      = "paper",
    y         = 1.2,
    yanchor   = "bottom"
  )
  
  share_atfm %>%
    plot_ly(
      labels        = ~REG_REASON,
      values        = ~DLY,
      text          = ~paste0(DLY_SHARE*100, "%"),
      textinfo      = 'text',
      sort          = FALSE,
      name          = "",
      hovertemplate = ~paste0(REG_REASON, ': ', DLY, ' min. (', DLY_SHARE*100, '%)'),
      marker        = list(colors = ~Colours),
      customdata    = ~YEAR,
      transforms    = list(
        list(
          type        = "filter",
          target      = "customdata",
          operation   = "=",
          value       = atfm_max_year
        )
      )
    ) %>%
    add_pie(
      hole         = 0.4,
      textposition = "outside"
    ) %>%
    layout(
      showlegend  = TRUE,
      updatemenus = list(button_type_list)) %>%
    config(
      displaylogo            = FALSE,
      modeBarButtonsToRemove = config_bar_remove_buttons
    )
  
}