
# monthly trend

if (nrow(params$atfm) > 0) {
  atfm_pm <- params$atfm %>%
    select(
      YEAR,
      MONTH_NUM,
      FLT_ARR_1,
      DLY_APT_ARR_1,
      AD_DISRUPTION,
      AD_CAPACITY,
      AD_WEATHER,
      AD_DISRUPTION_ATC,
      AD_CAPACITY_ATC,
      AD_STAFFING_ATC,
      AD_EVENTS
    ) %>%
    group_by(YEAR, MONTH_NUM) %>%
    summarise(
      FLT_ARR_TOT       = sum(FLT_ARR_1, na.rm = TRUE),
      DLY_APT_ARR_TOT   = sum(DLY_APT_ARR_1, na.rm = TRUE),
      AD_DISRUPTION     = sum(AD_DISRUPTION, na.rm = TRUE),
      AD_CAPACITY       = sum(AD_CAPACITY, na.rm = TRUE),
      AD_WEATHER        = sum(AD_WEATHER, na.rm = TRUE),
      AD_DISRUPTION_ATC = sum(AD_DISRUPTION_ATC, na.rm = TRUE),
      AD_CAPACITY_ATC   = sum(AD_CAPACITY_ATC, na.rm = TRUE),
      AD_STAFFING_ATC   = sum(AD_STAFFING_ATC, na.rm = TRUE),
      AD_EVENTS         = sum(AD_EVENTS, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    tidyr::pivot_longer(
      cols      = starts_with("AD_"),
      names_to  = "REG_REASON",
      values_to = "DLY"
    ) %>%
    mutate(
      AVG_ARR_ATFM_DLY = DLY_APT_ARR_TOT / FLT_ARR_TOT,
      AVG_ARR_ATFM_REG = DLY / FLT_ARR_TOT
    )
  
  
  filter_years <- atfm_pm %>%
    pull(YEAR) %>%
    unique()
  
  max_year <- max(filter_years)
  
  ########################## KEY MESSAGE #########################################
  
  
  MONTHLY_HIGH_ATFM <- atfm_pm %>%
    select(YEAR, MONTH_NUM, REG_REASON, AVG_ARR_ATFM_DLY, AVG_ARR_ATFM_REG) %>%
    group_by(YEAR) %>%
    filter(AVG_ARR_ATFM_DLY != 0) %>%
    filter(AVG_ARR_ATFM_REG != 0) %>%
    filter(AVG_ARR_ATFM_DLY == max(AVG_ARR_ATFM_DLY)) %>%
    filter(AVG_ARR_ATFM_REG == max(AVG_ARR_ATFM_REG)) %>% 
    arrange(YEAR) %>% 
    unique() %>% 
    mutate(
      REG_REASON = factor(REG_REASON,
                          levels     = atfm_grps,
                          labels     = atfm_lbl
      ))
  
  ###########################################################################
  
  annotations = vector(mode = "list", length = length(filter_years))
  for (i in 1:length(filter_years)) {
    MONTHLY_HIGH_ATFM_temp=filter(MONTHLY_HIGH_ATFM, YEAR==filter_years[i])
    if (nrow(MONTHLY_HIGH_ATFM_temp)>0) {
      annotation <- list(x = MONTHLY_HIGH_ATFM_temp$MONTH_NUM,
                         y = MONTHLY_HIGH_ATFM_temp$AVG_ARR_ATFM_DLY,
                         xref='x',
                         yref='y',
                         text = paste0(
                           "<b>Highest ATFM delay:<br>",
                           round(MONTHLY_HIGH_ATFM_temp$AVG_ARR_ATFM_DLY, 2), " min/arr<br>(",
                           round(MONTHLY_HIGH_ATFM_temp$AVG_ARR_ATFM_REG/
                                   MONTHLY_HIGH_ATFM_temp$AVG_ARR_ATFM_DLY*100, 0),
                           "% ", MONTHLY_HIGH_ATFM_temp$REG_REASON, ")</b>"),
                         yshift=40,
                         xshift = ifelse(MONTHLY_HIGH_ATFM_temp$MONTH_NUM==1,
                                         40,
                                         ifelse(MONTHLY_HIGH_ATFM_temp$MONTH_NUM==12, -40, 0)),
                         align = ifelse(MONTHLY_HIGH_ATFM_temp$MONTH_NUM==1,
                                        "left",
                                        ifelse(MONTHLY_HIGH_ATFM_temp$MONTH_NUM==12, "right", "center")),
                         showarrow = FALSE)
    } else {
      annotation <- list(x = 6,
                         y = 1,
                         xref='x',
                         yref='y',
                         text = "", #paste0("<b>No ATFM delay in ", filter_years[i], "</b>"),
                         yshift=0,
                         xshift = 0,
                         align = "center",
                         showarrow = FALSE)
    }
    
    annotations[[i]] <- annotation
    
  }
  
  # source_annotation=list(x         = 1,
  #                        y         = -0.1,
  #                        text      = "Source: Network Manager",
  #                        showarrow = F,
  #                        xref      = 'paper',
  #                        yref      = 'paper',
  #                        xanchor   = 'right',
  #                        yanchor   = 'auto',
  #                        xshift    = 0,
  #                        yshift    = -10,
  #                        font      = list(size=12))
  
  button_list=vector(mode = "list", length = length(filter_years))
  annot_list=vector(mode = "list", length = length(filter_years))
  for(x in 1:length(filter_years)){
    annot_list_temp=annot_list
    annot_list_temp[[x]]=annotations[[x]]
    if (!is.null(annotations[[x]])) {
      bl=list(
        label  = filter_years[x],
        method = "update",
        args   = list(list("transforms[0].value"=filter_years[x]),
                      list(annotations=list(annot_list_temp[[x]]
                                            # , source_annotation
                      )))
      )
    } else {
      bl=list(
        label  = filter_years[x],
        method = "update",
        args   = 
          # list(
          list("transforms[0].value"=filter_years[x])
        # , list(annotations=list(source_annotation))
        # )
      )
    }
    button_list[[x]]=bl
  }
  
  button_type_list <- list(
    type      = "buttons",
    bgcolor   = "#c3c1e8",
    active    = length(filter_years) - 1,
    direction = "right",
    xref      = "paper",
    x         = 0.3,
    xanchor   = "left",
    yref      = "paper",
    y         = 1.2,
    yanchor   = "bottom",
    buttons   = button_list
  )
  
  # set REG_REASON grouping and colors
  
  atfm_pm <- atfm_pm %>%
    mutate(
      REG_REASON = factor(REG_REASON,
                          levels     = atfm_grps,
                          labels     = atfm_lbl
      ))
  
  atfm_pm_fig <- atfm_pm %>%
    plot_ly(
      x          = ~MONTH_NUM,
      y          = ~AVG_ARR_ATFM_REG,
      customdata = ~YEAR,
      color      = ~REG_REASON,
      colors     = atfm_col,
      type       = "bar",
      transforms = list(
        list(
          type      = "filter",
          target    = "customdata",
          operation = "=",
          value     = max_year
        )
      )
    ) %>%
    # add_annotations(x         = 1,
    #                 y         = -0.1,
    #                 text      = "Source: Network Manager",
    #                 showarrow = F,
    #                 xref      = 'paper',
    #                 yref      = 'paper',
    #                 xanchor   = 'right',
    #                 yanchor   = 'auto',
    #                 xshift    = 0,
    #                 yshift    = -10,
    #                 font      = list(size=12)
  # ) %>%
  layout(
    barmode   = "stack",
    hovermode = "x unified",
    xaxis     = tick_yr_no_title,
    yaxis     = list(
      title       = "Average Arrival ATFM delay [min/arr]",
      titlefont   = list(size = 11),
      hoverformat = ".2f",
      rangemode   = "tozero"
    ),
    showlegend = TRUE,
    # legend = list(
    #   orientation = "h",
    #   xanchor="center",
    #   x = 0.5,
    #   y = -0.2
    # ),
    updatemenus = list(button_type_list)
  ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = config_bar_remove_buttons
    ) %>% 
    add_download_button(atfm_pm %>%
      select(YEAR, MONTH = MONTH_NUM, REG_REASON, AVG_ARR_ATFM_DLY = AVG_ARR_ATFM_REG), 
      "ARR_ATFM_DLY_MM")

  if (!is.null(annotations[[length(filter_years)]])) {
    atfm_pm_fig=atfm_pm_fig %>% 
      add_annotations(x         = annotations[[length(filter_years)]]$x,
                      y         = annotations[[length(filter_years)]]$y,
                      text      = annotations[[length(filter_years)]]$text,
                      showarrow = annotations[[length(filter_years)]]$showarrow,
                      xref      = annotations[[length(filter_years)]]$xref,
                      yref      = annotations[[length(filter_years)]]$yref,
                      yshift    = annotations[[length(filter_years)]]$yshift,
                      xshift    = annotations[[length(filter_years)]]$xshift,
                      align     = annotations[[length(filter_years)]]$align
      )
  }
  
  atfm_pm_fig
  
}





# Factsheet figure
atfm_pm_curr_year=filter(atfm_pm, YEAR == max(YEAR)) %>% 
  mutate(Month=factor(month.abb[MONTH_NUM], levels = month.abb))

atfm_pm_fig = ggplot(data=atfm_pm_curr_year) +
  geom_bar(aes(x=Month, y=AVG_ARR_ATFM_REG, fill=REG_REASON), stat="identity") +
  scale_fill_manual(values=atfm_col) +
  theme_factsheet() +
  # theme_bw() +
  # theme(plot.title = element_blank(),
  #       legend.title=element_blank(),
  #       legend.text=element_text(size=36),
  #       legend.position = "bottom",
  #       axis.text=element_text(size=100),
  #       axis.title=element_text(size=100),
  #       axis.title.x = element_blank(),
  #       plot.margin = unit(c(5.5, 20, 5.5, 60), "pt")) +
  labs(x="", y="Average arrival ATFM delay (min/arr)\n")
ggsave(here("media", "factsheet", paste0("Monthly_Arr_ATFM_Delay_", params$icao, ".pdf")), plot=atfm_pm_fig, 
       width = Arr_ATFM_Delay_layout1[2]*Page_width, height = Arr_ATFM_Delay_height1, units = "cm", dpi=100, limitsize = FALSE)

