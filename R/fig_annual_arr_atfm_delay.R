
if (nrow(params$atfm) > 0) {
  atfm_pa <- params$atfm %>%
    select(
      YEAR, FLT_ARR_1, DLY_APT_ARR_1, AD_DISRUPTION, AD_CAPACITY, AD_WEATHER,
      AD_DISRUPTION_ATC, AD_CAPACITY_ATC, AD_STAFFING_ATC, AD_EVENTS
    ) %>%
    group_by(YEAR) %>%
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
  
  # ------------------ ATFM Grouping and Colour Definition ------------------
  
  atfm_col <- c(
    "#595959", # rgb2hex(89,89,89)    # AD Events
    "#B9CDE5", # rgb2hex(185,205,229) # AD Disruption
    "#E6B9B8", # rgb2hex(230,185,184) # AD Capacity
    "#9BBB59", # rgb2hex(155,187,89)  # AD Weather
    "#4F81BD", # rgb2hex(79,129,189)  # AD Disruption ATC
    "#F79646", # rgb2hex(247,150,70)  # Staffing ATC
    "#C0504D" # rgb2hex(192,80,77)   # AD Capacity ATC
  )
  
  atfm_grps <- c(
    "AD_EVENTS",
    "AD_DISRUPTION",
    "AD_CAPACITY",
    "AD_WEATHER",
    "AD_DISRUPTION_ATC",
    "AD_STAFFING_ATC",
    "AD_CAPACITY_ATC"
  )
  
  atfm_lbl <- c(
    "Events",
    "Disruption",
    "Capacity",
    "Weather",
    "Disruption (ATC)",
    "Staffing (ATC)",
    "Capacity (ATC)"
  )
  
  # -------------------------------------------------------------------------
  
  atfm_pa <- atfm_pa %>%
    mutate(REG_REASON = factor(REG_REASON,
                               levels = atfm_grps,
                               labels = atfm_lbl
    ))
  
  atfm_pa %>%
    plot_ly(
      x      = ~YEAR,
      y      = ~AVG_ARR_ATFM_REG,
      color  = ~REG_REASON,
      colors = atfm_col,
      type   = "bar"
    ) %>%
    add_annotations(
      data      = atfm_pa %>% select(YEAR, AVG_ARR_ATFM_DLY) %>% unique(),
      x         = ~YEAR,
      y         = ~ AVG_ARR_ATFM_DLY + 0.03,
      text      = ~ round(AVG_ARR_ATFM_DLY, 2),
      showarrow = FALSE
    ) %>%
    layout(
      barmode   = "stack",
      hovermode = "x unified",
      xaxis     = list(title = ""),
      yaxis     = list(
        title       = "Average Arrival ATFM delay [min/arr]",
        titlefont   = list(size = 11),
        hoverformat = ".2f"
      )
      ##############################################################################
      # , annotations = list(
      #   x         = 1,
      #   y         = -0.1,
      #   text      = "Source: Network Manager",
      #   showarrow = FALSE,
      #   xref      = "paper",
      #   yref      = "paper",
      #   xanchor   = "right",
      #   yanchor   = "auto",
      #   xshift    = 0,
      #   yshift    = -5,
      #   font      = list(size = 12)
      # )
      ##############################################################################
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtonsToRemove = config_bar_remove_buttons
    )
}





# Factsheet figure

atfm_pa_fig = ggplot(data=atfm_pa) +
  geom_bar(aes(x=YEAR, y=AVG_ARR_ATFM_REG, fill=REG_REASON), stat="identity") +
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
ggsave(here("R", "Factsheet", "Figures", paste0("Annual_Arr_ATFM_Delay_", params$icao, ".png")), plot=atfm_pa_fig, 
       width = Arr_ATFM_Delay_layout1[1]*Page_width, height = Arr_ATFM_Delay_height1, units = "cm", dpi=100, limitsize = FALSE)

