
SLOT_MM_PLOT2 <- params$slot_mm %>%
  select(YEAR,
         MONTH_NUM,
         TOT_FLT_DEP_OUT_LATE_1,
         TOT_FLT_DEP_OUT_EARLY_1,
         TOT_FLT_DEP_IN_1) %>%
  tidyr::pivot_longer(
    cols = c(TOT_FLT_DEP_OUT_LATE_1, TOT_FLT_DEP_OUT_EARLY_1, TOT_FLT_DEP_IN_1),
    names_to = "SLOT_CAT",
    values_to = "NB_SLOT"
  )

slot_col <- c(
  "#BDD7EE", # blue
  "#C6E0B4", # green
  "#F8CBAD"
) # orange

SLOT_MM_PLOT2$SLOT_CAT <- factor(SLOT_MM_PLOT2$SLOT_CAT,
                                 levels = c(
                                   "TOT_FLT_DEP_OUT_EARLY_1",
                                   "TOT_FLT_DEP_IN_1",
                                   "TOT_FLT_DEP_OUT_LATE_1"
                                 ),
                                 labels = c(
                                   "Before the STW<br>(Early Take-Off Traffic)",
                                   "Within the STW<br>[-5 minutes, +10 minutes]",
                                   "After the STW<br>(Late Take-Off Traffic)"
                                 )
)

filter_years <- SLOT_MM_PLOT2 %>%
  pull(YEAR) %>%
  unique()

max_year <- max(filter_years)

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

SLOT_MM_PLOT2 %>%
  plot_ly(
    x             = ~MONTH_NUM,
    y             = ~NB_SLOT,
    customdata    = ~YEAR,
    color         = ~SLOT_CAT,
    colors        = slot_col,
    hovertemplate = "%{y:.r}",
    type          = "bar",
    transforms    = list(
      list(
        type        = "filter",
        target      = "customdata",
        operation   = "=",
        value       = max_year
      )
    )
  ) %>%
  layout(
    barmode   = "stack",
    hovermode = "x unified",
    xaxis     = tick_yr_no_title,
    yaxis     = list(
      title     = "Number of Regulated departures",
      titlefont = list(size = 11)
    ),
    showlegend = TRUE,
    updatemenus = list(button_type_list)
    ##############################################################################
    # ,annotations = list(
    #   x         = 1,
    #   y         = -0.1,
    #   text      = "Source: Network Manager",
    #   showarrow = FALSE,
    #   xref      = "paper",
    #   yref      = "paper",
    #   xanchor   = "right",
    #   yanchor   = "auto",
    #   xshift    = 0,
    #   yshift    = -10,
    #   font      = list(size = 12)
    # )
    ##############################################################################
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )





# Factsheet figure

slot_pm_curr_year=filter(SLOT_MM_PLOT2, YEAR == max(YEAR)) %>% 
  mutate(Month=factor(month.abb[MONTH_NUM], levels = month.abb))

slot_pm_fig = ggplot(data=slot_pm_curr_year) +
  geom_bar(aes(x=Month, y=NB_SLOT, fill=SLOT_CAT), stat="identity") +
  scale_fill_manual(values=slot_col) +
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
  labs(x="", y="Number of regulated departures\n")
ggsave(here("R", "Factsheet", "Figures", paste0("Monthly_ATFM_Slot_Adherence_", params$icao, ".png")), plot=slot_pm_fig, 
       width = ATFM_Slot_Adherence_layout1[2]*Page_width, height = ATFM_Slot_Adherence_height1, units = "cm", dpi=100, 
       limitsize = FALSE)

