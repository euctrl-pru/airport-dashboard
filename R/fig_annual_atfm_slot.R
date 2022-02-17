
SLOT_YY_PLOT2 <- params$slot_yy %>%
  select(YEAR, TOT_FLT_DEP_OUT_LATE_1, TOT_FLT_DEP_OUT_EARLY_1, TOT_FLT_DEP_IN_1) %>%
  tidyr::pivot_longer(
    cols = c(TOT_FLT_DEP_OUT_LATE_1, TOT_FLT_DEP_OUT_EARLY_1, TOT_FLT_DEP_IN_1),
    names_to  = "SLOT_CAT",
    values_to = "NB_SLOT"
  )

slot_col <- c(
  "#BDD7EE", # blue
  "#C6E0B4", # green
  "#F8CBAD"
) # orange

SLOT_YY_PLOT2$SLOT_CAT <- factor(SLOT_YY_PLOT2$SLOT_CAT,
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

SLOT_YY_PLOT2 <- SLOT_YY_PLOT2 %>%
  mutate(YEAR = as.character(YEAR))

SLOT_YY_PLOT2 %>%
  plot_ly(
    x             = ~YEAR,
    y             = ~NB_SLOT,
    type          = "bar",
    color         = ~SLOT_CAT,
    colors        = slot_col,
    hovertemplate = "%{y:.r}"
  ) %>%
  layout(
    barmode   = "stack",
    hovermode = "x unified",
    xaxis     = list(title = ""),
    yaxis     = list(
      title     = "Number of Regulated departures",
      titlefont = list(size = 11)
    )
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
    #   yshift    = -5,
    #   font      = list(size = 12)
    # )
    ##############################################################################
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  )





# Factsheet figure

slot_pa_fig = ggplot(data=SLOT_YY_PLOT2) +
  geom_bar(aes(x=YEAR, y=NB_SLOT, fill=SLOT_CAT), stat="identity") +
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
ggsave(here("R", "Factsheet", "Figures", paste0("Annual_ATFM_Slot_Adherence_", params$icao, ".png")), plot=slot_pa_fig, 
       width = ATFM_Slot_Adherence_layout1[1]*Page_width, height = ATFM_Slot_Adherence_height1, units = "cm", dpi=100, 
       limitsize = FALSE)

