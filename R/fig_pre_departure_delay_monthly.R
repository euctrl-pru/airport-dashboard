
PDDLY_MM <- params$pddly_mm %>%
  select(
    YEAR,
    MONTH_NUM,
    TOT_DLY_89,
    TOT_DLY_OTHER,
    TOT_DLY_UNID
  ) %>%
  tidyr::pivot_longer(
    cols      = starts_with("TOT_DLY_"),
    names_to  = "DLY_CAT",
    values_to = "DLY_DUR"
  )

#PDDLY_MM$DLY_CAT <- factor(
#  PDDLY_MM$DLY_CAT,
#  levels = c("TOT_DLY_UNID", "TOT_DLY_OTHER", "TOT_DLY_89"),
#  labels = c("Unidentified", "Other reasons", "ATC Pre-Departure <br> delay (code 89)")
#)

#predep_col <- c(
#  "#F8CBAD", # RGB 248,203,173 # Unidentified
#  "#BDD7EE", # RGB 189,215,238 # Other reasons
#  "#4682B4"
#) # RGB 198,224,180 # ATC Pre-Departure delay (code 89)

DLY_MM <- params$dly_mm %>%
  select( YEAR, MONTH,
          TOT_DLY_AIRLINE, 
          TOT_DLY_WEATHER, 
          TOT_DLY_EN_ROUTE, 
          TOT_DLY_SECURITY_AND_IMMIGRATION, 
          TOT_DLY_AIRPORT,
          TOT_DLY_REACTIONARY,
          TOT_DLY_MISCELLANEOUS,
          TOT_DLY_UNIDENTIFIED, 
          TOT_DLY_OTHER
  ) %>%
  tidyr::pivot_longer(
    cols      = starts_with("TOT_DLY_"),
    names_to  = "DLY_CAT",
    values_to = "DLY_DUR"
  )


DLY_MM$DLY_CAT <- factor(DLY_MM$DLY_CAT,
                         levels = c( "TOT_DLY_AIRLINE",
                                     "TOT_DLY_WEATHER",
                                     "TOT_DLY_EN_ROUTE",
                                     "TOT_DLY_SECURITY_AND_IMMIGRATION", 
                                     "TOT_DLY_AIRPORT",
                                     "TOT_DLY_REACTIONARY",
                                     "TOT_DLY_MISCELLANEOUS",
                                     "TOT_DLY_UNIDENTIFIED", 
                                     "TOT_DLY_OTHER"),
                         labels = c("Airline","Weather", "En-Route", "Security&Immig.","Airport","Reactionnary","Miscel.","Unidentified","Other"))

filter_years <- DLY_MM %>%
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

DLY_MM %>%
  plot_ly(
    x             = ~MONTH,
    y             = ~DLY_DUR,
    type          = "bar",
    color         = ~DLY_CAT,
    #colors        = predep_col,
    # legendgroup = ~DLY_CAT,
    hovertemplate = "%{y:.r}",
    customdata    = ~YEAR,
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
    barmode     = "stack",
    xaxis       = tick_yr_no_title,
    yaxis       = list(
      title     = "Pre-Departure delay [min]",
      titlefont = list(size = 11)
    ),
    showlegend = TRUE,
    updatemenus = list(button_type_list)
    ##############################################################################
    # ,annotations = list(
    #   x         = 1,
    #   y         = -0.1,
    #   text      = "Source: APDF",
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
  ) %>% 
  add_download_button(
    DLY_MM %>% select(YEAR, MONTH, DLY_CAT, DLY_DUR), 
    "PRE_DEP_DLY_MM")



# Factsheet figure

DLY_MM_curr_year=filter(DLY_MM, YEAR==max_year) %>% 
  mutate(Month=factor(month.abb[MONTH], levels = month.abb))

DLY_MM_fig = ggplot(data=filter(DLY_MM_curr_year, YEAR==max_year)) +
  geom_bar(aes(x=Month, y=DLY_DUR/1000, fill=DLY_CAT), stat="identity", position = position_stack(reverse = TRUE)) +
  # scale_fill_manual(values=slot_col) +
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
  labs(x="", y="Pre-Departure delay (min)\n") +
  scale_y_continuous(labels = label_number(suffix = "k"))
ggsave(here("media", "factsheet", paste0("Pre_Departure_Delay_Monthly_", params$icao, ".pdf")), plot=DLY_MM_fig, 
       width = Pre_dep_delay_layout1[2]*Page_width, height = Pre_dep_delay_height1, units = "cm", dpi=100, limitsize = FALSE)

