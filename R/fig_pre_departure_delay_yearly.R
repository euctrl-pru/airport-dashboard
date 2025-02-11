
#PDDLY_YY <- params$pddly_yy %>%
#  select(YEAR, TOT_DLY_89, TOT_DLY_OTHER, TOT_DLY_UNID) %>%
#  tidyr::pivot_longer(
#    cols      = starts_with("TOT_DLY_"),
#    names_to  = "DLY_CAT",
#    values_to = "DLY_DUR"
#  )
#
#predep_col <- c(
#  "#F8CBAD", # RGB 248,203,173 # Unidentified
#  "#BDD7EE", # RGB 189,215,238 # Other reasons
#  "#4682B4"
#) # RGB 198,224,180 # ATC Pre-Departure delay (code 89)
#
#PDDLY_YY$DLY_CAT <- factor(PDDLY_YY$DLY_CAT,
#                           levels = c("TOT_DLY_UNID", "TOT_DLY_OTHER", "TOT_DLY_89"),
#                           labels = c("Unidentified", "Other reasons", "ATC Pre-Departure <br>delay (code 89)")
#)
#
# PDDLY_YY <- PDDLY_YY %>%
#   mutate(YEAR = as.character(YEAR))
#
#PDDLY_YY_annot=group_by(PDDLY_YY, YEAR) %>% 
#  summarise(DLY_DUR_TOT=sum(DLY_DUR, na.rm = TRUE))
#
#PDDLY_YY=left_join(PDDLY_YY, PDDLY_YY_annot)
#
#PDDLY_YY %>%
#  plot_ly(
#    x             = ~YEAR,
#    y             = ~DLY_DUR,
#    type          = "bar",
#    color         = ~DLY_CAT,
#    colors        = predep_col,
#    # legendgroup = ~DLY_CAT,
#    hovertemplate = "%{y:.r}"
#  ) %>%
#  add_annotations(
#      data      = PDDLY_YY %>% select(YEAR, DLY_DUR_TOT) %>% unique(),
#      x         = ~ YEAR,
#      y         = ~ DLY_DUR_TOT + 100000,
#      text      = ~ paste0(round(DLY_DUR_TOT/1000000, 2), "M"),
#      showarrow = FALSE
#    ) %>%
#  layout(
#    barmode   = "stack",
#    hovermode = "x unified",
#    xaxis     = list(title = ""),
#    yaxis     = list(
#      title     = "Pre-Departure delay [min]",
#      titlefont = list(size = 11)
#    )
#    ##############################################################################
#    # ,annotations = list(
#    #   x         = 1,
#    #   y         = -0.1,
#    #   text      = "Source: APDF",
#    #   showarrow = FALSE,
#    #   xref      = "paper",
#    #   yref      = "paper",
#    #   xanchor   = "right",
#    #   yanchor   = "auto",
#    #   xshift    = 0,
#    #   yshift    = -5,
#    #   font      = list(size = 12)
#    # )
#    ##############################################################################
#  ) %>%
#  config(
#    displaylogo = FALSE,
#    modeBarButtonsToRemove = config_bar_remove_buttons
#  ) %>% 
# add_download_button(PDDLY_YY)


DLY_YY <- params$dly_yy %>%
  select(YEAR, TOT_DLY_AIRLINE, 
         TOT_DLY_WEATHER, 
         TOT_DLY_EN_ROUTE, 
         TOT_DLY_SECURITY_AND_IMMIGRATION, 
         TOT_DLY_AIRPORT,
         TOT_DLY_REACTIONARY,
         TOT_DLY_MISCELLANEOUS,
         TOT_DLY_UNIDENTIFIED, 
         TOT_DLY_OTHER) %>%
  tidyr::pivot_longer(
    cols      = starts_with("TOT_DLY_"),
    names_to  = "DLY_CAT",
    values_to = "DLY_DUR"
  )

DLY_YY$DLY_CAT <- factor(DLY_YY$DLY_CAT,
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

DLY_YY_annot=group_by(DLY_YY, YEAR) %>% 
  summarise(DLY_DUR_TOT=sum(DLY_DUR, na.rm = TRUE))

DLY_YY=left_join(DLY_YY, DLY_YY_annot)

if (length(unique(DLY_YY$YEAR))==1) {
  DLY_YY = mutate(DLY_YY, YEAR=as.character(YEAR))
}
  

plot = DLY_YY %>%
  plot_ly(
    x             = ~YEAR,
    y             = ~DLY_DUR,
    type          = "bar",
    color         = ~DLY_CAT,
    #colors        = predep_col,
    # legendgroup = ~DLY_CAT,
    hovertemplate = "%{y:.r}"
  ) %>%
  layout(
    barmode   = "stack",
    hovermode = "x unified",
    xaxis     = list(title = ""),
    yaxis     = list(
      title     = "Pre-Departure delay [min]",
      titlefont = list(size = 11)
    )
  ) %>%
  config(
    displaylogo = FALSE,
    modeBarButtonsToRemove = config_bar_remove_buttons
  ) %>% 
  add_download_button(
    DLY_YY %>% select(YEAR, DLY_CAT, DLY_DUR), 
    "PRE_DEP_DLY_YY")

if (length(unique(DLY_YY$YEAR))>1) {
  plot %>%
    add_annotations(
      data      = DLY_YY %>% select(YEAR, DLY_DUR_TOT) %>% unique(),
      x         = ~ YEAR,
      y         = ~ DLY_DUR_TOT + 100000,
      text      = ~ paste0(round(DLY_DUR_TOT/1000000, 2), "M"),
      showarrow = FALSE
    )
} else {
  plot
}


# Factsheet figure

DLY_YY_fig = ggplot(data=DLY_YY) +
  geom_bar(aes(x=YEAR, y=DLY_DUR/1000000, fill=DLY_CAT), stat="identity", position = position_stack(reverse = TRUE)) +
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
  scale_y_continuous(labels = label_number(suffix = "M"))
ggsave(here("media", "factsheet", paste0("Pre_Departure_Delay_Yearly_", params$icao, ".pdf")), plot=DLY_YY_fig, 
       width = Pre_dep_delay_layout1[1]*Page_width, height = Pre_dep_delay_height1, units = "cm", dpi=100, limitsize = FALSE)

