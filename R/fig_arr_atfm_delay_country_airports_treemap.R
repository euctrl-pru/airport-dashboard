
# Factsheet figure

ARR_ATFM_delay_country_apts1 <- filter(ATFM_DF, substr(AIRPORT, 1, 2)==substr(Airport, 1, 2) & YEAR==Last_complete_year)  %>%
  select(
    AIRPORT,
    YEAR,
    MONTH_NUM,
    FLT_ARR_1,
    DLY_APT_ARR_1
  ) %>%
  group_by(AIRPORT, YEAR) %>%
  summarise(
    FLT_ARR_TOT       = sum(FLT_ARR_1, na.rm = TRUE),
    DLY_APT_ARR_TOT   = sum(DLY_APT_ARR_1, na.rm = TRUE)) %>% 
  arrange(-FLT_ARR_TOT) %>% 
  ungroup() %>% 
  select(-YEAR, -FLT_ARR_TOT)

if (nrow(ARR_ATFM_delay_country_apts1)>6) {
  
  ARR_ATFM_delay_country_apts_other=ARR_ATFM_delay_country_apts1[7:nrow(ARR_ATFM_delay_country_apts1),]
  
  ARR_ATFM_delay_country_apts=rbind(head(ARR_ATFM_delay_country_apts1, 6), 
                                    cbind(AIRPORT="Other", 
                                          DLY_APT_ARR_TOT=sum(ARR_ATFM_delay_country_apts_other$DLY_APT_ARR_TOT)))
  
} else {
  
  ARR_ATFM_delay_country_apts=ARR_ATFM_delay_country_apts1
  
}

ARR_ATFM_delay_country_apts=ARR_ATFM_delay_country_apts %>% 
  mutate(DLY_APT_ARR_TOT=as.numeric(DLY_APT_ARR_TOT),
         fraction=DLY_APT_ARR_TOT/sum(DLY_APT_ARR_TOT),
         label = paste0(AIRPORT, "\n", round(fraction*100), "%"),
         AIRPORT=factor(AIRPORT, levels=ARR_ATFM_delay_country_apts$AIRPORT))

if (nrow(ARR_ATFM_delay_country_apts)>1) {
  
  if (sum(ARR_ATFM_delay_country_apts$DLY_APT_ARR_TOT)>0) {
    
  ARR_ATFM_delay_country_apts_fig=ggplot(ARR_ATFM_delay_country_apts, aes(area = DLY_APT_ARR_TOT, fill = AIRPORT, label = label)) +
    geom_treemap() +
    geom_treemap_text(colour = "white",
                      place = "centre",
                      size = 15,
                      grow = TRUE) +
    theme_factsheet() +
    theme(legend.position = "none")
  } else {
    
    ARR_ATFM_delay_country_apts_fig = ggplot() +
      geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), fill="blue") +
      geom_text(aes(x=0.5, y=1, label=paste0("No Arrival ATFM Delay in ", Last_complete_year)), 
                position = position_stack(vjust = 0.5), size=60, colour="white") +
      theme_factsheet() +
      theme_void()
    
  }
  
} else {
  
  if (ARR_ATFM_delay_country_apts$DLY_APT_ARR_TOT[1]>0) {
    
    ARR_ATFM_delay_country_apts_fig = ggplot(data=ARR_ATFM_delay_country_apts, aes(x=3, y=DLY_APT_ARR_TOT, fill=AIRPORT)) +
      geom_col() +
      scale_fill_brewer(palette=4) +
      coord_polar(theta="y") +
      geom_text(aes(x=3.4, label=label), position = position_stack(vjust = 0.5), size=60) +
      xlim(c(0.2, 3.5)) +
      theme_factsheet() +
      theme_void() +
      theme(legend.position = "none")
    
  } else {
    
    ARR_ATFM_delay_country_apts_fig = ggplot() +
      geom_rect(aes(xmin=0, xmax=1, ymin=0, ymax=1), fill="blue") +
      geom_text(aes(x=0.5, y=1, label=paste0("No Arrival ATFM Delay in ", Last_complete_year)), 
                position = position_stack(vjust = 0.5), size=60, colour="white") +
      theme_factsheet() +
      theme_void()
    
  }
}

ggsave(here("media", "factsheet", paste0("ARR_ATFM_delay_country_treemap_", Airport, ".pdf")), 
       plot=ARR_ATFM_delay_country_apts_fig,width = Arr_ATFM_Delay_layout1[3]*Page_width, height = Arr_ATFM_Delay_height1, 
       units = "cm", dpi=100, limitsize = FALSE)


