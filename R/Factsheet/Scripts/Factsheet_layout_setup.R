
# Lay-out setup for factsheets

linesize_factsheet=10

theme_factsheet = function(){
  
  font_factsheet="Calibri"
  size_general=150
  
  theme_bw() %+replace%
    theme(legend.title=element_blank(),
          legend.text=element_text(size=100),
          legend.position = "bottom",
          plot.margin = unit(c(5.5, 20, 5.5, 60), "pt"),
          plot.title = element_text(family = font_factsheet,            #set font family
                                    size   = size_general+10,           #set font size
                                    face   = 'bold',                    #bold typeface
                                    hjust  = 0,                         #left align
                                    vjust  = 2),                        #raise slightly
          
          plot.subtitle = element_text(family = font_factsheet,         #font family
                                        size  = 14),                    #font size
          
          plot.caption = element_text(family = font_factsheet,          #font family
                                      size   = 9,                       #font size
                                      hjust  = 1),                      #right align
          
          axis.title = element_text(family = font_factsheet,            #font family
                                    size   = size_general),             #font size
          
          axis.title.x = element_blank(),
          
          axis.text = element_text(family = font_factsheet,             #axis family
                                   size   = size_general),              #font size
          
          axis.text.x = element_text(margin=margin(5, b = 10)))         #margin for axis text
            
  
  
}

Page_width<<-450

General_layout<<-c(0.4, 0.3, 0.3)

Traffic_layout1<<-c(0.2, 0.5, 0.3)
Traffic_height1<<-100

Traffic_layout2<<-c(0.4, 0.6)
Traffic_height2<<-100

Traffic_layout3<<-1
Traffic_height3<<-100

Arr_ATFM_Delay_layout1<<-c(0.2, 0.5, 0.3)
Arr_ATFM_Delay_height1<<-100

ATFM_Slot_Adherence_layout1<<-c(0.2, 0.5, 0.3)
ATFM_Slot_Adherence_height1<<-100

VFE_layout1<<-c(0.4, 0.4, 0.2)
VFE_height1<<-100

Punctuality_layout1<<-c(0.4, 0.4, 0.2)
Punctuality_height1<<-100

Punctuality_layout2<<-c(0.4, 0.4, 0.2)
Punctuality_height2<<-100

ASMA_layout1<<-c(0.3, 0.7)
ASMA_height1<<-100

Pre_dep_delay_layout1<<-c(0.3, 0.7)
Pre_dep_delay_height1<<-100

Taxi_in_times_layout1<<-c(0.3, 0.7)
Taxi_in_times_height1<<-100

Taxi_out_times_layout1<<-c(0.3, 0.7)
Taxi_out_times_height1<<-100

Turnaround_times_layout1<<-c(0.3, 0.7)
Turnaround_times_height1<<-100
