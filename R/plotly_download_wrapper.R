library(plotly)
library(htmlwidgets)
library(magrittr)

# Wrapper function for Plotly graphs to add dataset download button
add_download_button <- function(plot, dataset, dataset_name = NULL) {
  # Infer dataset name if not provided
  if (is.null(dataset_name) || dataset_name == "") {
    dataset_name <- deparse(substitute(dataset))
  }
  
  # Convert dataset to CSV format
  csv_data <- paste(capture.output(write.csv(dataset, row.names = FALSE)), collapse = "\\n")
  
  # Embed CSV data into the HTML using JavaScript
  plot %>%
    config(
      modeBarButtonsToAdd = list(
        list(
          name = "Download data",
          icon = list(
            path = "M69.8,85.7H30.2c-8.768,0-15.9-7.133-15.9-15.9v-4c0-2.209,1.791-4,4-4s4,1.791,4,4v4c0,4.356,3.544,7.9,7.9,7.9h39.6c4.356,0,7.9-3.544,7.9-7.9v-4c0-2.209,1.791-4,4-4s4,1.791,4,4v4c0,8.768-7.133,15.9-15.9,15.9Z M68.628,47.171c-1.561-1.562-4.096-1.562-5.656,0l-8.972,8.972V18.3c0-2.209-1.791-4-4-4s-4,1.791-4,4V56.143l-8.971-8.972c-1.562-1.562-4.095-1.562-5.657,0-1.562,1.562-1.562,4.095,0,5.657l15.8,15.8h0c.186,.186,.391,.352,.61,.498,.1,.067,.208,.112,.312,.169,.125,.068,.244,.143,.377,.198,.134,.055,.273,.087,.411,.128,.112,.033,.22,.076,.336,.099,.259,.051,.521,.079,.784,.079s.525-.028,.783-.079c.117-.023,.226-.067,.339-.101,.137-.04,.275-.072,.408-.127,.133-.055,.254-.131,.38-.2,.103-.056,.21-.101,.308-.167,.22-.147,.426-.314,.612-.501l15.797-15.797c1.562-1.562,1.562-4.094,0-5.657Z",
            width = 100,
            height = 100
          ),
          click = JS(
            sprintf(
              "function(gd) {
                var csvContent = 'data:text/csv;charset=utf-8,%s';
                var encodedUri = encodeURI(csvContent);
                var link = document.createElement('a');
                link.setAttribute('href', encodedUri);
                link.setAttribute('download', '%s.csv');
                document.body.appendChild(link);
                link.click();
                document.body.removeChild(link);
              }",
              csv_data,
              dataset_name
            )
          )
        )
      )
    )
}

# # Sample dataset
# sample_data <- data.frame(
#   Category = c("A", "B", "C", "D"),
#   Value = c(10, 20, 30, 40)
# )
# 
# # Create a simple Plotly bar plot
# base_plot <- plot_ly(
#   data = sample_data,
#   x = ~Category,
#   y = ~Value,
#   type = "bar",
#   name = "Values"
# )
# 
# # Add a download button for the dataset
# enhanced_plot <- add_download_button(
#   plot = base_plot,
#   dataset = sample_data,
#   dataset_name = "sample_data"
# )
# 
# # Display the enhanced plot
# enhanced_plot
