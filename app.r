# LIBRARIES USED
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)

# UI STUFF
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

# TITLE
  h1("The Story Behind the Stories: Murakami by the Metrics"),

# DASHBOARD IMAGE & BIOGRAPHY TEXT
# Image on the Left and Text on the Right 
div(style = "display: flex; align-items: flex-start; gap: 20px; margin-bottom: 30px;", 

  # MURAKAMI IMAGE
  tags$img(
    src = "murakami.jpg", 
    height = "150px", 
    style = "flex-shrink; 0:"
  ),

  # DASHBOARD TEXT
  div(style = "max-width: 700px;", 
    class = "text", "Haruki Murakami is one of Japan's most celebrated contemparary authors, 
    known for his surreal storytelling, dreamlike prose, and exploration of themes such as loneliness, memory, and identity.
    Since the late 1970s, Murakami has captivated readers worldwide with novels like 'Norwegian Wood', 'Kafka on the Shore', and '1Q84'.
    This dashboard explores the literary landscape of renowned Japanese author Haruki Murakami through data-driven insights. From tracking his publishing productivity over the years,
    to analysing reader reception and popularity trends, the visualisatiosn aim to uncover the patterns behind his global acclaim. In addition, through the examination of
    book ratings, number of reviews accumulated and his publication frequency, we investigate how Murakami's storytelling resonates across time and readers worldwide.")
),

 # TABS FULL WIDTH 
  div(
    style = "width: 100%;",
    tabsetPanel(
      id = "tabs",
      tabPanel("Publishing Productivity", value = "evolution", plotlyOutput("pp_linegraph")),
      tabPanel("Top-Rated Books", value = "boxplot", plotlyOutput("trb_bargraph")),
      tabPanel("Ratings vs Reviews", value = "heatmap", plotlyOutput("shooting_heatmap"))
    )
  )
)                                                                       


# SERVER SECTION
server <- function(input, output, session) {

# REFER TO THE CSV
df3 <- read.csv("C:/Users/bianc/Downloads/murakami dataset/data/altmurakami.csv", stringsAsFactors = FALSE)
  # strings as factors set to false in order to allow for line chart. 
  
  # TAB 1: LINE GRAPH - Publishing Productivity
  # LINE ISN'T SHOWING, PUBLICATION DATE DATA TYPE NEEDS TO BE CHANGED TO NUMERIC. 
  df3$publication.date <- as.numeric(df3$publication.date)
  output$pp_linegraph <- renderPlotly({

    # Group by PUBLICATION YEAR and COUNT BOOKS and RETRIEVE BOOK TITLES 
    productivity_data <- df3 %>%
      group_by(publication.date) %>%
      summarise(
        books_published = n(),
        book_titles = paste(book, collapse = '<br>'),
        .groups = "drop"
      ) %>%
    arrange(publication.date)
    b-009
    # CREATING THE LINE GRAPH
    # hovering shows the count, year and books
    p1 <- ggplot(productivity_data, aes(x = publication.date, y = books_published,
                                        text = paste("Year: ", publication.date,
                                                    "<br>Books Published: ", books_published,
                                                    "<br><br>Titles:<br>", book_titles))) +

      geom_line(aes(group = 1), color = "#880808", linewidth = 1) +     # SHOWS THE LINE
      geom_point(color = "#880808", linewidth = 3) +    # EACH POINT CORRESPONDS TO A YEAR
      labs(
        title = "Murakami's Publishing Productivity Over Time",
        x = "Publication Year",
        y = "Number of Books Published",
        subtitle = "Books published per year"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p1, tooltip = "text")
  })
  

  # TAB 2: BAR GRAPH - TOP 15 RATED BOOKS
  output$trb_bargraph <- renderPlotly({

    # ACQUIRING THE TOP 15 BOOKS IN TERMS OF RATING
    top_books <- df3 %>%
      arrange(desc(Average.Rating)) %>%
      slice_head(n = 15) %>%
      mutate(book = reorder(book, Average.Rating))
    
    # Create boxplot/bar chart for top books
    p2 <- ggplot(top_books, aes(x = book, 
                                y = Average.Rating,
                                text = paste("Book Title: ", book,
                                            "<br> Year: ", publication.date,
                                            "<br> Average Rating: ", Average.Rating))) +
      geom_col(fill = "#880808", alpha = 0.8) +
      geom_text(aes(label = round(Average.Rating, 2)), 
                hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = "Top 15 Highest Rated Murakami Books",
        x = "Book Title",
        y = "Average Rating",
        subtitle = "Based on average reader ratings"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 10)
      ) +
      ylim(0, 5)
    
    ggplotly(p2, tooltip = "text")
  })
  
  # TAB 3: SCATTER PLOT - Ratings vs Reviews
  # THERE IS A MATHEMATHICAL ERROR RELATING TO THE DATA TYPE
    df3$Average.Rating <- as.numeric(df3$Average.Rating)
  
    # Need to remove the commas otherwise they will still be classified as strings after datatype conversion
    df3$Number.of.Reviews <- as.numeric(gsub(",", "", df3$Number.of.Reviews))

  output$shooting_heatmap <- renderPlotly({
    # CREATING THE SCATTER PLOT FOR RATINGS AND REVIEWS
    scatter_data <- df3 %>%
      filter(!is.na(Average.Rating) & !is.na(Number.of.Reviews)) %>%
      filter(Number.of.Reviews > 0)
    
    
    # Create scatter plot
    p3 <- ggplot(scatter_data, aes(
                                x = Number.of.Reviews, 
                                y = Average.Rating,
                                text = paste("Book Title: ", book,
                                             "<br> Year: ", publication.date,
                                             "<br>Number of Reviews: ", Number.of.Reviews,
                                             "<br> Average Rating: ", Average.Rating))) +

      geom_point(alpha = 0.7, color = "#880808") +
      scale_x_log10(labels = scales::comma_format()) + # FOR BETTER SCALING
      labs(
        title = "Book Ratings vs Number of Reviews",
        x = "Number of Reviews",
        y = "Average Rating",
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right"
      ) +
      coord_cartesian(ylim = c(0, 5))
    
    ggplotly(p3, tooltip = "text")
  })
}



shinyApp(ui = ui, server = server)
#runApp("C:/Users/bianc/Downloads/murakami dataset/datavis3.r")
#shiny::runApp("C:/Users/bianc/Downloads/murakami dataset/app.r")