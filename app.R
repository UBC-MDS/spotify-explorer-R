# author: Christopher Alexander, Jennifer Hoang, Michelle Wang, Wenxin Xiang
# date: 2022-03-15

library(dash)
library(dashHtmlComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(readr)
library(dplyr)
library(here)
library(tidyverse)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggthemes)

# Read data ------

df <- read_csv(here("data", "raw", "spotify.csv"))
df <- na.omit(df)

df$date <- as.Date(df$track_album_release_date, format = "%Y-%m-%d")


# Set up app frontend -----
app <- Dash$new(
  external_stylesheets = dbcThemes$MINTY,
  suppress_callback_exceptions = T
)

app$title("Spotify Explorer")


# Navbar -------------

NAV_STYLE <- list(
  "height" = "50px",
  "fontSize" = "large"
)

navbar <- dbcNavbar(
  dbcContainer(
    list(
      htmlA(
        dbcRow(
          list(
            dbcCol(htmlImg(src = "https://www.freepnglogos.com/uploads/spotify-logo-png/spotify-icon-marilyn-scott-0.png", height = "50px")),
            dbcCol(dbcNavbarBrand("Spotify Explorer", className = "py-10"))
          ),
          align = "center",
          className = "g-0",
          style = NAV_STYLE
        )
      )
    )
  )
)

# Tab layout -------------

get_tab_section <- htmlDiv(list(
  dccTabs(id = "tab", value = "tab-1", children = list(
    dccTab(label = "Artists/Genres", value = "tab-1"),
    dccTab(label = "Song Characteristics", value = "tab-2")
  )),
  htmlBr(),
  htmlDiv(id = "tab-content", style = list("padding" = "10px 10 10 10px"))
))


# Footer -------------

FOOTER_STYLE <- list(
  "position" = "fixed",
  "bottom" = 0,
  "left" = 0,
  "right" = 0,
  "height" = "25px",
  "padding" = "3px 0 0 5px",
  "backgroundColor" = "green",
  "color" = "white",
  "fontSize" = "small"
)


footer <- dbcContainer(
  list(
    htmlBr(),
    htmlFooter(
      list(
        "(C) Copyright MIT License: Christopher Alexander, Jennifer Hoang, Michelle Wang, Wenxin (Thea) Xiang. ",
        paste0("Last time updated on ",{Sys.Date()})
      ),
      style = FOOTER_STYLE
    )
  )
)


# Overall app layout ----

app$layout(htmlDiv(
  dbcRow(list(
    navbar,
    dbcCol(
      get_tab_section
    ),
    footer
  ),
  style = list("backgroundColor" = "#eeeeef")
  )
))


# Artist sidebar widgets and plots (tab-1) -------

artist_sidebar_widgets <- dbcCol(
  children = list(
    htmlH2("Overview", className = "display-30"),
    htmlH6(
      list(
      "Welcome! This is a dashboard displaying trends in popularity of artists, \
                genres and song types in Spotify. Happy exploring! ",
      htmlBr(),
      htmlBr(),
      "This section shows popularity of artists by genre and over time, as well as artist's distribution of popularity.",
      htmlBr()
      ),
      className = "display-30"),
    htmlBr(),
    htmlBr(),
    htmlH5("Artist Genre:"),
    dccDropdown(
      id = "genre_select",
      style = list("border-width" = "0", "width" = "100%"),
      options = list(
        list(label = "EDM", value = "edm"),
        list(label = "Latin", value = "latin"),
        list(label = "Pop", value = "pop"),
        list(label = "R&B", value = "r&b"),
        list(label = "Rap", value = "rap"),
        list(label = "Rock", value = "rock")
      ),
      value = "pop"
    ),
    htmlBr(),
    htmlBr(),
    htmlBr(),
    htmlBr(),
    htmlBr(),
    htmlH5("Artist Name:"),
    dccDropdown(
      id = "artist_selection",
      value = "Ed Sheeran",
      style = list("border-width" = "0", "width" = "100%"),
      options = unique(df$track_artist) %>%
        purrr::map(function(col) list(label = col, value = col))
    )
  ),
  width = list("offset" = 1, "size" = 3)
)

get_artist_section <- htmlDiv(
  list(
    dbcRow(list(
      artist_sidebar_widgets,
      dbcCol(list(
        dbcRow(list(
          htmlH3("Top Artists by Genre"),
          dccGraph(id = "top_artists_plot")
        )),
        htmlBr(),
        dbcRow(list(
          dbcCol(list(
            htmlH4("Artist's Popularity Over Time"),
            dccGraph(id = "artist_trend_plot")
          ), width = 6),
          dbcCol(list(
            htmlH4("Artist's Popularity Record"),
            dccGraph(id='artist_pop_hist_id')
          ), width = 6)
        ))
      ), width = list("offset" = 1, "size" = 7))
    ))
  )
)


# Popularity sidebar widgets and plots (tab-2)  ----

popularity_sidebar_widgets <- dbcCol(
  children = list(
    htmlH2("Music characteristics", className = "display-30"),
    htmlH6(
      list(
        "This section shows song characteristics for popular/not-popular songs of different genres.",
        htmlBr(),
        htmlBr(),
        htmlB('Popular: '), "Songs rated higher than median popularity.",
        htmlBr(),
        htmlB('Not popular: '), "Vice versa."
      ),
      className="display-30",
    ),
    htmlBr(),
    htmlH5("Music Features:"),
    dccDropdown(
      id = "xcol-widget",
      style = list("border-width" = "0", "width" = "100%"),
      options = list(
        list(label = "Danceability", value = "danceability"),
        list(label = "Energy", value = "energy"),
        list(label = "Loudness", value = "loudness"),
        list(label = "Acousticness", value = "acousticness"),
        list(label = "Speechiness", value = "speechiness"),
        list(label = "Instrumentalness", value = "instrumentalness"),
        list(label = "Liveness", value = "liveness"),
        list(label = "Valence", value = "valence"),
        list(label = "Tempo", value = "tempo"),
        list(label = "Duration (min)", value = "Duration (min)")
      ),
      value = "danceability"
    ),
    htmlBr(),
    htmlH5("Music Genres:"),
    dccDropdown(
      id = "genres",
      style = list("border-width" = "0", "width" = "100%"),
      options = list(
        list(
          label = "Electronic dance music",
          value = "electronic dance music"
        ),
        list(label = "Pop", value = "pop"),
        list(label = "Rap", value = "rap"),
        list(label = "Rock", value = "rock"),
        list(label = "Latin", value = "latin"),
        list(label = "R&B", value = "r&b")
      ),
      value = "electronic dance music",
    )
  ),
  width = list("offset" = 1, "size" = 3)
)


get_popularity_section <- htmlDiv(
  list(
    dbcRow(list(
      popularity_sidebar_widgets,
      dbcCol(list(
        htmlH3("Song Characteristics Distribution between Two Popularity Classes"),
        dccGraph(id = "pop_unpop_id_plot")
      ), width = list("offset" = 1, "size" = 7))
    ))
  )
)


# Switch Tabs ----
app$callback(
  output(id = "tab-content", property = "children"),
  list(input(id = "tab", property = "value")),
  function(tab) {
    if (tab == "tab-1") {
      return(get_artist_section)
    } else if (tab == "tab-2") {
      return(get_popularity_section)
    }
  }
)


# Top artists plot ----

#' Plot top 10 artists per genre by average track popularity
#'
#' @param genre genre of artist
#' @return ggplot bar plot object
app$callback(
  output("top_artists_plot", "figure"),
  list(input("genre_select", "value")),
  function(genre_select) {
    top10_df <- df %>%
      dplyr::filter(playlist_genre == genre_select) %>%
      group_by(track_artist) %>%
      summarise(mean_popularity = mean(track_popularity)) %>%
      arrange(desc(mean_popularity)) %>%
      head(10)
    
    p <- ggplot(
      top10_df,
      aes(x = mean_popularity, y = reorder(track_artist, mean_popularity))
    ) +
      geom_bar(stat = "identity", fill = '#5DBB63') +
      labs(x = "Average Track Popularity", y = "Artist")
    ggplotly(p)
  }
)

# Artist trend plot
app$callback(
  output("artist_trend_plot", "figure"),
  list(input("artist_selection", "value")),
  function(artist) {
    df_artist <- df[df$track_artist == artist, ]
    
    p2 <- ggplot(df_artist, aes(x = date, y = track_popularity)) +
      geom_line(stat = "summary", fun = mean, color = '#5DBB63', size = 1) +
      geom_point(stat = "summary", fun = mean, color = '#99EDC3', size = 1) +
      labs(x = "Date", y = "Average Track Popularity") +
      scale_x_date(date_labels = "%b-%Y") +
      ggthemes::scale_color_tableau()
    
    ggplotly(p2)
  }
)

# Artist Record Plot ----
app$callback(
  output('artist_pop_hist_id', 'figure'),
  list(input('artist_selection', 'value')),
  function(xcol) {
    chart <- ggplot(df %>% 
                      dplyr::filter(track_artist == xcol)) + 
      aes(x = track_popularity ) + 
      geom_histogram(fill = '#5DBB63') +
      geom_vline(
        aes(xintercept = mean(track_popularity),
            colour="red")) +
      labs(
        x = "Track popularity",
        y = "Count",
        colour="Mean popularity" )
    
    ggplotly(chart) %>% layout(dragmode = 'select')
  }
)

# Song Characteristic Distribution Plot ----

#' Plot density plot of song characteristics distribution with two popularity classes
#' 
#' @param genre genre of songs
#' @param feat song features to explore on x-axis
#' @return a ggplot showing the distribution

app$callback(
  output("pop_unpop_id_plot", "figure"),
  list(
    input("genres", "value"),
    input("xcol-widget", "value")
  ),
  function(genre, feat) {
    
    data_pop <- df
    data_pop$`Duration (min)` <- data_pop$duration_ms / 60000
    data_pop$`Popularity class` = if_else(
      data_pop$track_popularity <= median(data_pop$track_popularity),
      "Not popular",
      "Popular"
    )
    data_pop$Genres <- data_pop$playlist_genre
    data_pop$Genres <- replace(data_pop$Genres, 
                               data_pop$Genres == "edm", 
                               "electronic dance music")
    data_pop_query <- data_pop %>%
      filter(Genres == genre)
    plot <- ggplot(data_pop_query) +
      aes(x = !!sym(feat),
          color = `Popularity class`) +
      geom_density() +
      labs(x = str_to_title(feat)) +
      theme(
        text = element_text(size = 14)
      )
    ggplotly(plot)
  }
)

# App server --------------

app$run_server(host = '0.0.0.0')