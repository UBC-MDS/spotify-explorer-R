# author: Christopher Alexander, Jennifer Hoang, Michelle Wang, Wenxin Xiang
# date: 2022-03-01

library(dash)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(readr)
library(dplyr)

# read data ------

df <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv")
df <- na.omit(df)

df$date <- as.Date(df$track_album_release_date, format = "%Y-%m-%d")


# Set up app frontend -----
app <- Dash$new(
  external_stylesheets = dbcThemes$MINTY
)

app$title("Spotify Explorer App")


# navbar ------

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

# tab layout -------------

TAB_STYLE <- list(
  "marginBottom" = 20,
  "height" = "50px",
  "padding" = "3px 0 0 320px",
  "color" = "white",
  "fontSize" = "medium"
)

get_tab_section <- htmlDiv(list(
  dccTabs(id = "tab", value = "tab-1", children = list(
    dccTab(label = "Artists/Genres", value = "tab-1"),
    dccTab(label = "Song Characteristics", value = "tab-2")
  )),
  htmlDiv(id = "tab-content")
))

# Footer --

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
        "(C) Copyright MIT License: Christopher Alexander, Jennifer Hoang, Michelle Wang, Wenxin (Thea) Xiang."
      ),
      style = FOOTER_STYLE
    )
  )
)


# get_artist_section widget + plot

sidebar_widgets <- dbcCol(
  children = list(
    htmlH2("Overview", className = "display-30"),
    htmlH6(
      "Welcome! This is a dashboard displaying trends in popularity of artists, \
                genres and song types in Spotify. Happy exploring!",
      className = "display-30",
    ),
    htmlBr(),
    htmlBr(),
    htmlLabel("Artist Genre:"),
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


# app layout ----
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


# get_artist_section -------
get_artist_section <- htmlDiv(
  list(
    dbcRow(list(
      sidebar_widgets,
      dbcCol(list(
        dbcRow(list(htmlH3("Top Artists by Genre"),
                    dccGraph(id = "top_artists_plot"))),
        dbcRow(list(
          dbcCol(list(
            htmlH3("Artist's Popularity Over Time"),
            dccGraph(id = "artist_trend_plot")
          )),
          dbcCol(list(
            htmlH3("Artist's Popularity Record")
#            dccGraph(id = "plot_3")
          ))
        ))
      ))
    ))
  )
)


# get_popularity_section -------
get_popularity_section <- htmlDiv(
  list(htmlH3("Song Characteristics Distribution between Two Popularity Classes"))
)


# Switch Tab ----
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
      geom_bar(stat = "identity") +
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

    p <- ggplot(df_artist, aes(x = date, y = track_popularity)) +
      geom_line(stat = "summary", fun = mean) +
      labs(x = "Date", y = "Avg track Popularity") +
      scale_x_date(date_labels = "%b-%Y") +
      ggthemes::scale_color_tableau()

    ggplotly(p)
  }
)

# app server --------------

app$run_server(debug = T)
