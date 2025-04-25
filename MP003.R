# helper to install & load a package if missing
ensure_package <- function(pkg) {
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only = TRUE))
}

# make sure we have what we need
ensure_package(stringr)
ensure_package(fs)
ensure_package(readr)
ensure_package(fs)
ensure_package(dplyr)
ensure_package(tidyr)
ensure_package(httr2)
ensure_package(rvest)
ensure_package(datasets)
ensure_package(purrr)
ensure_package(DT)

load_songs <- function() {
  # 1. define URL and local paths
  url       <- "https://raw.githubusercontent.com/gabminamedez/spotify-data/refs/heads/master/data.csv"
  data_dir  <- fs::path("data", "mp03")
  file_name <- "spotify_songs.csv"
  file_path <- fs::path(data_dir, file_name)
  
  # 2. create directory if needed
  fs::dir_create(data_dir, recurse = TRUE)
  
  # 3. download only if missing
  if (!fs::file_exists(file_path)) {
    message("Downloading Spotify analytics to ", file_path, " …")
    utils::download.file(url, destfile = file_path, mode = "wb")
  }
  
  # 4. read it in
  df <- readr::read_csv(file_path, show_col_types = FALSE)
  
  # 5. clean up column names
  clean_names <- names(df) |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\s+", "_")
  names(df) <- clean_names
  
  return(df)
}

# Example usage:
songs <- load_songs()
head(songs)



# make sure tidyr and stringr are loaded
ensure_package(tidyr)
ensure_package(stringr)

# helper to clean up the bracket‐and‐quote formatting
clean_artist_string <- function(x) {
  x |>
    str_replace_all("\\['", "")    |>  # drop leading [' 
    str_replace_all("'\\]", "")    |>  # drop trailing '] 
    str_replace_all("[ ]?'", "")   |>  # drop any stray single quotes
    str_replace_all("[ ]*,[ ]*", ",")  # normalize commas
}

# split each row so there's one artist per line
songs_by_artist <- songs |>
  tidyr::separate_longer_delim(artists, delim = ",") |>   # one row per artist
  dplyr::mutate(artist = clean_artist_string(artists)) |> # clean it up
  dplyr::select(-artists)                                 # drop the old column

# inspect
dplyr::glimpse(songs_by_artist)






load_playlists <- function() {
  # ensure dependencies
  ensure_package <- function(pkg) {
    pkg <- as.character(substitute(pkg))
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    if (!require(pkg, character.only = TRUE)) install.packages(pkg)
    stopifnot(require(pkg, character.only = TRUE))
  }
  ensure_package(fs); ensure_package(jsonlite)
  
  data_dir <- fs::path("data", "mp03v3", "playlists")
  fs::dir_create(data_dir, recurse = TRUE)
  
  starts <- seq(0, 999000, by = 1000)
  ends   <- starts + 999
  
  all_playlists <- list()
  slices_loaded <- 0L
  target_n      <- 16   # <-- stop when we have 16 playlists
  
  # helper to try parsing & retry download if needed
  try_load_slice <- function(dest, url) {
    parse_once <- function() {
      jsonlite::fromJSON(dest, simplifyVector = FALSE)
    }
    slice <- tryCatch(parse_once(), error = function(e) e)
    if (inherits(slice, "error")) {
      message("  → parse error, retrying download…")
      dl_ok <- tryCatch({
        utils::download.file(url, destfile = dest, mode = "wb")
        TRUE
      }, error = function(e) {
        message("    download retry failed; skipping slice.")
        FALSE
      })
      if (dl_ok) {
        slice <- tryCatch(parse_once(), error = function(e2) {
          message("    still parse error after retry; skipping slice.")
          e2
        })
      }
    }
    slice
  }
  
  for (i in seq_along(starts)) {
    # 1) build names & URLs
    fname <- sprintf("mpd.slice.%d-%d.json", starts[i], ends[i])
    dest  <- fs::path(data_dir, fname)
    url   <- sprintf(
      "https://raw.githubusercontent.com/DevinOgrady/spotify_million_playlist_dataset/master/data1/%s",
      fname
    )
    
    # 2) download if missing
    if (!fs::file_exists(dest)) {
      message("[", i, "] downloading ", fname, " …")
      tryCatch(
        utils::download.file(url, destfile = dest, mode = "wb"),
        error = function(e) message("  → ", fname, " not found; skipping.")
      )
    } else {
      message("[", i, "] already on disk; reading ", fname)
    }
    
    # 3) parse & append only up to target_n
    if (fs::file_exists(dest)) {
      slice <- try_load_slice(dest, url)
      if (!inherits(slice, "error") && !is.null(slice$playlists)) {
        pls <- slice$playlists
        n_have <- length(all_playlists)
        n_need <- target_n - n_have
        
        if (n_need > 0 && length(pls) > 0) {
          # take only as many as needed
          take_idx <- seq_len(min(length(pls), n_need))
          all_playlists <- c(all_playlists, pls[take_idx])
          slices_loaded <- slices_loaded + 1L
          message("  ✓ added ", length(take_idx), " playlists (total now ", length(all_playlists), ")")
        }
        
        # stop early if we've reached our target
        if (length(all_playlists) >= target_n) {
          message("Reached ", target_n, " playlists; stopping.")
          break
        }
      }
    }
  }
  
  message("Done! Loaded ", length(all_playlists),
          " playlists from ", slices_loaded, " slices.")
  all_playlists
}

# Example usage:
playlists_16 <- load_playlists()
length(playlists_16)   # should be 16



# Now call it:
playlists <- load_playlists()

# Check:
length(playlists)       
str(playlists[[1]])    

# peek at the names in the first playlist
names(playlists[[1]])

# peek at one track’s fields
names(playlists[[1]]$tracks[[1]])




library(dplyr)
library(purrr)
library(stringr)

strip_spotify_prefix <- function(x) {
  # keep whatever comes after the last colon
  str_match(x, ".*:.*:(.*)")[,2]
}

playlist_tracks_tbl <- 
  map_dfr(playlists, function(pl) {
    # grab the playlist‐level info once
    pname     <- pl$name
    pid       <- pl$pid            # or pl$id, pl$uri, whatever your “playlist_id” field is
    followers <- pl$num_followers  # or pl$followers
    
    # now walk through each track in that playlist
    map_dfr(seq_along(pl$tracks), function(i) {
      tr <- pl$tracks[[i]]
      tibble(
        playlist_name      = pname,
        playlist_id        = pid,
        playlist_position  = i,
        playlist_followers = followers,
        
        artist_name = tr$artist_name,
        artist_id   = strip_spotify_prefix(tr$artist_uri),
        
        track_name = tr$track_name,
        track_id   = strip_spotify_prefix(tr$track_uri),
        
        album_name = tr$album_name,
        album_id   = strip_spotify_prefix(tr$album_uri),
        
        duration = tr$duration_ms
      )
    })
  })

# inspect
glimpse(playlist_tracks_tbl)



library(dplyr)


# 1) How many distinct tracks and artists?

n_distinct_tracks  <- playlist_tracks_tbl %>% pull(track_id)  %>% n_distinct()
n_distinct_artists <- playlist_tracks_tbl %>% pull(artist_id) %>% n_distinct()

cat("Distinct tracks: ",  n_distinct_tracks,  "\n",
    "Distinct artists:", n_distinct_artists, "\n\n")



# 2) Top 5 most‐popular tracks (by appearance count)

top5_tracks <- playlist_tracks_tbl %>%
  count(track_id, track_name, sort = TRUE) %>%
  slice_head(n = 5)

cat("Top 5 tracks by frequency in playlists:\n")
print(top5_tracks)
cat("\n")



# 3) Among those, which top track has NO entry in `songs`?

missing_in_songs <- playlist_tracks_tbl %>%
  count(track_id, track_name, sort = TRUE) %>%
  anti_join(songs %>% rename(track_id = id), by = "track_id") %>%
  slice_head(n = 1)

cat("Most‐popular track not in song‐characteristics:\n")
print(missing_in_songs)
cat("\n")


# 4) Most “danceable” track (from `songs`) & its playlist frequency

most_danceable <- songs |>
  slice_max(danceability, n = 1, with_ties = FALSE) |>
  transmute(
    track_id = id, track_name = name, danceability
  )

danceable_count <- playlist_tracks_tbl |>
  filter(track_id == most_danceable$track_id) |>
  tally(name = "playlist_occurrences")

cat("Most danceable track:\n")
print(most_danceable)
cat("It appears in", danceable_count$playlist_occurrences, "playlists.\n\n")


# ——————————————————————————————————————————
# 5) Which playlist has the longest *average* track length?
# ——————————————————————————————————————————
longest_avg <- playlist_tracks_tbl %>%
  group_by(playlist_name, playlist_id) %>%
  summarise(avg_duration = mean(duration), .groups = "drop") %>%
  slice_max(avg_duration, n = 1, with_ties = FALSE)

cat("Playlist with longest average duration:\n")
print(longest_avg)
cat("\n")


# ——————————————————————————————————————————
# 6) What’s the most‐followed playlist on Spotify?
# ——————————————————————————————————————————
most_followed <- playlist_tracks_tbl %>%
  distinct(playlist_name, playlist_id, playlist_followers) %>%
  slice_max(playlist_followers, n = 1, with_ties = FALSE)

cat("Most‐followed playlist:\n")
print(most_followed)









#––– 0) load libraries ––––––––––––––––––––––––––––––––––––––––––––––––––––––––  
library(tidyverse)
library(lubridate)    # for year()
theme_set(theme_minimal(base_size = 14))

#––– 1) Prepare & join –––––––––––––––––––––––––––––––––––––––––––––––––––––––  
# (a) rename songs id → track_id for join
songs2 <- songs |> 
  rename(track_id = id)

# (b) count how often each track appears in playlists
track_counts <- playlist_tracks_tbl |> 
  count(track_id, name = "appearance_count")



# (c) inner‐join
df <- track_counts |>
  inner_join(songs2, by = "track_id") |>
  
  # 1) Clean stray dashes/blanks (from before)
  mutate(
    raw_date = str_trim(release_date),
    raw_date = str_remove(raw_date, "-$"),
    raw_date = na_if(raw_date, "")
  ) |>
  
  # 2) Parse slashed dates (mdy), else fall back to ISO orders
  mutate(
    release_date = case_when(
      str_detect(raw_date, "^\\d{1,2}/") ~ 
        parse_date_time(raw_date, orders = "mdy", quiet = TRUE),
      TRUE ~ 
        parse_date_time(raw_date, orders = c("Ymd", "Y-m", "Y"), quiet = TRUE)
    )
  ) |>
  
  # 3) Now you can safely extract year, decade, etc.
  mutate(
    release_year  = year(release_date),
    decade        = (release_year %/% 10) * 10,
    duration_min  = duration_ms / 60000
  ) |>
  select(-raw_date)  # drop the helper

# Inspect the first few parsed dates
df |> select(track_id, raw_date = release_date, release_year) |> head()


library(ggplot2)

# 2) compute correlation
r <- cor(df$popularity, df$appearance_count, use = "complete.obs")
cat("Pearson r =", round(r, 3), "\n")

# 3) plot
df |>
  group_by(popularity) |>
  summarise(mean_apps = mean(appearance_count), .groups="drop") |>
  ggplot(aes(x = popularity, y = mean_apps)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 0.8, alpha = 0.5) +
  scale_y_log10() +
  labs(
    title = "Average # of Playlist Appearances by Popularity",
    x = "Spotify Popularity (0–100)",
    y = "Mean Playlist Appearances (log₁₀)"
  ) +
  theme_minimal(base_size = 14)
#This plot shows that—even on a log scale—the average number of playlist appearances barely budges as Spotify “popularity” rises. Most tracks sit right around one playlist appearance on average, and you only see small blips up to about 1.1–1.14 appearances per song at popularity scores in the low-60s and mid-70s. In other words, songs with higher Spotify popularity aren’t showing up in dramatically more playlists; the relationship is essentially flat.


library(dplyr)
library(purrr)
library(stringr)
library(lubridate)


# Setup: your two anchor songs (change these to your favorites)

anchor_names <- c("Promiscuous", "Mr. Brightside")

anchors <- songs2 |>
  filter(name %in% anchor_names) |>
  # rename only for this pipeline
  transmute(
    track_id,
    track_name   = name,
    artist_id = artists,
    release_date,
    key,
    tempo,
    danceability,
    acousticness
  )

anchor_ids <- anchors$track_id

anchor_ids

anchor_ids <- c(
  "7oK9VyNzrYvRFo7nQEYkWN",
  "47aQT2aV12TyilaoYi1NiD",
  "3SwlakM6VX47IwG0Wll5ek"
)

library(dplyr)

# 1) Identify which anchors are actually present
valid_anchors <- intersect(anchor_ids, playlist_tracks_tbl$track_id)

# 2) Which playlists contain those valid anchors?
pl_ids <- playlist_tracks_tbl %>%
  filter(track_id %in% valid_anchors) %>%
  pull(playlist_id) %>%
  unique()

# Sanity check: how many distinct tracks in those playlists?
playlist_tracks_tbl %>%
  filter(playlist_id %in% pl_ids) %>%
  distinct(track_id) %>%
  nrow()
# [1] 51   <-- confirms there are 51 distinct tracks in playlist 0

# 3) Now get co‐occurring tracks by:
#    • restricting to those same playlists,
#    • dropping anchor(s),
#    • counting frequency
cooccur <- playlist_tracks_tbl %>%
  filter(
    playlist_id %in% pl_ids,            # same playlist(s)
    ! track_id %in% valid_anchors       # but not the anchor itself
  ) %>%
  count(track_id, track_name, sort = TRUE)

cooccur

# Creating my playlist
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

# 1) Define playlist as a tibble with the exact track_ids in order
my_playlist <- tibble(
  playlist_position = 1:12,
  track_id = c(
    "1dzQoRqT5ucxXVaAhTcT0J",      # replace with real IDs
    "4QNpBfC0zvjKqPJcyqBy9W",
    "2CEgGE6aESpnmtfiZwYlbV",
    "5j9iuo3tMmQIfnEEQOOjxh",
    "5HuqzFfq2ulY1iBAW5CxLe",
    "6MjljecHzHelUDismyKkba",
    "4QNpBfC0zvjKqPJcyqBy9W",
    "0?YesImChangingID",
    "2ouURa1AIXp3AvkS52Jry5",
    "2HHtWyy5CgaQbC7XSoOb0e",
    "1SLikaDhWhhhnLJC58bkFI",
    "3bidbhpOYeV4knp8AIu8Xn"
  )
)

# 2) Join in the features
plot_df <- my_playlist %>%
  left_join(songs2, by = "track_id") %>%
  select(playlist_position,name, popularity, danceability, energy, valence)

# 3) Pivot longer for ggplot
long_df <- plot_df %>%
  pivot_longer(
    cols = c(popularity, danceability, energy, valence),
    names_to  = "metric",
    values_to = "value"
  )

# 4) Plot
ggplot(long_df, aes(x = playlist_position, y = value)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Midnight Drive: Feature Evolution",
    x     = "Track Position",
    y     = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


library(dplyr)
library(tidyr)

# 1) Join in the features
feature_plot_data <- my_playlist %>%
  left_join(songs2, by = "track_id") %>%
  
  # 2) Select only the columns we want
  select(playlist_position, name,
         popularity, danceability, energy, valence) %>%
  
  # 3) Pivot into long form
  pivot_longer(
    cols      = c(popularity, danceability, energy, valence),
    names_to  = "metric",
    values_to = "value"
  )



library(ggplot2)
library(dplyr)
library(scales)   # for pretty_breaks()

ggplot(feature_plot_data, aes(x = playlist_position, y = value, group = 1)) +
  # slightly thicker, crisper line and points
  geom_line(color = "#1DB954",   size = 1.2) +
  geom_point(color = "#1DB954",  size = 2.5, alpha = 0.8) +
  
  # two columns of facets, free y-scales
  facet_wrap(~ metric, scales = "free_y", ncol = 2) +
  
  # label every track position, small padding at ends
  scale_x_continuous(breaks = 1:12, expand = expansion(add = c(0.2, 0.2))) +
  
  # only 3 “pretty” y-breaks per panel, small padding
  scale_y_continuous(breaks = pretty_breaks(n = 3), 
                     expand = expansion(mult = 0.02)) +
  
  labs(
    title    = "Midnight Drive: Rise, Fall, and Rise Again",
    subtitle = "Audio-feature arc across 12 tracks",
    x        = "Track Position",
    y        = NULL,
    caption  = "Each panel shows how Danceability, Energy, Popularity & Valence\n“rise → fall → rise” to guide the emotional flow of Midnight Drive."
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    # only horizontal grid lines, very light
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    panel.grid.minor   = element_blank(),
    
    # more breathing room between facets
    panel.spacing = unit(1.5, "lines"),
    
    # bold, centered facet labels
    strip.text = element_text(face = "bold", size = 12),
    
    # cleaner axis text
    axis.text = element_text(color = "gray25"),
    axis.ticks = element_line(color = "gray25"),
    
    # punchier title/subtitle
    plot.title   = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle= element_text(size = 12, margin = margin(b = 10), hjust = 0),
    plot.caption = element_text(size = 10, color = "gray40", hjust = 0)
  )
