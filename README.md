This project consists of code and data supporting the Nothing But Stats! Shiny application, hosted [here](https://seho.shinyapps.io/nothing-but-stats/). 

# Project Tree

`app/`: contains the Shiny app hosted at the above location.

* `data/`: internal datasets used by Shiny app.
* `R/`: code that supports the Shiny app.
   * `ach.R`: achievements
   * `format.R`: table and plot formatting
   * `frivolities.R`: random fun things
   * `metadata.R`: metadata saved as objects
   * `news.R`: newsfeed generation
   * `plot.R`: plotting functions
   * `popup.R`: functions supporting pop-ups
   * `pr.R`: power rankings calculation
   * `read.R`: i/o functions
   * `summarize.R`: commonly used summarizing functions
   * `utils.R`: utility functions
* `www/`: image files.
* `server.R`: server-side configuration.
* `ui.R`: UI configuration.
* `update.R`: support code for `refresh.R`.

`refresh.R`: refresh the Shiny app with new data entered in Sheets.