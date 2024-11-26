This project consists of code and data supporting the Nothing But Stats! Shiny application, hosted [here](https://stats.nbn.today). 

# Project Tree

`app/`: contains the Shiny app hosted at the above location.

* `data/`: internal datasets used by Shiny app.
* `R/`: code that supports the Shiny app.
   * `format.R`: table and plot formatting
   * `frivolities.R`: random fun things
   * `metadata.R`: metadata saved as objects
   * `plot.R`: plotting functions
   * `popup.R`: functions supporting pop-ups
   * `pr.R`: power rankings calculation
   * `summarize.R`: commonly used summarizing functions
   * `utils.R`: utility functions
* `www/`: image and other files.
* `server.R`: server-side configuration.
* `ui.R`: UI configuration.

`refresh.R`: refresh the Shiny app with new data entered in Sheets.