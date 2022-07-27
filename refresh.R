setwd("C:/Users/bshk9/OneDrive/home/projects/nbn/nothing-but-stats")


# Player bios
googledrive::drive_download(
  "https://docs.google.com/spreadsheets/d/1i3vuaoJOxKrynLZsjV8VGv3eslunMPbrGsrax-xCyVk/", 
  "player-bio-database.csv", "csv", overwrite = TRUE
)

# 2020-21 stats
googledrive::drive_download(
  "https://docs.google.com/spreadsheets/d/1MPpOyd5ZsMhZYW1k9sSGge9ihD4C9mMd-5oiODvWP8g/",
  "allstats-20-21.csv", "csv", overwrite = TRUE
)
