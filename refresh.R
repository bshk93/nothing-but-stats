setwd("C:/Users/bshk9/OneDrive/home/projects/nbn/nothing-but-stats")


# Player bios
googledrive::drive_download(
  "https://docs.google.com/spreadsheets/d/1i3vuaoJOxKrynLZsjV8VGv3eslunMPbrGsrax-xCyVk/", 
  "player-bio-database.csv", "csv", overwrite = TRUE
)
