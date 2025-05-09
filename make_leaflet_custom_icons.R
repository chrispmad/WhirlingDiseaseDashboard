library(shiny)
library(magick)

# JUST TESTING #
fish_pos = leaflet::makeIcon(iconUrl = "https://cdn-icons-png.flaticon.com/512/15735/15735559.png",
                             iconWidth = 50, iconHeight = 50,
                             iconAnchorX = 25, iconAnchorY = 25,
                             className = 'red-square')

fish_neg = leaflet::makeIcon(iconUrl = "app/www/square-blue.png",
                             iconWidth = 50, iconHeight = 50,
                             iconAnchorX = 25, iconAnchorY = 25)

legend_html <- HTML(
  paste0(
    "<div style='background: #ffffff00; padding: 0px; border-radius: 5px;'>
     <strong>Additional Record Types</strong>
     <li><img src='",red_ex_icon$iconUrl,"' height='24' style='vertical-align:middle;'> Eradicated
     <li><img src='",native_range_square_icon$iconUrl,"' height='24' style='vertical-align:middle;'> Native Range
     <li><img src='",anecdotal_question_icon$iconUrl,"' height='24' style='vertical-align:middle;'> Anecdotal Reports
   </div>"
  )
)