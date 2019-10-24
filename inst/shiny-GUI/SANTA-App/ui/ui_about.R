# ABOUT - Tab panel  --------------------------------------------------------- #

tabPanel("About",
  includeHTML("data/about.html"),
  br(),
  wellPanel(
    em(textOutput("spline.ver1"), style = "color:grey") #"; font-size:small")
  )
)
# end ABOUT Tab panel -------------------------------------------------------- #
