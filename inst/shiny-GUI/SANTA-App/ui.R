# ui.R

# SANTA-App
# Based on SANTA v1.2.0, R >= 3.4.0, shiny >= 1.3.2, bslib
# Arnaud M. Wolfer
# Computational and Systems Medicine 
# Imperial College London
# 10/10/2019
# Licensed under GPLv3	
#
# Copyright (C) {2022}  {Arnaud M. Wolfer}
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#require(shiny)
#require(bslib)

shinyUI(fluidPage(theme = bslib::bs_theme(bootswatch = "spacelab"),
  navbarPage("santaR",
    inverse = TRUE,
    collapsible = TRUE,

  # -- About Tab --
    source(file.path("ui", "ui_about.R"),  local = TRUE)$value,

    # -- Import Tab --
    source(file.path("ui", "ui_import.R"),  local = TRUE)$value,

    # -- DF Search Tab --
    source(file.path("ui", "ui_dfSearch.R"),  local = TRUE)$value,

    # -- Analysis Tab --
    source(file.path("ui", "ui_analysis.R"),  local = TRUE)$value,

    # -- Export Tab --
    source(file.path("ui", "ui_export.R"),  local = TRUE)$value

  )   # end navbarPage
)   # end fluidPage
)   # end shinyUI
