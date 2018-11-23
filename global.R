
#initialize colors
v_light_gray <- '#a3a3a3' #'#CDCDCD'
med_gray <- '#696969' #"#808080"
v_dark_gray <- '#323232' #'#252525'
maroon1 <- '#24292e' # '#8c001a'
maroon2 <- '#7d430e'

#test color pallette
electric_lime <- '#88D317'
electric_blue <- '#4be0f6'
sunshine <- '#fda302' # orange
shadow <- '#535353'
cyan <- '#43c0f5'
charcoal <- '#3d3d3d'

library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(googleVis)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)

# style column names / header for datatables
dt_column_head <- JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#3d3d3d', 'color': '#fff'});",
  "}")

# set general theme for ggplots

my_theme <- theme(panel.background = element_blank(),
                  axis.text = element_text(size = '15'),
                  axis.title = element_blank())

custom_colors <- ""
# 
# # use custom color pallette across app
# custom_colors <- HTML(paste0('
#                                          /* logo */
#                              .skin-blue .main-header .logo {
#                              background-color:',maroon1,';
#                              }
#                              
#                              /* logo when hovered */
#                              .skin-blue .main-header .logo:hover {
#                              background-color:',maroon1,';
#                              }
#                              
#                              /* toggle button when hovered  */
#                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
#                              background-color:',v_dark_gray,';
#                              }
#                              
#                              /* navbar (rest of the header) */
#                              .skin-blue .main-header .navbar {
#                              background-color:',maroon1,';
#                              }
#                              
#                              /* main sidebar */
#                              .skin-blue .main-sidebar {
#                              background-color:',v_dark_gray,';
#                              }
#                              
#                              .skin-blue .sidebar-menu > li:hover > a,
#                              .skin-blue .sidebar-menu > li.active > a {
#                              color: white;
#                              background:',maroon1,';
#                              border-left-color:',maroon1,';
#                              }
#                              .skin-blue .sidebar-menu > li > .treeview-menu {
#                              margin: 0 1px;
#                              background:',med_gray,';
#                              }
#                              .skin-blue .treeview-menu > li.active > a,
#                              .skin-blue .treeview-menu > li > a:hover {
#                              color: white;
#                              background:',maroon1,';
#                              }
#                              
#                              .skin-blue .sidebar a {
#                              color: white;
#                              }
#                              .skin-blue .treeview-menu > li > a {
#                              color: white;
#                              }
#                              
#                              .small-box h3 {
#                              font-size: 38px;
#                              font-weight: 700;
#                              margin: 0 0 10px;
#                              white-space: nowrap;
#                              padding: 0;
#                              }
#                              .bg-primary {
#                              color: #fff;
#                              background-color: #337ab7;
#                              }
#                              '))
