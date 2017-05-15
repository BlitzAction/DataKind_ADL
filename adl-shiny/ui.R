#### App Header ####

shinyUI(fluidPage(style = "margin-top: 40px;",
  
  #theme = shinytheme("paper"),
  
#   titlePanel(title = div(div(span(tags$a(href = 'http://www.adl.org/',
#                          img(src = 'Logo_Anti-Defamation_League.jpg',
#                              width = 130)))), 
# div("The Anti-Defamation League is the leading resource on effective responses to violent bigotry, conducting an annual 
# Audit of Anti-Semitic Incidents, drafting model hate crime statutes for state legislatures, leading coalition efforts 
# to improve federal and state response to hate violence (including efforts to enact the 
# Matthew Shepard and James Byrd, Jr. Hate Crimes Prevention Act), and serving as a principal resource for the 
# FBI in developing training and outreach materials to assist in the implementation of the Hate Crime Statistics Act 
# (HCSA), which requires the Justice Department to collect statistics and report annually on hate violence from 
# law enforcement officials across the country.", 
#     style = "font-size: 18px; margin-bottom:10px;"), 
# div("Police participation in the HCSA, like all other crime data, is voluntary.  
# Unfortunately, for a variety of reasons, dozens of large cities either do not report hate crime data at all – or underreport 
# the data to the FBI.", 
#     style = "font-size: 18px; margin-bottom:10px;"),
# div("Studies show that knowing the nature and magnitude of the hate crime problem is fundamental 
# for resource allocation and crime deterrence.  More important, targeted communities are much more likely to 
# report crime and cooperate in investigations if they believe law enforcement authorities are ready and able to respond to hate violence.", 
#     style = "font-size: 18px; margin-bottom:10px;"))),
  
  tags$head(
    tags$style(HTML(".leaflet-container {
                    background-color:rgba(255,255,255,0.0);
                    }
                    .custom-popup .leaflet-popup-content-wrapper {
                    background:transparent;
                    color:#fff;
                    font-size:16px;
                    line-height:0px;
                    border-radius: 0px;
                    box-shadow: none;
                    }
                    .custom-popup .leaflet-popup-content-wrapper a {
                    color:rgba(255,255,255,0.1);
                    }
                    .custom-popup .leaflet-popup-tip-container {
                    width:30px;
                    height:15px;
                    }
                    .custom-popup .leaflet-popup-tip {
                    background: transparent;
                    border: none;
                    box-shadow: none;
                    }
                    .checkbox, .radio {
                    margin-bottom: 16px;
                    }

                    .nav.nav-tabs.shiny-tab-input.shiny-bound-input {
                    background-color: #D3D3D3;
                    }                    
                    
                    .nav.nav-tabs.shiny-tab-input.shiny-bound-input > li[class=active] > a {
                    background-color: #BFEFFF;
                    color: #ffffff;
                    }
                    
                    li, a, h1, h2, h3 {
                    font-family: 'effra', Arial, Lucida Grande, sans-serif; 
                    }
                    
                    h4{
                    font-weight: normal;
                    font-family: 'ff-tisa-web-pro', Georgia, serif;
                    }
                    
                    a{
                    color: #0034C6;
                    }

                    p { 
                    font-family: 'ff-tisa-web-pro, Georgia, serif;
                    color: 
                    #2F3031;
                    }

                    .nav-tabs > li > a {
                    text-transform: uppercase;
                    color: #0034C6;
                    }
                    .nav-tabs > li > a {
                    background-color: #f5f5f5;
                    border: solid 1px #fff;
                    border-bottom: 1px solid #ddd;
                    }
                    .nav-tabs > li.active {
                    background-color: #f5f5f5;
                    }
                    
                    .nav-tabs li.active > a > h3{
                    color: 
                    #2F3031;
                    }
                    .tab-pane{
                    border-left: 1px solid #ddd;
                    border-bottom: 1px solid #ddd;
                    border-right: 1px solid #ddd;
                    }
                    
                    #tab-1573-1 .col-sm-5, #tab-1573-2 .col-sm-5,  #tab-1573-3 .col-sm-5 {
                    padding-left:30px;
                    }
                    #tab-1573-1 .col-sm-7, #tab-1573-2 .col-sm-7,  #tab-1573-3 .col-sm-7 {
                    padding:15px;
                    padding-right:30px;
                    }
                    
                    #tab-1573-2 .col-sm-12{
                    padding:0 30px;
                    }
                    .dataTables_length{padding-left:15px;}
                    #DataTables_Table_0_filter{
                    padding-top:10px;
                    padding-right:15px;
                    }
                    .leaflet-container {
                    background-color:rgba(255,255,255,0.0);
                    }
                    .custom-popup .leaflet-popup-content-wrapper {
                    background:transparent;
                    color:#fff;
                    font-size:16px;
                    line-height:0px;
                    border-radius: 0px;
                    box-shadow: none;
                    }
                    .custom-popup .leaflet-popup-content-wrapper a {
                    color:rgba(255,255,255,0.1);
                    }
                    .custom-popup .leaflet-popup-tip-container {
                    width:30px;
                    height:15px;
                    }
                    .custom-popup .leaflet-popup-tip {
                    background: transparent;
                    border: none;
                    box-shadow: none;
                    }
                    .checkbox, .radio {
                    margin-bottom: 16px;
                    }

                    "))
    ),
#color: #333333;  
# #FF6600
  # navbarPage(
  #   title = NULL,

    tabsetPanel(type = "tabs",
                id = "student_tabs",
                
             tabPanel(h3("Underreported Hate Crimes"), 
                      
                      fluidRow(column(5,
                                      
                                      h4("Check the boxes to display cities that did not report hate crimes or reported zero (0) hate crimes in the selected year."),
                                      h3("Note: Make sure to zoom in for further detail"),
                                      h4(style = "padding-top: 10px;",
                                         checkboxGroupInput("layers_1", 
                                                            label = NULL,
                                                            choices = c("Did Not Report" = "Did Not Report",
                                                                        "Reported 0" = "Reported 0"),
                                                            selected = NULL,
                                                            inline = T)),
                                      
                                      column(6,
                                             h4(selectInput("input_years2", "Select year",
                                                            choices = c(2004:2015), 
                                                            multiple = FALSE, 
                                                            selectize = TRUE,
                                                            selected = 2015,
                                                            width = '98%'),
                                                style = "padding-top: 10px;")),
                                      
                                      column(6,
                                             h4(selectInput("input_states2", "Select state",
                                                            choices = states, 
                                                            multiple = FALSE, 
                                                            selectize = TRUE,
                                                            selected = "All states",
                                                            width = '98%'),
                                                style = "padding-top: 10px;")),
                                      
                                      column(12, 
                                             style = "padding-top: 10px;",
                                             h4(htmlOutput("annotation1"))),
                                      
                                      column(12, 
                                             style = "padding-top: 5px;",
                                             h4(htmlOutput("annotation2")))
                                      
                                      # column(12,
                                      #        h4(helpText(style = "color: #000000",
                                      #                    tags$b("Note:")),
                                      #           helpText(style = "color: #000000",
                                      #                    "No information available about cities that did not report 
                                      #                    any hate crimes before 2010. Information is only vailable 
                                      #                    before 2010 about cities that reported 0 hate crimes.")))
                                      ),
                               
                               column(7, 
                                      leafletOutput("map2_1", height = "695px")))
                      ),
             
             
             tabPanel(h3("Hate Crimes by Group"), 
                      
                      fluidRow(
                        column(5, 
                               h4("Select a year to see the total number of hate crimes in each state that were reported to the FBI that year. Click on a state to zoom in and see details."),
                               h4("Select a type of hate crime to see map shading and cities associated only with that type of hate crime."),
                               
                               fluidRow(
                                 column(6,
                                        h4(selectInput("input_years4", "Select year",
                                                       choices = unique(crime_by_city_long$date), 
                                                       multiple = FALSE, 
                                                       selectize = TRUE,
                                                       selected = 2015,
                                                       width = '98%'),
                                           style = "padding-top: 10px;")),
                                 
                                 column(6,
                                        h4(selectInput("input_states4", "Select state",
                                                       choices = states, 
                                                       multiple = FALSE, 
                                                       selectize = TRUE,
                                                       selected = "All states",
                                                       width = '98%'),
                                           style = "padding-top: 10px;"))
                               ),
                               fluidRow(
                                 column(6, #style = "border-right: 5px solid #DADADA;",
                                        h4(tags$b("Select crime type")),
                                        h4(uiOutput("dynamic_ui"))),
                                           
                                           # radioButtons("input_type2",
                                           #              label = NULL,
                                           #              choices = c("All types", crime_types),
                                           #              selected = "All types",
                                           #              inline = FALSE))),
                                 
                                 column(6, style = "border-left: 5px solid #DADADA;",
                                        h4(tags$i(tags$b(textOutput("Summary_label")))),
                                        h4(tags$i(textOutput("Total"))),
                                        h4(tags$i(textOutput("Anti_Jewish"))),
                                        h4(tags$i(textOutput("Anti_Muslim"))),
                                        h4(tags$i(textOutput("Race"))),
                                        h4(tags$i(textOutput("Sexual_Orientation"))),
                                        h4(tags$i(textOutput("Disability"))),
                                        h4(tags$i(textOutput("Ethnicity"))),
                                        h4(tags$i(textOutput("Gender"))),
                                        h4(tags$i(textOutput("Gender_Identity"))),
                                        h4(tags$i(textOutput("Race_Ethnicity_Ancestry")))
                                        )
                                 )
                               ),
                        
                        column(7, 
                               leafletOutput("map2_2", height = "695px"))),
                      
                      fluidRow(
                        column(12,
                               h4(helpText(style = "color: #000000",
                                           tags$b("Notes:")),
                                  helpText(style = "color: #000000; margin-bottom:40px;",
                                           tags$ul(
                                             tags$li("Hate crime data at the national level uses a broader 
                                                     data set than hate crime data at the state level, therefore a sum of hate crimes  
                                                     at the state level will not add up to the total hate crimes at the national level."),                                             
                                             tags$li("Hate crime data at the state level uses a broader data set than hate crime data 
                                                     at the city level. Therefore, a sum of hate crimes at the city level will 
                                                     not add up to the total hate crimes at the state level."),
                                             tags$li("Hate crimes based on race and ethnicity were merged into one category 
                                                     (i.e. Race/Ethnicity) as of 2015 and appear on the map accordingly."),
                                             tags$li("Religion-based hate crimes against Jews and Muslims are highlighted. 
                                                     Other types of religion-motivated hate crimes are not included."),
                                             tags$li("Except for cities where anti-Jewish and anti-Muslim hate crimes 
                                                     have been committed, city data is limited to cities with a population of 
                                                     over 100,000. All cities are included where anti-Jewish and anti-Muslim 
                                                     hate crimes have been committed."))
                                           )
                                  )
                               )
                        )
                      ),
             
             
             tabPanel(h3("Hate Crime Laws"), 
                      
                      fluidRow(
                        column(5, #style = "padding:10px;",# color: #4C9AD0;",
                               h4("Hover over a state to see the hate crime laws in place in that state."),
                               h4("Click on a type of hate crime law to see the states where it has been enacted."),
                               h4(radioButtons("input_type3",
                                               label = NULL,
                                               choices = list("All" = "All",
                                                              "Race" = "Race",
                                                              "Religion" = "Religion",
                                                              "National Origin" = "National Origin",
                                                              "Sexual Orientation" = "Sexual Orientation",
                                                              "Gender" = "Gender",
                                                              "Gender Identity" = "Gender Identity",
                                                              "Disability" = "Disability",
                                                              "Other" = "Other"), 
                                               selected = "All",
                                               inline = F))),
                        
                        column(7,
                               leafletOutput("map2_4", height = "400px"))),
                      
                      fluidRow(
                        h5(tabsetPanel(#title = NULL,
                                       type = "tabs",
                                  id = "tabset3", 
                                  tabPanel("Statutes by Enhancement Type", 
                                           DT::dataTableOutput("table_by_enhancement", width = "100%")),
                                  tabPanel("Other Statutes", 
                                           DT::dataTableOutput("table_by_other_types", width = "100%")))), 
                        style = "margin-bottom: 40px;")
                      )
             )
)
)