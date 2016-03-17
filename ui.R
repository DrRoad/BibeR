# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(navbarPage(
  "BibeR",
  tabPanel("Home", includeHTML("user guide.html")),
  tabPanel(
    "Load file", sidebarLayout(
      sidebarPanel(
        fileInput("file_txt", "Upload file here:"),
        p("Here accept single .txt or compressed .zip file saved from Web of Science."),
        p("Since it may take long time for web server to parse your raw .txt files, it is encouraged to save the parsed file as .csv format. Please click the button below."),
        p("Then, upload the .csv file, it just takes milli seconds to start the analysis!"),
        selectInput('save_file', 'Save as:', choices = c('No, thanks!' = 'no_thanks', 'csv' = 'csv')),
        conditionalPanel(condition = "input.save_file == 'csv'",
                         downloadButton("save_csv", "Download"))
      ),
      mainPanel(
        dataTableOutput('data_parse_all')
      ),
      position = "right"
    )
  ),
  
  tabPanel("Basic results",
           sidebarLayout(
             sidebarPanel(
               selectInput('basic_plot', "Select analysis:", 
                           c('year trend' = 'yt','journal table' = 'jt','category table' = 'cat',
                             'author table' = 'at','keyword table' = 'kt','country table' = 'ct',
                             'institution table' = 'it', 'country world map' = 'cwm')),
               conditionalPanel(condition = "input.basic_plot == 'yt'",
                                sliderInput('year_slider', 'Select year range:', min = 1991, max = 2015,
                                            value = c(1991, 2015), step = 1),
                                selectInput('year_slice', 'Slice?', c('No' = 'n', 'Yes' = 'y')),
                                conditionalPanel(condition = "input.year_slice == 'y'",
                                                 sliderInput('year_slice_interval',
                                                             'Select year interval:', min = 1, max = 50,
                                                             value = 5, step = 1))
                                ),
               conditionalPanel(condition = "input.basic_plot == 'jt'",
                                sliderInput('year_slider_for_journal_table', 'Select year range:', min = 1991, max = 2015,
                                            value = c(1991, 2015), step = 1)
               ),
               conditionalPanel(condition = "input.basic_plot == 'cat'",
                                sliderInput('year_slider_for_category_table', 'Select year range:', min = 1991, max = 2015,
                                            value = c(1991, 2015), step = 1)
               ),
               conditionalPanel(condition = "input.basic_plot == 'at'",
                                sliderInput('year_slider_for_author_table', 'Select year range:', min = 1991, max = 2015,
                                            value = c(1991, 2015), step = 1)
               ),
               conditionalPanel(condition = "input.basic_plot == 'kt'",
                                sliderInput('year_slider_for_keyword_table', 'Select year range:', min = 1991, max = 2015,
                                            value = c(1991, 2015), step = 1)
               ),
               conditionalPanel(condition = "input.basic_plot == 'ct'",
                                sliderInput('year_slider_for_country_table', 'Select year range:', min = 1991, max = 2015,
                                            value = c(1991, 2015), step = 1)
               ),
               conditionalPanel(condition = "input.basic_plot == 'it'",
                                sliderInput('year_slider_for_institution_table', 'Select year range:', min = 1991, max = 2015,
                                            value = c(1991, 2015), step = 1)
               ),
               conditionalPanel(condition = "input.basic_plot == 'cwm'",
                                sliderInput('year_slider_for_country_worldmap', 'Select year range:', min = 1991, max = 2015,
                                            value = c(1991, 2015), step = 1))
             ),
             mainPanel(
               conditionalPanel(condition = "input.basic_plot == 'yt'",
                                plotOutput('year_plot'),
                                conditionalPanel(condition = "input.year_slice == 'y'",
                                                 plotOutput('year_slice_plot'))
                                ),
               conditionalPanel(condition = "input.basic_plot == 'jt'",
                                dataTableOutput('journal_table')),
               conditionalPanel(condition = "input.basic_plot == 'cat'",
                                dataTableOutput('category_table')),
               conditionalPanel(condition = "input.basic_plot == 'at'",
                                dataTableOutput('author_table')),
               conditionalPanel(condition = "input.basic_plot == 'kt'",
                                dataTableOutput('keyword_table')),
               conditionalPanel(condition = "input.basic_plot == 'ct'",
                                dataTableOutput('country_table')),
               conditionalPanel(condition = "input.basic_plot == 'it'",
                                dataTableOutput('institution_table')),
               conditionalPanel(condition = "input.basic_plot == 'cwm'",
                                plotOutput('country_world_map'))
             )
           )),
  
  tabPanel("Advanced results",
           sidebarLayout(
             sidebarPanel(
               selectInput('adv_plot', "Select analysis:", 
                           c('heatmap' = 'heatmap','network' = 'network')),
               
               # if user select heatmap 
               conditionalPanel(condition = "input.adv_plot == 'heatmap'",
                                selectInput('heatmap', 'Select item:',
                                            c('journal' = 'j', 'category' = 'cat',
                                              'author' = 'au', 'keyword' = 'key',
                                              'country' = 'c', 'institution' = 'i')
                                            ),
                                conditionalPanel(condition = "input.heatmap == 'j'",
                                                 sliderInput('hj_slider', 'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('hj_all_or_not','Select mode:',
                                                             choices = c('Top journals' = 'tj',
                                                                         'Customize' = 'cust')),
                                                 conditionalPanel(condition = "input.hj_all_or_not == 'tj'",
                                                                  numericInput('heatmap_journal_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.hj_all_or_not == 'cust'",
                                                                  selectInput('heatmap_journal_multi', "Select journals:",
                                                                              choices = c(""), multiple = T))
                                                 ),
                                conditionalPanel(condition = "input.heatmap == 'cat'",
                                                 sliderInput('hcat_slider', 'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('hcat_all_or_not','Select mode:',
                                                             choices = c('Top categories' = 'tcat',
                                                                         'Customize' = 'cust')),
                                                 conditionalPanel(condition = "input.hcat_all_or_not == 'tcat'",
                                                                  numericInput('heatmap_category_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.hcat_all_or_not == 'cust'",
                                                                  selectInput('heatmap_category_multi', "Select categories:",
                                                                              choices = c(""), multiple = T))
                                                 ),
                                conditionalPanel(condition = "input.heatmap == 'au'",
                                                 sliderInput('hau_slider', 'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('hau_all_or_not','Select mode:',
                                                             choices = c('Top authors' = 'tau',
                                                                         'Customize' = 'cust')),
                                                 conditionalPanel(condition = "input.hau_all_or_not == 'tau'",
                                                                  numericInput('heatmap_au_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.hau_all_or_not == 'cust'",
                                                                  selectInput('heatmap_author_multi', "Select authors:",
                                                                              choices = c(""), multiple = T))
                                                 ),
                                conditionalPanel(condition = "input.heatmap == 'key'",
                                                 sliderInput('hkey_slider', 'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('hkey_all_or_not','Select mode:',
                                                             choices = c('Top keywords' = 'tkey',
                                                                         'Customize' = 'cust')),
                                                 conditionalPanel(condition = "input.hkey_all_or_not == 'tkey'",
                                                                  numericInput('heatmap_key_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.hkey_all_or_not == 'cust'",
                                                                  selectInput('heatmap_keyword_multi', "Select keywords:",
                                                                              choices = c(""), multiple = T))
                                                 ),
                                conditionalPanel(condition = "input.heatmap == 'c'",
                                                 sliderInput('hc_slider', 'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('hc_all_or_not','Select mode:',
                                                             choices = c('Top countries' = 'tc',
                                                                         'Customize' = 'cust')),
                                                 conditionalPanel(condition = "input.hc_all_or_not == 'tc'",
                                                                  numericInput('heatmap_c_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.hc_all_or_not == 'cust'",
                                                                  selectInput('heatmap_country_multi', "Select countries:",
                                                                              choices = c(""), multiple = T))
                                                 ),
                                conditionalPanel(condition = "input.heatmap == 'i'",
                                                 sliderInput('hi_slider', 'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('hi_all_or_not','Select mode:',
                                                             choices = c('Top institutions' = 'ti',
                                                                         'Customize' = 'cust')),
                                                 conditionalPanel(condition = "input.hi_all_or_not == 'ti'",
                                                                  numericInput('heatmap_i_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.hi_all_or_not == 'cust'",
                                                                  selectInput('heatmap_institution_multi', "Select institutions:",
                                                                              choices = c(""), multiple = T))
                                                 )
                                ),
               
               # if select network
               
               conditionalPanel(condition = "input.adv_plot == 'network'",
                                selectInput('network', "Select item:",
                                            choices = c('author' = 'au', 'keyword' = 'key',
                                                        'country' = 'c', 'institution' = 'i')
                                            ),
                                conditionalPanel(condition = "input.network == 'au'",
                                                 sliderInput('nau_slider',  'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('nau_all_or_not','Select mode:',
                                                             choices = c('Top authors' = 'tau',
                                                                         'Customize' = 'cust',
                                                                         'Cross match' = 'cm')),
                                                 conditionalPanel(condition = "input.nau_all_or_not == 'tau'",
                                                                  numericInput('network_author_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.nau_all_or_not == 'cust'",
                                                                  selectInput('network_author_multi', "Select authors:",
                                                                              choices = c(""), multiple = T)),
                                                 conditionalPanel(condition = "input.nau_all_or_not == 'cm'",
                                                                  selectInput('network_author_cm_keyword', "Select keywords:",
                                                                              choices = c(""), multiple = T),
                                                                  numericInput('network_author_number_for_cm', "Input number:", value = 5))
                                                 ),
                                
                                conditionalPanel(condition = "input.network == 'key'",
                                                 sliderInput('nkey_slider',  'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('nkey_all_or_not','Select mode:',
                                                             choices = c('Top keywords' = 'tkey',
                                                                         'Customize' = 'cust',
                                                                         'Cross match' = 'cm')),
                                                 conditionalPanel(condition = "input.nkey_all_or_not == 'tkey'",
                                                                  numericInput('network_keyword_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.nkey_all_or_not == 'cust'",
                                                                  selectInput('network_keyword_multi', "Select authors:",
                                                                              choices = c(""), multiple = T)),
                                                 conditionalPanel(condition = "input.nkey_all_or_not == 'cm'",
                                                                  selectInput('nkey_cm_which', 'Select which to cross match:',
                                                                              choices = c('Author' = 'au', 'Country' = 'c', 'Institution' = 'i')),
                                                                  conditionalPanel(condition = "input.nkey_cm_which == 'au'",
                                                                                   selectInput('network_keyword_cm_author', "Select authors:",
                                                                                               choices = c(""), multiple = T),
                                                                                   numericInput('network_keyword_number_for_cm', "Input number:", value = 5)),
                                                                  conditionalPanel(condition = "input.nkey_cm_which == 'c'",
                                                                                   selectInput('network_keyword_cm_country', "Select countries:",
                                                                                               choices = c(""), multiple = T),
                                                                                   numericInput('network_keyword_number_for_cm', "Input number:", value = 5)),
                                                                  conditionalPanel(condition = "input.nkey_cm_which == 'i'",
                                                                                   selectInput('network_keyword_cm_institution', "Select institutions:",
                                                                                               choices = c(""), multiple = T),
                                                                                   numericInput('network_keyword_number_for_cm', "Input number:", value = 5))
                                                                  )
                                                                  
                                                 ),
                                
                                conditionalPanel(condition = "input.network == 'c'",
                                                 sliderInput('nc_slider',  'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('nc_all_or_not','Select mode:',
                                                             choices = c('Top countries' = 'tc',
                                                                         'Customize' = 'cust',
                                                                         'Cross match' = 'cm')),
                                                 conditionalPanel(condition = "input.nc_all_or_not == 'tc'",
                                                                  numericInput('network_country_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.nc_all_or_not == 'cust'",
                                                                  selectInput('network_country_multi', "Select countries:",
                                                                              choices = c(""), multiple = T)),
                                                 conditionalPanel(condition = "input.nc_all_or_not == 'cm'",
                                                                  selectInput('network_country_cm_keyword', "Select keywords:",
                                                                              choices = c(""), multiple = T),
                                                                  numericInput('network_country_number_for_cm', "Input number:", value = 5))
                                                 ),
                                
                                conditionalPanel(condition = "input.network == 'i'",
                                                 sliderInput('ni_slider',  'Select year range:', min = 1991, max = 2015,
                                                             value = c(1991, 2015), step = 1),
                                                 selectInput('ni_all_or_not','Select mode:',
                                                             choices = c('Top institutions' = 'ti',
                                                                         'Customize' = 'cust',
                                                                         'Cross match' = 'cm')),
                                                 conditionalPanel(condition = "input.ni_all_or_not == 'ti'",
                                                                  numericInput('network_institution_number', "Input number:", value = 5)),
                                                 conditionalPanel(condition = "input.ni_all_or_not == 'cust'",
                                                                  selectInput('network_institution_multi', "Select institutions:",
                                                                              choices = c(""), multiple = T)),
                                                 conditionalPanel(condition = "input.ni_all_or_not == 'cm'",
                                                                  selectInput('network_institution_cm_keyword', "Select keywords:",
                                                                              choices = c(""), multiple = T),
                                                                  numericInput('network_institution_number_for_cm', "Input number:", value = 5))
                                                 )
                                
                                )
               
             ),
             mainPanel(
               
               conditionalPanel(condition = "input.adv_plot == 'heatmap'",
                                conditionalPanel(condition = "input.heatmap == 'j'",
                                                 
                                                 conditionalPanel(condition = "input.hj_all_or_not == 'tj'",
                                                                  plotOutput('journal_heatmap_top')),
                                                 conditionalPanel(condition = "input.hj_all_or_not == 'cust'",
                                                                  plotOutput('journal_heatmap_multi'))
                                ),
                                conditionalPanel(condition = "input.heatmap == 'cat'",
                                                 conditionalPanel(condition = "input.hcat_all_or_not == 'tcat'",
                                                                  plotOutput('category_heatmap_top')),
                                                 conditionalPanel(condition = "input.hcat_all_or_not == 'cust'",
                                                                  plotOutput('category_heatmap_multi'))
                                ),
                                conditionalPanel(condition = "input.heatmap == 'au'",
                                                 conditionalPanel(condition = "input.hau_all_or_not == 'tau'",
                                                                  plotOutput('author_heatmap_top')),
                                                 conditionalPanel(condition = "input.hau_all_or_not == 'cust'",
                                                                  plotOutput('author_heatmap_multi'))
                                ),
                                
                                conditionalPanel(condition = "input.heatmap == 'key'",
                                                 conditionalPanel(condition = "input.hkey_all_or_not == 'tkey'",
                                                                  plotOutput('keyword_heatmap_top')),
                                                 conditionalPanel(condition = "input.hkey_all_or_not == 'cust'",
                                                                  plotOutput('keyword_heatmap_multi'))
                                ),
                                conditionalPanel(condition = "input.heatmap == 'c'",
                                                 conditionalPanel(condition = "input.hc_all_or_not == 'tc'",
                                                                  plotOutput('country_heatmap_top')),
                                                 conditionalPanel(condition = "input.hc_all_or_not == 'cust'",
                                                                  plotOutput('country_heatmap_multi'))
                                ),
                                conditionalPanel(condition = "input.heatmap == 'i'",
                                                 conditionalPanel(condition = "input.hi_all_or_not == 'ti'",
                                                                  plotOutput('institution_heatmap_top')),
                                                 conditionalPanel(condition = "input.hi_all_or_not == 'cust'",
                                                                  plotOutput('institution_heatmap_multi'))
                                                 )
                                )
               
               ,
               conditionalPanel(condition = "input.adv_plot == 'network'",
                                conditionalPanel(condition = "input.network == 'au'",
                                                 conditionalPanel(condition = "input.nau_all_or_not == 'tau'",
                                                                  forceNetworkOutput('author_network_top')),
                                                 conditionalPanel(condition = "input.nau_all_or_not == 'cust'",
                                                                  forceNetworkOutput('author_network_multi')),
                                                 conditionalPanel(condition = "input.nau_all_or_not == 'cm'",
                                                                  forceNetworkOutput('author_network_cross_match'))
                                ),
                                
                                conditionalPanel(condition = "input.network == 'key'",
                                                 conditionalPanel(condition = "input.nkey_all_or_not == 'tkey'",
                                                                  forceNetworkOutput('keyword_network_top')),
                                                 conditionalPanel(condition = "input.nkey_all_or_not == 'cust'",
                                                                  forceNetworkOutput('keyword_network_multi')),
                                                 conditionalPanel(condition = "input.nkey_all_or_not == 'cm'",
                                                                  conditionalPanel(condition = "input.nkey_cm_which == 'au'",
                                                                                   forceNetworkOutput('keyword_network_author_cross_match')),
                                                                  conditionalPanel(condition = "input.nkey_cm_which == 'c'",
                                                                                   forceNetworkOutput('keyword_network_country_cross_match')),
                                                                  conditionalPanel(condition = "input.nkey_cm_which == 'i'",
                                                                                   forceNetworkOutput('keyword_network_institution_cross_match'))
                                                                  )
                                ),
                                conditionalPanel(condition = "input.network == 'c'",
                                                 conditionalPanel(condition = "input.nc_all_or_not == 'tc'",
                                                                  forceNetworkOutput('country_network_top')),
                                                 conditionalPanel(condition = "input.nc_all_or_not == 'cust'",
                                                                  forceNetworkOutput('country_network_multi')),
                                                 conditionalPanel(condition = "input.nc_all_or_not == 'cm'",
                                                                  forceNetworkOutput('country_network_cross_match'))
                                ),
                                conditionalPanel(condition = "input.network == 'i'",
                                                 conditionalPanel(condition = "input.ni_all_or_not == 'ti'",
                                                                  forceNetworkOutput('institution_network_top')),
                                                 conditionalPanel(condition = "input.ni_all_or_not == 'cust'",
                                                                  forceNetworkOutput('institution_network_multi')),
                                                 conditionalPanel(condition = "input.ni_all_or_not == 'cm'",
                                                                  forceNetworkOutput('institution_network_cross_match'))
                                                 )
                                )
               
               )
             )
           ),
  
  tabPanel("About", includeHTML("about.html"))
  )
)
