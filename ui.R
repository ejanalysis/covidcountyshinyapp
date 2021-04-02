# TO PUBLISH ON SERVER AS WEB APP:
#  # need to build my pkg, then host it on github, then reinstall to my local using install_github
#  # devtools::install_github('ejanalysis/covidcountyshinyapp')
#  Then publish update of just ui.R and server.R files
#  # and that way the locally installed version retains info on where it is on github
#  # so that hosting service can obtain and install it on their server

library(shiny)
library(covidcountyshinyapp)

defaultareanamestostart <- c(
    "District of Columbia, District of Columbia",
    'Montgomery County, Maryland',
    'Prince George\'s County, Maryland',
    'Howard County, Maryland',
    'Anne Arundel County, Maryland',
    'Charles County, Maryland',
    'Fairfax County, Virginia',
    'Fairfax city, Virginia',
    'Arlington County, Virginia',
    'Alexandria city, Virginia',
    'Loudoun County, Virginia',
    'Prince William County, Virginia',
    'Manassas Park city, Virginia',
    'Manassas city, Virginia',
    'Falls Church city, Virginia'
)
allcounties <- getcountyfullnames()
allstates <- sort(c(state.name, 'District of Columbia'))


shinyUI(
    fluidPage(
        titlePanel("COVID-19 Cases County-Level Daily Data from NYT"),
        ########### sidebarLayout #######
        sidebarLayout(
            ########## sidebarPanel ##########
            sidebarPanel(
                shiny::selectInput('countylistinput', 'Counties for Trend Plot',
                                   selected = defaultareanamestostart,
                                   multiple = TRUE, choices = allcounties),
                shiny::selectInput('statelistinput', 'States for Trend Plot (counties added to any selected above)',
                                   # selected, #start with none
                                   multiple = TRUE, choices = allstates),

                # shiny::h5('Whole States (in addition to above)'),
                # shiny::selectInput('stateTOTALlistinput', 'State Totals (each State as a whole)',
                #                   # selected, #start with none
                #                    multiple = TRUE, choices = allstates),

                shiny::actionButton(inputId = 'defaultareabutton', 'Default places'),
                shiny::hr(),
                # shiny::h5(paste('Cumulative to most recent day (in selected places):')),
                # shiny::tableOutput('selectedlist'),
                #
                #                 shiny::numericInput(inputId = 'n', label =  'Show the top n in US',
                #                                     value = 3, min = 1, max = 100, step = 1),
                #                 shiny::h5(paste('Cumulative to most recent day (in USA):')),
                #                 shiny::tableOutput('toplist'),
                #                 shiny::hr(),
                shiny::numericInput(inputId = 'ndays', label =  '# of days to show (last n)',
                                    value = 365, min = 30, max = 400, step = 1),
                shiny::numericInput(inputId = 'lastn', label =  '# of days to fit recent line to',
                                    value = 14, min = 2, max = 60, step = 1),
                shiny::numericInput(inputId = 'dayscontagious', label =  '# of days new case stays contagious',
                                    value = 14, min = 7, max = 60, step = 1),
                shiny::numericInput(inputId = 'avgtime', label = 'Days running avg.',
                                    value = 3, min = 1, max = 14, step = 1),
                shiny::numericInput(inputId = 'smoothspan', label = 'amount of smoothing for lowess',
                                    value = 0.15, min = 0.01, max = 0.9, step = 0.01),
                shiny::dateInput(inputId = 'throughdate', label = 'Exclude data after',
                                 value = Sys.Date(), min = '2020-01-01', max = Sys.Date()),
                shiny::radioButtons(inputId = 'ascertainmentbias', label = 'Actual cases per positive test (ascertainment)',
                                    choices = c(1,5,10), selected = 1),

                shiny::downloadButton(label = 'Download dataset for selected places', outputId = 'download_data'),

                # shiny::downloadButton(label = 'Download graphs for selected places', outputId = 'download_graphs'),

                width = 4), # end of side panel
            ########## mainPanel ##########
            mainPanel(
                tabsetPanel(
                    ##### panel 1 ######
                    tabPanel(title = 'Plots',

                             shiny::hr(),
                             textOutput('assumed.ascertainmentbias'),
                             textOutput('assumed.dayscontagious'),
                             shiny::hr(),

                             plotOutput('contagious'),       # new cases in last x days
                             plotOutput('contagiouseach'),    # new cases in last x days

                             plotOutput("barworstnow"),  # worst few for current cases per cap
                             plotOutput('barthese'),  #,  # worst few for cumulative cases per cap

                             plotOutput('trendnewrunfit'),  # new cases per day
                             plotOutput('trendnew')        # new cases per day

                             # plotOutput('cumvnew'),       # cumulative cases (sum of new cases in all prior days)
                             # plotOutput("trendplot"),     # cumulative cases for aggregate of counties
                             # plotOutput('trendplotneweachplace')  # cumulative cases in each county
                    ), # end of panel 1
                    ##### panel 2 ######
                    tabPanel(title = 'Table of all data',
                             DT::dataTableOutput(outputId = 'xtable')
                    )  # end of panel 2
                    ########## end ##########
                ) # end of tabsetPanel
            ) # end of mainPanel
        ) # end of sidebar layout
    ) # end of overall fluid page
) # end of app
