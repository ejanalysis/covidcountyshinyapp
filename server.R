# TO PUBLISH ON SERVER AS WEB APP:
#  # need to build my pkg, then host it on github, then reinstall to my local using install_github
#  # devtools::install_github('ejanalysis/covidcountyshinyapp')
#  Then publish update of just ui.R and server.R files
#  # and that way the locally installed version retains info on where it is on github
#  # so that hosting service can obtain and install it on their server


# for county daily data see 'https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv'
# for state daily data incl pct positive of all tests see
#   https://covidtracking.com/api/v1/states/daily.csv

# setwd("~/Dropbox/COVID/covidcounty")

# From ejanalysis.com (should be obsolete)
# require(ejscreen) # for county population totals # taken from ejscreen pkg data(bg19)
# require(analyze.stuff) # for lead.zeroes function (JUST THAT??)
# require(ejanalysis) # for 2 letter state abbreviations via get.state.info

library(shiny)
library(lubridate) # for dates handling
library(caTools) # for running average of new cases/day
library(covidcountyshinyapp)

# Downloads from website each time shiny app is launched
# (which is a waste and a bit slow if launched many times a day
# and not up-to-date if left running more than 24 hours):

x <- covidDownload()  # this will be in global environment of app
# print('download finished')

defaultareanamesviabutton <- c(
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

# # Example:
#  countylist <- defaultareanamesviabutton
#  covidPlotTrendNew(x, countylist = countylist)


shinyServer(function(input, output, session) {

    xlive <- shiny::reactive({
        countylist <- countylist()
        if (is.null(countylist) | length(countylist) == 0) {countylist <- unique(x$fullname)}
        data <- x[x$fullname %in% countylist, ]  # limit which counties to those selected

        # CALCULATE THE CUMULATIVE LAST 14 DAYS OF NEW CASES, PER 1OOK POP, BY COUNTY
        data$newrecentlyper100k <- 100000 * with(data, stillcontagious_percap_bycounty(date, new, fullname, pop, dayscontagious = input$dayscontagious))

        # LIMIT TO EXCLUDE DATES AFTER ___throughdate, AND ONLY SHOW THE LAST ___ndays DAYS
        data <- data[data$date <= input$throughdate, ]
        data <- data[data$date > (max(data$date) - input$ndays), ]

        # FACTOR IN ASCERTAINMENT BIAS HERE?
        quantfields <- c('new', 'cases', 'percap', 'newrecentlyper100k')
        data[ , quantfields] <- data[ , quantfields] * as.numeric(input$ascertainmentbias)
        data$oneper <- round(data$oneper / as.numeric(input$ascertainmentbias), 0)

        ############## CALCULATE Aggregate of all selected places ######
        datasum <- sum_counties(data, countylist = countylist, datecolname = 'date', countycolname = 'fullname')
        datasum$fullname <- 'sum'
        datasum$county <- 'sum'; datasum$state <- 'sum'; datasum$fips <- 0; datasum$fullnameST <- 'sum'; datasum$ST <- 'sum'
        if (any(sort(names(data)) != sort(names(datasum)))) {stop('names of columns in data and aggregated data differ')}
        datasum <- datasum[ , names(data)]
        data <- rbind(data, datasum)
        ##############        #print('updated dataset based on selected places and dates')
        data
    })

    output$xtable <- DT::renderDataTable(expr = {
        shown <- xlive()

        # Rename columns and fix rounding
        names(shown) <- gsub('cases', 'cum.cases', names(shown))
        names(shown)[names(shown) == 'new'] <- 'new.cases' # avoid changing newrecentlyper100k
        names(shown) <- gsub('percap', 'cum.percap', names(shown))
        shown$cum.percap <- round(shown$cum.percap, 5)
        shown$newrecentlyper100k <- round(shown$newrecentlyper100k, 1)

        # specify which fields to show, in what order
        shown <- shown[ , c('date', 'state', 'fullnameST', 'pop', 'deaths', 'cum.cases', 'cum.percap', 'new.cases', # 'oneper',
                            'newrecentlyper100k')]
        shown[order(shown$date, shown$fullnameST, decreasing = TRUE), ]
    },
    options = list(pageLength = 30,
                   search = list(search = 'sum')
    ),
    server = TRUE)

    output$assumed.dayscontagious <- renderText({
        paste('Assuming # currently contagious = all new cases in the last ', input$dayscontagious, 'days', sep = '' )
    })
    output$assumed.ascertainmentbias <- renderText({
        paste('Assuming # of true cases = ', as.numeric(input$ascertainmentbias), 'x tested positive', sep = '' )
    })


    countylist <- reactive({
        countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        countylist <- countylist[!is.na(countylist)]
        # countylist <- c(countylist, 'sum') # to have sum of all places but not sure I always want that eg if only one place selected
        countylist
    })

    wholestatelist <- reactive({
        #        input$stateTOTALlistinput # NOT USED YET

        # a summary row for each state on each day could be added to x dataset?
        # or a separate aggregate for every US states can be done once up front and added into each graphic or bar function?
        # or each selected state only can be aggregated on the fly and added into each graphic or bar function?

    })

    observeEvent(eventExpr = input$defaultareabutton, handlerExpr = {
        shiny::updateSelectInput(session, inputId = 'countylistinput',
                                 selected = defaultareanamesviabutton)
    })

    output$download_data <- shiny::downloadHandler(filename = 'covid_data.csv', content =  function(file) {
        write.csv(xlive(), file, row.names = FALSE)  # use reactive xlive, or do following:
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        # data <- x[x$fullname %in% countylist, ]  # limit which counties to those selected
        # data <- data[data$date > (max(data$date) - input$lastn), ]  # limit which days to the last n
        # write.csv(data, file, row.names = FALSE)
    })

    # output$download_graphs <- shiny::downloadHandler(filename = 'covid_data.jpg', content = function(file) {
    # DOES NOT WORK YET
    # renderPlot({
    # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
    # countylist <- countylist[!is.na(countylist)]
    #         covidPlotTrendNew(xlive(), countylist = countylist(),
    #                           averagingtime = input$avgtime, smoothspan = input$smoothspan,
    #                           ndays = input$ndays, lastn = input$lastn)
    #     # })
    # })

    output$trendplot <- renderPlot({
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        covidPlotTrend(xlive(), countylist = countylist(),
                       ndays = input$ndays)
    }) # , width = 900, height = 700)

    output$trendnew <- renderPlot({
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        covidPlotTrendNew(xlive(), countylist = countylist(),
                          averagingtime = input$avgtime, smoothspan = input$smoothspan,
                          ndays = input$ndays, lastn = input$lastn)
    })

    output$trendnewrunfit <- renderPlot({
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        covidPlotTrendNewRunFit(xlive(), countylist = countylist(),
                                averagingtime = input$avgtime,
                                ndays = input$ndays, lastn = input$lastn)
    })

    output$trendplotneweachplace <- renderPlot({
        countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        countylist <- countylist[!is.na(countylist)]
        covidPlotTrendNewEachPlace(xlive(), countylist = c(countylist(), 'sum') ,
                                   averagingtime = input$avgtime,
                                   ndays = input$ndays, lastn = input$lastn, percap = TRUE)
    })

    output$cumvnew <- renderPlot({
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        covidPlotCum_v_New(xlive(), countylist = countylist(),
                           averagingtime = input$avgtime,
                           ndays = input$ndays)
    })

    output$contagious <- renderPlot({
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        covidPlotContagious(xlive(), countylist = countylist(),
                            ndays = input$ndays, dayscontagious = input$dayscontagious)
    })

    output$contagiouseach <- renderPlot({
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        covidPlotContagiousEach(xlive(), countylist = c(countylist(), 'sum') ,
                                ndays = input$ndays, dayscontagious = input$dayscontagious)
    })

    output$toplist <- renderTable({
        # worst of all in US, regardless of selections
        # could recode covidPlotWorstFew to separate getting list and plotting it
        covidTableWorstFew(xnow = covidToday(x), n = input$n)
    })

    output$selectedlist <- renderTable({
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        # covidTableSelected(xnow = covidToday(x[x$fullname %in% countylist, ]))
        covidTableSelected(xnow = covidToday(xlive()))
    })

    output$barthese <- renderPlot({
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        covidPlotBarNow(x = xlive(), horiz = TRUE, n=15,
                        # covidPlotBarNow(x = x[x$fullname %in% countylist, ], horiz = TRUE,
                        main = 'Ever a confirmed case (cumulative, not necessarily still contagious)',
                        noworever = 'ever', inapp = TRUE)
    })

    output$barworstnow <- renderPlot({
        # countylist <- unique(c(input$countylistinput, countiesinstates(input$statelistinput, allcounties)))
        # countylist <- countylist[!is.na(countylist)]
        # covidPlotBarNow(x = x[x$fullname %in% countylist, ], horiz = TRUE,
        covidPlotBarNow(x = xlive(), horiz = TRUE,n=15,
                        main = paste('Recently confirmed case (in last', input$dayscontagious, 'days, so maybe still contagious)'),
                        dayscontagious = input$dayscontagious,
                        noworever = 'now', inapp = TRUE)
    })

    # NOTE: package has these functions

    # source('getcountyfullnames.R')
    # source('getcountypop.R')
    # source('countiesinstates.R')
    # source('sum_counties.R')
    # source('orderbylatest.R')
    # source('stillcontagious.R')
    # source('latestcontagiouspercap.R')
    # source('covidDownload.R')
    # source('covidtrackingDownload.R')
    # source('covidWrite.R')
    # source('covidToday.R')
    # source('covidPlotTrend.R')
    # source('covidPlotTrendNew.R')
    # source('covidPlotTrendNewEachPlace.R')
    # source('covidPlotTrendNewRunFit.R')
    # source('covidPlotCum_v_New.R')
    # source('covidPlotContagious.R')
    # source('covidPlotContagiousEach.R')
    # source('covidPlotWorstFew.R')
    # source('covidTableWorstFew.R')
    # source('covidTableSelected.R')
    # source('covidPlotBarNow.R')
})
