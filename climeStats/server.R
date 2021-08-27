shinyServer(function(input, output) {
    
    # ############################## Disclaimer ##############################
    # observeEvent(input$showDisclaimer,{
    #     showModal(modalDialog(
    #         title="Disclaimer","The content of this website is published in good faith.
    #         The author assumes no responsibility for any error or omission in the content of this site.
    #         The information contained in this site is provided with no guarantees of completeness or accuracy.
    #         Any action you take upon the information you find on this website is strictly at your own risk."
    #         ))
    # })
    
    ############################## climograph ##############################
    # Adding the "do it" button
    output$ui1 <- renderUI({
        req(input$file_preci, input$file_tempe)
        actionButton("Climograph_start_button", "Do it!")
    })
    # reactive expression
    Climograph_reactive <- eventReactive(input$Climograph_start_button,{
        pr <- import_data(nombre=input$file_preci$datapath)
        te <- import_data(nombre=input$file_tempe$datapath)
        do_Climograph(preci=pr,tempe=te)
        })
    # show the result
    output$plot1 <- renderPlotly({Climograph_reactive()})
    
    ############################## P trends ##############################
    # Set the degree of polynomial smoothing line
    output$ui2 <- renderUI({
        req(input$file_preci)
        selectInput(inputId="selectPolyDegree_P",label="Select the degree of the smoothing polynomial line",
                    choices=c(1:30), selected=1, multiple=FALSE)
    })
    # Set the starting month of the hydrological year
    output$ui3 <- renderUI({
        req(input$file_preci)
        selectInput(inputId="selectStartHydroYear_P",label="Select the first month of the hydrological year",
                    choices=c(1:12), selected=10, multiple=FALSE)
    })
    # Adding the "do it" button
    output$ui4 <- renderUI({
        req(input$file_preci)
        req(input$selectPolyDegree_P)
        req(input$selectStartHydroYear_P)
        actionButton("P_trends_start_button", "Do it!")
    })
    # reactive expression
    P_month_trends_reactive <- eventReactive(input$P_trends_start_button,{
        pr <- import_data(nombre=input$file_preci$datapath)
        dn <- as.integer(input$selectPolyDegree_P)
        plot_trend_months(grado=dn,meteo=pr,nombre="Precipitation")
    })
    P_year_trends_reactive <- eventReactive(input$P_trends_start_button,{
        pr <- import_data(nombre=input$file_preci$datapath)
        dn <- as.integer(input$selectPolyDegree_P)
        sm <- as.integer(input$selectStartHydroYear_P)
        plot_trend_years(grado=dn,meteo=pr,nombre="Precipitation",start_hydro_year=sm)
    })
    # show the result
    output$plot2 <- renderPlotly({P_month_trends_reactive()})
    output$plot3 <- renderPlotly({P_year_trends_reactive()})
    
    ############################## T trends ##############################
    # Set the degree of polynomial smoothing line
    output$ui5 <- renderUI({
        req(input$file_tempe)
        selectInput(inputId="selectPolyDegree_T",label="Select the degree of the smoothing polynomial line",
                    choices=c(1:30), selected=1, multiple=FALSE)
    })
    # Set the starting month of the hydrological year
    output$ui6 <- renderUI({
        req(input$file_tempe)
        selectInput(inputId="selectStartHydroYear_T",label="Select the first month of the hydrological year",
                    choices=c(1:12), selected=10, multiple=FALSE)
    })
    # Adding the "do it" button
    output$ui7 <- renderUI({
        req(input$file_tempe)
        req(input$selectPolyDegree_T)
        req(input$selectStartHydroYear_T)
        actionButton("T_trends_start_button", "Do it!")
    })
    # reactive expression
    T_month_trends_reactive <- eventReactive(input$T_trends_start_button,{
        te <- import_data(nombre=input$file_tempe$datapath)
        dn <- as.integer(input$selectPolyDegree_T)
        plot_trend_months(grado=dn,meteo=te,nombre="Temperature")
    })
    T_year_trends_reactive <- eventReactive(input$T_trends_start_button,{
        te <- import_data(nombre=input$file_tempe$datapath)
        dn <- as.integer(input$selectPolyDegree_T)
        sm <- as.integer(input$selectStartHydroYear_T)
        plot_trend_years(grado=dn,meteo=te,nombre="Temperature",start_hydro_year=sm)
    })
    # show the result
    output$plot4 <- renderPlotly({T_month_trends_reactive()})
    output$plot5 <- renderPlotly({T_year_trends_reactive()})
    
    ############################## P stats ##############################
    # Set the starting month of the hydrological year
    output$ui8 <- renderUI({
        req(input$file_preci)
        selectInput(inputId="rankYear_StartHydroYear_P",label="Select the first month of the hydrological year",
                    choices=c(1:12), selected=10, multiple=FALSE)
    })
    # Adding the "do it" button
    output$ui9 <- renderUI({
        req(input$file_preci)
        req(input$rankYear_StartHydroYear_P)
        actionButton("P_rankYear_start_button", "Do it!")
    })
    # reactive expressions
    do_P_rank_years <- eventReactive(input$P_rankYear_start_button,{
        rank_years(meteo=import_data(nombre=input$file_preci$datapath),
                   nombre="Precipitation",
                   start_hydro_year=as.integer(input$rankYear_StartHydroYear_P))
    })
    do_P_rank_days <- eventReactive(input$P_rankYear_start_button,{
        rank_days(meteo=import_data(nombre=input$file_preci$datapath),nombre=c("Precipitation"))
    })
    do_P_stats <- function(m=c(1:12)){
        req(input$file_preci)
        df <- import_data(nombre=input$file_preci$datapath)
        month_stats(tabla=df,nombre="Precipitation",mes=m)
        }
    # show the results
    output$ps25 <- DT::renderDataTable({do_P_rank_years()},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps26 <- DT::renderDataTable({do_P_rank_days()$highest},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps27 <- DT::renderDataTable({do_P_rank_days()$lowest},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps1 <- DT::renderDataTable({do_P_stats(m=1)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps2 <- DT::renderDataTable({do_P_stats(m=2)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps3 <- DT::renderDataTable({do_P_stats(m=3)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps4 <- DT::renderDataTable({do_P_stats(m=4)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps5 <- DT::renderDataTable({do_P_stats(m=5)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps6 <- DT::renderDataTable({do_P_stats(m=6)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps7 <- DT::renderDataTable({do_P_stats(m=7)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps8 <- DT::renderDataTable({do_P_stats(m=8)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps9 <- DT::renderDataTable({do_P_stats(m=9)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps10 <- DT::renderDataTable({do_P_stats(m=10)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps11 <- DT::renderDataTable({do_P_stats(m=11)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps12 <- DT::renderDataTable({do_P_stats(m=12)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    
    ############################## T stats ##############################
    # Set the starting month of the hydrological year
    output$ui10 <- renderUI({
        req(input$file_tempe)
        selectInput(inputId="rankYear_StartHydroYear_T",label="Select the first month of the hydrological year",
                    choices=c(1:12), selected=10, multiple=FALSE)
    })
    # Adding the "do it" button
    output$ui11 <- renderUI({
        req(input$file_tempe)
        req(input$rankYear_StartHydroYear_T)
        actionButton("T_rankYear_start_button", "Do it!")
    })
    # reactive expressions
    do_T_rank_years <- eventReactive(input$T_rankYear_start_button,{
        rank_years(meteo=import_data(nombre=input$file_tempe$datapath),
                   nombre="Temperature",
                   start_hydro_year=as.integer(input$rankYear_StartHydroYear_T))
    })
    do_T_rank_days <- eventReactive(input$T_rankYear_start_button,{
        rank_days(meteo=import_data(nombre=input$file_tempe$datapath),nombre=c("Temperature"))
    })
    do_T_stats <- function(m=c(1:12)){
        req(input$file_tempe)
        df <- import_data(nombre=input$file_tempe$datapath)
        month_stats(tabla=df,nombre="Temperature",mes=m)
    }
    # show the result
    output$ps28 <- DT::renderDataTable({do_T_rank_years()},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps29 <- DT::renderDataTable({do_T_rank_days()$highest},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps30 <- DT::renderDataTable({do_T_rank_days()$lowest},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps13 <- DT::renderDataTable({do_T_stats(m=1)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps14 <- DT::renderDataTable({do_T_stats(m=2)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps15 <- DT::renderDataTable({do_T_stats(m=3)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps16 <- DT::renderDataTable({do_T_stats(m=4)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps17 <- DT::renderDataTable({do_T_stats(m=5)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps18 <- DT::renderDataTable({do_T_stats(m=6)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps19 <- DT::renderDataTable({do_T_stats(m=7)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps20 <- DT::renderDataTable({do_T_stats(m=8)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps21 <- DT::renderDataTable({do_T_stats(m=9)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps22 <- DT::renderDataTable({do_T_stats(m=10)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps23 <- DT::renderDataTable({do_T_stats(m=11)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    output$ps24 <- DT::renderDataTable({do_T_stats(m=12)},options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)
    
    ############################## instructions ##############################
    output$demo_table=DT::renderDataTable(read.csv(file="www/peca3937.csv",header=TRUE,sep="\t")[1:1000,],
                                      options=list(scrollY=250,paging=FALSE,dom='t'),rownames= FALSE)#dom option to show only the table
    
    ############################## Custom year ##############################
    # setting the start month
    output$ui12 <- renderUI({
        selectInput(inputId="select_custom_1",label="Select the first month of the period:",
                    choices=c(1:12), selected=1, multiple=FALSE)
    })
    # setting the year when the custom period ends
    select_custom_2_react <- eventReactive(input$select_custom_1,{
        if(input$select_custom_1==1){opciones<-"current"}else{opciones<-c("current","next")}
        selectInput(inputId="select_custom_2",label="Year when the custom period ends:",
                    choices=opciones, selected=opciones[1], multiple=FALSE)
    })
    output$ui13 <- renderUI({select_custom_2_react()})
    # setting the closing month
    output$ui14 <- renderUI({
        req(input$select_custom_1)
        req(input$select_custom_2)
        x <- as.integer(input$select_custom_1)
        if(input$select_custom_2=="current"){elegir<-x:12}else{elegir<-1:(x-1)}
        selectInput(inputId="select_custom_3",label="Month closing the period:",choices=elegir, selected=elegir[1], multiple=FALSE)
    })
    # Set the degree of polynomial smoothing line
    output$ui15 <- renderUI({
        selectInput(inputId="selectPolyDegree_custom",label="Select the degree of the smoothing polynomial line",
                    choices=c(1:30), selected=1, multiple=FALSE)
    })
    # Adding the "do it" button
    output$ui16 <- renderUI({
        req(input$file_tempe)
        req(input$file_preci)
        actionButton("do_custom_analysis", "Do it!")
    })
    # reactive expressions
    do_custom_plot_P <- eventReactive(input$do_custom_analysis,{
        custom_plot(meteo=import_data(nombre=input$file_preci$datapath),
                    from=as.integer(input$select_custom_1),
                    to=as.integer(input$select_custom_3),
                    nombre="Precipitation",
                    grado=as.integer(input$selectPolyDegree_custom))
    })
    do_custom_plot_T <- eventReactive(input$do_custom_analysis,{
        custom_plot(meteo=import_data(nombre=input$file_tempe$datapath),
                    from=as.integer(input$select_custom_1),
                    to=as.integer(input$select_custom_3),
                    nombre="Temperature",
                    grado=as.integer(input$selectPolyDegree_custom))
    })
    # # show results
    output$plot6 <- renderPlotly({do_custom_plot_P()})
    output$plot7 <- renderPlotly({do_custom_plot_T()})

})
