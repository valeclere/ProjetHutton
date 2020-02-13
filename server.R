####################################################################
######################### Library ##################################
####################################################################

library(shiny)
library(sp)
library(rgdal)
library(raster)
library(lattice)
library(plotGoogleMaps)
library(RColorBrewer)
library(leaflet)
library(shinyjs)
library(devtools)
library(leafletGoogle)
library(spacetime)
library(pander)
library(markdown)
library(shinyBS)

###########
#******************************* # SOURCES # ****************************************************#
###########


# Uncomment the next line when not deploying on shinyapps.io
#source("verif_db.R")
source("calc.R")
source("chooser.R")
source("methods.R")
source("creationDataRaster.R")
source("utilitarian.R")
source("resoudre.R")

#Loading of the compiled .c code
#To comment when deploying on shiny apps
if (Sys.info()["sysname"]=="Windows"){dyn.load("methods.dll")}
if (Sys.info()["sysname"]=="Linux"){dyn.load("methods.so")}
#To uncomment when deploying on shiny apps
# dyn.load("methods.dll")

# Change Project directory to a sub directory www. Everything will happen there now.
setwd("./www/")

#Loading of the data
hc <<-raster(readGDAL("../data/CL_cscapeQ_r01.tif"))
areaselected <<- hc
Pr <<- read.table(paste0("../data/Project.txt"),h=T,sep="|")
HC <<- read.table(paste0("../data/Hard_Constraint.txt"),h=T,sep="|")
rst.hcl <<- creationHardConstraintRaster(HC)
use_HC <<- TRUE
Met <<-read.table(paste0("../data/Methods.txt"),h=T)
RasterForThrCM <<- raster()
rasterAverageElectre <<- raster()
RasterSavedSA <<-raster()
##################  
#****************************** # END OF SOURCES # **********************************************#
##################

# Define server logic required
shinyServer(function(input, output,session) {
  env<<-environment()
  
  #########################################
  ############## Headers ##################
  #########################################
  
  ## The two next lines are here to hide the 
  ## buffering page and show the real page
  hide("loading_page")
  shinyjs::show("main_content")
  
  # Remove previous maps and grid to avoid oversize issue
  unlink("./*.html")
  unlink("./*.png")
  unlink("./*.png")
  unlink("./weights*.txt")
  # Reset of the rasters saved and the rasters saved numbering
  rastersaved <<- stack()
  num <<- 0
  nbgoodpx<<-0
  px <<-0
  breakscol <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  # Define welcome message with the session username 
  output$welcome <- renderUI({
    tags$h3(paste("Welcome"),session$user,"!")
  })
  
  #test if the data is normalized
  output$testdata <- renderUI({
    validate(need(hc@data@min >= -1 && hc@data@max <= 1, "Data not normalized, please change it."))
  })
  
  # session var for maximes shapes ###
  sessionVars <- reactiveValues(area_cur=NULL,curtab=NULL)
  files2zip <- "BriefText.txt"
  
  
  ##########################################################################################################
  ###################### Hide/Show the panel with two shinyJs functions ###################################
  ##########################################################################################################
  
  # That's what makes the application more sequential
  # Panel are shown only when you have already looked at the preceding panels
  
  ########
  # Tab2 #
  ########
  
  observe({
    hide(selector = "#navbar li a[data-value=tab2]")})
  
  observeEvent(input$pann2, {
    shinyjs::show(selector = '#navbar li a[data-value=tab2]')
    hide(id = "advanced")
    updateTabsetPanel(session, 
                      inputId = "navbar", 
                      selected = "tab2")
  })
  
  ########
  # Tab3 #
  ########
  
  observe({
    hide(selector = "#navbar li a[data-value=tab3]")})
  
  observeEvent(input$pann3, {
    shinyjs::show(selector = "#navbar li a[data-value=tab3]")
    updateTabsetPanel(session, 
                      inputId = "navbar", 
                      selected = "tab3")
  })
  
  hide(id="pann4")
  
  ########
  # Tab4 #
  ########
  
  observe({
    hide(selector = "#navbar li a[data-value=tab4]")})
  
  observeEvent(input$pann4, {
    shinyjs::show(selector = "#navbar li a[data-value=tab4]")
    updateTabsetPanel(session, 
                      inputId = "navbar", 
                      selected = "tab4")
  })
  
  ########
  # Tab5 #
  ########
  
  observe({
    hide(selector = "#navbar li a[data-value=tab5]")})
  
  ###################
  # Tab6a and Tab6b #
  ###################
  
  observe({
    hide(selector = "#navbar li a[data-value=tab6a]")
    })
  
  observe({
    hide(selector = "#navbar li a[data-value=tab6b]")
    })
  
  observeEvent(input$pann6, {
    if(input$advanced == TRUE){
      # It's sadvanced even if it's b
      shinyjs::show(selector = "#navbar li a[data-value=tab6b]")
      updateTabsetPanel(session, 
                        inputId = "navbar", 
                        selected = "tab6b")
    }
    else {
      shinyjs::show(selector = "#navbar li a[data-value=tab6a]")
      updateTabsetPanel(session, 
                        inputId = "navbar", 
                        selected = "tab6a")
    }
  })
  
  hide(id="metsela")
  
  # Two Buttons to update the pannel automatically (adv and basic mode).
  observeEvent(input$thresholdPannel,{
    updateTabsetPanel(session, 
                      inputId = "navbar", 
                      selected = "tab7")
    showNotification(id= "warningThr", duration = 60, closeButton = TRUE, type = "warning", session = session, ui = "Be careful ! As the threshold value is present too many times, note that it could be not the right percentage of area in blue. Indeed if every threshold value is considered acceptable, then there will be more value than resquested. That's why these values are rejected and there is fewer values.")
    
  })
  
  observeEvent(input$thresholdPannela,{
    updateTabsetPanel(session, 
                      inputId = "navbar", 
                      selected = "tab7")
  })
  
  # Go to the advanced Pannel 
  observeEvent(input$goAdvPanel,{
    updateTabsetPanel(session, 
                      inputId = "navbar", 
                      selected = "tabAU")
    removeNotification(id = "warningThr" , session = session)
  })
  
  
  # Hide the buttons before you have clicked on the go button
  # Basic 
  hide(id="downloadRasterbuttona")
  hide(id='thresholdPannela')
  observeEvent(input$goButtonBasic, {
    # validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), ""))
    shinyjs::show(id="downloadRasterbuttona")
    shinyjs::show(id="thresholdPannela")
  })
  
  # If you're not an advanced user, you can't see the button going to advanced panel
  observeEvent(input$thresholdPannela,{
  shinyjs::hide(id="goAdvPanel")
  })

  
  shinyjs::disable(id = "goAdvPanel")
  observeEvent(input$goButtonThr,{
    shinyjs::enable(id = "goAdvPanel")
  })
  
  
  #Advanced User
  hide(id="savePannel")
  
  observeEvent(input$goButtonAdvanced, {
    # validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), ""))
    shinyjs::show(id="savePannel")
  })
  
  ########
  # Tab7 #
  ########
  
  observe({
    hide(selector = "#navbar li a[data-value=tab7]")})
  
  observeEvent(input$pann6, {
    shinyjs::show(selector = "#navbar li a[data-value=tab7]")})
  
  
  
  #################
  # Advanced user #
  #################
  
  # value = "tabAU",
  observe({
    hide(selector = "#navbar li a[data-value=tabAU]")})
  
  observeEvent(input$goButtonAdvanced, {
    if(input$advanced == TRUE){
      shinyjs::show(selector = "#navbar li a[data-value=tabAU]")
    }
  })
  observeEvent(input$goButtonBasic, {
    if(input$advanced == TRUE){
      shinyjs::show(selector = "#navbar li a[data-value=tabAU]")
    }
  })
  ################
  # Average tab  #
  ################
  
  hide(id = "tabAV_LWC2_desc")
  hide(id = "tabAV_Electre_desc")
  
  observe({
    input$saveButton
    input$Project
    
    #validate( need(temp_WeightssavedSize("LWC2") > 0, ""))
    shinyjs::show(id="tabAV_LWC2_desc")
  })
  
  observe({
    input$saveButton
    input$Project
    
   # validate( need(temp_WeightssavedSize("Electre") > 0, ""))
    shinyjs::show(id="tabAV_Electre_desc")
  })
  
  ########################
  # Sensitivity Analysis #
  ########################
  
  hide(id = "downloadSA")
  observe({
    input$goButtonThrSA
    
    shinyjs::show(id="downloadSA")
  })
  
  ############################################################################################################  
  ########################################## Help Menu #######################################################
  ############################################################################################################  
  
  # We need to put as much observe as we got pannel so here it's 8/9
  # Help message for the second tab
  observeEvent(input$helpProject, {
    showModal(modalDialog(title = "Help message for the user",
                          # readLines read *.txt (there is a warning message that's why warn=F)
                          includeHTML("../Help/selectProject.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for the third tab (Select Subset Area)
  observeEvent(input$helpArea, {
    showModal(modalDialog(title = "Help message for the user",
                          includeHTML("../Help/selectSpecificArea.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for the fourth tab (Hard Constraint)
  observeEvent(input$helpHC, {
    showModal(modalDialog(title = "Help message for the user",
                          includeHTML("../Help/listHardConstraint.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for the fifth tab (Criteria)
  observeEvent(input$helpCrit, {
    showModal(modalDialog(title = "Help message for the user",
                          includeHTML("../Help/selectCriteria.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for the sixth tab (Weight advanced user)
  observeEvent(input$helpWeight, {
    showModal(modalDialog(title = "Help message for the user",
                          includeHTML("../Help/selectWeight_advanced.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for the sixth bis tab (Weight basic user)
  observeEvent(input$helpWeighta, {
    showModal(modalDialog(title = "Help message for the user",
                          includeHTML("../Help/selectWeight.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for the seventh tab (Threshold)
  observeEvent(input$helpThr, {
    showModal(modalDialog(title = "Help message for the user",
                          includeHTML("../Help/threshold.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for the seventh tab (Compare Methods)
  observeEvent(input$helpCmpMth, {
    showModal(modalDialog(title = "Help message for the advanced user",
                          includeHTML("../Help/compare.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for the seventh tab (Stats)
  observeEvent(input$helpStats, {
    showModal(modalDialog(title = "Help message for the advanced user",
                          includeHTML("../Help/stats.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  
  # Help message for the seventh tab (Average)
  observeEvent(input$helpAverage, {
    showModal(modalDialog(title = "Help message for the advanced user",
                          includeHTML("../Help/average.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for Consensus 
  observeEvent(input$helpConsensus, {
    showModal(modalDialog(title = "Help message for the advanced user",
                          includeHTML("../Help/consensus.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for Compare Project
  observeEvent(input$helpCmpProj, {
    showModal(modalDialog(title = "Help message for the advanced user",
                          includeHTML("../Help/compareProject.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  # Help message for Sensitivity Analysis
  observeEvent(input$helpSensitivAnalysis, {
    showModal(modalDialog(title = "Help message for the advanced user",
                          includeHTML("../Help/sensitivityAnalysis.html"),
                          easyClose = TRUE, footer = NULL))
  })
  
  ################################################################################################################  
  ####################################### Select a project #######################################################
  ################################################################################################################  
  
  output$renderProject <- renderUI({
    # Read the names of the projects in the project database (Project.txt) and create a list
    Pnames <- sapply(Pr[,2], toString)
    Pnames <- c(Pnames, "My own Data")
    mylist <- vector(mode="list", length=length(Pnames))
    mylist= lapply(1:length(Pnames), function(i){
      mylist[[i]] <- i
    })
    names(mylist) <- Pnames
    
    # Create the radiobutton with each name of project
    radioButtons("Project",
                 label = "You have to choose a project, by default the project selected is the first one", 
                 choices = mylist, 
                 selected = 1)

  })
  
  
  #Hide/show the right information according to the kind of project which is chosen
  observeEvent(input$Project,{
    
    if (input$Project == 1){
      shinyjs::show(id="InformationForest")
      
      hide(id= "InformationAgriculture")
      
      hide(id="file")
      hide(id="uploaded")
      hide(id="instructionselectdata")
      hide(id="InformationMyOwnData")
      hide(id="BewareOwnData")
    }
    
    if (input$Project == 2){
      hide(id= "InformationForest")
      
      shinyjs::show(id="InformationAgriculture")
      
      hide(id="file")
      hide(id="uploaded")
      hide(id="instructionselectdata")
      hide(id="InformationMyOwnData")
      hide(id="BewareOwnData")
    }
    
    
    if (input$Project == 3){
      hide(id="InformationForest")
      
      hide(id= "InformationAgriculture")
      
      shinyjs::show(id="file")
      shinyjs::show(id="uploaded")
      shinyjs::show(id="instructionselectdata")
      shinyjs::show(id="InformationMyOwnData")
      shinyjs::show(id="BewareOwnData")
    }
  })
  
  
  # Depending on the project choosen, loading of the criteria rasters
  observe({
    if (length(input$Project) ==0){
      # By default, if the user doesn't click on the project tab, Project = Forest
      launchingProject(1, Pr)
      x1 <<- c(x1p,x1n)
      x1bis <<- factor(x1)
      initialisation <<- TRUE
    }
    else if (initialisation==TRUE){ 
      # To avoid the reloading of the data when the user clicks on the project tab for the first time
      initialisation <<- FALSE
    }
    else {
      # Uploading data and using the ones existing is different 
      if(input$Project!="3"){
        # Then
        launchingProject(input$Project, Pr)
        x1 <<- c(x1p,x1n)
        x1bis <<- factor(x1)
        #update hc if necessary
        if (input$Project=="2"){
          hc <<-raster(readGDAL("../data/CL_cscapeQ_r01.tif"))
        }
        #Reset the Compare Methods checkbox
        updateCheckboxGroupInput(session=session, 
                                 inputId="checkGroupCM",
                                 choices = list("LWC2" = 2, "Electre" = 3), 
                                 selected=NULL)
      }
    }
  })
  
  xdist <- 0
  ydist <- 0
  xcenter <- 0
  ycenter <- 0
  reactX <- -1
  mamapAvailable <- NA
  
  #Event button Download Example
  output$downloadData <- downloadHandler(
    filename = 'Example.zip',
    content = function(f.out.name) {
      cat(f.out.name,"\n")
      zip(zipfile = f.out.name ,files = 'Example')
    },
    contentType = "application/zip"
  )
  
  
  # Event button ok 
  observeEvent(input$uploaded,{
    if (input$Project=="3"){
      if (length(input$file) !=0){
        # Name of the folder
        nameuploaded <- unlist(strsplit(input$file$name,"\\."))[1]
        # Name diffrent from the others
        if (nameuploaded!= "Forest" && nameuploaded!="Agriculture" ){
          # Erase a file if they have the same name
          if (file.exists(file.path(getwd(),nameuploaded))){
            #file.remove(file.path(getwd(),nameuploaded))
            unlink(file.path(getwd(),nameuploaded), recursive = TRUE)
            file.remove(file.path(getwd(),"0"))
            unlink(file.path(getwd(),"0"), recursive = TRUE)
          }
          # Copy and unzip the data
          p <- input$file$datapath
          file.copy(p, getwd(), overwrite = TRUE)
          unzip(file.path(getwd(),"0"), exdir = ".")
          file.copy(file.path(getwd(),nameuploaded),"../data", overwrite=TRUE, recursive = TRUE)
          # Read the index file and put in the project table
          if (file.exists(file.path(file.path("../data", nameuploaded), "index.txt"))){
            LU <<- scan(file.path(file.path("../data", nameuploaded), "index.txt"), what="factor")
            u <- c(length(Pr[,1])+1, LU)
            
            # update levels
            levels(Pr[,2]) <- c(levels(Pr[,2]), u[2]) 
            levels(Pr[,3]) <- c(levels(Pr[,3]), u[3]) 
            levels(Pr[,4]) <- c(levels(Pr[,4]), u[4]) 
            levels(Pr[,5]) <- c(levels(Pr[,5]), u[5]) 
            levels(Pr[,6]) <- c(levels(Pr[,6]), u[6]) 
            
            Pr <- rbind(Pr, u)
            launchingProject(input$Project, Pr)
            Pr <<- Pr
            #update hc
            if (file.exists(file.path(file.path("../data", nameuploaded), "extent.tif"))){
              hc <<- raster(readGDAL(file.path(file.path("../data", nameuploaded), "extent.tif")))
            }else{
              output$erreur<- renderText({ 
                paste("extent.tif is missing")})
            }
            #Upload the Hard Constraints
            if (file.exists(file.path(file.path("../data", nameuploaded), "Hard_Constraint.txt"))){
              HC <<-read.table(paste0(file.path(file.path("../data", nameuploaded), "Hard_Constraint.txt")), h=T)
              if(length(HC)==0){
                use_HC<<-FALSE
              }else{
                rst.hcl <<- creationHardConstraintRasterUser(HC, nameuploaded)}
            }else{
              use_HC<<-FALSE
            }
            #Reset the Compare Methods checkbox
            updateCheckboxGroupInput(session=session, 
                                     inputId="checkGroupCM",
                                     choices = list("LWC2" = 2, "Electre" = 3), 
                                     selected=NULL)
            
            output$uploadok<- renderText({ 
              paste("Data uploaded")})
          }else{
            output$erreur<- renderText({ 
              paste("The file is not correct")})
          }
        }else{
          output$erreur<- renderText({ 
            paste("The name of the file is already used.")})
        }
      }else{
        output$erreur<- renderText({ 
          paste("Uploade your data")})
      }
    }else{
      output$uploadok<- renderText({ 
        paste("Please select My Data")})
    }
  })
  # Text below the title, indicating the project chosen
  output$typechosen<- renderText({ input$Project
    if (initialisation == TRUE){
      name <- Pr[1,2]
    } 
    else{ 
      if (input$Project=="3"){
        name <- "My Data"
      }else{
        name <- Pr[input$Project,2]}
      paste0("Project selected : ", name)} 
    
  })
  
  #################################################################################################################  
  ################################### Select subset of the project area ###########################################
  #################################################################################################################
  
  
  # Hide and show "polygon, rectangle and shapefile selection"
  observeEvent(input$allareaselect,{
    if (input$allareaselect == 0){
      
      hide(id= "divSubset")
    }
    
    if (input$allareaselect == 1){
      
      shinyjs::show(id= "divSubset")
      
    }
  })
  
  
  # Enable selection of a smaller area
  # Display the map with the google layer on which you can select the area you want to use 
  output$selectarealeaf <- renderLeaflet({
    
    # Transform the extent of the raster hc
    hcext <- extent(hc)
    hcp <- as(hcext,'SpatialPolygons')
    crs(hcp) <- crs(hc)
    hc4 <- spTransform(hcp,CRSobj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    leaflet()%>%
      addGoogleTiles(layerId = "googleTileSat",
                     options = tileOptions(maxZoom = 15,
                                           updateWhenZooming = TRUE),
                     type = "satellite",
                     group = "Satellite (default)") %>%
      addGoogleTiles(layerId = "googleTileRoad",
                     options = tileOptions(maxZoom = 15,
                                           updateWhenZooming = TRUE),
                     type = "roadmap",
                     group = "Roadmap") %>%
      addGoogleTiles(layerId = "googleTileTer",
                     options = tileOptions(maxZoom = 15,
                                           updateWhenZooming = TRUE),
                     type = "terrain",
                     group = "Terrain") %>%
      
      #################
    setView(lng = -4.10889,
            lat = 56.9176,
            zoom = 5) %>%
      addLayersControl(
        baseGroups = c("Satellite (default)", "Roadmap", "Terrain"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addRectangles(lng1 = hc4@bbox[1,1],
                    lat1 = hc4@bbox[2,1],
                    lng2 = hc4@bbox[1,2],
                    lat2 = hc4@bbox[2,2],color = "blue",fill = FALSE)
    #################
  })
  
  observe({
    zoomLevel <<- input$selectarealeaf_zoom
    zoomCenter <<- input$selectarealeaf_center
  })
  
  cptclickselectarealeaf <<- 0 
  ## Replace the ancient selection by the new one using the tool of maxime
  observeEvent(input$selectarealeaf_click, {
    if(is.null(input$selectarealeaf_click))
      return()
    
    # What to do if it's the free selection
    if(input$inputselect =="0"){
      sessionVars$area_cur$LAT <- c(sessionVars$area_cur$LAT, input$selectarealeaf_click$lat)
      sessionVars$area_cur$LNG <- c(sessionVars$area_cur$LNG, input$selectarealeaf_click$lng)
      leafletProxy("selectarealeaf") %>%
        removeShape(layerId = "areaselected") %>%
        addPolygons(lng = sessionVars$area_cur$LNG,
                    lat = sessionVars$area_cur$LAT,
                    layerId = "areaselected",
                    color = "#F03", fillColor = "#03F")
      bound <<- polygonBoundaries(sessionVars$area_cur)
    }
    
    # What to do if it's the rectangle selection
    if(input$inputselect =="2"){
      # cptclickselectarealeaf <<- 0 
      if(cptclickselectarealeaf ==2)
        return()
      else if(cptclickselectarealeaf ==0){
        sessionVars$area_cur$LAT <- c(sessionVars$area_cur$LAT, input$selectarealeaf_click$lat)
        sessionVars$area_cur$LNG <- c(sessionVars$area_cur$LNG, input$selectarealeaf_click$lng)
        cptclickselectarealeaf <<- cptclickselectarealeaf+1
        leafletProxy("selectarealeaf") %>%
          removeShape(layerId = "areaselected") %>%
          addPolygons(lng = sessionVars$area_cur$LNG,
                      lat = sessionVars$area_cur$LAT,
                      layerId = "areaselected",
                      color = "#F03", fillColor = "#03F")
      }
      else{
        sessionVars$area_cur$LAT <- c(sessionVars$area_cur$LAT, input$selectarealeaf_click$lat)
        sessionVars$area_cur$LNG <- c(sessionVars$area_cur$LNG, input$selectarealeaf_click$lng)
        cptclickselectarealeaf <<- cptclickselectarealeaf+1
        leafletProxy("selectarealeaf") %>%
          removeShape(layerId = "areaselected") %>%
          addRectangles(lng1 = sessionVars$area_cur$LNG[1],
                        lat1 = sessionVars$area_cur$LAT[1],
                        lng2 = sessionVars$area_cur$LNG[2],
                        lat2 = sessionVars$area_cur$LAT[2],
                        layerId = "areaselected",
                        color = "#F03", fillColor = "#03F")
      }
    }
  })
  
  # Function to Undo the last action you haved performed on the map ( click to add a point )
  observeEvent(input$undoarea,{
    # We must change the undo for the different selection
    # So this selection is the free selection
    if(input$inputselect =="0"){
      if(length(sessionVars$area_cur$LAT) > 0){
        sessionVars$area_cur$LAT <- sessionVars$area_cur$LAT[1:length(sessionVars$area_cur$LAT)-1]
        sessionVars$area_cur$LNG <- sessionVars$area_cur$LNG[1:length(sessionVars$area_cur$LNG)-1]
        leafletProxy("selectarealeaf") %>%
          removeShape(layerId = "areaselected") %>%
          addPolygons(lng = sessionVars$area_cur$LNG,
                      lat = sessionVars$area_cur$LAT,
                      layerId = "areaselected",
                      color = "#F03", fillColor = "#03F")
      }
    }
    # And this selection is the rectangle selection
    # We need to be careful about the click cpt
    if(input$inputselect =="2"){
      if(cptclickselectarealeaf ==0)
        return()
      else {
        if(length(sessionVars$area_cur$LAT) > 0){
          sessionVars$area_cur$LAT <- sessionVars$area_cur$LAT[1:length(sessionVars$area_cur$LAT)-1]
          sessionVars$area_cur$LNG <- sessionVars$area_cur$LNG[1:length(sessionVars$area_cur$LNG)-1]
          cptclickselectarealeaf <<- cptclickselectarealeaf-1
          leafletProxy("selectarealeaf") %>%
            removeShape(layerId = "areaselected") %>%
            addPolygons(lng = sessionVars$area_cur$LNG,
                        lat = sessionVars$area_cur$LAT,
                        layerId = "areaselected",
                        color = "#F03", fillColor = "#03F")
        }
      }
    }
  })
  
  # Function to Remove the entire polygon you have created to select the area 
  observeEvent(input$cleararealeaf,{
    # We must change the clear for the different selection
    # Just because of the cpt click
    # So this selection is the free selection
    if(input$inputselect =="0"){
      sessionVars$area_cur <- NULL
      leafletProxy("selectarealeaf") %>%
        removeShape(layerId = "areaselected")
    }
    if(input$inputselect =="2"){
      sessionVars$area_cur <- NULL
      cptclickselectarealeaf <<- 0
      leafletProxy("selectarealeaf") %>%
        removeShape(layerId = "areaselected")
    }
  })
  
  # Use with a shapefile
  observeEvent(input$sarea, {
    if(input$inputselect == "1"){
      output$renderShapefile<- renderPlot({
        # Uploade the .zip, unzip it and read it
        file.copy(input$sarea$datapath, getwd(), overwrite = TRUE)
        unzip(file.path(getwd(),"0"), exdir = "./Shapefile")
        dsa <- unlist(strsplit(input$sarea$name,"\\."))[1]
        nsa <- unlist(strsplit(dir(path=file.path("./Shapefile", dsa))[1],"\\."))[1]
        ogrInfo(path.expand(file.path("./Shapefile", dsa)), nsa)
        sa <<- readOGR(dsn=path.expand(file.path("./Shapefile", dsa)), 
                       layer=nsa)
        plot(sa)
      })
    }
    else{
      output$error <- renderPrint({
        error<- "Press Selection with a Shapefile first"
        cat(error)
      })
    }
  })
  
  # Select button to go to the next step
  
  observeEvent(input$selecta, {
    error <-""
    
    # Select the entire area
    if(input$allareaselect ==0){
      # Transform the extent of the raster hc
      hcext <- extent(hc)
      hcp <- as(hcext,'SpatialPolygons')
      crs(hcp) <- crs(hc)
      hc4 <- spTransform(hcp,CRSobj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      # Use the same pattern as the other one :)
      areaOSGB36 <<-extent(hc)
      areaselected <<- hc
      rst.pnl <<-  mask(rst.pnl.original, areaselected)
      rst.nl <<-  mask(rst.nl.original, areaselected)
      rst.pl <<- mask(rst.pl.original, areaselected)
      
      # Extent of the all area
      lng1 <-  hc4@bbox[1,1]
      lat1 <-  hc4@bbox[2,1]
      lng2 <-  hc4@bbox[1,2]
      lat2 <-  hc4@bbox[2,2]
      xcenter <<- (lng1+lng2)/2
      ycenter <<- (lat1+lat2)/2
      xdist <<- lng2-lng1
      ydist <<- lat2-lat1
   
      zoomin <- leafletGoogle::setZoomBound(xdist = xdist, ydist = ydist)
      
      # Show with the 'Next Step' button
      shinyjs::show(id="pann4")
      leafletProxy("selectarealeaf") %>%
        setView(lng = zoomCenter[[1]],
                lat = zoomCenter[[2]],
                zoom = zoomLevel)}
    else {
      # If the area is directly selected from the map by choosing which polygon to use
      if (input$inputselect=="0"){
        if(!is.null(sessionVars$area_cur)){
          
          # Create the SpatialPolygonDataFrame 
          polly <- Polygon(coords = list(sessionVars$area_cur$LNG,sessionVars$area_cur$LAT))
          polly2 <- Polygons(list(polly),ID=1)
          polly3 <- SpatialPolygons(list(polly2), proj4string = CRS(projargs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))
          polly4 <- spTransform(polly3, crs(hc))
          vect <- polly4@bbox
          # This is  the condition for the polygon to fit into the raster
          if(vect[1,1] > hc@extent@xmin & vect[2,2] < hc@extent@ymax
             & vect[2,1] > hc@extent@ymin & vect[1,2] < hc@extent@xmax){
            areaOSGB36 <<-extent(polly4)
            areaselected <<- polly4
            if ((rst.pnl@extent == areaOSGB36)==FALSE){
              rst.pnl <<-  mask(rst.pnl.original, areaselected,updatevalue=NA)
              rst.nl <<-  mask(rst.nl.original, areaselected,updatevalue=NA)
              rst.pl <<- mask(rst.pl.original, areaselected,updatevalue=NA)}
            else{
              error <- "(rst.pnl@extent == areaOSGB36)==FALSE"}
            # Show with the 'Next Step' button
            shinyjs::show(id="pann4")
            # These are the variables that needs to be changed
            vect <- polygonBoundaries(sessionVars$area_cur)
            xcenter <<- vect[5]
            ycenter <<- vect[6]
            xdist <<- vect[3]-vect[1]
            ydist <<- vect[4]-vect[2]
            zoomin <- leafletGoogle::setZoomBound(xdist = xdist, ydist = ydist)
            leafletProxy("selectarealeaf") %>%
              setView(lng = zoomCenter[[1]],
                      lat = zoomCenter[[2]],
                      zoom = zoomLevel)}
          else{
            error <-"The polygon needs to fit into the raster area"
            print("The polygon needs to fit into the raster area")
          }
        }
        else{
          error <-"Please select an area with at least a point"}
      }
      
      # Use with a shapefile ## inputselect=="1" ##
      else if(input$inputselect=="1"){
        areaOSGB36 <-extent(sa)
        ## Condition and error for the cut
        # The area selected needs to match the rasters, a text warns the user
        if (areaOSGB36@xmax > hc@extent@xmin & areaOSGB36@ymin < hc@extent@ymax
            & areaOSGB36@ymax > hc@extent@ymin & areaOSGB36@xmin < hc@extent@xmax){
          areaselected <<- sa
          rastersaved <<- stack()
          num <<- 0
          error <-"If you re-select an area after doing some analysis without downloading rasters, all the results will be lost."
          if ((rst.pnl@extent == areaOSGB36)==FALSE){
            rst.pnl <<-  cuttingAreaSelected(rst.pnl.original, areaselected)
            rst.nl <<-  cuttingAreaSelected(rst.nl.original, areaselected)
            rst.pl <<- cuttingAreaSelected(rst.pl.original, areaselected)
            # Show with the 'Next Step' button
            shinyjs::show(id="pann4")
          }
        }
        #If the area selected doesn't match the rasters, a text warns the user
        else {
          error <- "No data for this area, please select an other one"
        }
      }
      
      # Use a rectangle "Selection with a rectangle"
      else if (input$inputselect=="2"){
        #Selection of a smaller area by drawing a rectangle on the map
        if(!is.null(sessionVars$area_cur)){
          if(cptclickselectarealeaf ==2){
            # We need the two other points that hasn't been clicked on
            x1 <- sessionVars$area_cur$LNG[1]
            x2 <- sessionVars$area_cur$LNG[2]
            y1 <- sessionVars$area_cur$LAT[1]
            y2 <- sessionVars$area_cur$LAT[2]
            mat <- matrix(ncol = 2, nrow = 4, data = c(x1,x2,x2,x1,y1,y1,y2,y2))
            
            # Create the SpatialPolygonDataFrame 
            polly <- Polygon(coords = mat)
            polly2 <- Polygons(list(polly),ID=1)
            polly3 <- SpatialPolygons(list(polly2), proj4string = CRS(projargs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "))
            polly4 <- spTransform(polly3, crs(hc))
            vect <- polly4@bbox
            
            ## Condition and error for the cut
            if (vect[1,1] > hc@extent@xmin & vect[2,2] < hc@extent@ymax
                & vect[2,1] > hc@extent@ymin & vect[1,2] < hc@extent@xmax){
              
              areaOSGB36 <<-extent(polly4)
              areaselected <<- polly4
              
              if ((rst.pnl@extent == areaOSGB36)==FALSE){
                rst.pnl <<-  mask(rst.pnl.original, areaselected,updatevalue=NA)
                rst.nl <<-  mask(rst.nl.original, areaselected,updatevalue=NA)
                rst.pl <<- mask(rst.pl.original, areaselected,updatevalue=NA)}
              else{
                error <- "(rst.pnl@extent == areaOSGB36)==FALSE"}
              
              # These are the variables that needs to be changed
              vect <- polygonBoundaries(sessionVars$area_cur)
              xcenter <<- vect[5]
              ycenter <<- vect[6]
              xdist <<- vect[3]-vect[1]
              ydist <<- vect[4]-vect[2]
              # Show with the 'Next Step' button
              shinyjs::show(id="pann4")
              
              zoomin <- leafletGoogle::setZoomBound(xdist = xdist, ydist = ydist)
              leafletProxy("selectarealeaf") %>%
                setView(lng = zoomCenter[[1]],
                        lat = zoomCenter[[2]],
                        zoom = zoomLevel)}
            else {
              #If the area selected doesn't match the rasters, a text warns the user
              error <- "No data for this area, please select an other one"
            }
          }
          else{
            error <- "You need to select a rectangle by clicking on two points"
          }
        }
      } 
      output$error <- renderPrint({
        cat(error)
      })
    }
  })
  
  
  # Observe event to toggle the file input to show it only if you select the "Shapefile"
  # Option in the selection of the area
  observeEvent(input$inputselect,{
    if(input$inputselect != 1){
      hide(id="sarea")
      hide(id="sareazip")
      hide(id="sareaupload")
    }
    else{
      shinyjs::show(id="sarea")
      shinyjs::show(id="sareazip")
      shinyjs::show(id="sareaupload")
    }
  })
  
  
  ##################################################################################################################    
  ################################## Select Hard Constraints #######################################################
  ##################################################################################################################
  
  # Display the matrix which contains the hard constraints description
  output$matrixHC <- renderTable({
    validate( need(use_HC==TRUE , "No Hard Constraint Data"))
    
    #Transform the row data in understandable text
    descript <-sapply(HC[,4], toString)

    #Get back names of hard constraints
    tname <-sapply(HC[,3], toString)
    
    matrix <- matrix(descript, nrow=length(HC[,1]))
    dimnames(matrix)=list(tname,c("Description"))
    matrix
  })
  
  #Display the select input of hard constraint that the user wants to display on the map
  output$renderSelectHC <-renderUI({
    validate( need(use_HC==TRUE , ""))
    descript <-sapply(HC[,4], toString)
    choices <- list()
    for(i in 1:length(HC)){
      choices[i] <- toString(HC[i,3])
    }
    names(choices) <- descript
    selectInput(inputId = "hardconstraint", 
                label = "Choose a hard constraint to display:",
                choices = choices,
                selected = 1
    )
  })
  
  # Display the HC map
  output$mapShowHC <- renderLeaflet({
    input$hardconstraint
    
    if(is.null(input$hardconstraint)){
      delay(100,{})
    }
    if (!is.null(input$hardconstraint)){
      #The layer displayed is the one selected by the user in input hardconstraint
      layerdisplayed <- subset(rst.hcl,input$hardconstraint)
      ## This is the raster which is displayed on the map!!!!!
      layerd <- mask(layerdisplayed, areaselected)
      palos<-colorBin(palette = c("#808080", "#33cc33"),bins = 2,domain=c(-1,0), na.color = NA)
      colorszz <- c("#808080", "#33cc33")
      
        leaflet() %>%
          addGoogleTiles(layerId = "googleTileSat",
                         options = tileOptions(maxZoom = 15,
                                               updateWhenZooming = TRUE),
                         type = "satellite") %>%
          setView(lng = zoomCenter[[1]],
                  lat = zoomCenter[[2]],
                  zoom = zoomLevel) %>%
          addRasterImage(x = layerd, colors = palos, opacity = isolate(input$mapOpacityhc)/100, method = "ngb") %>%
        addLegend(position = "bottomright",colors = colorszz,labels =c("unacceptable", "acceptable"), opacity = isolate(input$mapOpacityhc)/100)
        }
      
    
  })
  
  # Memorize Zoom level and center 
  observe({
    zoomLevel <<- input$mapShowHC_zoom
    zoomCenter <<- input$mapShowHC_center
  })
  
  #Update the map
  observe({
    input$mapShowHC_zoom
    input$hardconstraint
    input$mapOpacityhc
    input$mapHCSelect
    
    # The console will trigger an error if the leaflet object called
    # Is not defined and this is sometimes the case as the map take less time to initalize then
    # the input hardconstraint, that's why we need to delay the execution
    # of the code for the first time you enter into the tab
    if(is.null(input$hardconstraint)){
      delay(200,{})#
    }
    
    isolate({
      zoomin <- leafletGoogle::setZoomBound(xdist = xdist, ydist = ydist)
    
      if(length(input$hardconstraint) != 0 ){
        if(input$mapShowHC_zoom == zoomin[1] || input$mapShowHC_zoom == zoomin[2] || input$mapShowHC_zoom == zoomin[3] || input$mapShowHC_zoom == zoomin[4] || input$mapShowHC_zoom == zoomin[5] || length(input$hardconstraint) != 0 || input$mapOpacityhc){
          #The layer displayed is the one selected by the user in input hardconstraint
         layerdisplayed <- subset(rst.hcl,input$hardconstraint)
         ## This is the raster which is displayed on the map!!!!!
         layerd <- mask(layerdisplayed, areaselected)
        
         palos<-colorBin(palette = c("#808080", "#33cc33"),bins = 2,domain=c(-1,0), na.color = NA)
       
          # Redraw the map now
          if(input$mapHCSelect == 0){
           leafletProxy("mapShowHC") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "satellite") %>%
              clearImages() %>%
              addRasterImage(x = layerd, colors = palos, opacity = (input$mapOpacityhc)/100, method = "ngb")    }
          else if (input$mapHCSelect == 1){
            leafletProxy("mapShowHC") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileRoad",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "roadmap") %>%
              clearImages() %>%
              addRasterImage(x = layerd, colors = palos, opacity = (input$mapOpacityhc)/100, method = "ngb")    }
          else{
            leafletProxy("mapShowHC") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileTer",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "terrain") %>%
              clearImages() %>%
              addRasterImage(x = layerd, colors = palos, opacity = (input$mapOpacityhc)/100, method = "ngb")    }
        }  
      }
    })
  })
  

  #Event to toggle the legend
  observe({
    input$mapshowhcS
    input$hardconstraint
    
    proxy <- leafletProxy("mapShowHC")
    colorszz <- c("#808080", "#33cc33")
    #Remove any existing legend, and only if the legend is enabled, create a new one.
    if (input$mapshowhcS) {
      proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = colorszz,labels =c("unacceptable", "acceptable"), opacity = input$mapOpacityhc/100)
    }
    else{
      proxy %>% clearControls()
    }
  })
  
  # List the hard constraints 
  output$renderChoiceHC <-renderUI({
    validate( need(use_HC==TRUE , ""))
    # Read the names of the hard constraints in the HC database and create a list
    # List of names
    HCnames <- sapply(HC[,3], toString)
    HCdesc <- sapply(HC[,4], toString)
    HCdesc <- gsub("_", " ", HCdesc)
    mylist <- vector(mode="list", length=length(HCnames))
    names(mylist) <- HCnames
    mylist=lapply(1:length(HCnames), function(i){
      mylist[[i]] <- names(mylist)[i]
    })
    names(mylist) <- HCdesc
    #Create the checkbox to choose hardconstraints
    checkboxGroupInput("checkGroupHC", "Choose the hard constraints :", 
                       choices=mylist, selected= mylist[1])
  })
  
  compilHC <- reactive({input$HCbutton
    # case of one contraint selected
    if(length(input$checkGroupHC)>0){
      hc_final <<-subset(rst.hcl, input$checkGroupHC[1])
      # case of two contraints selected
      if (length(input$checkGroupHC)==2){
        layer1 <- subset(rst.hcl, input$checkGroupHC[1])
        layer2 <- subset(rst.hcl, input$checkGroupHC[2])
        # If more than 1, they are merged
        hc_final <-overlay(layer1,layer2, fun=function(x,y){ifelse((x+y)==0,0,1)})
      }
      # case of three contraints selected
      if(length(input$checkGroupHC)==3){
        layer1 <- subset(rst.hcl, input$checkGroupHC[1])
        layer2 <- subset(rst.hcl, input$checkGroupHC[2])
        layer3 <- subset(rst.hcl, input$checkGroupHC[3])
        # If more than 1, they are merged
        hc_final <-overlay(layer1,layer2,layer3, fun=function(x,y,z){ifelse(x+y+z==0,0,1)})
      }
      # case of four contraints selected
      if(length(input$checkGroupHC)==4){
        layer1 <- subset(rst.hcl, input$checkGroupHC[1])
        layer2 <- subset(rst.hcl, input$checkGroupHC[2])
        layer3 <- subset(rst.hcl, input$checkGroupHC[3])
        layer4 <- subset(rst.hcl, input$checkGroupHC[4])
        # If more than 1, they are merged
        hc_final <-overlay(layer1,layer2,layer3,layer4, fun=function(x,y,z,k){ifelse(x+y+z+k==0,0,1)})
      }
      # case of more then four constraints selected
      if(length(input$checkGroupHC)>4){
        output$mapFinalHC <- renderPrint({
          cat(paste0("Please choose four hard constraints maximum."))
        }) 
      }
    }     # If 0 HC selected the final raster pixels values get 0
    else{
      hc_final <- rst.hcl[[1]]
      hc_final[hc_final]=0
    }
    return(hc_final)
  })
  
  
  #Add the hard constraint chosen to the criteria rasters
  observeEvent(input$HCbutton, {
    HCraster <- compilHC()
    nl <- addHC(rst.nl.original, HCraster)
    pl <- addHC(rst.pl.original, HCraster)
    pnl <- addHC(rst.pnl.original, HCraster)
    rst.pnl <<- mask(pnl, areaselected)
    rst.nl <<- mask(nl, areaselected)
    rst.pl <<- mask(pl, areaselected) 
    
    shinyjs::show(selector = "#navbar li a[data-value=tab5]")
    updateTabsetPanel(session, 
                      inputId = "navbar", 
                      selected = "tab5")
    
  })
  

  ##############################################################################################################  
  ########################################## Select Criteria ###################################################
  ##############################################################################################################
  
  #Hide functionnalities to change criteria in positive/negative
  shinyjs::hide("POSorNEG")
  shinyjs::hide("changeCriteria")
  
  # When you want to change a criteria from positive to negative or vice-versa
  observeEvent(input$changeCriteria,{
    shinyjs::hide("POSorNEG")
    shinyjs::hide("changeCriteria")
    shinyjs::show("positiveCriteria")
    shinyjs::show("matrixPN")
    shinyjs::show("legendCriteria")
  })  

  observeEvent(input$positiveCriteria, {
    shinyjs::hide("positiveCriteria")
    shinyjs::hide("matrixPN")
    shinyjs::hide("legendCriteria")
    shinyjs::show("POSorNEG")
    shinyjs::show("changeCriteria")
  })
  
  
  output$POSorNEG <- renderUI({
    input$Project
    checkboxGroupInput("tickPositiveCriteria", label=" Choose the positive criteria by ticking cases :" , choices = gsub("_", " ",HPN[,4]), selected = gsub("_", " ",HP[,4]))
  })
  
  #Display the matrix which contains the criteria description
  output$matrixPN <- renderUI({input$Project
    input$changeCriteria
    isolate({
    #Transform the row data in understandable text
    crit <- sapply(HPN[,4], toString)
    crit <- gsub("_", " ", crit)
    # I have to test if the checkbox "tickPositiveCriteria" has not a NULL output. 
    # If it is NULL, then it's because the checkbox is not initialized yet, so you have to find another way to get 'change' and avoid an error of length. 
    if (is.null(input$tickPositiveCriteria)){
    change <- crit %in% gsub("_", " ",HP[,4])
    }
    else {
    change <<- crit %in% input$tickPositiveCriteria
    }
    
    # HPN2 is created to get the description associated with his kind of criteria. It's usefull for the choosing box below.
    HPN2 <<- HPN[,4]
    HPN2 <<- ifelse(change, paste0("[POS] ",HPN2), paste0("[NEG] ",HPN2))
    
    rst.pl.original <<- checkPositiveCriteria(subset(x = rst.pnl.original, (1:length(HPN[,4]))[change]))
    rst.nl.original <<- checkNegativeCriteria(subset(x = rst.pnl.original, (1:length(HPN[,4]))[!change]))
    rst.pnl.original <<- stack(lapply(1:nlayers(rst.pnl.original), function(x){
      if (change[x]){
        checkPositiveCriteria(rst.pnl.original[[x]])
      }
      else{
        checkNegativeCriteria(rst.pnl.original[[x]])  
      }
      }))
    rst.pl <<- checkPositiveCriteria(subset(x = rst.pnl, (1:length(HPN[,4]))[change]))
    rst.nl <<- checkNegativeCriteria(subset(x = rst.pnl, (1:length(HPN[,4]))[!change]))
    rst.pnl <<- stack(lapply(1:nlayers(rst.pnl), function(x){
      if (change[x]){
        checkPositiveCriteria(rst.pnl[[x]])
      }
      else{
        checkNegativeCriteria(rst.pnl[[x]])  
      }
    }))
    
    crit[change] <- paste(crit[change],"#pos")
    crit[!change] <- paste(crit[!change],"#neg")
    criteria.description <- data.frame(criteria=crit)
    css <- c("#pos {background-color: #ffb84d;}",
             "#neg {background-color: #00ccff;}")
    htmltab <- markdownToHTML(
      text=pandoc.table.return(
        criteria.description,
        style="rmarkdown", split.tables=Inf
      ), 
      fragment.only=TRUE
    ) 
    colortable(htmltab, css)
  })
})
  
  
  
  #Display the select input of criteria that the user wants to display on the map
  output$renderSelectCriteria <- renderUI({input$Project
    descript <- sapply(HPN[,4], toString)
    descript <- gsub("_", " ", descript)
    choices <- list()
    for(i in 1:length(HPN[,3])){
      choices[i] <- toString(HPN[i,3])
    }
    names(choices) <- descript
    selectInput("criteria", "Choose a criteria to display:", 
                # choices=rownames(HPN)
                choices = choices)
  })
  
  #Make a legend to describe colors in the table 
  output$legendCriteria <- renderUI({
    withTags(
      div(img(src="blue_square.jpg"), b("Negative criteria"), style="display: inline-block ; align:center",br(),
      img(src="orange_square.jpg"), b("Positive criteria"))
      
    )
  })

  # Display the criteria map
  output$mapShowCriteria <- renderLeaflet({
    input$criteria
    input$HCbutton

    
    if(is.null(input$criteria)){
      delay(100,{})
    }
    isolate({
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      if(input$mapShowCriteria_zoom == zoomin[1] || input$mapShowCriteria_zoom == zoomin[2] || input$mapShowCriteria_zoom == zoomin[3] || input$mapShowCriteria_zoom == zoomin[4] || input$mapShowCriteria_zoom == zoomin[5] || length(input$criteria) != 0 || input$mapOpacitycrit){
        if (!is.null(input$criteria)){
          #The layer displayed is the one selected by the user in input criteria
          layerdisplayed<- subset(rst.pnl,input$criteria)
          ## This is the raster which is displayed on the map!!!!!
          layerd <- mask(layerdisplayed, areaselected)
          
          palos<-c("#ff0000","#0000ff")
          
         if(input$mapCritSelect == 0){
          leaflet() %>%
            addGoogleTiles(layerId = "googleTileSat",
                           options = tileOptions(maxZoom = 15,
                                                 updateWhenZooming = TRUE),
                           type = "satellite") %>%
            setView(lng = zoomCenter[[1]],
                    lat = zoomCenter[[2]],
                    zoom = zoomLevel) %>%
            addRasterImage(x = layerd,colors = palos,opacity = (input$mapOpacitycrit)/100, method = "ngb")            }
     }
    }
    })
  })

  
  # Observer to redraw the raster
  observe({ 
    input$mapShowCriteria_zoom
    input$mapOpacitycrit
    input$criteria
    input$mapCritSelect
    input$HCbutton
    # The console will trigger an error if the leaflet object called
    # Is not defined and this is sometimes the case as the map take less time to initalize then
    # the input hardconstraint, that's why we need to delay the execution
    # of the code for the first time you enter into the tab
    if(is.null(input$hardconstraint)){
      delay(200,{})
    }
    isolate({
      zoomin <- leafletGoogle::setZoomBound(xdist = xdist, ydist = ydist)
      if(input$HCbutton >= 1){
        if(length(input$criteria) != 0 ){
          if(input$mapShowCriteria_zoom == zoomin[1] || input$mapShowCriteria_zoom == zoomin[2] || input$mapShowCriteria_zoom == zoomin[3] || input$mapShowCriteria_zoom == zoomin[4] || input$mapShowCriteria_zoom == zoomin[5] || length(input$criteria) != 0 || input$mapOpacitycrit){
            #The layer displayed is the one selected by the user in input criteria
            layerdisplayed<- subset(rst.pnl,input$criteria)
            ## This is the raster which is displayed on the map!!!!!
            layerd <- mask(layerdisplayed, areaselected)
            palos<-c("#ff0000","#0000ff")
            # Redraw the map now
            if(input$mapCritSelect == 0){
              leafletProxy("mapShowCriteria") %>%
                clearTiles() %>%
                addGoogleTiles(layerId = "googleTileSat",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "satellite") %>%
                clearImages() %>%
                addRasterImage(x = layerd,colors = palos,opacity = (input$mapOpacitycrit)/100, method = "ngb")            }
            else if (input$mapCritSelect == 1){
              leafletProxy("mapShowCriteria") %>%
                clearTiles() %>%
                addGoogleTiles(layerId = "googleTileRoad",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "roadmap") %>%
                clearImages() %>%
                addRasterImage(x = layerd,colors = palos,opacity = (input$mapOpacitycrit)/100, method = "ngb")            }
            else{
              leafletProxy("mapShowCriteria") %>%
                clearTiles() %>%
                addGoogleTiles(layerId = "googleTileTer",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "terrain") %>%
                clearImages() %>%
                addRasterImage(x = layerd,colors = palos,opacity = (input$mapOpacitycrit)/100, method = "ngb")            }
            
          }
        }
      }
    })
  })
  # Memorize Zoom level and center 
  observe({
    zoomLevel <<- input$mapShowCriteria_zoom
    zoomCenter <<- input$mapShowCriteria_center
  })
  
  
  
  # Event to toggle the legend
  observe({
    input$mapshowcrit
    input$mapOpacitycrit
    input$criteria
    input$mapCritSelect
    input$HCbutton
    
    proxy <- leafletProxy("mapShowCriteria")
    colorszz <- c("#ff0000","#0000ff")
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    if (input$mapshowcrit) {
      proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = colorszz,labels = c("unacceptable", "acceptable"), opacity = input$mapOpacitycrit/100)
    }
    else{
      proxy %>% clearControls()
    }
  })
  
  output$chooserpdisplayed <-renderUI({
    input$Project  
    input$changeCriteria
    
    crit <- sapply(HPN[,4], toString)
    crit <- gsub("_", " ", crit)
    if (is.null(input$tickPositiveCriteria)){
      change <- crit %in% gsub("_", " ",HP[,4])
    } else {
    change <<- isolate({crit %in% input$tickPositiveCriteria})
    }
    # HPN2 is created to get the description associated with his kind of criteria. It's usefull for the choosing box below.
    HPN2 <- HPN[,4]
    HPN2 <- ifelse(change, paste0("[POS] ",HPN2), paste0("[NEG] ",HPN2))
    
    chooserInput("mychooserp", 
                 "available", 
                 "selected",
                 
                 leftChoices = HPN2[2:length(HPN$Number)],
                 rightChoices = HPN2[1],
                 
                
                 size = length(x1), 
                 multiple = TRUE)
  })


  
  ####################################################################################################################  
  #################################### Select weights advanced #######################################################
  ####################################################################################################################
  # Define input sliders as the selected criteria of the chooser input for any criteria advanced. 
  output$slidersp <- renderUI({
    input$Project
    
    if(TRUE){
      lapply(input$mychooserp, function(i) {
        tmp <- HPN[HPN$Description %in% substr(i,7,700),]$Nickname
        tmpdes <- HPN2[HPN$Description %in% substr(i,7,700)]
        descript <- gsub("_", " ", tmpdes)
        sliderInput(inputId = paste0("sliderp",which(x1bis==tmp)), label = descript, min=0, max=1, value=0,ticks=F)
      }
      )                              
    }
  })
  
  # Block "Go" button during 500 ms to avoid double click (annoying because it will run twice...)
  observeEvent(input$goButtonAdvanced,{
    shinyjs::disable(id = "goButtonAdvanced")
    shinyjs::delay(ms = 500 , expr = enable(id = "goButtonAdvanced"))
  })
 
  # Block "Go" button during 500 ms to avoid double click (annoying because it will run twice...)
  observeEvent(input$saveButton,{
    shinyjs::disable(id = "saveButton")
    shinyjs::delay(ms = 250 , expr = enable(id = "saveButton"))
  })
  
  ###
  # Reactive value for goButtonAdvanced and goButtonBasic 
  # to make the difference between adv and basic user
  observeEvent(input$goButtonAdvanced,{
    reactX <<- 0
  })
  observeEvent(input$goButtonBasic,{
    reactX <<- 1
  })
  
  
  observeEvent(input$goButtonAdvanced,{
    groups<-c()
    tmp <- paste0("GroupChoice",1)
    print(tmp)
    var <- get(input$tmp)
    print(var)
    groups <- c(groups,var)
    print(groups)
  })
  
  
  
  # Define input data as the values of each criteria weight (sliders)
  # Updated when goButtonAdvanced is pressed
  weightsVector <- reactive({
    nbrOfProject <- input$pann3
    input$goButtonAdvanced
    input$goButtonBasic
    
    isolate({      
      #creation of the vector weight
      y<-c()
      # Loop filling vector weight with positive and negative weights. 
      # Check if each criteria is selected to plot the sliders. 
      # If not remove the slider variable if it existed previously 

      m=unlist(lapply(1:length(x1) ,function(i){
        if(!(HPN[HPN$Nickname == toString(x1[i]),]$Description %in% substr(input$mychooserp,7,700))){
          y <-c(y,NA)

        }
        else{
          if (reactX == 0){
            y <-c(y,input[[paste0("sliderp", i)]])
          }
          else{
            y <-c(y,input[[paste0("sliderpa", i)]])
          }
        }
      }))
      y<-c(m)
      ar <- array(NA, dim=(length(x1)-length(input$mychooserp)+1))
      if(identical(y, c(ar))){
        y<-c(array(NA, (length(x1))))
      }
      return(y)
    })
  })
  
  #Waiting for press save button. Save weight combinations in the file created previously for the chosen method
  observe({
    if (input$saveButton == 0){return()}
    isolate({
      y <<- weightsVector() 
      if(input$methodsmain==2){    
        write(y, file = toString(Path["LWC2",2]),ncolumns = length(HP[,1])+length(HN[,1]),append=TRUE)}
      else if(input$methodsmain==3){     
        write(y, file = toString(Path["Electre",2]),ncolumns = length(HP[,1])+length(HN[,1]),append=TRUE)}
      else if(input$methodsmain==4){     
        write(y, file = toString(Path["LWC-G",2]),ncolumns = length(HP[,1])+length(HN[,1]),append=TRUE)}
    })
  })
  
  # Update of rastersaved when save Raster button is pressed 
  observe({
    if (input$saveRasterButton == 0){return()}
    isolate({
      names(temporaryrast) <- paste0(type,"_",num)
      rastersaved <<- addLayer(rastersaved, temporaryrast)
    })
  })
  whereIsSaved<<-""
  #Waiting for press save permanently button. Save final weight combinations in the file created previously for the chosen method(s)
  observe({
    if (input$savePermanentlyButton == 0){return()}
    isolate({y <- weightsVector()  
    if (input$bothMethods=="TRUE"){
      write(y, file = toString(Path["LWC2",1]),ncolumns = length(HP[,1])+length(HN[,1]),append=TRUE)
      write(y, file = toString(Path["Electre",1]),ncolumns = length(HP[,1])+length(HN[,1]),append=TRUE)
      whereIsSaved <<- c(toString(Path["LWC2",1]), toString(Path["Electre",1]))
    }
    else if(input$methodsmain==2){  
      print(getwd())
      write(y, file = toString(Path["LWC2",1]),ncolumns = length(HP[,1])+length(HN[,1]),append=TRUE)
      whereIsSaved <<- toString(Path["LWC2",1])
      }
    else if(input$methodsmain==3){     
      write(y, file = toString(Path["Electre",1]),ncolumns = length(HP[,1])+length(HN[,1]),append=TRUE)
      whereIsSaved <<-  toString(Path["Electre",1])
    }
    else if(input$methodsmain==4){     
      write(y, file = toString(Path["LWC-G",1]),ncolumns = length(HP[,1])+length(HN[,1]),append=TRUE)
      whereIsSaved <<-  toString(Path["LWC-G",1])
    }
    })
  })
  
  ### disable/enable save permanently or cancel your save. You can save permanently once. 
  observe({
      input$savePermanentlyButton
      shinyjs::disable("savePermanentlyButton")
      shinyjs::enable("CancelSave")
  })
  
  observe({
    input$CancelSave
    shinyjs::enable("savePermanentlyButton")
    shinyjs::disable("CancelSave")
    if (length(whereIsSaved)==1) {
      txtMem<-readLines(whereIsSaved)
      writeLines(con = whereIsSaved, text = txtMem[1:length(txtMem)-1] )
    }
    else {
      txtMem<-readLines(whereIsSaved[1])
      writeLines(con = whereIsSaved[1], text = txtMem[1:length(txtMem)-1] )
      txtMem<-readLines(whereIsSaved[2])
      writeLines(con = whereIsSaved[2], text = txtMem[1:length(txtMem)-1] )
      }
  })
  ### End disable/enable
  
  # Transform the radio input in an input variable depending of the Go button.	  
  radiomInput <- reactive({input$goButtonAdvanced | input$goButtonThr
    isolate({  
      m <- input$methodsmain
      return(m)						   
    })    
  })
  
  #Calculation of the unique raster with the positive and negative weights vector and the method chosen
  calculmapmain <- reactive({
    weightsVector() 
    isolate({ 
      radiomInput
      
      if(radiomInput()==2){rst.comb.w=lwc2(rst.pnl, weightsVector())}
      if(radiomInput()==3){rst.comb.w=electre(rst.pnl, weightsVector())}
      #if(radiomInput()==4){rst.comb.w=lwcg(rst.pnl, weightsVector(),input$GroupChoice)}
      
      # Normalization of the unique raster
      rst.comb.w2=normalizer(rst.comb.w)
      
      return(rst.comb.w2) 
    })   
  })
  
  # Define main map to visualize the unique raster created with the chosen weights combination 
  output$mapMain <- renderLeaflet({
    input$goButtonAdvanced
    weightsVector()
    calculmapmain()
    validate(need(!is.null(weightsVector()),""))
    validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), 
                  "Please put a positive weigth for at least one criterion or select new criteria (go back to select criteria tab)"))
    
    
    # Define progress message to warn the user to wait during the calculus 
    withProgress({
      setProgress(message = "Calculating, please wait",
                  detail = "This may take a few moments...")
      rst.comb.w2 <- calculmapmain()
      # The layer is cropped to match the area selected
      rst.comb.w2 <- crop(rst.comb.w2, extent(areaselected))
      if(input$inputselect=="1"){
        projection(rst.comb.w2) <- projection(areaselected)
        rst.comb.w2m <- mask(rst.comb.w2, areaselected)
      }else{
        rst.comb.w2m <- rst.comb.w2
      }
      
      temporaryrast <<- rst.comb.w2m
      y<<-weightsVector()
      
      num <<- num+1  
      # Definition of the colour palette  of the future map
      pal <- brewer.pal(10,"RdYlBu") 
      palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)
      
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      mamapAvailable <<- 0
      leaflet()%>%
        addGoogleTiles(layerId = "googleTileSat",
                       options = tileOptions(maxZoom = 15,
                                             updateWhenZooming = TRUE),
                       type = "satellite") %>%
        setView(lng = zoomCenter[[1]],
                lat = zoomCenter[[2]],
                zoom = zoomLevel) %>%
        addRasterImage(x = rst.comb.w2m, colors = palos, opacity = isolate(input$mapOpacitymain)/100, method="ngb") %>%
        addLegend(position = "bottomright",colors = pal,labels=c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity=input$mapOpacitymain/100)
      
    },
    session=session)
  })
  
  
  # Memorize Zoom level and center 
  observe({
    zoomLevel <<- input$mapMain_zoom
    zoomCenter <<- input$mapMain_center
  })
  
  # Update the mapmain
  observe({ 
    input$mapMain_zoom
    input$mapOpacitymain
    input$goButtonAdvanced
    input$mapMainSelect
    
    weightsVector()
    # The console will trigger an error if the leaflet object called
    # Is not defined and this is sometimes the case as the map take less time to initalize then
    # the input hardconstraint, that's why we need to delay the execution
    # of the code for the first time you enter into the tab
    validate(need(mamapAvailable ==0,""))
    
    isolate({
      pal  <- brewer.pal(10,"RdYlBu")  
      palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)

      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)

      if(input$goButtonAdvanced > 0 ){
        if(input$mapMain_zoom == zoomin[1] || input$mapMain_zoom == zoomin[2] || input$mapMain_zoom == zoomin[3] || input$mapMain_zoom == zoomin[4] || input$mapMain_zoom == zoomin[5] || input$mapOpacitymain){
          
          # Redraw the map now

          if(input$mapMainSelect == 0){
            leafletProxy("mapMain") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "satellite") %>%
              clearImages() %>%
              addRasterImage(x = temporaryrast,colors = palos,opacity = (input$mapOpacitymain)/100, method="ngb")
          }
          else if (input$mapMainSelect == 1){
            leafletProxy("mapMain") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileRoad",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "roadmap") %>%
              clearImages() %>%
            addRasterImage(x = temporaryrast,colors = palos,opacity = (input$mapOpacitymain)/100, method="ngb")
          }
          else{
            leafletProxy("mapMain") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileTer",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "terrain") %>%
              clearImages() %>%
             addRasterImage(x = temporaryrast,colors = palos,opacity = (input$mapOpacitymain)/100, method="ngb")
          }
      }
      }
    })
  })
  
  
  # Show the legend
  observe({
    input$mapshowmain
    #input$mapOpacitymain
    input$goButtonAdvanced
    #input$mapMainSelect
    proxy <- leafletProxy("mapMain")
    pal  <-brewer.pal(10,"RdYlBu")  
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    if (input$mapshowmain) {
      proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = pal,labels=c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity=input$mapOpacitymain/100)
    }
    else{
      proxy %>% clearControls()
    }
  })
  
  
  # Create an histogram to display the distribution of pixels for this combination of weigths
  output$histo <- renderPlot({ 
    validate(need(!is.null(weightsVector()),""))
    validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), 
                  "Please put a positive weigth for at least one criterion or select new criteria (go back to select criteria tab)"))
    
    # Colors and data for the histogram
    palos <- brewer.pal(10,"RdYlBu")  
    rst.comb.w2 <- calculmapmain()
    rst.comb.w2 <- crop(rst.comb.w2, extent(areaselected))
    if(input$inputselect=="1"){
      projection(rst.comb.w2) <- projection(areaselected)
      rst.comb.w2m <- mask(rst.comb.w2, areaselected)
    }else{
      rst.comb.w2m <- rst.comb.w2
    }
    #Plot the histogram
    hist(na.omit(rst.comb.w2m@data@values),
         main="Distribution of Pixels Score",col = palos,
         breaks=10,
         xlab="Pixel Score")
  })
  
  # Button to download the raster created, displayed only when a map has been displayed
  output$downloadRasterbutton <- renderUI({
    validate( need(Reduce(function(acc, cur) { acc = (acc || !is.na(cur)) }, weightsVector(), F), " "),
              need((length(weightsVector()[!is.na(weightsVector())]))>=2,"Not enough critera"),
              need(length((weightsVector()[!is.na(weightsVector())])[(weightsVector()[!is.na(weightsVector())])<=0.2])<=2,"Weigths too weak"))
    downloadButton('downloadRaster', 'Download Raster')
  })
  
  # Function enabling the downloading of the raster
  output$downloadRaster <- downloadHandler(
    filename = function(){return(paste0(type,"_",num,".tif"))},
    content = function(file) {writeRaster(temporaryrast, format="GTiff", file)}
  )
  
  output$nbGroup <- renderUI({
    validate(need(input$methodsmain==4,""))
    numericInput("nbGroup","Choose the number of group",2,2,length(input$mychooserp)-1,1)})
  
  output$GroupChoice <- renderUI({
    validate(need(input$methodsmain==4,""))
    lapply(input$mychooserp, function(i){
      tmp <- HPN[HPN$Description %in% substr(i,7,700),]$Nickname
      tmpdes <- HPN2[HPN$Description %in% substr(i,7,700)]
      descript <- gsub("_", " ", tmpdes)
      selectInput(inputId=paste0("GroupChoice",1),label=descript,choices=mapply(toString, 1:input$nbGroup),1)
    })})
  
  
  
  #######################################################################################################################
  ######################################### Select weights ##############################################################
  #######################################################################################################################
  
  #sliderInput for each criteria 
  output$sliderspa <- renderUI({input$Project
    if(length(input$mychooserp)>=1){
      lapply(input$mychooserp, function(i) {
        tmp <- HPN[HPN$Description %in% substr(i, 7, 700),]$Nickname
        tmpdes <- HPN2[HPN$Description %in% substr(i, 7, 700)]
        descript <- gsub("_", " ", tmpdes)
        sliderInput(inputId = paste0("sliderpa",which(x1bis==tmp)), label = descript, min=0, max=1, value=0,ticks=F)
      })
    }
  })
  
  # Block "Go" button during 500 ms to avoid double click (annoying because it will run twice...)
  observeEvent(input$goButtonBasic,{
    shinyjs::disable(id = "goButtonBasic")
    shinyjs::delay(ms = 500 , expr = enable(id = "goButtonBasic"))
  })
  
  
  calculmapmaina <- reactive({
    weightsVector() 
    validate(need(!is.null(weightsVector()),""))
    isolate({ 
      if(radiomInputa()==2){rst.comb.w=lwc2(rst.pnl, weightsVector())}
      if(radiomInputa()==3){rst.comb.w=electre(rst.pnl, weightsVector())}
    
      # Normalization of the unique raster
      rst.comb.w2=normalizer(rst.comb.w)        
      return(rst.comb.w2) 
    })   
  })
  
  # Transform the radio input in an input variable depending of the Go button.
  radiomInputa <- reactive({input$goButtonBasic | input$goButtonThr
    isolate({
      m <- input$methodsmaina
      return(m)
    })
  })

  # Define main map to visualize the unique raster created with the chosen weights combination
  output$mapMaina <- renderLeaflet({
    input$goButtonBasic    
    # If all weights sliders are on 0, warns the user
    validate(need(!is.null(weightsVector()),""))
    validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), 
                  "Please put a positive weigth for at least one criterion or select new criteria (go back to select criteria tab)"))
    
    # Define progress message to warn the user to wait during the calculus 
    withProgress({ 
      setProgress(message = "Calculating, please wait",
                  detail = "This may take a few moments...")
      
      rst.comb.w2 <- calculmapmaina()
      
      # The layer is cropped to match the area selected
      rst.comb.w2 <- crop(rst.comb.w2, extent(areaselected))
      if(input$inputselect=="1"){
        projection(rst.comb.w2) <- projection(areaselected)
        rst.comb.w2m <- mask(rst.comb.w2, areaselected)
      }else{
        rst.comb.w2m <- rst.comb.w2
      }
      
      temporaryrast <<- rst.comb.w2m
      
      num <<- num+1  
      
      # Definition of the colour palette  of the future map
      pal <- brewer.pal(10,"RdYlBu")  
      palos <- colorBin(palette = pal, bins = seq(0,1,.1), domain = seq(0,1,.1), na.color = NA) 
        
      mamapAvailable <<- 1
     
      leaflet()%>%
        addGoogleTiles(layerId = "googleTileSat",
                       options = tileOptions(maxZoom = 15,
                                             updateWhenZooming = TRUE),
                       type = "satellite") %>%
        setView(lng = zoomCenter[[1]],
                lat = zoomCenter[[2]],
                zoom = zoomLevel) %>%
        addRasterImage(x = rst.comb.w2m, colors = palos, opacity = (input$mapOpacitymaina)/100, method="ngb") %>%
        addLegend(position = "bottomright", colors = pal, labels=c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = input$mapOpacitymaina/100)
    },
    session=session)		 
  })
  
  # Memorize Zoom level and center 
  observe({
    zoomLevel <<- input$mapMaina_zoom
    zoomCenter <<- input$mapMaina_center
  })
  
  # Update the mapmaina
  observe({ 
    input$mapMaina_zoom
    input$mapOpacitymaina
    input$goButtonBasic
    input$mapMainaSelect
    # The console will trigger an error if the leaflet object called
    # Is not defined and this is sometimes the case as the map take less time to initalize then
    # the input hardconstraint, that's why we need to delay the execution
    # of the code for the first time you enter into the tab
    validate(need(mamapAvailable ==1,""))
    
    isolate({
      pal  <- brewer.pal(10,"RdYlBu")  
      palos <- colorBin(palette = pal, bins = seq(0,1,.1), domain = seq(0,1,.1), na.color = NA) 
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      
      if(input$goButtonBasic > 0 ){
        if(input$mapMaina_zoom == zoomin[1] || input$mapMaina_zoom == zoomin[2] || input$mapMaina_zoom == zoomin[3] || input$mapMaina_zoom == zoomin[4] || input$mapMaina_zoom == zoomin[5] || input$mapOpacitymaina){           
          
          # Redraw the map now
          if(input$mapMainaSelect == 0){
            leafletProxy("mapMaina") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "satellite") %>%
              clearImages() %>%
              addRasterImage(x = temporaryrast, colors = palos, opacity = (input$mapOpacitymaina)/100, method="ngb")            }
          else if (input$mapMainaSelect == 1){
            leafletProxy("mapMaina") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileRoad",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "roadmap") %>%
              clearImages() %>%
              addRasterImage(x = temporaryrast, colors = palos, opacity = (input$mapOpacitymaina)/100, method="ngb")            }
          else{
            leafletProxy("mapMaina") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileTer",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "terrain") %>%
              clearImages() %>%
              addRasterImage(x = temporaryrast, colors = palos, opacity = (input$mapOpacitymaina)/100, method="ngb")            }
        }
      }
    })
  })
  
  # Show the legend
  observe({
    input$mapshowmaina
    input$goButtonBasic
    proxy <- leafletProxy("mapMaina")
    pal  <-brewer.pal(10,"RdYlBu")  
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    if (input$mapshowmaina) {
      proxy %>% clearControls() %>% addLegend(position = "bottomright", colors = pal, labels = c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"))
    }
    else{
      proxy %>% clearControls()
    }
  })
  
  # Create an histogram to display the distribution of pixels for this combination of weigths
  output$histoa <- renderPlot({ 
    validate( need(weightsVector() != vector(mode = "numeric", length = length(HP[,1])+length(HN[,1])),  "Please put a positive weigth for at least one criterion or select new criteria (go back to select criteria tab)"))
    rst.comb.w2 <- calculmapmaina()
    rst.comb.w2 <- crop(rst.comb.w2, extent(areaselected))
    if(input$inputselect=="1"){
      projection(rst.comb.w2) <- projection(areaselected)
      rst.comb.w2m <- mask(rst.comb.w2, areaselected)
    }else{
      rst.comb.w2m <- rst.comb.w2
    }
    #Plot the histogram
    hist(na.omit(rst.comb.w2m@data@values),main="Distribution of Pixels Select Criteria",xlab=paste0(toString(Pr[Pr$Name==type,3])," expansion index"))
  })
  
  # Button to download the raster created, displayed only when a map has been displayed
  output$downloadRasterbuttona <- renderUI({
    validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), " "),
             need((length(weightsVector()[!is.na(weightsVector())]))>=2,"Not enough critera"),
             need(length((weightsVector()[!is.na(weightsVector())])[(weightsVector()[!is.na(weightsVector())])<=0.2])<=2,"Weigths too weak"))
    downloadButton('downloadRastera', 'Download Raster')
  })
  
  
  # Function enabling the downloading of the raster
  output$downloadRastera <- downloadHandler(
    filename = function(){return(paste0(type,"_",num,".tif"))},
    content = function(file) {writeRaster(temporaryrast, format="GTiff", file)}
  )
  
  #############################################################################################################  
  ################################### Select Threshold ########################################################
  #############################################################################################################  
  
  # Block "Go" button during 500 ms to avoid double click (annoying because it will run twice...)
  observeEvent(input$goButtonThr,{
    shinyjs::disable(id = "goButtonThr")
    shinyjs::delay(ms = 500 , expr = enable(id = "goButtonThr"))
  })
  
  ## The following part makes the two sliders to be reactive to each other -- once you move one of them the other one is triggered
  # Creation of the slider Threshold
  output$renderThr <- renderUI({
    sliderInput(inputId="thr", label = "Select the number of ha you want:", min = 50, max = datamaxThreshold(), post="ha", value = (50/100)*datamaxThreshold())
  })
  
  # Creation of the slider Threshold per quantile
  output$renderThrQ <- renderUI({
    sliderInput(inputId="thrQ", label = "Select the percentage of area you want:", min = 0.0161838485191779, max =100, post="%", value = 50)
  })
  
  
  # Define threshold input as the value of the slider "Threshold" in the panel threshold
  t <- "0"
  
  # Use of an input button to update
  thrInput <- reactive({input$goButtonAdvanced | input$goButtonThr | input$goButtonBasic
    isolate({      
      input$thr
    })    
  })
  
  # Use of an input button to update
  thrQInput <- reactive({input$goButtonAdvanced | input$goButtonThr | input$goButtonBasic 
    isolate({      
      input$thrQ
    })    
  })
  
  # Define the maximum of the sliders Threshold
  datamaxThreshold<- reactive({input$goButtonAdvanced | input$comparebutton | input$goButtonBasic
    layer <-rst.pnl[[1]]
    area <- maxThrCalculation(layer)
    return(area)
  })
  
  
  val <- reactive({input$thr}) #x
  valQ <- reactive({input$thrQ}) #y
  

  
  #Define the wanted threshold (area or quantile)

  observeEvent(input$thr,{
    t <<- "1"
    y <- as.integer((val()*100)/datamaxThreshold())
    updateSliderInput(session, "thrQ", value = y, step = 0.01)
  })
  
  observeEvent(input$thrQ,{
    t <<- "2"
    x <- as.integer(valQ()*(datamaxThreshold()/100))
    updateSliderInput(session, "thr", value = x, step = 0.01)
  })
##End of the "two sliders reactivity"
  
  
  # Define threshold map to visualize the best pixels under the threshold  
  output$mapThr <- renderLeaflet({
    input$goButtonThr 
    # If all weights sliders are on 0, warns the user
    validate(need(input$goButtonThr >0,""))
    validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), 
                  "Please put a positive weigth for at least one criterion or select new criteria (go back to select criteria tab)"))
    
    datamaxThr <<- datamaxThreshold()
    # Define progress message to warn the user to wait during the calculus 
    withProgress({
      setProgress(message = "Calculating, please wait",detail = "This may take a few moments...")
      
     # We need to know whether we used adv or basic mode
      if(mamapAvailable==0){
        rst.comb.w <- calculmapmain()}
      else{
        rst.comb.w <- calculmapmaina()}

      thr=as.numeric(cellStats(rst.comb.w,stat=function(x,na.rm){stats::quantile(x,probs=1-thrInput()/datamaxThreshold(),na.rm=T)}))
      
      somma <-nbpx_notNA(rst.comb.w)
      
      # calculus of the threshold raster
      
      if(input$inputselect=="1"){
        projection(rst.comb.thr) <- projection(areaselected)
        rst.comb.thr <<- mask(rst.comb.w, areaselected)}
      else{
        rst.comb.thr <<- rst.comb.w}
     
      rst.comb.thr2 <- binarythr(rst.comb.thr,thr,thrQInput()/100)
      
      RasterforThreshold <<- rst.comb.thr2
      ##Save raster for sensitivity analysis (only advanced user)
      
      c <- cellStats(rst.comb.thr2,sum)
      d <- somma -c
      pc <-c/somma
      pd <-d/somma
      
      # Definition of the colour palette  of the future map + zoom for the map
      pal  <- c("#ff0000","#0000ff")
      palos <- colorBin(palette = pal, domain = seq(0,1), bins = 2, na.color = NA)
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      
      # Leaflet map which replace the old map
      leaflet()%>%
        addGoogleTiles(layerId = "googleTileSat",
                       options = tileOptions(maxZoom = 15,
                                             updateWhenZooming = TRUE),
                       type = "satellite") %>%
        setView(lng = zoomCenter[[1]],
                lat = zoomCenter[[2]],
                zoom = zoomLevel) %>%

        addRasterImage(x = rst.comb.thr2, colors = palos, opacity = isolate(input$mapOpacityThr)/100, method="ngb")
    }, session=session)
  })    
  
  
  # Memorize Zoom level and center 
  observe({
    zoomLevel <<- input$mapThr_zoom
    zoomCenter <<- input$mapThr_center
  })
  
  
  # Redraw the raster :)
  observe({ 
    input$mapThr_zoom
    input$mapOpacityThr
    input$mapThrSelect
    
    
    validate(need((mamapAvailable==1 || mamapAvailable==0),""))
    # The console will trigger an error if the leaflet object called
    # Is not defined and this is sometimes the case as the map take less time to initalize then
    # the input trh, that's why we need to delay the execution
    # of the code for the first time you enter into the tab
    if(is.null(thrInput()) || is.null(thrQInput())){
      delay(200,{})
    }
    
    isolate({
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      if(!is.null(thrInput()) || !is.null(thrQInput())){
        if(input$mapThr_zoom == zoomin[1] || input$mapThr_zoom == zoomin[2] || input$mapThr_zoom == zoomin[3] || input$mapThr_zoom == zoomin[4] || input$mapThr_zoom == zoomin[5] || length(input$criteria) != 0 || input$mapOpacityThr){

          pal  <- c("#ff0000","#0000ff")
          palos <- colorBin(palette = pal, domain = seq(0,1), bins = 2, na.color = NA)
          
          # Redraw the map now
          if(input$mapThrSelect == 0){
            leafletProxy("mapThr") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "satellite") %>%
              clearImages() %>%
              addRasterImage(x = RasterforThreshold,colors = palos, opacity = input$mapOpacityThr/100,method="ngb")            }
          else if (input$mapThrSelect == 1){
            leafletProxy("mapThr") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileRoad",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "roadmap") %>%
              clearImages() %>%
              addRasterImage(x = RasterforThreshold,colors = palos, opacity = input$mapOpacityThr/100,method="ngb")            }
          else{
            leafletProxy("mapThr") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileTer",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "terrain") %>%
              clearImages() %>%
              addRasterImage(x = RasterforThreshold, colors = palos, opacity = input$mapOpacityThr/100,method="ngb")            }
        }
      }  
    })
  })
  
  # Event to toggle the legend
  observe({
        input$mapshowThr
        input$goButtonThr
        
        proxy <- leafletProxy("mapThr")
        colorszz <- c("#ff0000","#0000ff")
        # Remove any existing legend, and only if the legend is
        # enabled, create a new one.
        if (input$mapshowThr) {
          proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = colorszz, labels = c("below", "above"), opacity = input$mapOpacityThr/100)
        }
        else{
          proxy %>% clearControls()
        }
      })
  
observeEvent(input$goButtonThr, {
    #Statistics displayed on Thereshold tab
    output$stata  <- renderUI({   
      #If all weights sliders are on 0, display no text
      validate( need(weightsVector() != vector(mode = "numeric", length = length(HP[,1])+length(HN[,1])), ""))
      rst.comb.w <- calculmapmain()
      if(input$inputselect=="1"){
        projection(rst.comb.w) <- projection(areaselected)
        rst.comb.wm <- mask(rst.comb.w, areaselected)
      }else{
        rst.comb.wm <- rst.comb.w
      }
      somma <-nbpx_notNA(rst.comb.wm)

      # Calculus of the raster value which corresponds approximately to the quantile of the ratio 1-threshold/total area
      if(t=="0"){
        thr=as.numeric(cellStats(rst.comb.wm,stat=function(x,na.rm){quantile(x,probs=1,na.rm=T)}))}
      
      else if (t == "1"){
        thr=as.numeric(cellStats(rst.comb.wm,stat=function(x,na.rm){quantile(x,probs=1-thrInput()[1]/datamaxThreshold(),na.rm=T)}))
      } else if (t == "2"){
        thr=as.numeric(cellStats(rst.comb.wm,stat=function(x,na.rm){quantile(x,probs=1-thrQInput()[1]/100,na.rm=T)}))
      }
      
      #Calculus of the threshold raster
      rst.comb.thr2=binarythr(rst.comb.wm,thr,thrQInput()/100)
      if(input$inputselect=="1"){
        projection(rst.comb.thr2) <- projection(areaselected)
        rst.comb.thr2m <- mask(rst.comb.thr2, areaselected)
      }else{
        rst.comb.thr2m <- rst.comb.thr2
      }
      
      #Count the number of good pixels
      c <- cellStats(rst.comb.thr2m,sum)
      
      #Calculus of the number of bad pixels
      d <- somma -c
      
      #Percentage of c and d
      pc <-c/somma*100
      pd <-d/somma*100

      # Area of c and d
      Agpx <-(xres(rst.comb.thr2m)*yres(rst.comb.thr2m))/10000*c
      Abpx <-(xres(rst.comb.thr2m)*yres(rst.comb.thr2m))/10000*d
      
      # Chosen method
      if(t =="1"){
        m <<- "Area"
      } else {
        m <<- "Percentage"
      }
      
      #Creation of the stata div 
      tags$div(  id="stata",
                 tags$p("Method : ", m),
                 tags$p("Number of pixels above the Threshold: ",c),
                 tags$p("Percentage of good pixels : ",tags$b(round(pc, digits = 1)),"%"),
                 tags$p("Area of good pixels : ",tags$i(round(Agpx,digits = 2)),"ha"),
                 tags$p("Number of bad pixels : ",d),
                 tags$p("Percentage of bad pixels : ",tags$b(round(pd, digits = 1)),"%"),
                 tags$p("Area of bad pixels : ",tags$i(round(Abpx,digits = 2)),"ha")
      )
      
    })
  })
  
  
  
  ##############################    
  ##                          ##
  ##      Advanced Panel      ##
  ##                          ##
  ##############################
  
  
  
  
  ################################################################################################    
  #################################### Compare methods ###########################################
  ################################################################################################
  

  ##################################################
  ################## by values #####################
  ##################################################

  # Retrieval of the names of the methods to compare (Panel "comparison of methods") from input$checkGroupCM
  mthr=1
  
  methodscomparedname <- reactive({
    input$comparebutton
    input$goButtonAdvanced
    
    isolate({ 
      n <-array(0,3)
      if(length(input$checkGroupCM)==2){
        K=sapply(1:2, function(i){
          if(input$checkGroupCM[i]==2){n[i]<- "LWC2"}
          else if(input$checkGroupCM[i]==3){n[i]<- "Electre"}})
        n[1] <- paste0(K[1], "-", K[2])
        n[2:3]<- K[1:2]
      }
      else {n[] <-0}
      return(n)
    })   
  })		
  
  
  # Calculation of the raster for the two methods chosen and comparison of the two methods
  methodscomparedcalcul <- reactive({methodscomparedname()
    isolate({ 
      n<- methodscomparedname()
      if (n[1] != 0){
        rst.comb <-stack()
        Q= lapply(2:3, function(i){
          if(n[i]=="LWC2"){rst.comb.w<-lwc2(rst.pnl,weightsVector())}
          else if(n[i]=="Electre"){rst.comb.w=electre(rst.pnl, weightsVector())}
          rst.comb.w=normalizer(rst.comb.w)
          rst.comb <- addLayer(rst.comb, rst.comb.w)
        })
        rst.comb=stack(Q)
        # Absolute difference between the two layers
        comp <- abs(rst.comb[[1]] - rst.comb[[2]])
        rst.comb <- addLayer(rst.comb, comp)
        names(rst.comb) <- c(n[2],n[3],n[1])
      }
      return(rst.comb) 
    })    
  })	
  
  # Define input selection list of the layers which can be displayed in the panel "Compare methods":
  # The unique raster with the positive and negative weights vector for each method
  # The absolute difference between the two unique rasters
  output$renderSelectlayerCM <- renderUI({methodscomparedname()
    isolate({
      n<- methodscomparedname()
      if (n[1] != 0){
        selectInput("methodscomp", "Choose a layer to display:",choices=lapply(1:length(n), function(i) {n[i]}))
      }
    })
  })
  
  # Define input selection list of the thresholdlayers which can be displayed in the panel "Compare methods" 
  output$renderSelectlayerThrCM <- renderUI({methodscomparedname()
    isolate({
      n<- methodscomparedname()
      if (n[1] != 0){
        selectInput("thresholdcomp", "Choose a layer to display:", 
                    choices=lapply(1:length(n), function(i) {n[i]}))
      }
    })
  })  
  
  #Text above the map, depends on the layer to be displayed selected
  output$txtmapCM <- renderText({input$methodscomp
    n<- methodscomparedname()
    validate(need(n[1] !=0," "))
    if (is.null(input$methodscomp) == F){
      if (input$methodscomp =="LWC2" |input$methodscomp =="Electre"){
        txt<-"The best value is 1 (blue) and the worst 0 (red) for the project chosen Expansion considering these criteria, the weights selected and the chosen method"
      }
      else{
        txt<-"Absolute difference of the values obtained with the two methods chosen."
      }
      txt
    }
  })
  
  # Display the layer selected on input$methodscomp
  output$mapCM <- renderLeaflet({
    input$methodscomp
    
    if (is.null(input$methodscomp) == F){
      n<- methodscomparedname()
      validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), 
                    "Please put a positive weigth for at least one criterion or select new criteria (go back to select criteria tab)"))
      validate( need(n[1] !=0, "Please choose 2 methods"))
      withProgress({ 
        setProgress(message = "Calculating, please wait",detail = "This may take a few moments...")
        rst.comb <- methodscomparedcalcul()
        
        # Substitution of the dash (-) by a point to match the selected layer and the layer to display
        dash <- gsub("-",".",input$methodscomp)
        if (names(rst.comb)[1]== dash | names(rst.comb)[2]== dash | names(rst.comb)[3] == dash){
          layerdisplayed <- subset(rst.comb,dash)
          
          # The layer is cropped to match the area selected
          layerdisplayed <- crop(layerdisplayed, extent(areaselected))
          if(input$inputselect=="1"){
            projection(layerdisplayed) <- projection(areaselected)
            layerd <- mask(layerdisplayed, areaselected)
          }else{
            layerd <- layerdisplayed
            # layerd = image to display into a addRaster
          }
          RasterForCM <<- layerd
          # Definition of the colour palette  of the map for one methods
          pal <- brewer.pal(10,"RdYlBu")
          zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
          # The colors used on the map depends of the layer chosen to be displayed
          if (input$methodscomp =="LWC2" |input$methodscomp =="Electre"){
            
          
            palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)          
            
            leaflet()%>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "satellite") %>%
              setView(lng = zoomCenter[[1]],
                      lat = zoomCenter[[2]],
                      zoom = zoomLevel) %>%
              addRasterImage(x = layerd,colors = palos,opacity = input$mapOpacityCM/100, method="ngb") 
             
          }
          else{
            
            palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA, reverse = TRUE)
          
            leaflet()%>%
            addGoogleTiles(layerId = "googleTileSat",
                            options = tileOptions(maxZoom = 15,
                                                 updateWhenZooming = TRUE),
                             type = "satellite") %>%
              setView(lng = zoomCenter[[1]],
                      lat = zoomCenter[[2]],
                      zoom = zoomLevel) %>%
              addRasterImage(x = layerd,colors = palos,opacity = input$mapOpacityCM/100, method = "ngb")
              
          }
        }
      },session=session)
    }
  })
  
  # Memorize Zoom level and center for method compare 
  observe({
    zoomLevel <<- input$mapCM_zoom
    zoomCenter <<- input$mapCM_center
  }) 
  
  # Update the mapCM
  observe({ 
    input$mapOpacityCM
    input$mapCM_zoom
    input$mapCMSelect
    
    validate(need(!is.null(input$methodscomp),""))
    
    isolate({
      pal <- brewer.pal(10,"RdYlBu")
      palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA,reverse = T)
      
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      
      if(input$goButtonAdvanced > 0 ){
        if(input$mapCM_zoom == zoomin[1] || input$mapCM_zoom == zoomin[2] || input$mapCM_zoom == zoomin[3] || input$mapCM_zoom == zoomin[4] || input$mapCM_zoom == zoomin[5] || input$mapOpacitymain){
          
          rst.comb <- methodscomparedcalcul()
          
          # Substitution of the dash (-) by a point to match the selected layer and the layer to display
          dash <- gsub("-",".",input$methodscomp)
          if (names(rst.comb)[1]== dash | names(rst.comb)[2]== dash | names(rst.comb)[3] == dash){
            layerdisplayed <- subset(rst.comb,dash)
            
            # The layer is cropped to match the area selected
            layerdisplayed <- crop(layerdisplayed, extent(areaselected))
            if(input$inputselect=="1"){
              projection(layerdisplayed) <- projection(areaselected)
              layerd <- mask(layerdisplayed, areaselected)
            }else{
              layerd <- layerdisplayed
              # layerd = image to display into a addRaster
            }
            
            # Redraw the map now
            if(input$mapCMSelect == 0){
              leafletProxy("mapCM") %>%
                clearTiles() %>%
                addGoogleTiles(layerId = "googleTileSat",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "satellite") %>%
                clearImages() %>%
                addRasterImage(x = RasterForCM,colors = palos,opacity = (input$mapOpacityCM)/100, method = "ngb")
            }
            else if (input$mapCMSelect == 1){
              leafletProxy("mapCM") %>%
                clearTiles() %>%
                addGoogleTiles(layerId = "googleTileRoad",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "roadmap") %>%
                clearImages() %>%
                addRasterImage(x = RasterForCM,colors = palos,opacity = (input$mapOpacityCM)/100, method = "ngb")
            }
            else{
              leafletProxy("mapCM") %>%
                clearTiles() %>%
                addGoogleTiles(layerId = "googleTileTer",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "terrain") %>%
                clearImages() %>%
                addRasterImage(x = RasterForCM,colors = palos,opacity = (input$mapOpacityCM)/100, method = "ngb")
            }
          }
        }
      }
    })
  })
  
  # Show the legend
  observe({
    input$mapshowCM
    input$methodscomp
    
    validate(need(!is.null(input$methodscomp),""))
    
    pal <- brewer.pal(10,"RdYlBu")
    proxy <- leafletProxy("mapCM")
    
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    if (input$mapshowCM) {
      if (input$methodscomp =="LWC2" |input$methodscomp =="Electre"){
          proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = pal, labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = input$mapOpacityCM/100)
      }
      else {
        proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = rev(pal), labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = input$mapOpacityCM/100)
        
      }
    }
    else{
      proxy %>% clearControls()
    }
  })

  
  # Define the scatter plot of the two methods chosen
  output$scatter <- renderPlot({ methodscomparedname()
    n<- methodscomparedname()
    validate( need(weightsVector() != vector(mode = "numeric", length = length(HP[,1])+length(HN[,1])), "Please put a positive weigth for at least one criterion (go back to select weights tab) or select new criteria (go back to select criteria tab)"))
    validate( need(n[1] !=0, "Please choose 2 methods"))
    withProgress({ 
      setProgress(message = "Calculating, please wait",detail = "This may take a few moments...")
      rst.comb <- methodscomparedcalcul()# The layer is cropped to match the area selected
      rst.comb <- crop(rst.comb, extent(areaselected))
      if(input$inputselect=="1"){
        projection(rst.comb) <- projection(areaselected)
        rst.combm <- mask(rst.comb, areaselected)
      } else {
        rst.combm <- rst.comb
      }
      layer1 <- subset(rst.combm,n[2])
      layer2 <- subset(rst.combm,n[3])
      plot(na.omit(layer1), na.omit(layer2), xlab=n[2], ylab=n[3])
    },session=session)
  })
  
  
  ############################################
  ############ by threshold ##################
  ############################################
  
  # Define threshold input as the value of the slider "Threshold" in the panel "compare methods"
  thrInputCM <- reactive({ input$thrCM | input$thrCMQ
    if(mthr==1){
      y<-input$thrCM
    }else if (mthr==0){
      y <-input$thrCMQ
    }
    return(y)      
  })
  
  
  # Observe which threshold you use
  observeEvent(input$thrCM, {
    mthr <<- 1
  })
  observeEvent(input$thrCMQ, {
    mthr<<- 0
  })
  
  # Display the threshold sliderinput in Compare methods
  output$renderThrCM <- renderUI({
    sliderInput(inputId="thrCM", label = "Threshold per ha",min = 0, max =datamaxThreshold(), post=" ha", value = (50/100)*datamaxThreshold())
  })
  output$renderThrCMQ <- renderUI({
    sliderInput(inputId="thrCMQ", label = "Threshold per %",min = 0, max =100, post=" %", value = 50)
  })
  
  valCM <- reactive({input$thrCM}) #x
  valCMQ <- reactive({input$thrCMQ}) #y
  
  #Define the wanted threshold (area or quantile)
  observeEvent(input$thrCM,{
    t <<- "1"
    y <- as.integer((valCM()*100)/datamaxThreshold())
    updateSliderInput(session, "thrCMQ", value = y, step = 0.01)
  })
  
  observeEvent(input$thrCMQ,{
    t <<- "2"
    x <- as.integer(valCMQ()*(datamaxThreshold()/100))
    updateSliderInput(session, "thrCM", value = x, step = 0.01)
  })
  ## End of the "two sliders reactivity"
  
  # Display the threshold map of the layer selected in input$thresholdcomp
  output$mapThrCM <- renderLeaflet({
    input$thresholdcomp
    input$thrCMQ
    
    isolate({
      if (is.null(input$thresholdcomp) == F){
        n<- methodscomparedname()
        # If all weights sliders are on 0, warns the user
        validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), 
                      "Please put a positive weigth for at least one criterion or select new criteria (go back to select criteria tab)"))
        
        # Define progress message to warn the user to wait during the calculus 
        withProgress({ 
          setProgress(message = "Calculating, please wait",detail = "This may take a few moments...")
          
          rst.comb <- methodscomparedcalcul()
          dash <- gsub("-",".",input$thresholdcomp)
          rst.comb.thr <- stack()
          if (names(rst.comb)[1]== dash | names(rst.comb)[2]== dash | names(rst.comb)[3] == dash){
            Q=lapply(2:3, function(i){
              rst.comb.thr.w <- subset(rst.comb, n[i])
              if(input$inputselect=="1"){
                projection(rst.comb.thr.w) <- projection(areaselected)
                rst.comb.thr.w <- mask(rst.comb.thr.w, areaselected)
              }
              
              
              area <- maxThrCalculation(rst.comb.thr.w)
              # Calculus of the raster value which corresponds approximately to the quantile of the ratio 1-threshold/total area
              if (mthr==0){
                thr=as.numeric(cellStats(rst.comb.thr.w,stat=function(x,na.rm){quantile(x,probs=1-thrInputCM()[1]/100,na.rm=T)}))
              }
              else if (mthr==1){
                thr=as.numeric(cellStats(rst.comb.thr.w,stat=function(x,na.rm){quantile(x,probs=1-thrInputCM()[1]/area,na.rm=T)}))
              }
              
              
              # calculus of the threshold raster
              rst.comb.thr.w=calcthr(rst.comb.thr.w,thr)
              rst.comb.thr <- addLayer(rst.comb.thr, rst.comb.thr.w)
            })
            
            rst.comb.thr<-stack(Q)
            comp <- overlay(rst.comb.thr$layer.1,rst.comb.thr$layer.2, fun=function(x,y){ifelse(x>=0 & y>=0,10,ifelse(x<0 & y<0,-10, ifelse(x>=0 & y<0,-5,5)))})
            
            rst.comb.thr <- addLayer(rst.comb.thr, comp)
            names(rst.comb.thr) <- c(n[2],n[3],n[1])
            
            layerdisplayed <- subset(rst.comb.thr,dash)
            # The layer is cropped to match the area selected
            layerdisplayed <- crop(layerdisplayed, extent(areaselected))
            if(input$inputselect=="1"){
              projection(layerdisplayed) <- projection(areaselected)
              layerd <- mask(layerdisplayed, areaselected)
            }else{
              layerd <- layerdisplayed
            }
            RasterForThrCM <<- layerd
            nbgoodpx <<- length(layerd[layerd==10])
            px <<- sum(is.na(layerd[1:ncell(layerd)])==FALSE)
            if (input$thresholdcomp =="LWC2" |input$thresholdcomp =="Electre"){
             
              pal  <- c("#ff0000","#0000ff")
              palos <- colorBin(palette = pal, domain=c(-10,0,10), bins = c(-10,0,10),na.color = NA )
              leaflet()%>%
                addGoogleTiles(layerId = "googleTileSat",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "satellite") %>%
                setView(lng = zoomCenter[[1]],
                        lat = zoomCenter[[2]],
                        zoom = zoomLevel) %>%
                addRasterImage(x = layerd,colors = palos,opacity = input$mapOpacityThrCM/100, method = "ngb") 
                }
            else {
              # Definition of the colour palette  of the future map
              pal  <- brewer.pal(4,"RdYlBu")
              palos <- colorBin(palette = pal, domain=c(-10,-5,0,5,10), bins = c(-10,-5,0,5,10),na.color = NA)
              
              leaflet()%>%
                addGoogleTiles(layerId = "googleTileSat",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "satellite") %>%
                setView(lng = zoomCenter[[1]],
                        lat = zoomCenter[[2]],
                        zoom = zoomLevel) %>%
                addRasterImage(x = layerd,colors = palos,opacity = input$mapOpacityThrCM/100, method = "ngb") 
                
            }
          }
        }, session=session)
      }
    })
  })
  
  # Memorize Zoom level and center for method compare 
  observe({
    zoomLevel <<- input$mapThrCM_zoom
    zoomCenter <<- input$mapThrCM_center
  }) 
  
  
  observe({ 
    input$mapThrCM_zoom
    input$mapOpacityThrCM
    input$mapThrCMSelect
    input$thresholdcomp
    input$thrCMQ
    
    validate(need(!is.null(input$thresholdcomp),""))
    
    isolate({
      if (input$thresholdcomp =="LWC2" |input$thresholdcomp =="Electre"){
        pal  <- c("#ff0000","#0000ff")
        palos <- colorBin(palette = pal, domain=c(-10,0,10), bins = c(-10,0,10),na.color = NA)
        
      } else {
         pal <- brewer.pal(4,"RdYlBu")
         palos <- colorBin(palette = pal, domain=c(-10,-5,0,5,10), bins = c(-10,-5,0,5,10),na.color = NA)
         
      }
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      
      if(input$goButtonAdvanced > 0 ){
        if(input$mapThrCM_zoom == zoomin[1] || input$mapThrCM_zoom == zoomin[2] || input$mapThrCM_zoom == zoomin[3] || input$mapThrCM_zoom == zoomin[4] || input$mapThrCM_zoom == zoomin[5] || input$mapOpacityThrCM){
          
       
            # Redraw the map now
            if(input$mapThrCMSelect == 0){
              leafletProxy("mapThrCM") %>%
                clearTiles() %>%
                addGoogleTiles(layerId = "googleTileSat",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "satellite") %>%
                clearImages() %>%
                addRasterImage(x = RasterForThrCM,colors = palos,opacity = (input$mapOpacityThrCM)/100, method = "ngb")
            }
            else if (input$mapThrCMSelect == 1){
              leafletProxy("mapThrCM") %>%
                clearTiles() %>%
                addGoogleTiles(layerId = "googleTileRoad",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "roadmap") %>%
                clearImages() %>%
                addRasterImage(x = RasterForThrCM,colors = palos,opacity = (input$mapOpacityThrCM)/100, method = "ngb")
            }
            else{
              leafletProxy("mapThrCM") %>%
                clearTiles() %>%
                addGoogleTiles(layerId = "googleTileTer",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "terrain") %>%
                clearImages() %>%
                addRasterImage(x = RasterForThrCM,colors = palos,opacity = (input$mapOpacityThrCM)/100, method = "ngb")
            }
          }
        }
      
    })
  })
  
  # Show the legend
  observe({
    input$mapshowThrCM
    input$thresholdcomp
    input$thrCMQ
    
    validate(need(!is.null(input$thresholdcomp),""))
    
    proxy <- leafletProxy("mapThrCM")
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    if (input$mapshowThrCM) {
      if (input$thresholdcomp =="LWC2" |input$thresholdcomp =="Electre"){
        pal  <- c("#ff0000","#0000ff")
        proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = pal, labels = c("Unacceptable pixels", "Acceptable pixels"), opacity = input$mapOpacityThrCM/100)
      }
      else {
        pal <- brewer.pal(4,"RdYlBu")
        proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = pal, labels = c("Unacceptable pixels","Acceptable for the first method","Acceptable for the second method", "Acceptable pixels"), opacity = input$mapOpacityThrCM/100)
      }
    }
    else{
      proxy %>% clearControls()
    }
  })
  
  # Calculus used when the user want to compare the threshold between two methods
  # It calculate the area above the threshold for both methods selected
  areaboth <- reactive({
    thrInputCM()
    areagoodpx <- (xres(rst.pnl[[1]])*yres(rst.pnl[[1]]))/10000* nbgoodpx
    return(areagoodpx)
  })
  percentageboth <- reactive({thrInputCM()
    percentgoodpx <- 100*nbgoodpx/px
    return(percentgoodpx)
  })
  
  # Displayed the number obtained with areabothA ou areabothQ
  output$txtgoodarea <- renderText({
    n<- methodscomparedname()
    validate(need((weightsVector() != vector(mode = "numeric", length = length(HP[,1])+length(HN[,1])) & n[1] !=0 & input$thresholdcomp !="LWC" & input$thresholdcomp !="LWC2" & input$thresholdcomp !="Electre")," "))
    if (mthr==1){
      paste0("Areas above threshold for both methods : ",areaboth()," ha")
    }else if (mthr==0){
      paste0("Areas above threshold for both methods : ",percentageboth()," %")
    }
  })
  
  
  ##############################################################################
  ############################ Stats ###########################################
  ##############################################################################
  
  # Create a matrix which contains the positive criteria weights
  output$matrixCurrentWeightsP <- renderTable({input$Project
    weightsVector()  								
    validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), 
                  "Please put a positive weigth for at least one criterion or select new criteria (go back to select criteria tab)"))
    
    
    #Transform the row data in understandable text
    descript <-sapply(HPN[,4], toString)
    descript <- gsub("_", " ", descript)
    
    matrix <- cbind(descript,weightsVector())
    colnames(matrix) <- c('Criteria','Weight')
    matrix               
  })
  
  output$christomatLWC2 <- renderTable({
    input$Project
    input$saveButton  
    #If no weights are saved, warns the user
    validate( need(temp_WeightssavedSize("LWC2") > 0, 
                   "Please save criteria weights with method LWC2 (go back to select weights tab)"))
    matrix_Stats(readWeightssaved("LWC2","temp"))
  })
  
  output$christomatElectre <- renderTable({
    input$Project
    input$saveButton  
    #If no weights are saved, warns the user
    validate( need(temp_WeightssavedSize("Electre") > 0, 
                   "Please save criteria weights with method Electre (go back to select weights tab)"))
    matrix_Stats(readWeightssaved("Electre", "temp"))
  })
  
  output$downloadCWH_LWC2 <- downloadHandler( 
    filename = function(){return("WeightsLWC2.csv")},
    content = function(file) { write.csv(readWeightssaved("LWC2","temp"), file)}
  )
  
  output$downloadCWH_Electre <- downloadHandler( 
    filename = function(){return("WeightsElectre.csv")},
    content = function(file) {write.csv(readWeightssaved("Electre","temp"), file)}
  )
  
  #######################################################################################################################
  ##################################### Average #########################################################################					  
  #######################################################################################################################
  # Display the map
  output$mapAV_LWC2 <- renderLeaflet({  
    input$Project
    input$saveButton
    #If no weights are saved, warns the user
    validate( need(temp_WeightssavedSize("LWC2") > 0, "Please save criteria weights with method LWC2 (go back to select weights tab)"))
    isolate({
    #Creation progress warning
    withProgress({ 
      setProgress(message = "Calculating, please wait",detail = "This may take a few moments...")
      vec <- weightAverage(readWeightssaved("LWC2", "temp"))
      
      #Create the corresponding map in an iframe
      rst.comb.w=lwc2(rst.pnl, vec)
      rst.comb.w2=normalizer(rst.comb.w)
      # The layer is cropped to match the area selected
      rst.comb.w2 <- crop(rst.comb.w2, extent(areaselected))
      if(input$inputselect=="1"){
        projection(rst.comb.w2) <- projection(areaselected)
        rst.comb.w2m <- mask(rst.comb.w2, areaselected)
      }else{
        rst.comb.w2m <- rst.comb.w2
      }
      rasterAverageLWC <<- rst.comb.w2m

      pal <- brewer.pal(10,"RdYlBu")
      palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)
      
      leaflet()%>%
        addGoogleTiles(layerId = "googleTileSat",
                       options = tileOptions(maxZoom = 15,
                                             updateWhenZooming = TRUE),
                       type = "satellite") %>%
        setView(lng = zoomCenter[[1]],
                lat = zoomCenter[[2]],
                zoom = zoomLevel) %>%
        addRasterImage(x = rst.comb.w2m,colors = palos,opacity = isolate(input$mapOpacityAv)/100, method = "ngb") %>%
        addLegend(position = "bottomright",colors = pal,labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = isolate(input$mapOpacityAv)/100)
      
    }, session=session)
    })
  })
  
  # Update the map with everything
  observe({
    input$mapAV_LWC2_zoom
    input$mapOpacityAv
    input$mapAvSelect
    input$saveButton
    
    isolate({
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      
      if(input$goButtonAdvanced > 0 ){
        if(input$mapAV_LWC2_zoom == zoomin[1] || input$mapAV_LWC2_zoom == zoomin[2] || input$mapAV_LWC2_zoom == zoomin[3] || input$mapAV_LWC2_zoom == zoomin[4] || input$mapAV_LWC2_zoom == zoomin[5] || input$mapOpacitymain){
          #Create the corresponding map in an iframe
         
          pal <- brewer.pal(10,"RdYlBu")
          palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)
          # Redraw the map now
          if(input$mapAvSelect == 0){
            leafletProxy("mapAV_LWC2") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "satellite") %>%
              clearImages() %>%
              addRasterImage(x = rasterAverageLWC,colors = palos,opacity = input$mapOpacityAv/100, method = "ngb")
          }
          else if (input$mapAvSelect == 1){
            leafletProxy("mapAV_LWC2") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileRoad",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "roadmap") %>%
              clearImages() %>%
              addRasterImage(x = rasterAverageLWC,colors = palos,opacity = input$mapOpacityAv/100, method= "ngb")
          }
          else{
            leafletProxy("mapAV_LWC2") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileTer",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "terrain") %>%
              clearImages() %>%
              addRasterImage(x = rasterAverageLWC,colors = palos,opacity = input$mapOpacityAv/100, method = "ngb")
          }
          
        }
      }
    })
  })
  
  # Show the legend
  observeEvent(input$mapshowAv,{
    proxy <- leafletProxy("mapAV_LWC2")
    pal <-brewer.pal(10,"RdYlBu")
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    if (input$mapshowAv) {
      proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = pal,labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = input$mapOpacityAv/100)
    }
    else{
      proxy %>% clearControls()
    }
  })
  #table of weights for LWC
  output$tabAV_LWC2 <- renderTable({input$Project
    input$saveButton  
    #If no weights are saved, warns the user
    validate( need(temp_WeightssavedSize("LWC2") > 0, ""))
    vec <- weightAverage(readWeightssaved("LWC2", "temp"))
    weightArray(vec)
  })
  
  # Display the Electre map 
  output$mapAV_Electre <- renderLeaflet({ 
    input$Project
    input$saveButton 
    
    #If no weights are saved, warns the user
    validate( need(temp_WeightssavedSize("Electre") > 0, "Please save criteria weights with method Electre (go back to select weights tab)"))
    
    #Creation progress warning
    withProgress({ 
      setProgress(message = "Calculating, please wait",detail = "This may take a few moments...")
      vec <- weightAverage(readWeightssaved("Electre", "temp"))
      
      #Create the corresponding map in an iframe
      rst.comb.w=electre(rst.pnl, vec)
      rst.comb.w2=normalizer(rst.comb.w)
      # The layer is cropped to match the area selected
      rst.comb.w2 <- crop(rst.comb.w2, extent(areaselected))
      if(input$inputselect=="1"){
        projection(rst.comb.w2) <- projection(areaselected)
        rst.comb.w2m <- mask(rst.comb.w2, areaselected)
      }else{
        rst.comb.w2m <- rst.comb.w2
      }
      
      rasterAverageElectre <<- rst.comb.w2m
      
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      pal <- brewer.pal(10,"RdYlBu")
      palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)      
      leaflet()%>%
        addGoogleTiles(layerId = "googleTileSat",
                       options = tileOptions(maxZoom = 15,
                                             updateWhenZooming = TRUE),
                       type = "satellite") %>%
        setView(lng = zoomCenter[[1]],
                lat = zoomCenter[[2]],
                zoom = zoomLevel) %>%
        addRasterImage(x = rst.comb.w2m,colors = palos,opacity = isolate(input$mapOpacityAv2)/100, method="ngb") %>%
        addLegend(position = "bottomright",colors = pal,labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity =  isolate(input$mapOpacityAv2)/100)
    }, session=session)
  })
  
  
  # Update the Electre Map
  observe({ 
    input$mapAV_Electre_zoom
    input$mapOpacityAv2
    input$mapAvSelect2
    input$saveButton 
    
    
    isolate({
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      
      if(input$goButtonAdvanced > 0 ){
        if(input$mapAV_Electre_zoom == zoomin[1] || input$mapAV_Electre_zoom == zoomin[2] || input$mapAV_Electre_zoom == zoomin[3] || input$mapAV_Electre_zoom == zoomin[4] || input$mapAV_Electre_zoom == zoomin[5] || input$mapOpacitymain){
     
          zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
          pal <- brewer.pal(10,"RdYlBu")
          palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)
          # Redraw the map now
          if(input$mapAvSelect2 == 0){
            leafletProxy("mapAV_Electre") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "satellite") %>%
              clearImages() %>%
              addRasterImage(x = rasterAverageElectre,colors = palos, opacity = input$mapOpacityAv2/100, method = "ngb")
          }
          else if (input$mapAvSelect2 == 1){
            leafletProxy("mapAV_Electre") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileRoad",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "roadmap") %>%
              clearImages() %>%
              addRasterImage(x = rasterAverageElectre,colors = palos, opacity = input$mapOpacityAv2/100, method = "ngb")
          }
          else{
            leafletProxy("mapAV_Electre") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileTer",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "terrain") %>%
              clearImages() %>%
              addRasterImage(x = rasterAverageElectre,colors = palos, opacity = input$mapOpacityAv2/100, method = "ngb")
          }
          
        }
      }
    })
  })
  
  # Show the legend
  observeEvent(input$mapshowAv2,{
    proxy <- leafletProxy("mapAV_Electre")
    pal <- brewer.pal(10,"RdYlBu")
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    if (input$mapshowAv2) {
      proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = pal,labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = input$mapOpacityAv2/100)
    }
    else{
      proxy %>% clearControls()
    }
  })
  
  output$tabAV_Electre <- renderTable({ input$Project
    input$saveButton 
    #If no weights are saved, warns the user
    validate( need(temp_WeightssavedSize("Electre") > 0, ""))
    vec <- weightAverage(readWeightssaved("Electre", "temp"))
    weightArray(vec)
  })

  
  ##############################################################################################################  
  ############################################ Consensus #######################################################
  ##############################################################################################################
  
  # This is the renderLeaflet code for the 'LWC2/LWC' map 
  # I've figure out that the code is pretty much the 
  # same as the one for Average map and Weigth adv
  # while reading the previous code of the last student
  output$mapCON_LWC2 <- renderLeaflet({  
    input$Project
    input$savePermanentlyButton
    
    #If no weights are saved, warns the user
    validate(need(((input$methodsmain==2 | input$bothMethods==1)&input$savePermanentlyButton>0 & !is.na(readWeightssaved("LWC2")) & !is.null(readWeightssaved("LWC2"))) , 
                   "Please save permanently weights with method LWC2 (go back to select weights tab)"))
    
    isolate({
    #Creation progress warning
    withProgress({ 
      setProgress(message = "Calculating, please wait",
                  detail = "This may take a few moments...")
      vec <- weightAverage(readWeightssaved("LWC2"))
      
      #Create the map with the consensus vector
      rst.comb.w<-lwc2(rst.pnl, vec)
      rst.comb.w2<-normalizer(rst.comb.w)
      # The layer is cropped to match the area selected
      rst.comb.w2 <- crop(rst.comb.w2, extent(areaselected))
      if(input$inputselect=="1"){
        projection(rst.comb.w2) <- projection(areaselected)
        rst.comb.w2m <- mask(rst.comb.w2, areaselected)
      }else{
        rst.comb.w2m <- rst.comb.w2
      }
      
      rasterConsensusLWC <<- rst.comb.w2m
      
      #Define zoom for map 
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      
      #Creation of color palette used in map 
      pal <- brewer.pal(10,"RdYlBu")
      palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)
      
      #Display the map
      leaflet()%>%
        addGoogleTiles(layerId = "googleTileSat",
                       options = tileOptions(maxZoom = 15,
                                             updateWhenZooming = TRUE),
                       type = "satellite") %>%
        setView(lng = zoomCenter[[1]],
                lat = zoomCenter[[2]],
                zoom = zoomLevel) %>%
        addRasterImage(x = rst.comb.w2m,colors = palos,opacity = isolate(input$mapOpacityCON)/100, method= "ngb") %>%
        addLegend(position = "bottomright", colors = pal, labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = isolate(input$mapOpacityCON)/100)
      
    }, session=session)
  })
  })
  
  # Update the map
  observe({ 
    input$mapCON_LWC2_zoom
    input$mapOpacityCON
    input$mapCONSelect

    isolate({
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      if(input$goButtonAdvanced > 0 ){
        if(input$mapCON_LWC2_zoom == zoomin[1] || input$mapCON_LWC2_zoom == zoomin[2] || input$mapCON_LWC2_zoom == zoomin[3] || input$mapCON_LWC2_zoom == zoomin[4] || input$mapCON_LWC2_zoom == zoomin[5] || input$mapOpacitymain){

          pal <- brewer.pal(10,"RdYlBu")
          palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)
          
          # Redraw the map now
          if(input$mapCONSelect == 0){
            leafletProxy("mapCON_LWC2") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "satellite") %>%
              clearImages() %>%
              addRasterImage(x = rasterConsensusLWC,colors = palos,opacity = input$mapOpacityCON/100, method = "ngb")
          }
          else if (input$mapCONSelect == 1){
            leafletProxy("mapCON_LWC2") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileRoad",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "roadmap") %>%
              clearImages() %>%
              addRasterImage(x = rasterConsensusLWC,colors = palos,opacity = input$mapOpacityCON/100, method = "ngb")
          }
          else{
            leafletProxy("mapCON_LWC2") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileTer",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "terrain") %>%
              clearImages() %>%
              addRasterImage(x = rasterConsensusLWC,colors = palos,opacity = input$mapOpacityCON/100, method = "ngb")
          }
          
        }
      }
    })
  })
  
  # Legend for the map
  observeEvent(input$mapshowCON,{
    proxy <- leafletProxy("mapCON_LWC2")
    pal  <-brewer.pal(10,"RdYlBu")
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    if (input$mapshowCON) {
      proxy %>% clearControls() %>% addLegend(position = "bottomright", colors = pal, labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = input$mapOpacityCON/100)
    }
    else{
      proxy %>% clearControls()
    }
  })
  
  # This is the vector which represent the 'weigth vector' 
  # used for the current analysis by the user 
  output$tabCON_LWC2 <- renderTable({input$Project
    input$savePermanentlyButton  
    #If no weights are saved, warns the user
    validate( need((!is.na(readWeightssaved("LWC2")) & !is.null(readWeightssaved("LWC2"))), ""))
    vec <- weightAverage(readWeightssaved("LWC2"))
    weightArray(vec)
  })
  
  ############### This is the renderLeaflet code for the 'Electre' map 
  output$mapCON_Electre <- renderLeaflet({  
    input$Project
    input$savePermanentlyButton
    
    #If no weights are saved, warns the user
    validate( need(((input$methodsmain==3 | input$bothMethods==1)&input$savePermanentlyButton>0 & !is.na(readWeightssaved("Electre")) & !is.null(readWeightssaved("Electre"))),
                   "Please save permanently criteria weights with method Electre (go back to select weights tab)"))
    #Creation progress warning
    isolate({
    withProgress({ 
      setProgress(message = "Calculating, please wait",
                  detail = "This may take a few moments...")
      vec <- weightAverage(readWeightssaved("Electre"))
      #Create the map with the consensus vector
      rst.comb.w <- electre(rst.pnl, vec)
      rst.comb.w2 <-normalizer(rst.comb.w)
      # The layer is cropped to match the area selected
      rst.comb.w2 <- crop(rst.comb.w2, extent(areaselected))
      if(input$inputselect=="1"){
        projection(rst.comb.w2) <- projection(areaselected)
        rst.comb.w2m <<- mask(rst.comb.w2, areaselected)
      }else{
        rst.comb.w2m <<- rst.comb.w2
      }
      rasterConsensusElectre <<- rst.comb.w2m
      
      pal <- brewer.pal(10,"RdYlBu")
      palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)
      
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      
      leaflet()%>%
        addGoogleTiles(layerId = "googleTileSat",
                       options = tileOptions(maxZoom = 15,
                                             updateWhenZooming = TRUE),
                       type = "satellite") %>%
        setView(lng = zoomCenter[[1]],
                lat = zoomCenter[[2]],
                zoom = zoomLevel) %>%
        addRasterImage(x = rst.comb.w2m,colors = palos,opacity = isolate(input$mapOpacityCON2)/100, method = "ngb") %>%
        addLegend(position = "bottomright",colors = pal,
                  labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = isolate(input$mapOpacityCON2)/100)
      
    }, session=session)
  })
  })
  
  # Update the map
  observe({ 
    input$mapCON_Electre_zoom
    input$mapOpacityCON2
    input$mapCONSelect2
    
    isolate({
      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
      if(input$goButtonAdvanced > 0 ){
        if(input$mapCON_Electre_zoom == zoomin[1] || input$mapCON_Electre_zoom == zoomin[2] || input$mapCON_Electre_zoom == zoomin[3] || input$mapCON_Electre_zoom == zoomin[4] || input$mapCON_Electre_zoom == zoomin[5] || input$mapOpacitymain){

          #Create the map with the consensus vector
          pal <- brewer.pal(10,"RdYlBu")
          palos <- colorBin(palette = pal, domain=seq(0,1,.1), bins=seq(0,1,.1),na.color = NA)
          # Redraw the map now
          if(input$mapCONSelect2 == 0){
            leafletProxy("mapCON_Electre") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "satellite") %>%
              clearImages() %>%
              addRasterImage(x = rasterConsensusElectre,colors = palos,opacity = input$mapOpacityCON2/100, method = "ngb")
          }
          else if (input$mapCONSelect2 == 1){
            leafletProxy("mapCON_Electre") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileRoad",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "roadmap") %>%
              clearImages() %>%
              addRasterImage(x = rasterConsensusElectre,colors = palos,opacity = input$mapOpacityCON2/100, method = "ngb")
          }
          else{
            leafletProxy("mapCON_Electre") %>%
              clearTiles() %>%
              addGoogleTiles(layerId = "googleTileTer",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "terrain") %>%
              clearImages() %>%
              addRasterImage(x = rasterConsensusElectre,colors = palos,opacity = input$mapOpacityCON2/100, method = "ngb")
          }
          
        }
      }
    })
  })
  
  # Legend for the map
  observeEvent(input$mapshowCON2,{
    proxy <- leafletProxy("mapCON_Electre")
    pal  <-brewer.pal(10,"RdYlBu")
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    if (input$mapshowCON2) {
      proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = pal,
                          labels =c("0","0.10","0.20","0.30","0.40","0.50","0.60","0.70","0.80","0.90"), opacity = input$mapOpacityCON2/100)
    }
    else{
      proxy %>% clearControls()
    }
  })
  
  output$tabCON_Electre <- renderTable({input$Project
    input$savePermanentlyButton
    #If no weights are saved, warns the user
    validate( need((!is.na(readWeightssaved("Electre")) & !is.null(readWeightssaved("Electre"))), ""))
    vec <- weightAverage(readWeightssaved("Electre"))
    weightArray(vec)
  })
  
  
  

  
  
  ####################################################################################################################  
  #################################### Compare projects ##############################################################
  ####################################################################################################################  
  
  mthr <<-0
  #'
  #' # List the rasters stored in rastersaved and create a checkboxGroupInput with these rasters
 # output$selectrastersaved <- renderUI({input$saveRasterButton
#    input$plot_brush
#    validate( need(nlayers(rastersaved) >0, "Please save rasters (go back to the select weigths tab"))
#    keys <- names(rastersaved)
#    mylist <- vector(mode="list", length=length(keys))
#    
#    mylist= lapply(1:length(keys), function(i){
#      key <- names(rastersaved)[i]
#      mylist[[key]] <- names(rastersaved)[i]
#    })
#    names(mylist) <- keys
#    checkboxGroupInput("checkGroupCPrRaster",
#                       label = h3("Choose two or more rasters"),
#                       choices = mylist,
#                       selected = NULL)
#  })
#  
#  # Store the rasters chosen by the users (selected in input$checkGroupCPrRaster) to be compared
#  rasterscompared <- reactive({input$CPrbutton
#    isolate({ 
#      rasterscomp<-stack()
#      if(length(input$checkGroupCPrRaster)>1){
#        Q = lapply(1:length(input$checkGroupCPrRaster), function(i){
#          layer <- subset(rastersaved, input$checkGroupCPrRaster[i])
 ##         if(input$inputselect=="1"){
#            projection(layer) <- projection(areaselected)
#            layerd <- mask(layer, areaselected)
#          }
#          rasterscomp <- addLayer(rasterscomp, layer)
#        })
#        rasterscomp<-stack(Q)
#      }
##      else {rasterscomp<-stack()}
#      return(rasterscomp) 
#    })   
#  })	
  
#  # Define input selection list of the layers which can be displayed  
#  output$selectlayerCPrcompared <- renderUI({rasterscompared()
#    n<- names(rasterscompared())
#    validate( need(length(n) > 1, " "))
#    isolate({
#      selectInput("Prnamethr", "Choose a layer:",choices=lapply(1:length(n), function(i) {n[i]}))
#    })
#  })
##  
#  # Calculus of the best project for each pixel of the area selected
#  bestproject <- reactive({rasterscompared()
#    isolate({ 
#      rastercomp <- rasterscompared()
#      bestPr <-which.max(rastercomp)
#      bestPr <- ratify(x=bestPr,att=NULL,append.names=TRUE)
#      #rat <- levels(bestPr)[[1]]
#      #rat$legend <- t(t(names(rastercomp)))
#      #levels(bestPr) <- rat
#      return(bestPr) 
#    })   
#  })		
#  
#  # Creation of the raster corresponding to the best pixels of the project selected in input$Prnamethr 
#  rasterwithBestPixels <- reactive({input$Prnamethr
#    validate(need(length(input$Prnamethr) !=0, " "))
##    isolate({ 
#      rasterscomp <-rasterscompared()
#      n <- names(rasterscompared())
#      i <- match(input$Prnamethr,n)
#      layerthr <- subset(rasterscompared(),input$Prnamethr)
#      rst <- overlay(layerthr,bestproject(),fun=function(x,y){ifelse(y == i,x,NA)})
#      return(rst) 
#    })   
#  })	
  
  # Define the maximum of the sliders Threshold
#  datamaxThresholdCPr <- reactive({rasterwithBestPixels()
#    validate(need(length(input$Prnamethr) !=0, " "))
#    # This calculus is needed to know the number of pixels of the unique raster which are not NA
##    rstHC <- rasterwithBestPixels()
#    # The layer is cropped to match the area selected
#    rstHC <- crop(rstHC, extent(areaselected))
#    if(input$inputselect=="1"){
#      projection(rstHC) <- projection(areaselected)
#      rstHC <- mask(rstHC, areaselected)
#    }
#    area <-maxThrCalculation(rstHC)
#    return(area)
#  })
#  
##  # Download button of the rasterwithBestPixels
#  output$downloadCPrbutton <- renderUI({
#    validate(need(length(input$Prnamethr) !=0 & nlayers(rasterscompared()) > 1, " "))
#    downloadButton('downloadRasterCPr', 'Download Raster')
#  })
  
#  # Download button of the legend of rasterwithBestPixels
#  output$downloadCPrLegendbutton <- renderUI({
#    validate(need(length(input$Prnamethr) !=0 & nlayers(rasterscompared()) > 1, " "))
#    downloadButton('downloadLegendCPr', 'Download Legend')
#  })
#  
#  # Function enabling the downloading of the raster
#  output$downloadRasterCPr <- downloadHandler(
#    filename = function(){return(paste0("bestproject.tif"))},
#    content = function(file) {writeRaster(bestproject(), format="GTiff", file)}
#  )
  
#  # Function enabling the downloading of the legend
#  output$downloadLegendCPr <- downloadHandler( 
#    filename = function(){return("Legend.csv")},
#    content = function(file) {write.csv(levels(bestproject()), file)}
#  )
#  
#  # Map displaying rasterwithBestPixels
#  output$mapCPr <- renderLeaflet({
#    # rasterscompared()
#    rasterscomp <-rasterscompared()
#    validate( need(nlayers(rasterscomp) > 1, 
#                   "Please choose two layers or more"))
#    
#    # Define progress message to warn the user 
#    # to wait during the calculus 
#    withProgress({ 
#      setProgress(message = "Calculating, please wait",
#                  detail = "This may take a few moments...")
#      
#      bestproject <- bestproject()
#      bestproject[is.na(bestproject)] <- 0
#      
#      # The layer is cropped to match the area selected
#      bestproject <- crop( bestproject, extent(areaselected))
#      if(input$inputselect=="1"){
#        projection( bestproject) <- projection(areaselected)
#        bestproject <- mask( bestproject, areaselected)
#      }
#      
#      # Definition of the colour palette  of the future map
#      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
#      #if(nlayers(rasterscomp)>2){
#      palos  <- brewer.pal(nlayers(rasterscomp)+1,"RdYlBu")
#      
#      # }
#      # else{
#      #  palos <- c("#ff0000","#0000ff")
#      # } 
#      
#      # Creation of the html request to create the map with Google Maps. 
#      # create an html map file and 2 png files a grid and a legend
#      b=sapply(0:(nlayers(rasterscomp)), function(i){
#        b<-c(i+0.5)}
#      )
#      
#      leaflet()%>%
#        addGoogleTiles(layerId = "googleTileSat",
#                       options = tileOptions(maxZoom = 15,
#                                             updateWhenZooming = TRUE),
#                       type = "satellite") %>%
#        setView(lng = zoomCenter[[1]],
#                lat = zoomCenter[[2]],
#                zoom = zoomLevel) %>%
#        addRasterImage(x = bestproject,colors = palos,opacity = (input$mapOpacityAv)/100) %>%
#        addLegend(position = "bottomright",colors = palos,
#                  labels =c("NaN",names(rasterscomp)))
#      
#    },session=session)		 
#  })
#  
#  
#  # Update the map
#  observe({ 
#    input$mapCPr_zoom
#    input$mapOpacityCPr
#    input$mapCPrSelect
#    rasterscomp <-rasterscompared()
#    
#    validate( need(nlayers(rasterscomp) > 1, 
#                   "Please choose two layers or more"))
#    
#    isolate({
#      zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
#      if(input$goButtonAdvanced > 0 ){
#        if(input$mapCPr_zoom == zoomin[1] || input$mapCPr_zoom == zoomin[2] || input$mapCPr_zoom == zoomin[3] || input$mapCPr_zoom == zoomin[4] || input$mapCPr_zoom == zoomin[5] || input$mapOpacitymain){
#          
#          bestproject <- bestproject()
#          bestproject[is.na(bestproject)] <- 0
#          
#          # The layer is cropped to match the area selected
#          bestproject <- crop( bestproject, extent(areaselected))
#          if(input$inputselect=="1"){
#            projection( bestproject) <- projection(areaselected)
#            bestproject <- mask( bestproject, areaselected)
#          }
#          
#          #if(nlayers(rasterscomp)>2){
#          palos  <- brewer.pal(nlayers(rasterscomp)+1,"RdYlBu") 
#          
#          # }
#          #else{
#          #  palos <- c("#ff0000","#0000ff")
#          #}
#          
#          # Redraw the map now
#          if(input$mapCPrSelect == 0){
#            leafletProxy("mapCPr") %>%
#              clearTiles() %>%
#              addGoogleTiles(layerId = "googleTileSat",
#                             options = tileOptions(maxZoom = 15,
#                                                   updateWhenZooming = TRUE),
#                             type = "satellite") %>%
#              clearImages() %>%
#              addRasterImage(x = bestproject,colors = palos,opacity = (input$mapOpacityCPr)/100)
#          }
#          else if (input$mapCPrSelect == 1){
#            leafletProxy("mapCPr") %>%
#              clearTiles() %>%
#              addGoogleTiles(layerId = "googleTileRoad",
#                            options = tileOptions(maxZoom = 15,
 #                                                  updateWhenZooming = TRUE),
##                             type = "roadmap") %>%
#              clearImages() %>%
#              addRasterImage(x = bestproject,colors = palos,opacity = (input$mapOpacityCPr)/100)
#          }
#          else{
#            leafletProxy("mapCPr") %>%
#              clearTiles() %>%
#              addGoogleTiles(layerId = "googleTileTer",
#                             options = tileOptions(maxZoom = 15,
#                                                   updateWhenZooming = TRUE),
#                             type = "terrain") %>%
#              clearImages() %>%
#              addRasterImage(x = bestproject,colors = palos,opacity = (input$mapOpacityCPr)/100)
#          }
#          
##        }
#      }
#    })
#  })
#  
#  # Legend for the map
#  observeEvent(input$mapshowCPr,{
#    proxy <- leafletProxy("mapCPr")
#    rasterscomp <-rasterscompared()
#    validate( need(nlayers(rasterscomp) > 1, " "))
#    
#    #if(nlayers(rasterscomp)>2){
#    palos  <- brewer.pal(nlayers(rasterscomp),"RdYlBu") 
#    
#    # }
#    # else{
#    #   palos <- c("#ff0000","#0000ff")
#    #  }
#    
#    # Remove any existing legend, and only if the legend is
#    # enabled, create a new one.
#    if (input$mapshowCPr) {
#      proxy %>% addLegend(position = "bottomright",colors = palos,
#                          labels =nlayers(rasterscomp)+1)
#    }
#    else{
#      proxy %>% clearControls()
#    }
#  })
  
  # Define threshold input as the value of the slider "Threshold" in the panel "compare methods"
#  thrInputCPr <- reactive({ 
#    input$thrCPr 
#    input$thrCPrP
#    if(mthr==1){
#      y<-input$thrCPr
#    }else if (mthr==0){
#      y <-input$thrCPrP
#    }
#    return(y)      
#  })
#  
#  # Observe which threshold you use
#  observeEvent(input$thrCPr, {
#    mthr <<- 1
#    m <<- "Area"
#    #Creation of the div 
#    tags$div(  id="textthr",
#               tags$p("Method : ", m))
#  })
#  observeEvent(input$thrCPrP, {
#    mthr<<-0
#    m <<- "Percentage"
#    #Creation of the div 
#    tags$div(  id="textthr",
#               tags$p("Method : ", m))
#  })
#  
#  # Create the threshold input of the tab Compare projects
#  output$renderThrCPr <- renderUI({
#    sliderInput(inputId="thrCPr", label = "Threshold per ha",min = 0, max =datamaxThresholdCPr(), post=" ha", value = (50/100)*datamaxThresholdCPr())
#  })
#  output$renderThrCPrP <- renderUI({
#    sliderInput(inputId="thrCPrP", label = "Threshold per %",min = 0, max =100, post=" %", value = 50)
#  })
#  
#  valCPr <- reactive({input$thrCPr}) #x
#  valCPrP <- reactive({input$thrCPrP}) #y
#  
#  #Define the wanted threshold (area or quantile)
#  observeEvent(input$thrCPr,{
#    t <<- "1"
#    y <- as.integer((valCPr()*100)/datamaxThresholdCPr())
#    updateSliderInput(session, "thrCPrP", value = y, step = 0.01)
#  })
#  
#  observeEvent(input$thrCPrP,{
#    t <<- "2"
#    x <- as.integer(valCPrP()*(datamaxThresholdCPr()/100))
#    updateSliderInput(session, "thrCPr", value = x, step = 0.01)
#  })
#  ## End of the "two sliders reactivity"
#  
#  # Define threshold map for each best project
#  output$mapCPrThr <- renderLeaflet({
#    input$Prnamethr
#    if (is.null(input$Prnamethr) == F){
#      validate( need(length(input$Prnamethr)==1 & nlayers(rasterscompared()) > 1, " "))
#      withProgress({ 
#        setProgress(message = "Calculating, please wait",
#                    detail = "This may take a few moments...")
#        
#        rst <- rasterwithBestPixels()
#        area <-datamaxThresholdCPr()
#        
#        thr=as.numeric(cellStats(rst,stat=function(x,na.rm){quantile(x,probs=1-thrInputCPr()[1]/area,na.rm=T)}))
#        
#        # calculus of the threshold raster
#        rstCPr.thr<-calcthr(rst,thr)
#        # The layer is cropped to match the area selected
#        rstCPr.thr <- crop(rstCPr.thr, extent(areaselected))
#        if(input$inputselect=="1"){
#          #projection(areaselected) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
#          projection(rstCPr.thr) <- projection(areaselected)
#          rstCPr.thr <- mask(rstCPr.thr, areaselected)
#        }
#        
#        # Definition of the colour palette  of the map for one method  
#        palos  <-c("#ff0000","#0000ff")
#        zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
#        
#        leaflet()%>%
#          addGoogleTiles(layerId = "googleTileSat",
#                         options = tileOptions(maxZoom = 15,
#                                               updateWhenZooming = TRUE),
#                         type = "satellite") %>%
#          setView(lng = zoomCenter[[1]],
#                  lat = zoomCenter[[2]],
#                  zoom = zoommin[1]) %>%
#          addRasterImage(x = rstCPr.thr,colors = c("#ff0000","#0000ff"),opacity = (input$mapOpacityCPr2)/100) %>%
#          addLegend(position = "bottomright",colors = palos,
#                    labels =c("bellow","above "))
#        
#      },session=session)
#      
#    }
#  })
#  
#  # Update the map
#  observe({ 
#    input$mapCPrThr_zoom
#    input$mapOpacityCPr
#    input$mapCPrSelect
#    rasterscomp <-rasterscompared()
#    
#    validate( need(length(input$Prnamethr)==1 & nlayers(rasterscompared()) > 1, " "))
#    withProgress({ 
#      setProgress(message = "Calculating, please wait",
#                  detail = "This may take a few moments...")
#      
#      isolate({
#        zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
#        if(input$mapCPrThr_zoom == zoomin[1] || input$mapCPrThr_zoom == zoomin[2] || input$mapCPrThr_zoom == zoomin[3] || input$mapCPrThr_zoom == zoomin[4] || input$mapCPrThr_zoom == zoomin[5] || input$mapOpacitymain){
#          
#          rst <- rasterwithBestPixels()
#          area <-datamaxThresholdCPr()
#          
  #        thr=as.numeric(cellStats(rst,stat=function(x,na.rm){quantile(x,probs=1-thrInputCPr()[1]/area,na.rm=T)}))
          
  #        # calculus of the threshold raster
  #        rstCPr.thr<-calcthr(rst,thr)
  #        # The layer is cropped to match the area selected
  #        rstCPr.thr <- crop(rstCPr.thr, extent(areaselected))
  #        if(input$inputselect=="1"){
  #          #projection(areaselected) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
  #          projection(rstCPr.thr) <- projection(areaselected)
  #          rstCPr.thr <- mask(rstCPr.thr, areaselected)
  #        }
  #        # Definition of the colour palette  of the map for one method  
  #        palos  <-c("#ff0000","#0000ff")
  #        zoomin <- leafletGoogle::setZoomBound(xdist=xdist, ydist = ydist)
   #       # Redraw the map now
   #       if(input$mapCPrSelect2 == 0){
   #         leafletProxy("mapCPrThr") %>%
   #           clearTiles() %>%
   #           addGoogleTiles(layerId = "googleTileSat",
   #                          options = tileOptions(maxZoom = 15,
   #                                                updateWhenZooming = TRUE),
  #                           type = "satellite") %>%
  #            clearImages() %>%
  #            addRasterImage(x = rstCPr.thr,colors = c("#ff0000","#0000ff"),opacity = (input$mapOpacityCPr2)/100)
  #        }
  #        else if (input$mapCPrSelect2 == 1){
  #          leafletProxy("mapCPrThr") %>%
  #            clearTiles() %>%
  #            addGoogleTiles(layerId = "googleTileRoad",
  #                           options = tileOptions(maxZoom = 15,
  #                                                 updateWhenZooming = TRUE),
  #                           type = "roadmap") %>%
  #            clearImages() %>%
  #            addRasterImage(x = rstCPr.thr,colors = c("#ff0000","#0000ff"),opacity = (input$mapOpacityCPr2)/100)
 #         }
 #         else{
 #           leafletProxy("mapCPrThr") %>%
 #             clearTiles() %>%
#              addGoogleTiles(layerId = "googleTileTer",
 #                            options = tileOptions(maxZoom = 15,
 #                                                  updateWhenZooming = TRUE),
 #                            type = "terrain") %>%
 #             clearImages() %>%
 #             addRasterImage(x = rstCPr.thr,colors = c("#ff0000","#0000ff"),opacity = (input$mapOpacityCPr2)/100)
 #         }
 #       }
 #     })
 #   })
#  })
  
#  # Legend for the map
  #observeEvent(input$mapshowCPr2,{
  #  proxy <- leafletProxy("mapCPrThr")
  #  palos  <-c("#ff0000","#0000ff")
  #  # Remove any existing legend, and only if the legend is
  #  # enabled, create a new one.
  #  if (input$mapshowCPr2) {
  #    proxy %>% addLegend(position = "bottomright",colors = palos,
  #                        labels =c("bellow","above"))
  #  }
#    else{
  #    proxy %>% clearControls()
  #  }
  #})
  
  
  
  ####################################################################################################################  
  ################################ Sensitivity Analysis ##############################################################
  ####################################################################################################################  
  
  ### All the input are here ###
  output$renderSA1 <- renderUI({
    sliderInput(inputId="disruption", label = "Please choose the disruption :",
                min = 0, max =50, post=" %", value = 5,step = 5)
  })
  
  # Creation of the slider That should move thanks to the one that's right above this one 
  output$renderSA2 <- renderUI({
    sliderInput(inputId="intervals", label = "Please choose the number of intervals to use :",
                min = 1, max =((input$disruption)/5), post=" interval(s)", value = 1,step = 1)
  })
  
  
  ## The following part makes the two sliders to be reactive to each other -- once you move one of them the other one is triggered
  # Creation of the slider Threshold
  output$renderThrSA <- renderUI({
    sliderInput(inputId="thrSA", label = "Select the number of ha you want:",min = 50, max =datamaxThr, post="ha", value = datamaxThr*0.5, step=25)
  })
  
  # Creation of the slider Threshold per quantile
  output$renderThrQSA <- renderUI({
    sliderInput(inputId="thrQSA", label = "Select the percentage of area you want:",min = 0.0161838485191779, max =100, post="%", value = 50, step=0.1)
  })
  
  ### End of the input ###
  
  
  # Use of an input button to update
  thrSAInput <- reactive({input$goButtonThrSA
    isolate({      
      input$thrSA
    })    
  })
  
  # Use of an input button to update
  thrQSAInput <- reactive({input$goButtonThrSA 
    isolate({      
      input$thrQSA
    })    
  })
  
  valSA <- reactive({input$thrSA}) 
  valQSA <- reactive({input$thrQSA}) 
  
  
  #Define the wanted threshold (area or quantile)
  observeEvent(input$thrSA,{
    y <- as.integer((valSA()*100)/datamaxThr)
    updateSliderInput(session, inputId="thrQSA", value = y, step = 1)
  })
  
  observeEvent(input$thrQSA,{
    x <- as.integer(valQSA()*(datamaxThr/100))
    updateSliderInput(session, inputId="thrSA", value = x, step = 1)
  })
  ## End of the "two sliders reactivity"

  ### SENSITIVITY ANALYSIS BEGINNING 
  observeEvent(input$goButtonThrSA, {
  # If all weights sliders are on 0, warns the user
  validate(need(input$goButtonThrSA >0,""))
  validate(need(Reduce(function(acc, cur) { acc = (acc || (!is.na(cur) && (cur != 0))) }, weightsVector(), F), 
                "Please put a positive weigth for at least one criterion or select new criteria (go back to select criteria tab)"))
  isolate({
  # Define progress message to warn the user to wait during the calculus 
  withProgress({
    setProgress(message = "Calculating, please wait",detail = "This may take a few moments...")
    
    # We need to know whether we used adv or basic mode
    if(mamapAvailable==0){
      rst.comb.w <- calculmapmain()}
    else{
      rst.comb.w <- calculmapmaina()}
    
    thr=as.numeric(cellStats(rst.comb.w,stat=function(x,na.rm){stats::quantile(x,probs=1-thrSAInput()/datamaxThr,na.rm=T)}))
    
    somma <-nbpx_notNA(rst.comb.w)
    
    # calculus of the threshold raster
    
    if(input$inputselect=="1"){
      projection(rst.comb.thr) <- projection(areaselected)
      rst.comb.thr <- mask(rst.comb.w, areaselected)}
    else{
      rst.comb.thr <- rst.comb.w}
    
    # Fill in the map with O and 1 depends on values(rst.comb.thr2) above threshold (1) or below (0)
    rst.comb.thr2 <- binarythr(rst.comb.thr,thr,thrQInput()/100)
    
    rst.map.sa <<- rst.comb.thr2
    
    
    ###############
    
    
    # The weight Vector of the differents Criteria
    y <- weightsVector()

    y <- y/sum(y,na.rm=T)
    
    # Weight vector without 'NaN' 
    ybis <- y[!is.na(y)]
    
    indexCriteria <- (1:length(y))[!is.na(y)]
    criteria <- lapply(HPN[indexCriteria,3],toString)
    # Inital values of the variables
    valx <- 1
    valz <- 1
    # At the moment disturb = 0.2 
    # You need to uncomment the next line
    disturbance <<- input$disruption/100  # disturb=alteration of criteria
    
    # It's the number of measures you want to take so it needs to be higher than 1
    intervals <- input$intervals
    
    #print(getwd())
    setwd("..")
    #print(getwd())
    setwd(paste0(getwd(),"/Images"))
    
    # We need to create as much matrix as we need, ie intervals.
    # And store them into the "store" vector
    store <- list()
    for(i in 1:intervals){
      store[[i]] <- list(# It is the previous x
        x = Matrix::Matrix(nrow = length(ybis),ncol = length(y)),
        # And this is the previous z
        z = Matrix::Matrix(nrow = length(ybis),ncol = length(y))
      )
    }
    
    ## We need to have the matrix of the disturbed vector y <- weigthvector()
    ## So there are two matrix : 
    # z is y + disturb
    for(u in 1:intervals){
      valx <- 1
      valz <- 1
      
      #We have to define the new disturbance 
      disturb <- round(((disturbance*u)/intervals),3)
      
      for(i in 1:length(y)){
        if(!is.na(y[i])){
          buf <- y
          if((y[i]*(1+disturb)<1)){ 
            # Disturbance on weight values
            y[i] <- y[i]*(1+disturb) 
          }
          else{
            y[i] <- 1
          }
          # standardization of weight vector (don't change y[i] else you will modify the disturbance...)
          y[(i!=1:length(y)) & (!is.na(y))] <- y[(i!=1:length(y)) & (!is.na(y))]*((1-y[i])/(1-buf[i])) 
          for(a in 1:length(y)){
            # All the disturbed weights are kept in store
            store[[u]]$z[valz,a] <- y[a]
          }
          y <- buf
          valz <- valz+1
        }
      }
      for(i in 1:length(y)){
        if(!is.na(y[i])){
          buf <- y
          if(y[i]*(1-disturb)>0){
            y[i] <- y[i]*(1-disturb)
          }
          else{
            y[i] <- 0
          }
          y[(i!=1:length(y)) & (!is.na(y))] <- y[(i!=1:length(y)) & (!is.na(y))]*((1-y[i])/(1-buf[i])) 
          for(a in 1:length(y)){
            # All the disturbed weights are kept in store
            store[[u]]$x[valx,a] <- y[a]
          }
          y <- buf
          valx <- valx+1
        }
      }
    }
    
    # For z = ++
    # For x = -- 
    listOfRasterStack <- stack()
    rasterStackVariation <- stack()
    listOfRasterStackChange <- list()

    
    # The goal is to keep all the raster after changing the weight of a criteria.
    # I am using a list of rasterStack. Each rasterStack is for one criteria and all the raster inside the rasterStack are the different disturbance of the weight. 
    # So you can access to rasters by : listOfRasterStack[criteria(index)][disturbance(index(negative disturbance first))] 
    
    # if LWC
    if(radiomInput()==2){
      listOfRasterStack <- lapply(1:length(ybis), function(j){
        #For each criteria
        rasterStackVariation <- addLayer(x=rasterStackVariation, 
                                         # Calculation on the negative disturbance 
                                         lapply(1:intervals, function(i){
                                           tmpX <- lwc2(rst.pnl, store[[i]]$x[j,])
                                           thr=as.numeric(cellStats(tmpX,stat=function(x,na.rm){stats::quantile(x,probs=1-thrSAInput()/datamaxThr,na.rm=T)}))
                                           binarythr(tmpX,thr,thrQSAInput()/100)
                                          }),
                                         # Calculation on the positive disturbance
                                         lapply(1:intervals, function(i){
                                           tmpZ <- lwc2(rst.pnl, store[[i]]$z[j,])
                                           thr=as.numeric(cellStats(tmpZ,stat=function(x,na.rm){stats::quantile(x,probs=1-thrSAInput()/datamaxThr,na.rm=T)}))
                                           binarythr(tmpZ,thr,thrQSAInput()/100)
                                          })
        )
        rasterStackVariation
      })
    }
    #if ELECTRE 
    if(radiomInput()==3){
      listOfRasterStack <- lapply(1:length(ybis), function(j){
        rasterStackVariation <- addLayer(x=rasterStackVariation, 
                                         # Calculation on the negative disturbance
                                         lapply(1:intervals, function(i){
                                           tmpX <- electre(rst.pnl, store[[i]]$x[j,])
                                           thr=as.numeric(cellStats(tmpX,stat=function(x,na.rm){stats::quantile(x,probs=1-thrSAInput()/datamaxThr,na.rm=T)}))
                                           binarythr(tmpX,thr, thrQSAInput()/100)
                                          }),
                                         # Calculation on the positive disturbance
                                         lapply(1:intervals, function(i){
                                           tmpZ <- electre(rst.pnl, store[[i]]$z[j,])
                                           thr=as.numeric(cellStats(tmpZ,stat=function(x,na.rm){stats::quantile(x,probs=1-thrSAInput()/datamaxThr,na.rm=T)}))
                                           binarythr(tmpZ,thr,thrQSAInput()/100)
                                         })
        )
        rasterStackVariation
        })
    }
    
    listOfRasterStackChange <<- lapply(1:length(listOfRasterStack), function(i) {
      listOfRasterStack[[i]]-rst.map.sa
    })
    
    ahah <<- listOfRasterStack
    ### END OF CALCULATION
    
    ### FILE TO DOWNLOAD 
    #I create columns with data 
    AboveToBelow <- c()
    Static <- c()
    BelowToAbove <- c()
    ModifiedCriteria <- c()
    Change <- c()
    vec <- 1:(2*intervals)
    v<-ifelse(vec>intervals,(disturbance/intervals)*100*(vec-intervals),(disturbance/intervals)*100*vec*-1)
    for(i in 1:length(ybis)){
      for( j in 1:(2*intervals)){ 
        val<-table(values(listOfRasterStack[[i]][[j]]-rst.map.sa))
        AboveToBelow <- c(AboveToBelow,val["-1"])
        Static <- c(Static,val["0"])
        BelowToAbove <- c(BelowToAbove,val["1"])
        ModifiedCriteria <- c(ModifiedCriteria,criteria[[i]])
        Change <- c(Change, paste0(toString(v[j]),"%"))
      }
    }
    # Use all the colums to create a data.frame 
    dfSA <- data.frame(ModifiedCriteria, Change, BelowToAbove,Static,AboveToBelow)
    
    file.remove(list.files(pattern=".tif"))
    file.remove(list.files(pattern=".RData"))
    description <- sapply(HPN[,3][!is.na(y)],toString)

    qte<-sapply(100*(1:intervals)*disturbance/intervals,toString)
    qte<-rev(qte)
    print(qte)

    # I change the value in the raster because it is not possible to get a raster with the value -1. So I fill in the raster with value between 0 and 1.     
    listOfRasterStackChangeForDownload <- lapply(1:length(listOfRasterStack), function(i) {
       (listOfRasterStack[[i]]-rst.map.sa +1)/5})
    # Write raster by gtiff file 
    for (j in 1:length(ybis)){
      filenamePlus <-  sapply(qte, function(x){paste0("+",toString(x),"%",".tif")})
      filenameMoins <- sapply(qte, function(x){paste0("-",toString(x),"%",".tif")})
      filename= c(filenameMoins, filenamePlus)
      print(filename)
      writeRaster(x = listOfRasterStackChangeForDownload[[j]], filename = description[j],format="GTiff",overwrite= TRUE,bylayer =T,suffix=filename, NAflag = 0)
    }
    
    # the dataframe is used to be written in csv (Excel)
    write.csv(dfSA, file="Results.csv",col.names = c("Modified criteria","Variation on weigth","Below to Above", "Static", "Above to Below"))
    
    files2zip <<- dir(getwd())
    # Change once again the wd
    setwd("../www")
    print("getwd()")
    #print(getwd())
    
    },session=session)
  })
  })

  

  
  
  # Displaying the results of SA on the map
  output$mapThrSA <- renderLeaflet({
    input$goButtonThrSA
    input$criteriaToShow
    input$btnChangeOnWeight
    
    
    validate(need(input$goButtonThrSA != 0,message = "Please run sensitivity analysis to display the map"))
    validate(need(!is.null(input$btnChangeOnWeight) ,message = "Bug"))
    validate(need(!is.null(input$criteriaToShow),message = "Bug"))
    isolate({
    # I have to use inputs to get the right map
    # But I can't use the name of the criteria or the disturbance. I have to use a number (index) and find it in listOfRasterStackChange. 
    intervals<-isolate(input$intervals)
    y<-weightsVector()
    indexCriteria <- (1:length(y))[!is.na(y)]
    criteria <- lapply(HPN[indexCriteria,3],toString)
    display <<- (1:length(indexCriteria))[criteria==input$criteriaToShow]
    change<<- as.numeric(input$btnChangeOnWeight)
    
    RasterSavedSA <<- listOfRasterStackChange[[display]][[change]]

    # Create leaflet widget --------------------------------------------------------
    
    pal  <- c("#b30000","#a6a6a6","#009933")
    palos<- colorBin(palette = pal, bins = c(-1.5,-0.5,0.5,1.5), domain=c(-1.5,-0.5,0.5,1.5),na.color = NA)

    if(input$mapThrSelectSA == 0){
              m<-leaflet()%>%
                addGoogleTiles(layerId = "googleTileSat",
                               options = tileOptions(maxZoom = 15,
                                                     updateWhenZooming = TRUE),
                               type = "satellite") %>%
                addRasterImage(listOfRasterStackChange[[display]][[change]],
                               colors = palos,
                               opacity = input$mapOpacityThrSA/100,
                               method="ngb") %>%
                setView(lng = zoomCenter[[1]],
                        lat = zoomCenter[[2]],
                        zoom = zoomLevel)
              
              m
        }
    else if (input$mapThrSelectSA == 1){
            m<-leaflet()%>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "roadmap") %>%
              addRasterImage(listOfRasterStackChange[[display]][[change]],
                             colors = palos,
                             opacity = input$mapOpacityThrSA/100,
                             method="ngb") %>%
              setView(lng = zoomCenter[[1]],
                      lat = zoomCenter[[2]],
                      zoom = zoomLevel)
            m
      
        }
    else{
            m<-leaflet()%>%
              addGoogleTiles(layerId = "googleTileSat",
                             options = tileOptions(maxZoom = 15,
                                                   updateWhenZooming = TRUE),
                             type = "terrain") %>%
              addRasterImage(listOfRasterStackChange[[display]][[change]],
                             colors = palos,
                             opacity = input$mapOpacityThrSA/100,
                             method="ngb") %>%  
              setView(lng = zoomCenter[[1]],
                      lat = zoomCenter[[2]],
                      zoom = zoomLevel)
            
            m
    }
    })
  })
  observe({
    input$mapOpacityThrSA
    input$mapThrSelectSA
    input$mapThrSA_zoom
    
    isolate({
    pal  <- c("#b30000","#a6a6a6","#009933")
    palos<- colorBin(palette = pal, bins = c(-1.5,-0.5,0.5,1.5), domain=c(-1.5,-0.5,0.5,1.5),na.color = NA)

      if(input$mapThrSelectSA == 0){
        leafletProxy("mapThrSA")%>%
          clearTiles() %>%
          addGoogleTiles(layerId = "googleTileSat",
                         options = tileOptions(maxZoom = 15,
                                               updateWhenZooming = TRUE),
                         type = "satellite") %>% 
          clearImages() %>%
          addRasterImage(RasterSavedSA,
                         colors = palos,
                         opacity = input$mapOpacityThrSA/100,
                         method="ngb")
        
        
      }
      else if (input$mapThrSelectSA == 1){
        leafletProxy("mapThrSA")%>%
          clearTiles() %>%
          addGoogleTiles(layerId = "googleTileSat",
                         options = tileOptions(maxZoom = 15,
                                               updateWhenZooming = TRUE),
                         type = "roadmap") %>%
          clearImages %>%
          addRasterImage(RasterSavedSA,
                         colors = palos,
                         opacity = input$mapOpacityThrSA/100,
                         method="ngb")
        
        
      }
      else{
       leafletProxy("mapThrSA")%>%
          clearTiles() %>%
          addGoogleTiles(layerId = "googleTileSat",
                         options = tileOptions(maxZoom = 15,
                                               updateWhenZooming = TRUE),
                         type = "terrain") %>%
          clearImages() %>%
          addRasterImage(RasterSavedSA,
                         colors = palos,
                         opacity = input$mapOpacityThrSA/100,
                         method="ngb") 
      }
  })
  })

  
  # Memorize Zoom level and center for method compare 
  observe({
    zoomLevel <<- input$mapThrSA_zoom
    zoomCenter <<- input$mapThrSA_center
  }) 
  
  # Event to toggle the legend
  observe({
    input$goButtonThrSA
    input$criteriaToShow
    input$btnChangeOnWeight
    input$mapOpacityThrSA
    input$mapThrSelectSA
    input$mapThrSA_zoom
    input$mapshowThrSA
    
    proxy <- leafletProxy("mapThrSA")
    pal  <- c("#b30000","#a6a6a6","#009933")
    # Remove any existing legend, and only if the legend is enabled, create a new one.
    if (input$mapshowThrSA) {
      proxy %>% clearControls() %>% addLegend(position = "bottomright",colors = pal,labels = c("Above to below", "Static" ,"Below to above"), opacity = input$mapOpacityThrSA/100)
    }
    else{
      proxy %>% clearControls()
    }
  })
  
  output$renderChangeOnWeight <- renderUI({
    input$goButtonThr
    input$goButtonThrSA
    
    validate(need(input$goButtonThrSA != 0,message = "Please run sensitivity analysis !"))
    intervals<-isolate(input$intervals)
    disruption<-isolate(input$disruption)
    choices<-list()
    #Value to catch the choice. Value is linked with the position in RasterStack, that's why I reverse half of the value.
    choices <- c(intervals:1,intervals+(1:intervals))
    #Push the name to choose the change
    names(choices) <- c(paste0("-",as.character(round(((intervals:1)*disruption)/intervals),2),"%"),paste0("+",as.character(round(((1:intervals)*disruption)/intervals),2),"%"))

    radioButtons(inputId ="btnChangeOnWeight" ,label = "How much is this criterion disrupted ? " ,choices = choices ,inline = T)
  })
  
  
  # Display the list of criteria involved 
  output$renderSelectCriteriaToShow <-renderUI({input$goButtonThr
    choices <- list()
    y<-weightsVector()
    y<-(1:length(y))[!is.na(y)] #Get index number of criteria with a weigth value 
    #Get the name of criteria
    choices<- lapply(HPN[y,3], toString)
    descript <-lapply(HPN[y,4], toString)
    descript <- gsub("_", " ", descript)
    names(choices) <- descript
    selectInput("criteriaToShow", "Which criterion is disrupted ?", 
                choices = choices,selected=1)
  })
  
  #Display diagram under the map
  output$histoSA <- renderPlot({
    input$goButtonThrSA
    input$criteriaToShow
    input$btnChangeOnWeight
    
    validate(need(input$goButtonThrSA != 0,message = "Please run sensitivity analysis to display the map"))
    validate(need(!is.null(input$btnChangeOnWeight) ,message = "Bug"))
    validate(need(!is.null(input$criteriaToShow),message = "Bug"))
    
    # Read multiple shapefiles with standardised name ----
    intervals<-isolate(input$intervals)
    y<<-weightsVector()
    indexCriteria <- (1:length(y))[!is.na(y)]
    criteria <- lapply(HPN[indexCriteria,3],toString)
    display <<- (1:length(indexCriteria))[criteria==input$criteriaToShow]
    change<<- as.numeric(input$btnChangeOnWeight)
    print(display)
    print(change)
    
    value <- table(values(listOfRasterStackChange[[display]][[change]]))
    pal <- ifelse(names(value)==0, "#a6a6a6",ifelse(names(value)=="1","#009933","#b30000"))
    labels_category  <- ifelse(names(value)=="0", "Static", ifelse(names(value)=="1", "Below to above", "Above to below"))
    labels_number <- sapply(value,function(x){toString(x)})
    pie <- pie(value, col=pal, labels=labels_category)
    text_pie(value, labels = labels_number)    
  })

  
  # Output of downloadButton. 
  # Let downloading the zip file with pictures of raster and a table with results from sensitivity analysis
  output$downloadSA <- downloadHandler(
    filename = function() {
      paste("file2zip", "zip", sep = ".")
    },
    content = function(fname) {
      setwd("../Images")
      print(getwd())
      zip(zipfile=fname, files=files2zip)
      setwd("../www")
      print(getwd())
    },
    contentType = "application/zip"
  )
})