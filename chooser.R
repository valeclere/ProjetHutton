library(shiny)
library(sp)
library(rgdal)
library(raster)
library(lattice)
library(plotGoogleMaps)
library(RColorBrewer)


#ChooserInput: create the boxes with the corresponding data in and call the JavaScript file

chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,size, multiple) {
  
			leftChoices  <- lapply(leftChoices, tags$option)
			rightChoices <- lapply(rightChoices, tags$option)
			if (multiple)
			multiple <- "multiple"
			else
			multiple <- NULL



			return(tagList(
							singleton(tags$head(tags$script(src="chooser-binding.js"),
							                    tags$style(type="text/css",HTML(".chooser-container { display: inline-block; max-width: 35; overflow-x: auto; vertical-align: top;}")))),
							div(id=inputId, class="chooser",
															div(id=leftLabel, 
															    class="chooser-container chooser-left-container",
															    tags$select(class="left", size=size, multiple=multiple, leftChoices)),
															div(class="chooser-container chooser-center-container",
															    icon("arrow-circle-o-right", "right-arrow fa-3x"),
															    tags$br(),
															    icon("arrow-circle-o-left", "left-arrow fa-3x")),
															div(id=rightLabel, 
															    class="chooser-container chooser-right-container",
															    tags$select(class="right", size=size, multiple=multiple, rightChoices))
								)
							)
)}

#registerInputHandler : create a variable containing the selected criteria on the right

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
																		if (is.null(data))
																		NULL
																		else
																		as.character(data$right)
																	}, force = TRUE)


