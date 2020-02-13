# areaselected <- rst.pl.original
# launchingProject(1, Pr)
# Prinfo <- Pr[1,]
# type <<- Prinfo[1,2]
# xp <<-HP[,3]
# xn <<-HN[,3]
# x1p <<-sapply(xp, toString)
# x1n <<-sapply(xn, toString)
# HP<<- read.table(paste0("../data/",type,"/", Prinfo[1,5]),h=T)
# HN<<- read.table(paste0("../data/",type,"/", Prinfo[1,6]),h=T)
# HPN<<- rbind(HP,HN)


# You need to uncomment the observe to make the app works with the user system ;)

#           shinyServer(function(input, output,session) {
#              env<-environment()
#              # observe({
#              # 	input$userID
#              # 	IdentifiedUser(env)
#              # #	hide("loading_page")
#              # #	shinyjs::show("main_content")
#              # })

# In the MapThr I've remove this : 
#
#     if(mamapAvailable==0){
#       # rst.comb.w <- calculmapmain()
#       rst.comb.w <- calculmapmain()}
#     else{
#       rst.comb.w <- calculmapmaina()}
# 
#     # Calculus of the raster value which corresponds approximately to the quantile of the ratio 1-threshold/total area
#     # if(t=="0"){
#     #   thr=as.numeric(cellStats(rst.comb.w,stat=function(x,na.rm){quantile(x,probs=1,na.rm=T)}))}
#     # else if (t == "1"){
#     #   thr=as.numeric(cellStats(rst.comb.w,stat=function(x,na.rm){quantile(x,probs=1-thrInput()[1]/area,na.rm=T)}))}
#     # else if (t == "2"){
#     #   thr=as.numeric(cellStats(rst.comb.w,stat=function(x,na.rm){quantile(x,probs=1-thrQInput()[1]/100,na.rm=T)}))}
