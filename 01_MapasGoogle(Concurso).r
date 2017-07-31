#USAGE
#
#FUNCTION PARAMETERS: @filename
#@submission: path+filename containing trajectories in CSV format
process.data<-function(filename)
{
	libraries()
	# dt<-read.csv2(filename,sep=",")
	dt<-read.csv2('03_Paso2_pruebaScriptConcurso.csv',sep=',')
	head(dt$polyline)
	
	dt$call_type<-as.character(dt$call_type)
	dt$day_type<-as.character(dt$day_type)
	dt$missing_data<-as.character(dt$missing_data)
	dt$polyline<-as.character(dt$polyline)
	str(dt)
	print(unique(dt$missing_data))

	my_poly<-dt$polyline[2]
	print(my_poly)
	print("")
	i<-1

	for (mp in c(1:length(dt$polyline)))
	{
		print(mp)
		# print(miliseconds_to_date(dt$travel_timestamp[mp]))
	  print(miliseconds_to_date(dt$travel_timestamp[1]))
		# my_poly<-dt$polyline[mp]
	  my_poly<-dt$polyline[1]
		# if(nchar(my_poly)>2)
		# {
		# 	ab<-unlist(strsplit(my_poly,"[]]"))
	  ab<-unlist(strsplit(my_poly,"[]]"))
		# 	polyline<-c(substr(ab[1],3,nchar(ab[1])),sapply(ab[2:(length(ab)-1)],my.substr))
	  polyline<-c(substr(ab[1],3,nchar(ab[1])),sapply(ab[2:(length(ab)-1)],my.substr))
		# 
		# 	lat<-c(as.numeric(sapply(polyline,getlat)))
	  lat<-c(as.numeric(sapply(polyline,getlat)))
		# 	lon<-c(as.numeric(sapply(polyline,getlon)))
	  	lon<-c(as.numeric(sapply(polyline,getlon)))
		# 
		# 	create_map(length(dt$polyline),lat,lon) 
		# }
		# print(mp)
		# print(miliseconds_to_date(dt$TIMESTAMP[mp]))
		# x<-scan()
	}
}

miliseconds_to_date<-function(dt)
{
  mdy<-month.day.year((dt/3600/24))
  
  dt<-dt%%(3600*24)
  h<-floor(dt/3600)
  dt<-dt-(h*3600)
  m<-floor(dt/60)
  dt<-dt-(m*60)
  s<-round(dt)
  
  final<-sprintf("%s/%s/%s %s:%s:%s",format_alg(4,mdy$year),format_alg(2,mdy$month),format_alg(2,mdy$day),format_alg(2,h),format_alg(2,m),format_alg(2,s))
  return(final)
}

create_map<-function(ntraj,lat,lon,zoom=13)
{
  
  #random colors
  colfunc<-colorRampPalette(c("green","red"))

  center = c(mean(lat), mean(lon))
  print(sprintf("Zoom: %d",zoom))
  MyMap <- GetMap(center="Porto", zoom=zoom,GRAYSCALE=FALSE,destfile = "MyTile3.png");

  point_size<-1.0
  
  print(lat)
  print(lon)
  tmp <- PlotOnStaticMap(MyMap, lat = lat,lon = lon, cex=point_size,pch=20, col=colfunc(length(lat)), add=FALSE)
  str(tmp)
}

load.lib<-function(libT,l=NULL)
{
	lib.loc <- l
	print(lib.loc)
	
	if (length(which(installed.packages(lib.loc=lib.loc)[,1]==libT))==0)
	{
		install.packages(libT, lib=lib.loc,repos='http://cran.us.r-project.org')
	}
}

format_alg<-function(i,n)
{
  s<-sprintf("%d",n)
  while(nchar(s)<i)
  {
    s<-sprintf("0%s",s)
  }
  return(s)
}

libraries<-function()
{
	load.lib("RgoogleMaps")
	library(RgoogleMaps)
	load.lib("colorRamps")
	library(colorRamps)
	load.lib("tm")
	library(tm)
	load.lib("chron")
	library(chron)
}

my.substr<-function(ele)
{
	ab<-substr(ele,4,nchar(ele))
	if (substr(ab,1,1)==' ')
		return(substr(ab,3,nchar(ab)))
	return(ab)
}

getlat<-function(ele)
{
	return(as.numeric(unlist(strsplit(ele,"[,]"))[2]))
}

getlon<-function(ele)
{
	return(as.numeric(unlist(strsplit(ele,"[,]"))[1]))
}

#Ejecución de las funciones
setwd("C:/Users/Usuario/Documents/01_UBA/Maestria Data Mining/Proyectos/02_Integrador/00_Datasets/03_Procesados/02_Pasos")

process.data('03_Paso2_pruebaScriptConcurso.csv')


