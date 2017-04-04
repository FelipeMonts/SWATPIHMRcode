#############################################################################################################################
#
#  Program find the most abundant HRU in the SWAT input files, inside each PIHM mesh
#  to translate then the inputs of the HRUS into Cycles format for simulation
#
#
#  Felipe Montes,  2017/03/29
#
##############################################################################################################################


############################### Record Time To start##########################################################



TimeStart<-Sys.time()  ;



###############################################################################################################
#                          Loading Packages and setting up working directory                        
###############################################################################################################



#  Tell the program where the package libraries are  #####################


.libPaths("C:/Felipe/Sotware&Coding/R_Library/library")  ;

#  Set Working directory


setwd("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/WE-38/WE38_Files_PIHM_Cycles20170208/Rcode") ; 


###############################################################################################################
#                         Call packages neded to process the data 
#                             
###############################################################################################################

library("foreign") ;



###############################################################################################################
#                         
###############################################################################################################


Int_HRU_Mesh<-read.dbf('../SwatPIHM/WE38IntersectionMeshHRU_Calc.dbf', as.is=T ) ;


head(Int_HRU_Mesh) 

str(Int_HRU_Mesh)

names(Int_HRU_Mesh)



# OneTirangle<-Int_HRU_Mesh[Int_HRU_Mesh$FID_NTPIHM==714,] ;
# 
# OneTirangle[,c( "FID_WE38Ca" ,"Ele_ID" , "Shape_Area" ,"F_AREA" , "FID_FullHR" , "HRUGIS", "LU_NUM" ,"LU_CODE" )] ;
# 
# head(OneTirangle);

Int_HRU_Mesh$LU_CODE<-as.factor(Int_HRU_Mesh$LU_CODE);

Mesh.Triangle.Area<-aggregate(formula=Shape_Area~LU_CODE+FID_NTPIHM, data=Int_HRU_Mesh, FUN=sum, simplify=T) ;

head(Mesh.Triangle.Area)
str(Mesh.Triangle.Area)

Area.Dominant.LU<-aggregate(formula=Shape_Area~FID_NTPIHM,data=Mesh.Triangle.Area, FUN = max, simplify=T);

str(Area.Dominant.LU)
head(Area.Dominant.LU)


str(Mesh.Triangle.Area[Mesh.Triangle.Area$Shape_Area %in% Area.Dominant.LU$Shape_Area,])

str(AAAA)

head(AAAA, 30) 

AAAA$Area.Fraction<-AAAA$Shape_Area.x/AAAA$Shape_Area.y  ;


aggregate(formula=Area.Fraction~LU_NUM%in%FID_NTPIHM , data=AAAA , FUN=which.max() , simplify=T)
