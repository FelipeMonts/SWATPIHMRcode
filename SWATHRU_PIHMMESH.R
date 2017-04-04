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


setwd("C:/Felipe/PIHM-CYCLES/PIHM/PIHM_Felipe/CNS/WE-38/WE38_Files_PIHM_Cycles20170208/SWATPIHMRcode") ; 


###############################################################################################################
#                         Call packages neded to process the data 
#                             
###############################################################################################################

library("foreign") ;



###############################################################################################################
#                         Read the database and select fields
###############################################################################################################


Int_HRU_Mesh<-read.dbf('../SwatPIHM/WE38IntersectionMeshHRU_Calc.dbf', as.is=T ) ;




head(Int_HRU_Mesh) 

str(Int_HRU_Mesh)

names(Int_HRU_Mesh)


############### check if all the trinagles number sequence exist


Triangles.numbers.missing<-which(!c(1:882) %in% as.numeric(levels(as.factor(Int_HRU_Mesh$FID_NTPIHM))))   ;



# ############# code to use one triangel to test results

# Triangle.no<-799
# OneTirangle<-Int_HRU_Mesh[Int_HRU_Mesh$FID_NTPIHM==Triangle.no,] ;
# 
# aggregate(formula=Shape_Area~LU_CODE, data=OneTirangle, FUN=sum, simplify=T)

Int_HRU_Mesh$LU_CODE<-as.factor(Int_HRU_Mesh$LU_CODE);

LU.Area.Mesh<-aggregate(formula=Shape_Area~LU_CODE+FID_NTPIHM, data=Int_HRU_Mesh, FUN=sum, simplify=T) ;

head(LU.Area.Mesh)
str(LU.Area.Mesh)

LU.Area.Mesh.max<-aggregate(formula=Shape_Area~FID_NTPIHM,data=LU.Area.Mesh, FUN = max, simplify=T);

str(LU.Area.Mesh.max)
head(LU.Area.Mesh.max)


LU.Area.Mesh.Dominant<-LU.Area.Mesh[LU.Area.Mesh$Shape_Area %in% LU.Area.Mesh.max$Shape_Area,]


#  LU.Area.Mesh.Dominant[LU.Area.Mesh.Dominant$FID_NTPIHM==Triangle.no,]



