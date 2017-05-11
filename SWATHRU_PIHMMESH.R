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
#                         Call packages needed to process the data 
#                             
###############################################################################################################

library("foreign") ;
library("dplyr") ;



###############################################################################################################
#                         Read the database of PIHM Mesh and Swat HRU intersection polygons and 
#                        select the appropriate fields
###############################################################################################################


Int_HRU_Mesh<-read.dbf('../SwatPIHM/WE38MeshIntersectionHRUManhantango.dbf', as.is=T ) ;




head(Int_HRU_Mesh) 

str(Int_HRU_Mesh)

names(Int_HRU_Mesh)


############### check if all the trinagles number sequence exist


Triangles.numbers.missing<-which(!c(1:883) %in% as.numeric(levels(as.factor(Int_HRU_Mesh$Ele_ID))))   ;

Triangles.numbers.missing

# ############# code to use one triangle to test results

# Triangle.no<-3
# OneTirangle<-Int_HRU_Mesh[Int_HRU_Mesh$Ele_ID==Triangle.no,] ;
# 
# aggregate(formula=TriaHruAre~LU_CODE, data=OneTirangle, FUN=sum, simplify=T)


########### Make LU_CODE as a factor to differentiate between land Use types

Int_HRU_Mesh$LU_CODE<-as.factor(Int_HRU_Mesh$LU_CODE);


############## find the total area of each LU_CODE inside each trinagle of the PIHM mesh by summing  
############## the areas of each different LU_CODE level inside each trinagle of the PIHM mesh identified in the field Ele_ID


LU.Area.Mesh.count<-aggregate(formula=TriaHruAre~Ele_ID+LU_CODE, data=Int_HRU_Mesh, FUN=length, simplify=T) ;
head(LU.Area.Mesh.count[order(LU.Area.Mesh.count$Ele_ID),],50);

LU.Area.Mesh.sum<-aggregate(formula=TriaHruAre~Ele_ID+LU_CODE, data=Int_HRU_Mesh, FUN=sum, simplify=T) ;

head(LU.Area.Mesh.sum[order(LU.Area.Mesh.sum$Ele_ID),],50) ;
str(LU.Area.Mesh.sum)


############# Find the LU_CODE level with the maximum total area within each PIHM MESH triangle

#####split the area sumation into groups determined by each triangle, then find the maximum area sumation within each 
# group (trinagle)

temp.1<-lapply(split(LU.Area.Mesh.sum,LU.Area.Mesh.sum$Ele_ID),function(x) x[which.max(x$TriaHruAre),]) ;
head(temp.1)

#take the information of maximum area sumation for each trinagle and grouped together in a dataframe
LU.Area.Mesh.Dominant<-do.call(rbind.data.frame, temp.1)  ;
head(LU.Area.Mesh.Dominant,20);




#  LU.Area.Mesh.Dominant[LU.Area.Mesh.Dominant$Ele_ID==Triangle.no,]


############# Extract the area of each triangle 

Mesh.Triangle.Area<-read.dbf('../SwatPIHM/NTPIHM_Mesh20170228.dbf', as.is=T ) ;

head(Mesh.Triangle.Area)
str(Mesh.Triangle.Area)


########### Merge the LU.Area.Mesh.Dominant and the Mesh.Triangle.Area data frames


Mang.Triangle<-merge(Mesh.Triangle.Area,LU.Area.Mesh.Dominant, by="Ele_ID")  ;

head(Mang.Triangle)

str(Mang.Triangle)

Mang.Triangle$Fraction<-Mang.Triangle$TriaHruAre/Mang.Triangle$TriangArea  ;

names(Mang.Triangle)[1]<-c("TriangleNo")

head(Mang.Triangle,20)

######### write out the file with the dominant land use (management) for each tringle


write.table(Mang.Triangle[,c( "TriangleNo" ,"LU_CODE", "Fraction" )],"../SwatPIHM/WE38_Triangle_LU_2.txt", sep="\t", quote=F, row.names = F);



###### compare the  outputs of the previous version of the algorithm with the current version to see if there are some other errors


WE38_Triangle_LU<-read.table("../SwatPIHM/WE38_Triangle_LU.txt", header=T, as.is=T);
head(WE38_Triangle_LU);
str(WE38_Triangle_LU);


WE38_Triangle_LU_2<-read.table("../SwatPIHM/WE38_Triangle_LU_2.txt" , header=T, as.is=T);
head(WE38_Triangle_LU_2);
str(WE38_Triangle_LU_2);

Errors<-WE38_Triangle_LU[!WE38_Triangle_LU$Fraction %in% WE38_Triangle_LU_2$Fraction,] ;

Errors_2<-anti_join(WE38_Triangle_LU,WE38_Triangle_LU_2, by=NULL);


##### Merge the land use with the trinagle mesh for display in QGIS

Mesh.Triangle.Lu<-merge(Mesh.Triangle.Area,WE38_Triangle_LU_2, by.x="Ele_ID",by.y="TriangleNo");
head( Mesh.Triangle.Lu)


write.dbf(Mesh.Triangle.Lu,"../SwatPIHM/NTPIHM_Mesh20170228Copy.dbf");
