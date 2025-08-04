library(openxlsx) library(stats) library(factoextra)

#inisialisasi data
Data = read.xlsx("C:/Users/ACER SWIFT/OneDrive/Documents/Praktikum Semester 6/Analisis Multivariat/Data Bencana Alam 2021 (Data TUBES).xlsx", sheet = "Data kel 4" )
data = Data[,2:6] summary(data)
dataclus = as.matrix(data) dataclus

#normalisasi data min_max_normalization=function(data){ min_values = apply(data, 2, min) max_values = apply(data, 2, max)
normalized_data = (data-min_values)/(max_values-min_values) return(normalized_data)}
#melakukan min-max normalization pada data set.seed(27)
normalized_data = min_max_normalization(dataclus) normalized_data
dataclustering = as.matrix(normalized_data)

#COMPLETE LINKAGE
#PEMERIKSAAN ASUMSI mssing value dan Uji Multiko summary(is.na(dataclustering))
library(car) cor(dataclustering)

# Menghitung jarak euclidean
dist_matrix <- dist(x = dataclustering, method = "euclidean") dist_matrix1 = as.matrix(dist_matrix) write.xlsx(dist_matrix1, "Normalisasi.xlsx")
getwd()

# Penentuan K optimum
fviz_nbclust (dataclustering, hcut, method = "silhouette")

# Melakukan clustering hierarki dengan metode complete linkage hc_complete <- hclust(dist_matrix, method = "complete")

# Visualisasi dendrogram
# Plot dendrogram untuk visualisasi hasil clustering
plot(hc_complete, labels = rownames(Data$Provinsi), main = "Dendrogram Complete Linkage Clustering")
rect.hclust(hc_complete,3, border = 2:6)
#melihat anggota
cluster = cutree(hc_complete,3)
cluster
#menghitung silhoutte score
silhoutte_score = silhouette(cluster, dist_matrix) fviz_silhouette(silhoutte_score)

#creater a new data frame with data data points
clustered_datapp <- data.frame(Provinsi = Data$Provinsi,
                               cluster=factor(cluster))
clustered_datapp
datacluster = clustered_datapp
#karakteristik cluster
aggregate(data, list(clustered_datapp$cluster),mean)

#Pemetaan library(leaflet) library(ggplot2) library(rgdal) library(spdep) library(raster) library(gdata)

#Pemetaan
setwd("C:/Users/ACER SWIFT/OneDrive/Documents/Praktikum Semester 6/Analisis Multivariat")
datashp	=	readOGR(dsn	=	"C:/Users/ACER SWIFT/OneDrive/Documents/Praktikum Semester 6/Analisis Multivariat/BATAS PROVINSI DESEMBER 2019 DUKCAPIL",
layer = "BATAS_PROVINSI_DESEMBER_2019_DUKCAPIL")

indonesia_shp	<-
datashp[c(1,34,32,26,8,33,4,19,17,18,6,9,10,5,11,3,2,22,23,12,14,13,15,16,31,29, 28,30,7,27,20,21,25,24),]

text(indonesia_shp, "PROVINSI", cex=0.5) indonesia_shp@data<-
cbind(KAB_KOTA=datacluster[,1],S.I=datacluster[,2],indonesia_shp@data) sp.label <- function(x, label) {list("sp.text", coordinates(x), label,cex=0.5)} NUMB.sp.label <- function(x) {sp.label(x, as.vector(x@data$PROVINSI))} make.NUMB.sp.label <- function(x) {do.call("list", NUMB.sp.label(x))}

spplot(indonesia_shp[,2],sp.layout=make.NUMB.sp.label(indonesia_shp), cex=0.5,col="black", col.regions=c("yellow", "#E74C3C", “green”))


