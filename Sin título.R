#libreries
#APLpack
#hotelling

#faces
datos_caras <- datos[1:21,c(2, 13:22)]

caras_mujer <- subset(datos_caras, sexo == "F")
caras_hombre <- subset(datos_caras, sexo == "M")

caras_mujer_cont <- caras_mujer[2:10]
caras_hombre_cont <- caras_hombre[2:10]

caras_mujer_prom <- colMeans(caras_mujer_cont)
caras_hombre_prom <- colMeans(caras_hombre_cont)

caras_hm <- rbind(caras_hombre_prom, caras_mujer_prom)

caras <- faces(caras_hm) 

#hotelling
hot_stat <- hotelling.stat(caras_hombre_cont, caras_mujer_cont)
hot_test <- hotelling.test(caras_hombre_cont, caras_mujer_cont)

#manova
man <- manova(cbind(a_nariz, s_ojos, labios, l_oreja, l_frente, c_cuello, l_cejas, l_cara, a_ojo,
                    c_cabeza) ~ sexo, data = datos)
#cluster
datos_cluster <- datos[1:21, 7:22]
row.names(datos_cluster) <- datos[1:21, 1]
matris_dist <- dist(datos_cluster)
cluster <- hclust(matris_dist)
p <- plot(as.dendrogram(cluster))
