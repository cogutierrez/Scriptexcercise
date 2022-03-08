
# Presentación: Evaluación 1-R --------------------------------------------

#Análisis de comportamiento retail

#Magíster en Ciencias Sociales mención Sociología de la Modernización
#Profesor: Sebastián Massa Slimming - smassas@ug.uchile.cl
#Estudiantes: Sergio Araya, Felipe Donoso, Camila Espinoza y Constanza Gutiérrez

# Cargar paquetes ---------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)


# Información de las bases a trabajar -------------------------------------

#Clust: Base que contiene información montos vendidos según sucursal.

#sem_cmc: Semana comercial.
#id_clus: Identificador del cluster.
#S1: Plaza Tobalaba.
#S2: Parque Arauco.
#S3: Costanera Center.
#S4: Alto Las Condes.
#S5: Los Dominicos.
#S6: La Florida.

#Clientes: Base que contiene información sobre características de clientes.

#customer_id: Identificador del cliente.
#fecha_nacimiento: Expresada solo en año, sin considerar día ni mes.
#genero: Descripción de género cliente (M = Masculino; F = Femenino).
#historial: Historial de compra (última) contabilizada en días.
#compra: Tipo de compra, ya sea vía internet o sucursal.
#comuna: Comuna de residencia cliente.

#Tarjetas: Base que contiene característica de tarjetas de clientes. Variables adicionales (continuación) a la base de clientes.

#cupo_disponible: Cupo que posee tarjeta de crédito.
#pendiente_pago: Deuda pendiente de pago.
#tarjetas: Número de tarjetas.
#avance_efect: Solicita o no avance en efectivo (0 = No solicita; 1 = Solicita).
#solicitud: Si ha solicitado o no, un crédito anteriormente (0 = Nunca; 1 = Sí).
#cuotas: Número de cuotas al momento de comprar producto.
#repos: Solicitud de reposición de producto (0 = No reposición; 1 = Reposición).
#dev: Devolución de producto (0 = No devolución; 1 = Devolución).

#Transacciones:Base que contiene los movimientos de los clientes.

#transaccion_id: Identificador de transacción.
#cust_id: Identificador del cliente.
#data_trans: Año de transacción.
#sucursal: Sucursal donde se realiza transacción
#total_compra: Suma total compra.
##categoria: Categoría producto.


# Cargar base de datos ----------------------------------------------------

library(readxl)
Clientes <- read_xls(path = "C:/Users/Constanza Gutiérrez/Desktop/Magíster/Tercer semestre/Martes Herramientas/Evaluaciones/Prueba en grupo/data/data.xls",
                 sheet="Clientes")
Clientes
       
Clust <- read_xls(path = "C:/Users/Constanza Gutiérrez/Desktop/Magíster/Tercer semestre/Martes Herramientas/Evaluaciones/Prueba en grupo/data/data.xls",
                 sheet="Clust")

Clust

Tarjeta <- read_xls(path = "C:/Users/Constanza Gutiérrez/Desktop/Magíster/Tercer semestre/Martes Herramientas/Evaluaciones/Prueba en grupo/data/data.xls",
                 sheet="Tarjeta")

Tarjeta

Transacciones<- read_xls(path = "C:/Users/Constanza Gutiérrez/Desktop/Magíster/Tercer semestre/Martes Herramientas/Evaluaciones/Prueba en grupo/data/data.xls",
                  sheet="Transacciones")

Transacciones

data <- read_xls(path = "C:/Users/Constanza Gutiérrez/Desktop/Magíster/Tercer semestre/Martes Herramientas/Evaluaciones/Prueba en grupo/data/data.xls",
                 sheet="Clust", "Clientes", "Tarjeta", "Transacciones")

data

# Desarrollo de las preguntas ---------------------------------------------

#1.	Visualizar estructura y tipo de variables de cada hoja. Cambiar formato de variables en caso de ser necesario.
str(Clientes)
str(Clust)
str(Tarjeta)
str(Transacciones)

#2.	Renombrar variables S1…Sn atendiendo la definición real del nombre de la variable (base clust)
Clust %>% 
  rename(
    "Semana Comercial"="sem_cmc",
    "Identificador del cluster"="ID_clust",
    "Plaza Tobalaba" ="S1",
    "Parque Arauco" ="S2",
    "Costanera Center" ="S3",
    "Alto Las Condes"="S4",
    "Los Dominicos" ="S5",
    "La Florida" = "S6"
     )

#3.	Crear variable edad, sobre la base de la fecha de nacimiento (base clientes).
Hoy<-Sys.Date()
AñoHoy<-as.numeric(format(Hoy,"%Y"))
Clientes<-Clientes %>% mutate(Edad= AñoHoy-fecha_nacimiento)
Clientes

#4.	Crear variable, en la base clientes, que permita separar a los clientes por grupo de edad (“menores de 30”, “entre 30 y 40 años” y “mayores de 40”).
Clientes<-Clientes %>% mutate(Grupoedad=if_else(Edad < 30 ,"menores de 30",
                                                if_else(Edad >= 30 & Edad <=40,
                                                        "entre 30 y 40 años",
                                                        "mayores de 40")))
Clientes %>% filter(Edad==30 & Edad==40) %>% select(Edad,Grupoedad)
Clientes

#5.	Cruzar la base clientes con base transacciones, mediante el identificador del cliente.
ClientesTransacciones<-left_join(Clientes,Transacciones,
                                 by=c("customer_Id"="cust_id"))
ClientesTransacciones

#6.	Identificar número de ventas por año según sucursal (base transacciones)
Transacciones %>% group_by(data_trans,sucursal) %>% count()

#7.	¿Qué comuna es la que menos realiza compras por internet? (base clientes)
Clientes %>% group_by(comuna,compra) %>%
  count() %>%
  filter(compra=="Internet") %>%
  mutate(n=as.numeric(n)) %>% 
  arrange(n) %>%  
  head(1)
str(Tarjeta)
str(Clientes)
str(Transacciones)

#8.	¿Qué comuna es la que posee mayor deuda promedio? (base clientes y base tarjetas)
ClientesTarjetas<-Clientes %>% bind_cols(Tarjeta) 
ClientesTarjetas %>% group_by(comuna) %>% 
  summarize(Deudacomuna=mean(pendiente_pago)) %>%
  arrange(desc(Deudacomuna)) %>% 
  head(1)

#9.	Suma de deuda de clientes (pendiente de pago) según tipo de compra (base clientes y base tarjetas).
ClientesTarjetas %>% group_by(compra) %>% summarize(Deudatotal=sum(pendiente_pago))

#10. Promedio de cupo total, desviación estándar y mediana por género (base clientes y base tarjetas).
ClientesTarjetas %>% group_by(genero) %>% 
  summarize(promcupo=mean(cupo_disponible),
            DS=sd(cupo_disponible),
            mediana=median(cupo_disponible)) %>% 
  na.omit()

#11. ¿Qué comuna es la que tiene mayor monto promedio de avance en efectivo? Considerar solo los clientes que solicitaron avance (base clientes y base tarjetas).
ClientesTarjetas %>% filter(avance_efect==1) %>% 
  group_by(comuna) %>%
  summarize(promedio_avance_comuna = mean(monto_avance)) %>% 
  arrange(promedio_avance_comuna) %>% 
  head(1)

#12. Determinar el monto total de la tarjeta de crédito, considerando el total de la compra, cupo total disponible y saldo pendiente de pago (base tarjeta y base transacciones).
ClientesTarjetasTransacciones<-left_join(ClientesTarjetas,Transacciones,
                                         by=c("customer_Id"="cust_id"))
ClientesTarjetasTransacciones %>% filter(tarjetas==1) %>%
  summarize(cupototal=sum(cupo_disponible),
            compratotal=sum(total_compra),
            saldototal=sum(pendiente_pago))


#13.¿Cuántos clientes, de edad superior a 27 años, realizaron compra vía internet, según género? (base clientes)
Clientes %>% filter(Edad>27,compra=="Internet") %>% 
  group_by(genero) %>% count()

#14.¿Qué grupo de edad es el que más ha comprado en la sucursal de Los Dominicos y Parque Arauco? (base clientes y transacciones).
ClientesTransacciones %>% 
  filter(sucursal %in% c("Alto Las Condes","Parque Arauco")) %>% 
  group_by(Grupoedad) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(1)

#15.¿Qué sucursal es la que más ha vendido en la categoría moda mujer? Ordenar de acuerdo a ranking por compras totales (base transacciones).
Transacciones %>% filter(categoria=="Moda Mujer") %>% 
  group_by(sucursal) %>% 
  summarize(total_compra=sum(total_compra)) %>% 
  arrange(desc(total_compra)) %>% 
  head(1)

#16.Número de clientes que solicitaron reposición o devolución del producto, según tipo de compra. (base clientes y base tarjetas)
ClientesTarjetas %>% filter(dev==1|repos==1) %>% 
  group_by(compra) %>% count()

#17.¿Cuántos clientes, de los que sí poseen tarjeta, solicitan un avance en efectivo? (base clientes y tarjetas) 
ClientesTarjetas %>% filter(tarjetas==1&avance_efect==1) %>% nrow()

#18.¿Cuál es la comuna de residencia (de los clientes) que más visitan la sucursal Parque Arauco? (base clientes y base transacciones).
ClientesTransacciones %>% group_by(comuna) %>%
  filter(sucursal=="Parque Arauco") %>% 
  count() %>% 
  arrange(desc(n)) %>% head(1)

#19.Proporción de clientes cuyo cupo total sea mayor al millón de pesos, según género (base clientes y base tarjetas).
a<-ClientesTarjetas %>% filter(cupo_disponible>1000000) %>% 
  group_by(genero) %>% count()
ClientesTarjetas %>% filter(cupo_disponible>1000000) %>% 
  group_by(genero) %>% count() %>% mutate(proporción=n/(a[1,2]+a[2,2]))

#20.Del total de compras realizadas por internet, ¿cuántas solicitudes de devolución de producto hay? (base clientes y base tarjetas).
ClientesTarjetas %>% filter(compra=="Internet",dev==1) %>% count()

#21.¿Cuál es la categoría que vende más, según sucursal? (base transacciones)
Transacciones %>% group_by(sucursal,categoria) %>% 
  count() %>% 
  arrange(desc(n)) %>%
  head()

#22.Monto medio del cupo total que posee los clientes de género masculino (base clientes y base tarjetas)
ClientesTarjetas %>% filter(genero=="M")%>% summarize(genero="M",mediacupo=median(cupo_disponible),
                                                      promedio=mean(cupo_disponible))

#23.¿Cuántas mujeres, cuya fecha de nacimiento es 1989, solicitaron avance en efectivo? (base clientes y base tarjetas)
ClientesTarjetas %>% filter(genero=="F",fecha_nacimiento=="1989",avance_efect==1) %>% 
  count() %>% mutate(tipo="mujeresnacidas1989solicitanavance")

#24.¿Cuántos clientes (tanto hombres como mujeres), provenientes de las comunas de Puente Alto, Quilicura y Lo Prado, solicita avance en efectivo? (base clientes y tarjetas).
ClientesTarjetas %>% filter(comuna%in%
                              c("Puente Alto","Quilicura"," Lo Prado"),
                            avance_efect==1) %>% 
  count()

#25.Transformar la base clust para que las sucursales figuren en filas (en vez de columnas).
trasponerclust<-data.frame(t(Clust[-1]))
names(trasponerclust)<-t(Clust[1])
Clust
trasponerclust

