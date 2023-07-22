library(tidyverse)
install.packages("dplyr")

getwd()
setwd("C:/Users/indatech/Desktop/universidad/electiva ciencia de datos")
list.files()
amazon_sales = read.csv("Amazon Sale Report.csv")
View(file)

columns =  c("Status", "Fulfilment", "Category")
df_limpio = file[, columns]


any(is.na(df_limpio$Status))
any(is.na(df_limpio$Fulfilment))
any(is.na(df_limpio$Category))

#metricas 
# 1)Tasa de envC-o fallidos: 
#Representa en porcentualmente los envC-os cancelados sobre el total de envC-os
# columnas necesarias: -Status 

total_cancelados = sum(df_limpio$Status == "Cancelled")
total_cancelados

total_envios =  length(df_limpio$Status)
total_envios 

tasa_envios_cancelados = (total_cancelados/total_envios)*100
tasa_envios_cancelados

tasa_envios_correctos = 100 - tasa_envios_cancelados       
tasa_envios_correctos

porcentajes1 = c(tasa_envios_cancelados,tasa_envios_correctos)
categorias1 =  c("Envios cancelados","Envios confirmados")
etiquetas <- paste0(categorias1, " (", round(porcentajes1, 2), "%)")

par(mar = c(2, 2, 2, 2))
grafica1 = pie(porcentajes1,labels = etiquetas,main="EnvC-os")
grafica1


# 2)Tasa de envC-o realizado por  Merchant:
# Representa porcentualmente los despachos de envC-os realizados por Merchant
# sobre el total de despachos realizados.

total_Merchant = sum(df_limpio$Fulfilment == "Merchant")
total_Merchant

total_Amazon = sum(df_limpio$Fulfilment == "Amazon")
total_Amazon

total_envios =  length(df_limpio$Status)
total_envios 

tasa_envios_Merchant = (total_Merchant/total_envios)*100
tasa_envios_Merchant

tasa_envios_Amazon = (total_Amazon/total_envios)*100
tasa_envios_Amazon

porcentajes2 = c(tasa_envios_Merchant,tasa_envios_Amazon)
categorias2 =  c("Envios de Merchant","Envios de Amazon")
etiquetas2 <- paste0(categorias2, " (", round(porcentajes2, 2), "%)")

grafica2 = pie(porcentajes2,labels = etiquetas2,main="Fulfilment")
grafica2

# metrica 3
install.packages("ggplot2")
library(ggplot2)

df_amazon <- df_limpio[df_limpio$Fulfilment == "Amazon", ]
envios_por_amazon <- table(df_amazon$Category)

amazon <- ggplot(data =data.frame(Categoria = names(envios_por_amazon), Envios = envios_por_amazon) ,
                  mapping =  aes(x = factor(Categoria), y = factor(Envios.Freq))) +
                      geom_bar(stat = "identity", fill = "steelblue") +
                        xlab("CategorC-a") +
                         ylab("EnvC-os") +
                          ggtitle("EnvC-os por CategorC-a (Amazon)") +
                           theme_bw()


df_merchant <- df_limpio[df_limpio$Fulfilment == "Merchant", ]
envios_por_merchant <- table(df_merchant$Category)

merchant <- ggplot(data =data.frame(Categoria = names(envios_por_merchant), Envios = envios_por_merchant) ,
                   mapping =  aes(x = factor(Categoria), y = factor(Envios.Freq))) +
                    geom_bar(stat = "identity", fill = "green") +
                    xlab("CategorC-a") +
                    ylab("EnvC-os") +
                    ggtitle("EnvC-os por CategorC-a (Amazon)") +
                    theme_bw()
amazon
merchant


# install.packages("openxlsx")

#library(openxlsx)
#write.xlsx(df_limpio, "bd_limpia.xlsx")


################################################################################

print(unique(file$Sales.Channel))

print(unique(file$ship.service.level))

print(unique(file$Courier.Status))
print(unique(file$Status))

print(unique(file$B2B))

dates = as.Date(amazon_sales$Date, format = "%m-%d-%y")
class(dates)
max(dates)
min(dates)


# -	Cantidad de ordenes por aC1o (fecha) barra y pastel --------------------

amazon_sales = read.csv("Amazon Sale Report.csv")

#Convertir la columna "Date" a un objeto de fecha
amazon_sales$Date <- as.Date(amazon_sales$Date, format = "%m-%d-%y")

#Crear una nueva columna con el nombre del mes
amazon_sales$Month <- format(amazon_sales$Date, "%B")

#convertir la columna "Month" en un factor y 
#establecer el orden de los niveles segC:n el orden cronolC3gico de los meses:
library(dplyr)
amazon_sales <- amazon_sales %>%
  mutate(Month = factor(Month, levels = month.name))

# Obtener la cantidad de C3rdenes por mes
orders_by_month <- table(amazon_sales$Month)

# Crear un factor con los nombres de los meses en orden numC)rico
month_factor <- factor(names(orders_by_month), levels = month.name)

# Crear un grC!fico de barras para mostrar la cantidad de C3rdenes por mes
barplot(orders_by_month, main = "Cantidad de pedidos por mes", xlab = "Mes", ylab = "Cantidad de pedidos")


# ########################### ---------------------------------------------


# Cargar el dataset de ventas
amazon_sales = read.csv("Amazon Sale Report.csv")

# Agrupar los datos por mes y calcular la suma de las ventas
ventas_por_mes <- aggregate(amazon_sales$Cantidad, by = list(Mes = format(ventas$Fecha, "%B")), FUN = sum)

# Crear la grC!fica de barras
barplot(ventas_por_mes$x, names.arg = ventas_por_mes$Mes, xlab = "Meses", ylab = "Ventas", main = "Ventas por mes")

##############################################################################

library(tidyverse)

# Import the dataset
amazon_sales = read.csv("Amazon Sale Report.csv")

# Filter the data to only include records between March 31, 2022 and June 29, 2022
df <- df %>% filter(Date >= "2022-03-31") %>% filter(Date <= "2022-06-29")

# Create a new column that calculates the month of the sale
df <- df %>% mutate(Month = month(Date))

# Group the data by month and count the number of records in each group
monthly_sales <- df %>% group_by(Month) %>% tally()

# Create a bar chart that shows the number of records per month
barplot(monthly_sales$n, names = monthly_sales$Month, xlab = "Month", ylab = "Number of Records")


















##############################################################################
# -	Cantidad de ordenes por aC1o y meses barra y pastel
# install.packages("gganimate")
# install.packages("ggplot2")
# install.packages('gifski')
library(gifski)
library(gganimate)
library(dplyr)
library(ggplot2)
amazon_sales = read.csv("Amazon Sale Report.csv")
amazon_sales$Date <- as.Date(amazon_sales$Date, format = "%m-%d-%y")

amazon_sales$Date <- sort(amazon_sales$Date)

amazon_sales$Year <- format(amazon_sales$Date, "%Y")
amazon_sales$Month <- format(amazon_sales$Date, "%B")  

# Grafico de barra

orders_by_month <- amazon_sales %>%
  group_by(Year, Month) %>%
  count()%>%
  arrange(Year, match(Month, month.name))

orders_by_month$Month <- sort(orders_by_month$Month)

ggplot(orders_by_month, aes(x = paste(Year, Month, sep = "-"), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Cantidad de Ordenes por fecha",
       x = "fecha",
       y = "Cantidad de Ordenes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Grafico de torta

ggplot(orders_by_month, aes(x = "", y = n, fill = Month)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Cantidad de C3rdenes por mes en el aC1o 2022",
       fill = "Mes",
       x = NULL,
       y = NULL)



##############################################################################
# -	DistribuciC3n de las C3rdenes de compra segC:n su precio, por ejemplo, agrupando las C3rdenes en categorC-as de bajo, medio, 
# alto y muy alto precio

amazon_sales$Price_Category <- cut(amazon_sales$Amount, breaks = c(100, 500, 1000, 1500, Inf), labels = c("Bajo", "Medio", "Alto", "Muy Alto"))

ggplot(amazon_sales, aes(x = Amount)) +
  geom_histogram(binwidth = 50 , fill = "steelblue") +
  labs(title = "DistribuciC3n de C3rdenes de compra segC:n su precio",
       x = "Precio",
       y = "Cantidad de Crdenes")



##############################################################################
# -	CategorC-as mas compradas por fecha. (2022-03-31 a 2022-06-29) Plot-dispersion

start_date <- as.Date("2022-03-31")
end_date <- as.Date("2022-06-29")
filtro_fecha <- amazon_sales[amazon_sales$Date >= start_date & amazon_sales$Date <= end_date, ]

category_counts <- filtro_fecha %>%
  group_by(Date, Category) %>%
  summarize(Orders = n())


ggplot(category_counts, aes(x = Date, y = Category, size = Orders, color = Orders)) +
  geom_point() +
  scale_size_continuous(range = c(5, 15)) +
  labs(title = "CategorC-as mC!s compradas por fecha (2022-03-31 a 2022-06-29)",
       x = "Fecha",
       y = "CategorC-a",
       size = "Cantidad de Crdenes",
       color = "Cantidad de Crdenes")


##############################################################################

# -	Utilizando la columna "Category" como eje x y la columna "Amount" como eje y, se puede trazar un grC!fico de lC-neas 
# para comparar las ventas entre diferentes categorC-as de productos. Esto puede ayudar a identificar las categorC-as mC!s
# populares o las que generan mayores ingresos. 

# Crear el grC!fico de dispersiC3n
ggplot(amazon_sales, aes(x = Category, y = Amount)) +
  geom_point() +
  labs(title = "Ventas por categorC-a de productos",
       x = "CategorC-a de Producto",
       y = "Monto de Ventas")

ggplot(amazon_sales, aes(x = Category, y = Amount, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Ventas por categorC-a de productos (GrC!fico de LC-neas)",
       x = "CategorC-a de Producto",
       y = "Monto de Ventas")

##############################################################################

# -	Tendencia de ventas por estado de la India: Utilizando la columna "state" como eje x y la columna "Amount" 
# como eje y, se puede trazar un grC!fico de C!reas para mostrar la tendencia de las ventas en cada estado de la India.
# Esto puede ayudar a identificar los estados con mayor actividad de ventas

state_sales <- amazon_sales %>%
  group_by(ship.state) %>%
  summarize(Total_Sales = sum(Amount))

state_sales <- state_sales[complete.cases(state_sales), ]

ggplot(state_sales, aes(x = Total_Sales)) +
  geom_area(aes(y = ship.state, fill = Total_Sales), alpha = 0.5) +
  geom_point(aes(y = ship.state), color = "blue", size = 3) +
  labs(title = "Tendencia de ventas por estado de la India",
       x = "Ventas Totales",
       y = "Estado de la India")
##############################################################################
# -	DistribuciC3n de las ventas por tamaC1o de producto: Utilizando la columna "Size" como variable categC3rica y la columna "Amount" 
# como variable numC)rica, se puede crear un grC!fico de boxplot para mostrar la distribuciC3n de las ventas en cada tamaC1o de producto. 
# Esto puede ayudar a identificar los tamaC1os de producto con mayores ventas o con mayor variabilidad en las ventas

ggplot(amazon_sales, aes(x = Size, y = Amount)) +
  geom_boxplot() +
  labs(title = "DistribuciC3n de las ventas por tamaC1o de producto",
       x = "TamaC1o de Producto",
       y = "Ventas")



##############################################################################
# -	Ventas a lo largo del tiempo, por ejemplo, la cantidad de C3rdenes de compra por mes o trimestre

amazon_sales$Date <- as.Date(amazon_sales$Date, format = "%m-%d-%y")
amazon_sales$YearMonth <- format(amazon_sales$Date, "%Y-%m")

meses <- amazon_sales %>%
  group_by(YearMonth) %>%
  summarize(Orders = n())

ggplot(meses, aes(x = YearMonth, y = Orders, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  labs(title = "Cantidad de C3rdenes de compra por mes",
       x = "Mes",
       y = "Cantidad de Crdenes")



##############################################################################
# -	Estatus de courier (enviado, sin enviar, cancelado). 
data_status <- amazon_sales %>%
  group_by(Courier.Status) %>%
  summarize(Orders = n())

data_status[data_status == ""] <- NA

data_status <- na.omit(data_status)

ggplot(data_status, aes(x = Courier.Status, y = Orders, fill = Courier.Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de C3rdenes de compra por estado del courier",
       x = "Estado del Courier",
       y = "Cantidad de Crdenes",
       fill = "Estado del Courier")

ggplot(data_status, aes(x = "", y = Orders, fill = Courier.Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "ProporciC3n de C3rdenes de compra por estado del courier",
       x = NULL,
       y = NULL,
       fill = "Estado del Courier")


##############################################################################
# -	Cantidad de ordenes de compra por categoria

data_envio <- amazon_sales %>%
  group_by(Category) %>%
  summarize(Orders = n())


ggplot(data_envio, aes(x = Category, y = Orders, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Cantidad de C3rdenes de compra por categorC-a",
       x = "CategorC-a de EnvC-o",
       y = "Cantidad de Crdenes",
       fill = "CategorC-a de EnvC-o")

ggplot(data_envio, aes(x = "", y = Orders, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "ProporciC3n de C3rdenes de compra por categorC-a de envC-o",
       x = NULL,
       y = NULL,
       fill = "CategorC-a de EnvC-o")

# Estas son las nuevas graficas 
datos_sin_na <- na.omit(amazon_sales)


ggplot(datos_sin_na, aes(x = Date, y = Amount, color = Category)) +
  geom_point() +
  labs(title = "GrC!fico de DispersiC3n de Monto de Venta por Fecha",
       x = "Fecha de la Venta",
       y = "Monto de la Venta",
       color = "CategorC-a de Producto") +
  theme_minimal()



ganancia_por_region_y_categoria <- amazon_sales %>%
  group_by(ship.city, Category) %>%
  summarize(Ganancia_Total = sum(Amount))

ggplot(ganancia_por_region_y_categoria, aes(x = Category, y = Ganancia_Total, fill = ship.city)) +
  geom_bar(stat = "identity") +
  labs(title = "Ganancia Total por CategorC-a de Producto y RegiC3n",
       x = "CategorC-a de Producto",
       y = "Ganancia Total",
       fill = "Ciudad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

