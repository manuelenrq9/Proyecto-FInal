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

