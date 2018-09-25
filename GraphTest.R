head(tabla_clientes)
library(plyr)
#organizando datos
GerenalMes <- ddply(tabla_clientes, c("Mes"), summarise, Promedio = mean(Ventas), Sd = sd(Ventas), se = Sd/sqrt(3))

#Analysis del promedio y la variación observada
ggplot(GerenalMes, aes(x=Mes, y=Promedio, group = 1)) + geom_ribbon(aes(ymin = Promedio-Sd, ymax = Promedio + Sd), alpha = .4, fill = "grey70") + geom_line() +scale_y_continuous(labels = dollar)

# Analisis del promedio y la variación en función de la cantidad de datos
ggplot(GerenalMes, aes(x=Mes, y=Promedio, group = 1)) + geom_ribbon(aes(ymin = Promedio-se, ymax = Promedio + se), alpha = .4, fill = "grey70") + geom_line() +scale_y_continuous(labels = dollar)

# Tendencia general de ventass
ggplot(tabla_clientes, aes(Mes, Ventas, group = 1)) + geom_point() + stat_smooth(method=lm, level = 0.99)+scale_y_continuous(labels = dollar)

# Distribuición de densidad de los valores de venta
ggplot(tabla_clientes, aes(x=Ventas)) + geom_line(stat="density")+expand_limits(y=0)+scale_x_continuous(labels = dollar)

# Distribuición de los valores de venta
ggplot(tabla_clientes, aes(x=Ventas)) + geom_histogram(bins = 20, fill="white", colour="black")+scale_x_continuous(labels = dollar)

# Analisis por mes. Igual al grafico de linea
ggplot(tabla_clientes, aes(x=Mes, y=Ventas)) + geom_boxplot()

# Analysis de tendencia lineald e cada cliente
ggplot(tabla_clientes, aes(Mes, Ventas, fill = Cliente, colour = Cliente, group = Cliente)) + geom_point() + stat_smooth(method=lm, level = 0.9)+scale_y_continuous(labels = dollar)
