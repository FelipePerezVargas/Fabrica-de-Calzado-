library(ggplot2)

vs <- read.table("/Users/Admin/Documents/Perugia/Enero/vs.csv"
                   ,header = TRUE, sep = ",");vs
# Basic barplot
p<-ggplot(data=vs, aes(x=vs[,1], y=vs[,2])) +
  geom_bar(stat="identity")
p + coord_flip()
p

# Horizontal bar plot


# Change the width of bars
ggplot(data=vs, aes(x=vs[,1], y=vs[,2])) +
  geom_bar(stat="identity", width=0.5)
# Change colors
ggplot(data=vs, aes(x=vs[,1], y=vs[,2])) +
  geom_bar(stat="identity", color="blue", fill="white")
# Minimal theme + blue fill color
p<-ggplot(data=vs, aes(x=vs[,1], y=vs[,2])) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()
p

p + scale_x_discrete(limits=c("MONTADO3", "RMONTADO3"))



ggplot(data=vs, aes(x=vs[,1], y=vs[,2])) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=vs[,2]), vjust=-0.3, size=3.5)+
  theme_minimal()
# Inside bars
ggplot(data=vs, aes(x=vs[,1], y=vs[,2])) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=vs[,2]), vjust=1.6, color="white", size=3.5)+
  theme_minimal()

ggplot(vs, aes(x=factor(vs[,1])))+
  geom_bar(stat="bin", width=0.7, fill="steelblue")+
  theme_minimal()

p<-ggplot(data=vs, aes(x=vs[,1], y=vs[,2], color=vs[,1])) +
  geom_bar(stat="identity", fill="white")
p

p + scale_x_discrete(limits=c("MONTADO2", "RMONTADO2", "MONTADO3", "RMONTADO3","MONTADO4", "RMONTADO4"))


df2 <- data.frame(Calidad=rep(c("REPROCESOS","PARES"), each=3),
                  Departamento=c("MONTADO2", "MONTADO3", "MONTADO4", "MONTADO2","MONTADO3", "MONTADO4"), Cantidad=vs[,2])
head(df2)
library(plyr)
df_sorted <- arrange(df2, Departamento, Cantidad) 
head(df_sorted)

df_cumsum <- ddply(df_sorted, "Departamento",
                   transform, label_ypos=cumsum( Cantidad))
head(df_cumsum)

ggplot(data=df_cumsum, aes(x=Departamento, y=Cantidad, fill=Calidad, color=Departamento)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=label_ypos, label=Cantidad), hjust=1.6, 
            color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  coord_flip()+
  theme_minimal()
###########################################TArea ####################
library(plotly)
library(dplyr)

df <- structure(c(106487, 495681, 1597442, 2452577, 2065141, 2271925, 4735484, 3555352, 8056040, 4321887, 2463194, 347566, 621147, 1325727, 1123492, 800368, 761550, 1359737, 1073726, 36, 53, 141, 41538, 64759, 124160, 69942, 74862, 323543, 247236, 112059, 16595, 37028, 153249, 427642, 1588178, 2738157, 2795672, 2265696, 11951, 33424, 62469, 74720, 166607, 404044, 426967, 38972, 361888, 1143671, 1516716, 160037, 354804, 996944, 1716374, 1982735, 3615225, 4486806, 3037122, 17, 54, 55, 210, 312, 358, 857, 350, 7368, 8443, 6286, 1750, 7367, 14092, 28954, 80779, 176893, 354939, 446792, 33333, 69911, 53144, 29169, 18005, 11704, 13363, 18028, 46547, 14574, 8954, 2483, 14693, 25467, 25215, 41254, 46237, 98263, 185986), .Dim = c(19, 5), .Dimnames = list(c("1820-30", "1831-40", "1841-50", "1851-60", "1861-70", "1871-80", "1881-90", "1891-00", "1901-10", "1911-20", "1921-30", "1931-40", "1941-50", "1951-60", "1961-70", "1971-80", "1981-90", "1991-00", "2001-06"), c("Europe", "Asia", "Americas", "Africa", "Oceania")))
df.m <- melt(df)
df.m <- rename(df.m, Period = Var1, Region = Var2)

p <- ggplot(df.m, aes(x = Period, y = value/1e+06,fill = Region)) + ggtitle("Migration to the United States by Source Region (1820-2006), In Millions")
p <- p + geom_bar(stat = "identity", position = "stack")

p <- ggplotly(p)

p

