library(ggplot2)
library(dplyr)
library(gridExtra)

quacks <- read.csv("quacks.csv")
qSummary <- read.csv("qSummary.csv")
quackInfo <- read.csv("quackInfo.csv")
players <- read.csv("players.csv")

# figura 4 repeticiones

svg("04_repeticiones.svg", width=12, height=6)
ggplot(players, aes(x=Runs)) + geom_histogram(binwidth=1, col="skyblue4", fill="skyblue2") + theme_bw() + labs(x="N. repeticiones", y="Jugadores") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()

# figura 5 heatmap de partidas vs repeticiónv s resultados
runs <- subset(qSummary, run<16)
groupedRuns <- runs %>% group_by(run, quacker1Stars) %>% tally()

svg("05_iter_resul.svg", width=15, height=6)

ggplot(groupedRuns, aes(x=run, y=quacker1Stars, fill=n, label=n)) + geom_tile(col="white") + geom_text(size=7, colour='grey10') + scale_fill_distiller(name='# de partidas', type='seq',palette = "Blues", direction=-1)+theme_minimal() + theme(legend.position="none") + xlab("n. iteración") + ylab("resultado")+ theme(axis.ticks = element_blank(), panel.background = element_blank()) + coord_fixed()+theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"))

dev.off()


# figura 6 reacción quack

### Plot perc actions per quack
datint <- as.data.frame(prop.table(table(quacks$quackId, quacks$quackAction),1)*100)
colnames(datint) <- c("quackId", "Action", "Freq")
datint <- left_join(datint, quackInfo[,c("quackId", "summaryEs", "isFakeNews")], by="quackId")
ratioFakeNews <- subset(datint, Action=="REPORT")
colnames(ratioFakeNews) <- c("quackId", "Action", "percFakeNews", "isFakeNews")
datint <- left_join(datint, ratioFakeNews[,c("quackId", "percFakeNews")], by="quackId")
datint$isFakeNews[is.na(datint$isFakeNews)] <- 0 ; datint$isFakeNews <- as.factor(as.character(datint$isFakeNews))
datint$isFakeNewsText <- ifelse(datint[,"isFakeNews"]=="true", "fake news", "otro contenido")

svg("06_reacciones.svg", width=14, height=7)
g1 <- ggplot(subset(datint, isFakeNews=="true"), aes(x=reorder(summaryEs, percFakeNews), y=Freq, fill=Action)) + geom_bar(stat="identity") + coord_flip() + theme_bw() + labs(title="fake news", x = "contenido Quack", y = "% de reacción", fill = "Tipo de reacción") + theme(legend.position="bottom") + scale_fill_brewer(palette="Accent") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), strip.text.y=element_text(size=14))
g2 <- ggplot(subset(datint, isFakeNews=="false"), aes(x=reorder(summaryEs, percFakeNews), y=Freq, fill=Action)) + geom_bar(stat="identity") + coord_flip() + theme_bw() + labs(title="otro contenido", x = "contenido Quack", y = "% de reacción", fill = "Tipo de reacción") + theme(legend.position="none") + scale_fill_brewer(palette="Accent") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), strip.text.y=element_text(size=14))
grid.arrange(g1,g2, ncol=2)
dev.off()

# figura 7  meanReadingTime

svg("07_reading.svg", width=12, height=7)
ggplot(runs, aes(x=run, y=meanReadingTime, group=run, fill=as.factor(quacker1Stars))) + geom_boxplot() + facet_wrap(~quacker1Stars) + theme_bw() + theme(legend.position="none") + ylim(c(0,15)) + labs(x="n. iteración", y="tiempo medio de lectura") + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

dev.off()

