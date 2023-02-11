puffinbill <- read.csv("/Users/Kornel/Desktop/puffinbill.csv")
view(puffinbill)

# Poniewa¿ regresja logistyczna polega na tym
# ¿e dopasowujemy modele w których mamy jedynki i zera,
# przekodujemy sexcode, jak 1 to female, jak 0 to male, 
# do tego s³u¿y funkcja ifelse ¿eby wyprodukowaæ ten sexcode

sex<- puffinbill$sex
curlen<- puffinbill$curlen
sexcode<- ifelse(sex == "F", 1, 0)

# przedstawiamy wykres z podpisanymi osiami oraz wartoœciami 0-male i 1-female

plot(curlen, jitter(sexcode, 0.15), pch = 19,
     xlab = "Bill length (mm)", ylab = "Sex (0 - male, 1 - female)")
 

# Dopasowujemy generalized linear model (Ogólny model liniowy),
# który jest modelem regresji liniowej sexcode jest binarn¹ odpowiedzi¹ 0 lub 1,
# predyktor jest tutaj ciaglym pojedynczym predyktorem to curlen.
# Mamy do czynienia z dwumianow¹ zmienn¹
# lines() rysuje sigmoidaln¹ krzyw¹, która zosta³a wpasowana do naszych danych, 
# te dane mog¹ powstaæ tylko jako binarne wyniki (0 albo 1)
# Krzywa jest modelem propabilistycznym, który jest najbardziej sk³onny do generowania
# estymatorów które zgenerowa³y obserwacje kobiet i mê¿czyzn/

model <- glm(sexcode~curlen, binomial)     
summary(model)

## Odrzucamy null hypothesis (hipoteze zerow¹), poniewa¿ bill length nie ma wp³ywu na p³eæ(sex)
## oraz na 1 i 0, poniewa¿ prawdopobienstwo uzyskania tej statystyki, jeœli hipoteza zerowa
## jest prawdziwa jest naprawdê bardzo ma³a wiec odrzucamy hipotezê zerow¹.

xv<- seq(min(curlen), max(curlen), 0.01)
yv<-predict(model,list(curlen=xv),type="response")
lines(xv, yv, col ="red")

# Mo¿emy stworzyæ bardziej wyrafinowany wykres, który
# pokazuje czêstoœæ rozmieszczenia obserwacji

install.packages("popbio")
library(popbio)
logi.hist.plot(curlen,sexcode,boxp=FALSE,type="count",col="gray", xlabel ="size")
