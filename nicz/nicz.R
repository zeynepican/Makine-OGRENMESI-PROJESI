install.packages("readxl")
library(readxl)
install.packages('caTools')
library(caTools)
install.packages("class")
library(class)

# Veri setini okuma ve eksik veri kontrol??
nicz <- read_excel("nicz.xlsx")

colSums(is.na(nicz))

# Ayk??r?? de??erleri temizleme
boxplot(nicz$Area)
boxplot(nicz$Perimeter)
boxplot(nicz$EquivDiameter)

y=which(nicz$Area %in% boxplot.stats(nicz$Area)$out)
z=which(nicz$Perimeter %in% boxplot.stats(nicz$Perimeter)$out)
x=which(nicz$EquivDiameter %in% boxplot.stats(nicz$EquivDiameter)$out)
birlik = union(y,z)
birlikte=union(birlik,x)
nicz=nicz[-birlikte,]
 #ayk??r?? de??erleri sildikten sonraki veri seti
table(nicz$Class)

# Veri setini fakt??r yapma
nicz$Class = factor(nicz$Class, labels = c(1:6))

# E??itim ve test setlerini olu??turma
install.packages('caTools')
library(caTools)
set.seed(123)
split <- sample.split(nicz$Class, SplitRatio = 0.80)
training_set <- subset(nicz, split == TRUE)
test_set <- subset(nicz, split == FALSE)

# ??zellikleri standartla??t??rma
training_set[, 1:3] <- scale(training_set[, 1:3])
test_set[, 1:3] <- scale(test_set[, 1:3])
training_set
test_set

# Modeli e??itme ve tahmin yapma
install.packages("class")
library(class)
y_pred = knn(train = training_set[, -4],
             test = test_set[, -4],
             cl = training_set$Class,
             k = 5,
             prob = TRUE)

#do??ruluk matrisi olusturma
cm = table(test_set$Class, y_pred)
cm
#hata ve do??ruluk oran??
acc=(cm[1,1]+cm[2,2]+cm[3,3]+cm[4,4]+cm[5,5]+cm[6,6])/sum(cm) #dopruluk katsay}s}n} gvrelim (accuracy)
err=1-acc #hata oran} (error rate)
