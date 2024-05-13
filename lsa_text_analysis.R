library(tm)
library(ggplot2) # for text mining
library(lsa) # for visualization
# creating data
text <-c("transpoting food by cars will cause global warming.so we should go local.",
         "we should try to convince our parents to stop using cars because it will cause global warming.",
         "some food ,such as mango, requires a warm weather to grow. so they have to be transpoted to canada.",
         "a typical electronic circuit can be built witha battery, a bulb,and a switch.",
         "electricity flows from batteries to the bulb, just like water flow through a tube.",
         "batteries have chemical enegies in it. then the electrons flow through the bulb to light it up.",
         "birds can fly because they have feather and they are light.",
         "why some birds like pigeon can fly while some others like chicken cannot?",
         "feathers is important for birdsfly. if feather on the birds wings is removed . This bird cannot fly.")
text

# factoring
View <- factor(rep(c("View 1", "View 2", "View 3"), each = 3))
View

# Converting it to dataframe 

df <- data.frame(text, View, stringsAsFactors = FALSE)
df

# Preparing corpus

corpus <- Corpus(VectorSource(df$text))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, stemDocument, language = "english")
corpus

# Term-Document matrix

td.mat <- as.matrix(TermDocumentMatrix(corpus))
td.mat
View(td.mat)

# Distance Matrix

dist.max <- dist(t(as.matrix(td.mat)))
dist.max

# Classical Multidimensional Scaling

fit <- cmdscale(dist.max, eig = TRUE, k = 2)
fit

# Converting to dataframe

points <- data.frame(x = fit$points[,1], y = fit$points[,2])
View(points)

# Distance matrix plot

ggplot(points, aes(x = x, y = y)) +
  geom_point(data = points, aes(x = x, y = y, color = df$View)) +
  geom_point(data = points, aes(x = x, y = y - 0.2, labels = row.names(df))) 

# Weighting
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat)
View(td.mat.lsa)

# LSA
lsaSpace <- lsa(td.mat.lsa)
lsaspace

# Computing distance matrix for LSA
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace)))
dist.mat.lsa

# Classical Multidimensional Scaling for LSA

fit_lsa <- cmdscale(dist.mat.lsa , eig = TRUE, k = 2)
fit_lsa

# Converting to dataframe

points_lsa <- data.frame(x = fit$points[,1], y = fit$points[,2])
View(points_lsa)

# Distance matrix plot LSA

ggplot(points, aes(x = x, y = y)) +
  geom_point(data = points_lsa, aes(x = x, y = y, color = df$View)) +
  geom_point(data = points_lsa, aes(x = x, y = y - 0.2, labels = row.names(df)))

# Importing 3D Visualization library
library(scatterplot3d)

# Classical Multidimensional Scaling for LSA
fit2 <- cmdscale(dist.mat.lsa , eig = TRUE , k = 3)
fit2

colors <- rep(c("blue", "green", "red"), each = 3)
colors

# 3D Plot
scatterplot3d(fit2$points[,1], fit2$points[,2], fit2$points[,3],
              color = colors, pch = 16, main = "Semantic space scaled to 3D",
              xlab = "x", ylab = "y", zlab = "z", type = "h")
