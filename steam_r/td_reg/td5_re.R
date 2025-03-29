
#### Exercice 1 ####
# Soit X une variable aléatoire réelle suivant la loi 0.5*N(-2,1)+0.5*N(2,1).

# 1 Tracer la densité f de X (sur l'intervalle [-6,6]) 

# 2. Ecrire une fonction qui permet de générer un échantillon i.i.d. (de taille n=5000) 
# suivant la loi de X

# 3. Tracer les estimateurs à noyau (noyau gaussien) pour les trois paramètres de lissage suivants 
# h1 : choisi par référence à une loi normale; 
# h2 = choisi selon la méthode SJ, 
# h3  : choisi selon la méthode CV


#### Solution ####

#1

f <- function(x) 0.5*dnorm(x, mean=-2, sd=1)+0.5*dnorm(x, mean=2, sd=1)
plot(f, xlim = c(-6,6))

# 2
# générer un échantillon de taille n=5000 de X
n <- 5000
U <- runif(n)
genere_X <- function(u){ 
    lapply(X=u, FUN=function(u){(u<=0.5)*rnorm(n=1,mean=-2, sd=1)+(u>0.5)*rnorm(n=1,mean=2, sd=1)}) 
}

X <- as.numeric(genere_X(U)) # génère un échantillon de X de taille n
hist(X)

k <- function(t){(1/sqrt(2*pi))*exp(-t^2/2)} # Le noyau (une densité de proba symétrique, le noyau gaussien).
h <- n^(-1/5)*sd(X) # La fenêtre de lissage.

# On utilise lapply() pour éviter la boucle for, de plus x peut être un vecteur 
f_h <- function(x) lapply(X=x, FUN=function(x){(1/h)*mean(k((x-X)/h))})

plot(f, xlim = c(-6,6), ylim = c(0,0.25))
curve(f_h, add = TRUE, col = 'red')

Ecart_Iq <- summary(X)["3rd Qu."]-summary(X)["1st Qu."]
h1 <- 1.06*min(sqrt((n-1)/n)*sd(X), Ecart_Iq/1.349)*n^(-1/5) # La largeur de la fenêtre de lissage.

h0 <- bw.nrd0(X)
h1 <- bw.nrd(X) 
h2 <- bw.SJ(X) 
h3 <- bw.ucv(X)

f0 <- density(X, bw=h0)
f1 <- density(X, bw=h1)
f2 <- density(X, bw=h2)
f3 <- density(X, bw=h3)
f4 <- density(X, bw=0.01)

plot(f, xlim=c(-6,6), ylim=c(0,0.25))
lines(f0, col="black", lty=3)
lines(f1, col="red", lty=2)
lines(f2, col="blue", lty=2)
lines(f3, col="green", lty=2)
lines(f4, col="yellow", lty=1)
grid()

#### Exercice 2 ###############

# Soit X une variable aléatoire réelle suivant la loi 0.3*N(-2,1)+0.7*N(2,1)

# 1 Tracer la densité f de X (sur l'intervalle [-6,6]) 

# 2. Ecrire une fonction qui permet de générer un échantillon i.i.d. (de taille n=5000) suivant la loi de X

# 3. Tracer les estimateurs à noyau (noyau gaussien) pour les trois paramètres de lissage suivants 
# h1 : choisi par référence à une loi normale; 
# h2 = choisi selon la méthode SJ, 
# h3  : choisi selon la méthode CV


#### Solution ####
#1
f <- function(x) 0.3*dnorm(x, mean=-2)+0.7*dnorm(x, mean=2)
plot(f, xlim=c(-6,6))

#2
# générer un échantillon de taille n=5000 de X
n <- 5000
U <- runif(n)
genere_X <- function(u){ 
    lapply(X=u, FUN=function(u){(u<0.3)*rnorm(1,-2)+(u>0.3)*rnorm(1,2)}) 
}

X <- as.numeric(genere_X(U)) # génère un échantillon de X de taille n
hist(X)

k <- function(t){(1/sqrt(2*pi))*exp(-t^2/2)} # Le noyau (une densité de proba symétrique, le noyau gaussien).
h <- n^(-1/5)*sd(X) # La fenêtre de lissage.
# On utilise lapply() pour éviter la boucle for, de plus x peut être un vecteur 
f_h <- function(x) lapply(X=x, FUN=function(x){(1/h)*mean(k((x-X)/h))})

plot(f, xlim=c(-6,6))
curve(f_h, xlim=c(-6,6), add=TRUE, col="red", lty=2)

# Ecart_Iq <- summary(X)["3rd Qu."]-summary(X)["1st Qu."]
# h1 <- 1.06*min(sqrt((n-1)/n)*sd(X), Ecart_Iq/1.349)*n^(-1/5) # La largeur de la fenêtre de lissage.

h1 <- bw.nrd(X) 
h2 <- bw.SJ(X) 
h3 <- bw.ucv(X)

f1 <- density(X, bw=h1)
f2 <- density(X, bw=h2)
f3 <- density(X, bw=h3)
f4 <- density(X, bw=0.01)

plot(f, xlim=c(-6,6), ylim=c(0,0.3))
lines(f1, col="red", lty=2)
lines(f2, col="blue", lty=2)
lines(f3, col="green", lty=2)
lines(f4, col="yellow", lty=1)

