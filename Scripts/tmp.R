
n <- sample(3:20, 1)
p <- sample(1:10, 1)

m1 <- matrix(NA, nrow=n, ncol = p)
print(m1)

m2 <- m1
for(i in 1:n){
    for(j in 1:p){
        if(j>=i) m2[i,j] <- j - (j-1) + (i-1)
        if(j < i) m2[i,j] <- j
    }

}
print(m2)


m3 <- which.cohort(m2)
print(m3)
m4 <- matrix(NA, nrow=n, ncol = p)
for(i in 1:n){
    for(j in 1:p){
        m4[i,j] <- i - j + p
    }

}

print(m4)

m5 <- matrix(NA, nrow=n, ncol = p)
for(i in 1:n){
    for(j in 1:p){
        m5[i,j] <- j - i + n
    }

}

print(m5)
