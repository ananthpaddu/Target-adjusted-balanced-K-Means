set.seed(100)

x=rnorm(100)
y=rnorm(100)
balksample=data.frame(x=x,y=y)

usethis::use_data(balksample, compress="xz", overwrite = T)
