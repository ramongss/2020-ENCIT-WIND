packs<-function(x)
{
  if (!require(x,character.only = TRUE)){
    install.packages(x,character.only = TRUE)
    library(x,character.only = TRUE)
  }
  else
  {
    library(x,character.only = TRUE)
  }
}
