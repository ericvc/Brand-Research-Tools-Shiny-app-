#Plot layout function - takes the number of URLs entered into the sidebar as an argument. Returns the
#plotting dimensions for the resulting visualizations.
plf_wc <- function(n){
  dim = c()
  if(n==1) dim = c(1,1) * 350 
  if(n==2) dim = c(1,2) * c(320,450)
  if(n==3) dim = c(1,3) * c(320,350)
  if(n==4) dim = c(2,2) * c(320,450)
  if(n==5) dim = c(2,3) * c(320,350)
  return(dim)
}
