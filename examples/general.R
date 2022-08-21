# function example
  get_yesterday <- function(){
    x <- (Sys.Date() - 1) 
    return(x) 
    # fixed spacing for pull test 
  }

#variables can be declared with = or <-  
  x = 1:10
  length(x)
  y <- x+4 

#basic plot example
  #plot y over x, main is the title
  plot( y ~ x, main="First plot: Y over X")

  #plot x over y as a line 
  plot (x ~ y, main="Second Plot: X over Y", type='l')
  
  #see man page
  ?plot


#arrays example
  #create array 
  arr = c(0.1, 0.3, 0.5, 0.7, 0.9)
  arr2 <- c(1,2,3,4,5)
  
  #select second element 
  arr[2]
  
  #select element 3-5
  arr[3:5]
  
  #select first element, second element, and fifth element
  arr[c(1,5,2)]
  
  #pch = 19 means solid dot
  plot(arr2 ~ arr, pch=19)
  
