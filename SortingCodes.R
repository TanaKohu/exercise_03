# Function for finding the index of minimum value
IndexOfMin <- function(array, first, last)
{
  index <- first
  
  for (k in (first + 1):last)
  {
    if (array[k] < array[index])
    {
      index <- k
    }
  }
  
  return (index)
}

array <- c(2,5,9,3)
first <- 1
last <- length(array)
index <- IndexOfMin(array, first, last)

# Selection Sort algorithm
SelectionSort <- function(a, n)
{
  for (i in 1:(n - 1))
  {
    j <- IndexOfMin(a, i, n)
    swap1 <- a[i]
    swap2 <- a[j]
    a[i] <- swap2
    a[j] <- swap1
  }
  return (a)
}



# recursive version of Selection sort
RecursiveSelectionSort <- function(a, first, last)
{
  if (first < last)
  {
   index <- IndexOfMin(a, first, last)
   swap1 <- a[first]
   swap2 <- a[index]
   a[first] <- swap2  # zapis najdeneho minima na najnizsiu poziciu
   a[index] <- swap1  # posun na dalsie
   
   a <- RecursiveSelectionSort(a, first + 1, last)
  }

  return (a)

}

array <- c(2,5,9,3,6)
first <- 1
last <- length(array)
index <- IndexOfMin(array, first, last)
order <- SelectionSort(array, last)
order2 <- RecursiveSelectionSort(array, first, last)