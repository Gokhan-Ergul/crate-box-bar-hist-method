
#Read and categorized the txt file
file_contents <- read.table("C://Users/Gökhan ergül//Desktop//DatesetNA.txt", header = TRUE)
df <- data.frame(file_contents)
df$Gender <- as.factor(df$Gender)
df$Group<- as.factor(df$Group)

#all the functions I created are here
my_length <- function(vector) {
  count <- 0
  for (element in vector) {
    count <- count + 1
  }
  return(count)
}

my_max <- function(values) {
  max_value <- values[1]  
  
  for (value in values[-1]) {
    if (!is.na(value) && value > max_value) {
      max_value <- value
    }
  }
  
  return(max_value)
}

my_min <- function(x) {
  if (my_length(x) == 0) {
    stop("Empty vector supplied")
  }
  
  min_val <- x[1] 
  
  
  for (i in 2:my_length(x)) {
    if (!is.na(x[i]) && x[i] < min_val) {
      min_val <- x[i]
    }
  }
  
  return(min_val)
}

my_sort <- function(vector) {
  vector<-vector[!is.na(vector)]
  n <- my_length(vector)
  
  if (n <= 1) {
    return(vector)
  } else {
    for (i in 1:(n - 1)) {
      for (j in 1:(n - i)) {
        # Check if both values are not NA before comparison
        if (!is.na(vector[j]) & !is.na(vector[j + 1]) && vector[j] > vector[j + 1]) {
          temp <- vector[j]
          vector[j] <- vector[j + 1]
          vector[j + 1] <- temp
        }
      }
    }
    return(vector)
  }
}

my_unique<- function(columns){
  columns<-columns[!is.na(columns)]
  count <- 0
  seen <- c()
  
  for (g in columns) {
    if (!(g %in% seen)) {
      count <- count + 1
      seen <- c(seen, g)
    }
  }
  return(my_sort(seen))
}

my_number_of_unique<- function(columns){
  columns<-columns[!is.na(columns)]
  count<-0
  count_vector <- c()
  for (i in 1:my_length( my_unique(columns))){
    for(j in columns == my_unique(columns)[i] ){
      if(j == TRUE){
        count <- count + 1
      }
    }
    count_vector <- c(count_vector, count)
    count <- 0
  }
  return(count_vector)
}

my_seq <- function(lowlim, uplim, step) {
  myvector <- c()
  i <- lowlim
  while (i <= uplim) {
    myvector <- c(myvector, i)
    i <- i + step
  }
  return(myvector)
}

real_bins<-function(data, select_column, bins){
  
  min <- my_min(data[[select_column]])
  range <- my_max(data[[select_column]]) - my_min(data[[select_column]])
  step <- range/bins
  vector <-c(min)
  for(i in 1:bins){
    vector<- c(vector,min+step)
    min <-min + step
  }
  
  
  return(vector)
}

conter_real_bins<- function(data, select_column,bins){
  
  bins_values <- real_bins(df,select_column,bins)
  vector3 <- c()
  counter <- 0
  for (i in 1:bins) {
      for (l in data[[select_column]]) {
        if (!is.na(l) && !is.na(bins_values[i]) && !is.na(bins_values[i + 1])) {
          if (l > bins_values[i] && l <= bins_values[i +1]) {
            counter <- counter + 1
          }
        }
      }
      vector3<-c(vector3, counter)
      
    
    counter <- 0  
  }
  
  vector3[1]<- vector3[1]+1
  my_vector<- c()
  for(i in 1:my_length(vector3)){
    my_vector<-c(my_vector,vector3[i],vector3[i])
  }
  
  return(my_vector)
  
}

display_alert <- function(message) {
  cat(paste("ALERT: ", message, "\n"))
}

my_median <- function(values) {
  values <- values[!is.na(values)]
  
  n <- length(values)
  
  sorted_values <- my_sort(values)
  if (n %% 2 == 1) {
    return(sorted_values[(n + 1) %/% 2])
  } else {
    return((sorted_values[n %/% 2] + sorted_values[(n %/% 2) + 1]) / 2)
  }
}

my_ceil <- function(x) {
  if (x %% 1 == 0) {
    return(x)
  } else {
    return((x+1)%/%1)
  }
}

my_quantile_25 <- function(values) {
  values <- values[!is.na(values)]
  sorted_values <- my_sort(values)
  
  index <- 0.25 * (my_length(sorted_values) + 1)
  
  lower_index <- my_ceil(index)
  upper_index <- lower_index + 1
  
  if (lower_index == upper_index) {
    return(sorted_values[lower_index])
  }
  
  lower_value <- sorted_values[lower_index]
  upper_value <- sorted_values[upper_index]
  return((upper_value - lower_value) * (index - lower_index) + lower_value)
}

my_quantile_75 <- function(values) {
  values <- values[!is.na(values)]
  sorted_values <- my_sort(values)
  
  index <- 0.75 * (my_length(sorted_values) + 1)
  
  lower_index <- my_ceil(index)
  upper_index <- lower_index + 1
  
  if (lower_index == upper_index) {
    return(sorted_values[lower_index])
  }
  
  lower_value <- sorted_values[lower_index]
  upper_value <- sorted_values[upper_index]
  return((upper_value - lower_value) * (index - lower_index) + lower_value)
}

my_outliers <- function(values) {
  values <- values[!is.na(values)]
  Q1 <- my_quantile_25(values)
  Q3 <- my_quantile_75(values)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outliers <- values[values < lower_bound | values > upper_bound]
  
  return(outliers)
}

my_min_for_boxplot <- function(x) {
  x<- x[!is.na(x)]
  if (my_length(x) == 0) {
    stop("Empty vector supplied")
  }
  if(my_length(my_outliers(x))==0){
    my_min(x)
  }else{
    copyx<-x
    for(i in my_outliers(x)){
      for(j in x){
        if(i == j){
          copyx<-copyx[copyx!=i]
        }
      }
    }
    return(my_min(copyx))
  }
  
}

my_max_for_boxplot <- function(x){
  x<- x[!is.na(x)]
  if (my_length(x) == 0) {
    stop("Empty vector supplied")
  }
  if(my_length(my_outliers(x))==0){
    my_max(x)
  }else{
    copyx<-x
    for(i in my_outliers(x)){
      for(j in x){
        if(i == j){
          copyx<-copyx[copyx!=i]
        }
      }
    }
    return(my_max(copyx))
  }
}

#Barpot
my_barplot <- function(data, select_columns, xlabel = 'x label', ylabel = 'y label',div = "V",
                       xlimit = NULL, ylimit = NULL, colors = NULL, mainlab = 'Barplot') {
  
  if(div=="H"){
    par(mfrow = c(my_length(select_columns),1))
  } else if(div=="V"){
    par(mfrow = c(1, my_length(select_columns)))
  }
  
  for (col_index in 1:length(select_columns)) {
    select_column <- select_columns[col_index]
    
    if (is.null(xlimit)) {
      xlimit = c(0, my_length(my_unique(data[[select_column]])))
    }
    
    if (is.null(ylimit)) {
      max_y <- my_max(my_number_of_unique(data[[select_column]]))
      min_y <- my_min(my_number_of_unique(data[[select_column]]))
      ylimit = c(min_y, max_y)
    }
    
    if (is.null(colors)) {
      colors <- c("skyblue", "salmon", "green", "orange", "purple", "pink",
                  "yellow", "brown", "cyan", "gray")
    }
    
    lab <- my_unique(data[[select_column]])
    x <- 1:my_length(my_unique(data[[select_column]]))
    y <- my_number_of_unique(data[[select_column]])
    
    if (xlimit[1] > my_length(x)) {
      cat("There is no bar in this range, please check your xlimit value\n")
    }
    if (ylimit[1] > my_max(y)) {
      cat("There is no bar in this range, please check your ylimit value\n")
    }
    
    plot(x, y, type = "n", xlab = xlabel, ylab = ylabel, ylim = ylimit,
         xlim = xlimit + 0.4, main = mainlab, axes = FALSE)
    
    for (i in 1:my_length(x)) {
      # Define coordinates for the rectangle bar
      x_coords <- c(x[i] - 0.25, x[i] + 0.25, x[i] + 0.25, x[i] - 0.25)
      y_coords <- c(0, 0, y[i], y[i])
      polygon(x_coords, y_coords, col = colors[col_index])
    }
    
    count <- ylimit[2] %/% 6
    sec_count <- ylimit[1]
    axis(1, at = x, labels = lab)
    axis(2, at = my_seq(lowlim = ylimit[1], uplim = ylimit[2], step = count)) 
  }
  
  par(mfrow = c(1, 1))  # Reset plot layout
}


my_barplot(df, c("Group","Gender"))
my_barplot(df, "Gender","Sadik Can Barut", ylimit = c(0,80), xlimit = c(0,2),
                   color = "lightpink", "ALi Haydar Kaynakci",
                   mainlab = "Gokhan Ergul")
my_barplot(df, c("Group","Gender"),div = "H")



#Hist
##NOTE: MY_HIST A LITTLE BIT SENSITIVE
##Therefore, in my_hist() method, you will get different tables than original hist().
##But my_hist() is make correct tables too.
my_hist <- function(data, select_columns, bins = 10, xlabel = "X Label", ylabel = "Y Label", 
                    xlimit = NULL, ylimit = NULL, colors = "skyblue",
                    mainlab = "Histogram") {
  
  par(mfrow = c(1, my_length(select_columns)))
  if(my_length(select_columns) > 1){
    if (is.null(xlimit)) {
      li1 <- c()
      li2 <- c()
      for(i in 1:my_length(select_columns)){
        li1 <- c(li1, my_min(data[[select_columns[i]]]))
        li2 <- c(li2, my_max(data[[select_columns[i]]]))
      }
      
      xlimit <- c(my_min(li1) %/% 1, my_max(li2) %/% 1 + 1)
    }
    
    if (is.null(ylimit)) {
      li1 <- c()
      li2 <- c()
      for(i in 1:my_length(select_columns)){
        y <- conter_real_bins(data, select_columns[i], bins)
        li1 <- c(li1, my_min(y))
        li2 <- c(li2, my_max(y))
      }
      
      ylimit <- c(my_min(li1), my_max(li2))
    }
    
  } else {
    if (is.null(xlimit)) {
      xlimit <- c(my_min(data[[select_columns]]) %/% 1, my_max(data[[select_columns]]) %/% 1 + 1)
    }
    
    if (is.null(ylimit)) {
      y <- conter_real_bins(data, select_columns, bins)
      ylimit <- c(my_min(y), my_max(y))
    }
  }
  
  
  colarr<-0
  for (select_column in select_columns) {
    colarr<- colarr+1
    
    x <- real_bins(data, select_column, bins)
    my_xvector <- c()
    for (i in 1:my_length(x)) {
      if (i == 1 || i == my_length(x)) {
        my_xvector <- c(my_xvector, x[i])
      } else {
        my_xvector <- c(my_xvector, x[i], x[i])
      }
    }
    x <- my_xvector
    y <- conter_real_bins(data, select_column, bins)
    
    if (xlimit[1] > my_max(x)) {
      cat("There is no bar in this range, please check your xlimit value\n")
    }
    if (ylimit[1] > my_max(y)) {
      cat("There is no bar in this range, please check your ylimit value\n")
    }
   
    plot(x, y, type = "n", xlab = xlabel, ylab = ylabel, ylim = ylimit,
         xlim = xlimit, main = mainlab, axes = FALSE)
    
    for (i in 1:(my_length(x) - 1)) {
      xleft <- x[i]
      xright <- x[i + 1]
      ybottom <- 0
      ytop <- y[i + 1]  
      polygon(c(xleft, xright, xright, xleft), c(ybottom, ybottom, ytop, ytop), col = colors[colarr])
    }
    
    count <- (ylimit[2] - ylimit[1]) %/% 4
    sec_count <- (xlimit[2] - xlimit[1]) %/% 4
    
    
    axis(1, at = my_seq(xlimit[1], xlimit[2], sec_count))
    axis(2, at = my_seq(ylimit[1], ylimit[2], count))
  }
  
  par(mfrow = c(1, 1))  # Reset plot layout
}

my_hist(df, select_columns = "Var3")

my_hist(data = df, select_column = "Var3",bins = 14, xlabel = "Sadik Can Barut", 
        ylimit = c(0,20), xlimit = c(50,70),
        color = "lightgrey",
        ylabel = "ALi Haydar Kaynakci",
        mainlab = "Gokhan Ergul")

my_hist(data = df, select_column = c("Var3", "Var4"), bins = 12, xlabel = "Sadik Can Barut",
        color = c("brown", "cyan"),
        ylabel = "ALi Haydar Kaynakci",
        mainlab = "Gokhan Ergul")


#Boxplot

my_boxplot<-function(data, select_columns,category =  NULL, xlabel = 'x label', ylabel = 'y label', 
         xlimit = NULL, ylimit = NULL, colors = NULL, div="V", mainlab = 'Boxplot') {
  
  if(!is.null(category)){
    if(div=="H"){
      par(mfrow = c(my_length(select_columns),1))
    } else if(div=="V"){
      par(mfrow = c(1, my_length(select_columns)))
    }
  }
  
  for (col_index in 1:my_length(select_columns)) {
    select_column <- select_columns[col_index]
    
    if (is.null(colors)) {
      colors <- c("skyblue", "salmon", "green", "orange", "purple", "pink",
                  "yellow", "brown", "cyan", "gray")
    }
    
    if (is.null(ylimit)) {
      max_y <- my_max(data[[select_column]])
      min_y <- my_min((data[[select_column]]))
      ylimit = c(min_y, max_y)
    }
    if(my_length(select_columns)>1 && !is.null(category)){
      max_y <- my_max(data[[select_column]])
      min_y <- my_min((data[[select_column]]))
      ylimit = c(min_y, max_y)
    }
    
    if(is.null(category)){
      if (is.null(xlimit)) {
        xlimit = c(0, my_length(my_unique(select_columns)))
      }
      
      lab <- my_unique(select_columns)
      x <- 1:my_length(select_columns)
      y<-1:my_length(select_columns) #There is no metter of Y the important things is length of y
      plot(x, y, type = "n", xlab = xlabel, ylab = ylabel, ylim = ylimit,
           xlim = xlimit + 0.5, main = mainlab, axes = F,frame.plot = T)
      i<-0
      for (k in select_columns) {
        i<- i+1
        x_coords <- c(i - 0.2, i + 0.2, i + 0.2, i - 0.2)
        y_coords <- c(my_quantile_25(data[[k]]), my_quantile_25(data[[k]]),
                      my_quantile_75(data[[k]]), my_quantile_75(data[[k]]))
        
        polygon(x_coords, y_coords, col = colors[i])
        lines(c(x[i] - 0.2, x[i] + 0.2), c(my_median(data[[k]]), my_median(data[[k]])),
              col = "black",lwd = 3)
        lines(c(x[i], x[i]), c(my_min_for_boxplot(data[[k]]), my_quantile_25(data[[k]])),
              col = "black",lwd = 1,lty = 5)
        lines(c(x[i], x[i]), c(my_quantile_75(data[[k]]), my_max_for_boxplot(data[[k]])),
              col = "black",lwd = 1,lty = 5)
        lines(c(x[i] - 0.1, x[i] + 0.1), c(my_min_for_boxplot(data[[k]]), my_min_for_boxplot(data[[k]])),
              col = "black",lwd = 1)
        lines(c(x[i] - 0.1, x[i] + 0.1), c(my_max_for_boxplot(data[[k]]), my_max_for_boxplot(data[[k]])),
              col = "black",lwd = 1)
        
        if(my_length(my_outliers(data[[k]]))!=0){
          for(j in my_outliers(data[[k]])){
            lines(c(x[i], x[i]), c(j,j), col = "grey",lwd = 10)
          }
        }
      }
    }else{
      if (is.null(xlimit)) {
        xlimit = c(0, my_length(my_unique(data[[category]])))
      }
      
      lab <- my_unique(data[[category]])
      x <- 1:my_length(my_unique(data[[category]]))
      y <- 1:my_length(my_unique(data[[category]]))
      plot(x, y, type = "n", xlab = xlabel, ylab = ylabel, ylim = ylimit,
           xlim = xlimit + 0.5, main = mainlab, axes = F,frame.plot = T)
      j<-0
      for(i in my_unique(data[[category]])){
        j<-j+1
        x_coords <- c(j - 0.2, j + 0.2, j + 0.2, j - 0.2)
        category_values <- data[[select_column]][data[[category]]==i]
        rectbot<-my_quantile_25(category_values)
        recttop<-my_quantile_75(category_values)
        y_coords <- c(rectbot, rectbot,recttop, recttop)
        polygon(x_coords, y_coords, col = colors[j])
        lines(c(x[j] - 0.2, x[j] + 0.2), c(my_median(category_values), my_median(category_values)),
              col = "black",lwd = 3)
        lines(c(x[j], x[j]), c(my_min_for_boxplot(category_values), my_quantile_25(category_values)),
              col = "black",lwd = 1,lty = 5)
        lines(c(x[j], x[j]), c(my_quantile_75(category_values), my_max_for_boxplot(category_values)),
              col = "black",lwd = 1,lty = 5)
        lines(c(x[j] - 0.1, x[j] + 0.1), c(my_min_for_boxplot(category_values), my_min_for_boxplot(category_values)),
              col = "black",lwd = 1)
        lines(c(x[j] - 0.1, x[j] + 0.1), c(my_max_for_boxplot(category_values), my_max_for_boxplot(category_values)),
              col = "black",lwd = 1)
        
        if(my_length(my_outliers(category_values))!=0){
          for(k in my_outliers(category_values)){
            lines(c(x[j], x[j]), c(k,k), col = "grey",lwd = 10)
          }
        }
      }
      j<-0
    }
    
    if (xlimit[1] > my_length(x)) {
      print("There is no bar in this range, please check your xlimit value")
    }
    if (ylimit[1] > my_max(my_max(data[[select_column]]))) {
      print("There is no bar in this range, please check your ylimit value")
    }
    
    count <- ((ylimit[2]-ylimit[1])%/% 7)+1
    if(ylimit[2]-ylimit[1]<=4){
      count<-0.5
    }
    axis(1, at = x, labels = lab)
    axis(2, at = my_seq(lowlim =(ylimit[1]%/%1), uplim = (ylimit[2]%/%1) +1, step = count))
  }
  
  par(mfrow = c(1, 1))
}
my_boxplot(df,"Var6", category ="Gender")
my_boxplot(df,c("Var1","Var4","Var5"))
my_boxplot(df,c("Var2","Var4","Var5"),category = "Gender",div = "H")
my_boxplot(df, c("Var1","Var7"), category ="Group")
my_boxplot(df,"Var3")
