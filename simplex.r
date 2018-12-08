
GetPivotColumn <- function(last_row, col_tableu){
  for(j in 1:col_tableu){
    if(last_row[j] < 0 ){
      max_col = j
      break
    }
  }
  
  for(i in 1:col_tableu){
    if(last_row[i] < 0){
      if(abs(last_row[i]) > abs(last_row[max_col])){ 
        max_col = i 
      }
    }
  }
  return(max_col)
}

CheckNegative <- function(last_row, col_tableu){
  for(i in 1:col_tableu){
    if(last_row[i] < 0) { return(TRUE) }
  }
  return(FALSE)
}

GetTestRatio <- function(pivot_col, RHS, row_tableu){
  pivot_col <- pivot_col[-row_tableu]
  RHS <- RHS[-row_tableu]
  test_ratio = RHS / pivot_col
  #print(test_ratio)
  
  for(j in 1:(row_tableu-1)){
    if(test_ratio[j] > 0){
      smallest_pos_index = j
      break
    }
  }
  
  for(i in 1:(row_tableu-1)){
    if(test_ratio[i] > 0 && test_ratio[i] < test_ratio[smallest_pos_index]){
      smallest_pos_index = i
    }
  }

  return(smallest_pos_index)
}

ComputeTemp <- function(val_to_zero, pivot_row){
  temp = val_to_zero * pivot_row
  return(temp)
}

ComputeNewRow <- function(temp, old_row){
  new_row = old_row -temp
  return(new_row)
}

InitValues <- function(col_names, col){
  values <- list()
  
  for(i in 1:(col-1)){
    values[col_names[i]] = 0
  }
  
  return(values)
}

GetValues <- function(tableu, row_tableu, col_tableu){
  col_names = colnames(tableu)
  values = InitValues(col_names, col_tableu)
  
  for(row in 1:row_tableu){
    for(col in 1:col_tableu){
      if(tableu[row, col] == 1){
        values[col_names[col]] = (tableu[row, col_tableu])
      }    
    }
  }  
  return(values)
}

preprocessMatrix <- function(init_matrix){
  init_tableu = matrix(data=0, nrow = 9, ncol = 25)
  r = nrow(init_tableu)
  c = ncol(init_tableu)
  r2 = nrow(init_matrix)
  c2 = ncol(init_matrix)
  dimnames(init_tableu) = list(
    c("s1", "s2", "s3", "s4","s5","s6","s7","s8", "Z"),
    c("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "Z", "RHS")
  )
  
  col_counter = 1
  for(row in 1:3){
    for(col in 1:5){
      init_tableu[row, paste("s", row, sep = "")] = 1
      init_tableu[row, col_counter] = 1
      col_counter = col_counter+1
    }
  }
  
  col_counter = 1
  old_counter = col_counter
  for(row in 4:8){
    for(col in 1:3){
      init_tableu[row, paste("s", row, sep = "")] = -1
      init_tableu[row, col_counter] = 1
      col_counter = col_counter+5
    }
    col_counter = old_counter+1
    old_counter = col_counter
  }
  
  col_counter = 2
  row_counter = 1
  for(col in 1:15){
    init_tableu[r, col] = init_matrix[row_counter, col_counter]
    col_counter = col_counter + 1
    if(col_counter > 6){
      col_counter = 2
      row_counter = row_counter+1
    }
  }
  init_tableu[r, "Z"] = 1
  
  RHS = getRHS_given(init_matrix, r2, c2)
  for(row in 1:(r-1)){
    init_tableu[row, c] = RHS[row]
  }
  return(init_tableu)
}

checkNegative2 <- function(mod_tableu){
  if(mod_tableu["s4", "s4"]<0 || mod_tableu["s5", "s5"]<0 || mod_tableu["s6", "s6"]<0 || mod_tableu["s7", "s7"]<0 || mod_tableu["s8", "s8"]<0){
    return(TRUE)
  }else{ return(FALSE) }
}

removeNegative <- function(mod_tableu){
  start_col = 19
  last_col = 23
  start_row = 4
  last_row = 8
  
  if_neg = checkNegative2(mod_tableu)
  print(if_neg)
  #pos_index = getFarthestPos(mod_tableu, curr_row)
}

SimplexMethod <- function(tableu){
  row_tableu = nrow(tableu)
  col_tableu = ncol(tableu)
  last_row = tableu[row_tableu, ]
  
  neg = CheckNegative(last_row, col_tableu)
  while(neg == TRUE){
    pivot_col_index = GetPivotColumn(last_row, col_tableu)
    pivot_col = tableu[,pivot_col_index]
    #print(pivot_col)
    RHS = tableu[, col_tableu]
    
    pivot_row_index = GetTestRatio(pivot_col, RHS, row_tableu)
    #print(pivot_row_index)
    pivot_element = tableu[pivot_row_index, pivot_col_index]
    pivot_row = tableu[pivot_row_index, ] / pivot_element
    tableu[pivot_row_index,] = pivot_row
    #print(tableu[pivot_row_index,])
    
    for(i in 1:row_tableu){
      if(i != pivot_row_index){
        val_to_zero = tableu[i, pivot_col_index]
        temp = ComputeTemp(val_to_zero, pivot_row)
        new_row = ComputeNewRow(temp, tableu[i, ])
        tableu[i, ] = new_row
      }
    }
    last_row = tableu[row_tableu, ]
    neg = CheckNegative(last_row, col_tableu)
    #print(tableu)
  }
  values = GetValues(tableu,row_tableu, col_tableu)
  return(tableu)
  
}

getRHS_given <- function(init_matrix, row, col){
  RHS <- c()
  
  for(i in 1:3){
    RHS <- c(RHS, init_matrix[i, 1])
  }
  
  for(j in 2:6){
    RHS <- c(RHS, init_matrix[row, j])
  }
  return(RHS)
}

init_tableu = matrix(
  c(7, 11, 1, 0, 0, 0, 0, 77,
    10, 8, 0, 1, 0, 0, 0, 80,
    1, 0, 0, 0, 1, 0, 0, 9,
    0, 1, 0, 0, 0, 1, 0, 6, 
    -150, -175, 0, 0, 0, 0, 1, 0),
  nrow = 5,
  ncol = 8,
  byrow = TRUE
)

dimnames(init_tableu) = list(
  c("s1", "s2", "s3", "s4", "Z"),
  c("r", "p", "s1", "s2", "s3", "s4", "Z", "RHS")
)

df2 = matrix(c(310, 10, 8, 6, 5, 4,
               260, 6, 5, 4, 3, 6,
               280, 3, 4, 5, 5, 9,
               0, 0, 0, 0, 0, 0,
               NA, 180, 80, 200, 160, 220
      ),
      nrow = 5,
      ncol = 6,
      dimnames = list(c("Denver", "Phoenix", "Dallas", "Shipping", "Demands" ), c("Supply", "California", "Utah", "New Mexico", "Illinois", "New York")),
      byrow = TRUE)

#print(init_tableu)
#res = SimplexMethod(init_tableu)
#print(res)
init_tableu = preprocessMatrix(df2)
print(init_tableu)
removeNegative(init_tableu)







