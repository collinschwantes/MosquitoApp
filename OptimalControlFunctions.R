### Optimal Control functions


#defining functions for optimization code

#Permutaion function: For a given list of numbers, this function outputs a matrix, where each row is a unique permutation of the list
uperm = function(d) {
  dat <- factor(d)
  N <- length(dat)
  n <- tabulate(dat)
  ng <- length(n)
  if(ng==1) return(d)
  a <- N-c(0,cumsum(n))[-(ng+1)]
  foo <- lapply(1:ng, function(i) matrix(combn(a[i],n[i]),nrow=n[i]))
  out <- matrix(NA, nrow=N, ncol=prod(sapply(foo, ncol)))
  xxx <- c(0,cumsum(sapply(foo, nrow)))
  xxx <- cbind(xxx[-length(xxx)]+1, xxx[-1])
  miss <- matrix(1:N,ncol=1)
  for(i in seq_len(length(foo)-1)) {
    l1 <- foo[[i]]
    nn <- ncol(miss)
    miss <- matrix(rep(miss, ncol(l1)), nrow=nrow(miss))
    k <- (rep(0:(ncol(miss)-1), each=nrow(l1)))*nrow(miss) + 
      l1[,rep(1:ncol(l1), each=nn)]
    out[xxx[i,1]:xxx[i,2],] <- matrix(miss[k], ncol=ncol(miss))
    miss <- matrix(miss[-k], ncol=ncol(miss))
  }
  k <- length(foo)
  out[xxx[k,1]:xxx[k,2],] <- miss
  out <- out[rank(as.numeric(dat), ties="first"),]
  foo <- cbind(as.vector(out), as.vector(col(out)))
  out[foo] <- d
  t(out)
  
}



#this function computes the "P" product and exponentials for a given k
P_vals = function(z, rho, Npulse, tau, k){
  P_func = function(x){ rho * 1 /(log(1/(1-rho)) - 2 * pi * complex(real = 0, imaginary = 1) * x)}
  
  z_vec = matrix(0, nrow = Npulse, ncol = 1);
  
  for(i in 1:Npulse){
    z_vec[i, 1] = z[i]
  }
  
  #determine number of integer vectors to permute (will be smaller for
  #only two and three pulses). number of combos = N_terms 
  
  if (k == 0){
    if (Npulse == 2){
      N_terms = 5
    }else if(Npulse == 3){
      N_terms = 7
    }else {
      N_terms = 8
    }
    
    
    #create a matrix to hold the integer vectors to permute, and
    #initialize to zero. Each column will give an integer vector
    int_vecs = matrix(0, nrow=Npulse, ncol=N_terms);
    
    #the first 5 integer vectors have at most two non-zero elements.
    #This loop replaces the first two zeros in the first 5 columns of int_vecs
    #with the appropriate integers.        
    for (i in 1:5){
      int_vecs[1:2, i] = c((i - 1), -(i -1))
    }
    
    #if there are are three or more pulses, we need to include
    #integer vectors with three non-zero terms
    if (Npulse >= 3){
      int_vecs[1:3, 6] = c(2, -1,-1)
      int_vecs[1:3, 7] = c(-2, 1, 1)
    }
    
    #if there are four or more pulses, then we need to include an
    #integer vector with four non-zero terms
    if (Npulse >= 4){
      int_vecs[1:4, 8] = c(1, 1, -1,-1)
    }
    
    
    #calculates the "P" functions for each column of integer combos in
    #int_vec.  "P" function values are independet of integer
    #permutations. pre_factors is a list which holds the "P"-function
    #values for each integer combo 
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i]); 
      }
    }
    
    
    #Now calculate the exponential terms which will multiply the "P"functions in the pre_factors list
    #post_factors = list of the exponential terms to multiply the
    #               corresponding "P" functions in the pre_factors list
    post_factors = numeric(N_terms) #initialize to zero
    
    for (i in 1: N_terms){ #for each integer combo given by the columns of int_vecs... 
      
      #calculates the unique permutations of the ith integer combo
      #int_vecs(:, i) = ith column of int_vecs matrix
      #perm_list = Nperms x Npulse matrix, where Nperms is the number
      #           of unique permutations of the ith integer combo
      # Each row of perm_list gives a unique permutaion of the ith integer combo 
      perm_list = uperm(int_vecs[,i])
      
      if( i == 1){
        Nperms = 1
      }else{
        Nperms = length(perm_list[,1]) #number of unique permutations of the ith integer combo
      }
      
      
      for (m in 1:Nperms){ #for each uniqe permutation of the ith integer combo, add the corresponding      exponential to the running total of the corresponding post_factor value
        
        if(i == 1){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1)  *   (perm_list %*% z) / tau) 
        } else {
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1)  * (perm_list[m,] %*% z) / tau)
        }
      }#perm_list[m, ] = mth row of perm_list = mth unique permutaton of ith integer combo
      
      
    }
    
    
    
    val = 0
    for (i in 1:N_terms){ #add together the (pre_factor * post_factor) for each integer combo
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)          
    
  }else if(abs(k) == 1){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else{
      N_terms = 8
    }
    
    
    int_vecs = matrix(0,nrow=Npulse, ncol=N_terms)
    for (i in 1:5){
      int_vecs[1:2, i] = sign(k) * c(kk + (i - 1), -(i -1))
    }
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) * c(1 , 1 , -1)
      int_vecs[1:3, 7] = sign(k) * c(3 , -1 , -1) 
      int_vecs[1:3, 8] = sign(k) * c(2 , -2 , 1)
    }
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i]) 
      }
    }        
    
    post_factors = numeric(N_terms)  
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] +exp(-2 * pi * complex(real = 0, imaginary = 1)  * perm_list[m, ] %*% z / tau)
      }
    }
    
    
    val = 0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)                      
    
  }
  else if(abs(k) == 2){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else if (Npulse == 3){
      N_terms = 6
    }else{
      N_terms = 7
    }
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk + 2, -2)
    int_vecs[1:2, 5] = sign(k) * c(kk + 3, -3)
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) *c(2, -1,1)
    }
    if (Npulse >= 4){
      int_vecs[1:4, 7] = sign(k) *c(1, 1, 1,-1)
    }
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i]) 
      }
    }        
    
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 2 && i == 2){
        Nperms = 1;
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else{
        
        Nperms = dim(perm_list)[1]
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    
    
    val = 0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val) 
    
    
  }else if(abs(k) == 3){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else{
      N_terms = 8
    }
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk + 2, -2)
    int_vecs[1:2, 5] = sign(k) * c(kk + 3, -3)
    
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) *c(1, 1, 1)
      int_vecs[1:3, 7] = sign(k) *c(3, 1, -1)
      int_vecs[1:3, 8] = sign(k) *c(2, 2, -1)            
    }       
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 3 && i == 6){
        Nperms = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else{
        Nperms = dim(perm_list)[1]
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    
    
    val = 0
    
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
    
  }else if(abs(k) == 4){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else if(Npulse == 3){
      N_terms = 7
    }else{
      N_terms = 8
    }
    
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk +1, -1)
    int_vecs[1:2, 5] = sign(k) * c(kk + 2, -2)
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) *c(4, -1, 1)
      int_vecs[1:3, 7] = sign(k)*c(2, 1, 1)            
    }
    if (Npulse >= 4){
      int_vecs[1:4, 8] = sign(k) *c(1, 1, 1, 1)
    }         
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 4 && i == 8){
        Nperms = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else if(Npulse == 2 && i == 3){
        Nperms = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      } else{
        
        Nperms = dim(perm_list)[1]
        
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 5){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else{
      N_terms = 7
    }
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 5] = sign(k) * c(kk + 2, -2)
    
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) *c(2, 2, 1);
      int_vecs[1:3, 7] = sign(k) *c(3, 1, 1);           
    }
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 6){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 6
    }else{
      N_terms = 7
    }
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk - 3, 3)
    int_vecs[1:2, 5] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 6] = sign(k) * c(kk + 2, -2)
    
    
    if (Npulse >= 3){
      int_vecs[1:3, 7] = sign(k) *c(4, 1, 1)           
    }
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 2 && i == 4){
        Nperms = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else{
        
        Nperms = dim(perm_list)[1]
        
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 7){
    kk = abs(k)
    
    N_terms = 6
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk - 3, 3)
    int_vecs[1:2, 5] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 6] = sign(k) * c(kk + 2, -2)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 8){
    kk = abs(k)
    
    N_terms = 6
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 5] = sign(k) * c(kk -3, +3)
    int_vecs[1:2, 6] = sign(k) * c(kk -4, +4)
    
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 2 && i == 6){
        Nperm = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else{
        
        Nperms = dim(perm_list)[1]
        
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 9){
    kk = abs(k)
    
    N_terms = 5
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 5] = sign(k) * c(kk -3, +3)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 10){
    kk = abs(k)
    
    N_terms = 4
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk -2, 2)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 11){
    kk = abs(k)
    
    N_terms = 4
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk -2, 2)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    out = val
    
  } else{
    kk = abs(k)
    
    N_terms = 3
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * P_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[i] * post_factors[i] 
    }
    
    return(val)
    
  }
}




#this function computes the "Q" product and exponentials with the fourier modes for a given k
Q_vals = function(z, rho, Npulse, tau, j, jj){
  Q_func = function(x){ (rho / (1 - rho)) * 1 /(log(1/(1-rho)) + 2 * pi * complex(real = 0, imaginary = 1) * x)}
  
  k = -(j + jj)  
  
  #determine number of integer vectors to permute (will be smaller for
  #only two and three pulses). number of combos = N_terms
  if (k == 0){
    if (Npulse == 2){
      N_terms = 5
    }else if(Npulse == 3){
      N_terms = 7
    }else {
      N_terms = 8
    }
    
    
    #create a matrix to hold the integer vectors to permute, and
    #initialize to zero. Each column will give an integer vector
    int_vecs = matrix(0, nrow=Npulse, ncol=N_terms);
    
    #the first 5 integer vectors have at most two non-zero elements.
    #This loop replaces the first two zeros in the first 5 columns of int_vecs
    #with the appropriate integers.        
    for (i in 1:5){
      int_vecs[1:2, i] = c((i - 1), -(i -1))
    }
    
    #if there are are three or more pulses, we need to include
    #integer vectors with three non-zero terms
    if (Npulse >= 3){
      int_vecs[1:3, 6] = c(2, -1,-1)
      int_vecs[1:3, 7] = c(-2, 1, 1)
    }
    
    #if there are four or more pulses, then we need to include an
    #integer vector with four non-zero terms
    if (Npulse >= 4){
      int_vecs[1:4, 8] = c(1, 1, -1,-1)
    }
    
    
    #calculates the "P" functions for each column of integer combos in
    #int_vec.  "P" function values are independet of integer
    #permutations. pre_factors is a list which holds the "P"-function
    #values for each integer combo 
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i]); 
      }
    }
    
    
    #Now calculate the exponential terms which will multiply the "P"functions in the pre_factors list
    #post_factors = list of the exponential terms to multiply the
    #               corresponding "P" functions in the pre_factors list
    post_factors = numeric(N_terms) #initialize to zero
    
    for (i in 1: N_terms){ #for each integer combo given by the columns of int_vecs... 
      
      #calculates the unique permutations of the ith integer combo
      #int_vecs(:, i) = ith column of int_vecs matrix
      #perm_list = Nperms x Npulse matrix, where Nperms is the number
      #           of unique permutations of the ith integer combo
      # Each row of perm_list gives a unique permutaion of the ith integer combo 
      perm_list = uperm(int_vecs[,i])
      
      if( i == 1){
        Nperms = 1
      }else{
        Nperms = length(perm_list[,1]) #number of unique permutations of the ith integer combo
      }
      
      
      for (m in 1:Nperms){ #for each uniqe permutation of the ith integer combo, add the corresponding      exponential to the running total of the corresponding post_factor value
        
        if(i == 1){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1)  *   (perm_list %*% z) / tau) 
        } else {
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1)  * (perm_list[m,] %*% z) / tau)
        }
      }#perm_list[m, ] = mth row of perm_list = mth unique permutaton of ith integer combo
      
      
    }
    
    
    
    val = 0
    for (i in 1:N_terms){ #add together the (pre_factor * post_factor) for each integer combo
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)          
    
  }else if(abs(k) == 1){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else{
      N_terms = 8
    }
    
    
    int_vecs = matrix(0,nrow=Npulse, ncol=N_terms)
    for (i in 1:5){
      int_vecs[1:2, i] = sign(k) * c(kk + (i - 1), -(i -1))
    }
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) * c(1 , 1 , -1)
      int_vecs[1:3, 7] = sign(k) * c(3 , -1 , -1) 
      int_vecs[1:3, 8] = sign(k) * c(2 , -2 , 1)
    }
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i]) 
      }
    }        
    
    post_factors = numeric(N_terms)  
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] +exp(-2 * pi * complex(real = 0, imaginary = 1)  * perm_list[m, ] %*% z / tau)
      }
    }
    
    
    val = 0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)                      
    
  }
  else if(abs(k) == 2){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else if (Npulse == 3){
      N_terms = 6
    }else{
      N_terms = 7
    }
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk + 2, -2)
    int_vecs[1:2, 5] = sign(k) * c(kk + 3, -3)
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) *c(2, -1,1)
    }
    if (Npulse >= 4){
      int_vecs[1:4, 7] = sign(k) *c(1, 1, 1,-1)
    }
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i]) 
      }
    }        
    
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 2 && i == 2){
        Nperms = 1;
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else{
        
        Nperms = dim(perm_list)[1]
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    
    
    val = 0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val) 
    
    
  }else if(abs(k) == 3){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else{
      N_terms = 8
    }
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk + 2, -2)
    int_vecs[1:2, 5] = sign(k) * c(kk + 3, -3)
    
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) *c(1, 1, 1)
      int_vecs[1:3, 7] = sign(k) *c(3, 1, -1)
      int_vecs[1:3, 8] = sign(k) *c(2, 2, -1)            
    }       
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 3 && i == 6){
        Nperms = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else{
        Nperms = dim(perm_list)[1]
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    
    
    val = 0
    
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
    
  }else if(abs(k) == 4){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else if(Npulse == 3){
      N_terms = 7
    }else{
      N_terms = 8
    }
    
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk +1, -1)
    int_vecs[1:2, 5] = sign(k) * c(kk + 2, -2)
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) *c(4, -1, 1)
      int_vecs[1:3, 7] = sign(k)*c(2, 1, 1)            
    }
    if (Npulse >= 4){
      int_vecs[1:4, 8] = sign(k) *c(1, 1, 1, 1)
    }         
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 4 && i == 8){
        Nperms = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else if(Npulse == 2 && i == 3){
        Nperms = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      } else{
        
        Nperms = dim(perm_list)[1]
        
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 5){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 5
    }else{
      N_terms = 7
    }
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 5] = sign(k) * c(kk + 2, -2)
    
    
    if (Npulse >= 3){
      int_vecs[1:3, 6] = sign(k) *c(2, 2, 1);
      int_vecs[1:3, 7] = sign(k) *c(3, 1, 1);           
    }
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 6){
    kk = abs(k)
    
    if (Npulse == 2){
      N_terms = 6
    }else{
      N_terms = 7
    }
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk - 3, 3)
    int_vecs[1:2, 5] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 6] = sign(k) * c(kk + 2, -2)
    
    
    if (Npulse >= 3){
      int_vecs[1:3, 7] = sign(k) *c(4, 1, 1)           
    }
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 2 && i == 4){
        Nperms = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else{
        
        Nperms = dim(perm_list)[1]
        
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 7){
    kk = abs(k)
    
    N_terms = 6
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk - 3, 3)
    int_vecs[1:2, 5] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 6] = sign(k) * c(kk + 2, -2)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 8){
    kk = abs(k)
    
    N_terms = 6
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 4] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 5] = sign(k) * c(kk -3, +3)
    int_vecs[1:2, 6] = sign(k) * c(kk -4, +4)
    
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      
      if(Npulse == 2 && i == 6){
        Nperm = 1
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list %*% z / tau)
      }else{
        
        Nperms = dim(perm_list)[1]
        
        
        for (m in 1:Nperms){
          post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
        }
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 9){
    kk = abs(k)
    
    N_terms = 5
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk -2, 2)
    int_vecs[1:2, 5] = sign(k) * c(kk -3, +3)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 10){
    kk = abs(k)
    
    N_terms = 4
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk -2, 2)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    return(val)
    
  }else if(abs(k) == 11){
    kk = abs(k)
    
    N_terms = 4
    
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    int_vecs[1:2, 4] = sign(k) * c(kk -2, 2)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[1,i] * post_factors[i] 
    }
    
    out = val
    
  } else{
    kk = abs(k)
    
    N_terms = 3
    int_vecs = matrix(0,Npulse, N_terms)
    
    int_vecs[1:2, 1] = sign(k) * c(kk + 0, 0)
    int_vecs[1:2, 2] = sign(k) * c(kk - 1, 1)
    int_vecs[1:2, 3] = sign(k) * c(kk + 1, -1)
    
    
    pre_factors = matrix(1, ncol=N_terms, nrow=1)
    
    for (i in 1:N_terms){
      for (m in 1:Npulse){
        pre_factors[1,i] = pre_factors[1,i] * Q_func(int_vecs[m,i])  
      }
    }       
    post_factors = numeric(N_terms)
    for (i in 1: N_terms){
      perm_list = uperm(int_vecs[,i])
      Nperms = dim(perm_list)[1]
      
      
      for (m in 1:Nperms){
        post_factors[i] = post_factors[i] + exp(-2 * pi * complex(real = 0, imaginary = 1) * perm_list[m, ] %*% z / tau)
      }
    }
    
    val=0
    for (i in 1:N_terms){
      val = val + pre_factors[i] * post_factors[i] 
    }
    
    return(val)
    
  }
}


#this function computes the fourier sum for 1 pulse 
sum_1_pulse = function(z, rho, mu, tau, kmax, modes){
  
  jmax = length(modes) #modes is the list of fourier modes, jmax = number of list elements
  out_val = 0 #running total for output value
  
  P_func = function(x) {rho * 1 /(log(1/(1-rho)) - 2 * pi * complex(real = 0, imaginary = 1) * x)}
  Q_func = function(x) {(rho / (1 - rho)) * 1 /(log(1/(1-rho)) + 2 * pi * complex(real = 0, imaginary = 1) * x)}
  
  
  for (k in -kmax:kmax){
    
    pre_factor = P_func(k)
    
    post_factor = exp( -2 * pi * complex(real = 0, imaginary = 1) * k * z / tau) 
    P_val = pre_factor * post_factor
    
    Q_val = 0
    
    for (j in (-jmax+1):(jmax-1)){ 
      pre_factor = Q_func(-k-j)
      post_factor = exp( -2 * pi * complex(real = 0, imaginary = 1) * (-k -j)  * z / tau)
      
      if (j ==0){
        Q_val =Q_val +  pre_factor * post_factor * modes[1] / mu
      }else if (j <0){
        Q_val =Q_val + pre_factor * post_factor * Conj(modes[-(j-1)]) / mu 
      }else if (j>0){
        Q_val =Q_val + pre_factor * post_factor * modes[j+1] / mu
      } 
    }
    
    
    out_val = out_val + (P_val)*(Q_val) /( 1 -  log(1-rho) / (mu * tau) - 2 * pi * complex(real = 0, imaginary = 1) * k /(mu * tau));   
    
  } 
  
  return(Re(out_val))
  
}

# this function computes the Fourier sum for N pulses, where the "P" and "Q" products and exponentials are computeded by the "P_vals" and "Q_vals" functions  
sum_N_pulse = function(z, rho, Npulse, mu, tau, kmax, modes){
  jmax = length(modes)
  out_val = 0 #running total for output
  
  
  for (k in -kmax:kmax){
    
    P_val = P_vals(z, rho, Npulse, tau, k) #compute "P" product and exponentials at this k value
    
    Q_val = 0 #running total for "Q" product and exponentials
    #add together "Q" products and exponentials at (k + j) value for all j emergence fourier modes
    for (j in (-jmax+1):(jmax-1)){ 
      
      #if (abs(k+j) < kmax){
      
      if (j ==0){
        Q_val =Q_val +  Q_vals(z, rho, Npulse, tau, k, j)* modes[1] / mu #zero fourier mode 
      }else if(j <0){
        Q_val = Q_val + Q_vals(z, rho, Npulse, tau, k, j) * Conj(modes[-(j-1)]) / mu #negative fourier modes
      }else if(j>0){
        Q_val =Q_val + Q_vals(z, rho, Npulse, tau, k, j) * modes[j+1] / mu #positive fourier modes
      } 
      #}
    }
    
    #add contribution from this k value to running output total
    out_val = out_val + (P_val)*(Q_val) /( 1 - Npulse * log(1-rho) / (mu * tau) - 2 * pi * complex(real= 0, imaginary = 1) * k /(mu * tau))
  }
  out = Re(out_val)
  return(out)
  
}

