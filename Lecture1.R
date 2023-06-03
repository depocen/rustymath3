#Yaroslav Rosokha
#Econ690 Computational Economics - Spring 2015
#Lecture 1 Code

# -------- Functions ---------
#Utility function log(c)+1 (if c>0, 0 otherwise)
utility=function(consumption){  
  if(consumption>0){return(log(consumption)+1);
  }else{return(0);}
}

#Utility function that returns a vector
utility_vec=function(consumption){ 
  util=log(consumption)+1;#Calculate the utility vector
  util[consumption==0]=0; #replace the utility of consumption=0 with 0
  return(util);
}


# -------- Parameters ---------
eps     = .00001 #Convergence criteria parameter
b       = .99    #Discount factor parameter
W_grid  = seq(0,100,1) #W grid
N_grid  = length(W_grid) #Number of grid points


# -------- Iterative Policy Evaluation: Eat-Half Policy ---------
V0 = rep(0,N_grid)        #Initialize V0
V1 = rep(0,N_grid)        #Initialize V1
policy = ceiling(W_grid/2)  #Define the policy

#This is extra: I would like to store value function on each successive iteration
t   = 0 # Current iteration
Vt  = cbind(V0,rep(t,N_grid),W_grid,rep("Half",N_grid)) #cbind (column-bind) stacks vectors/matrices side by side

diff = 1 #Initialize difference between value function to be large
while(diff>eps){ #While the difference is greater than our covergence criteria (eps), keep iterating
    
  for(i in 1:N_grid){ #For each state i
    saving=W_grid[i]-policy[i]
    index1=which(W_grid==saving) #Determine index which corresponds to the next state, W'
    V1[i]=utility(policy[i])+b*V0[index1] #
  }
  diff = max(abs(V1-V0)) #calculate the new difference
  V0=V1 #update our approximation V0 to be V1
  
  #This is extra 
  t=t+1 #increment varialbe which keeps track of the iteration
  print(paste("Difference at time",t,"is",diff)) #output the difference on each iteration
  Vt=rbind(Vt,cbind(V0,rep(t,N_grid),W_grid,rep("Half",N_grid))) #store the value function from the current iteration. rbind (row bind) stacks vectors/matrices on top of each other
}

#Store Value and Policy Functions
V_half=V0
P_half=policy

#Simple Plot
plot(W_grid,V_half)




# --------Iterative Policy Evaluation: Random Policy ---------
V0 = rep(0,N_grid)        #Initialize V0
V1 = rep(0,N_grid)        #Initialize V1

#This is extra: I would like to store value function on each successive iteration 
t = 0

diff = 1 #Initialize difference between value function to be large
while(diff>eps){ #While the difference is greater than our covergence criteria (eps), keep iterating
  for(i in 1:N_grid){ #For each state i
    #Determine the consumption
    if(i==1){consumption=0; index1=1; #First state is when W=0. We treat this state separetaly
    }else{consumption=1:W_grid[i];index1=i-consumption;}  #If in state with index i, randomly pick c from 1:W[i]. Index of the next state is i-consumption 
    V1[i]=sum(1/(length(consumption))*(utility_vec(consumption)+b*V0[index1]));
  }
  diff = max(abs(V1-V0))  #calculate the new difference
  V0 = V1 #update our approximation V0 to be V1
  
  #This is extra 
  t=t+1 #increment varialbe which keeps track of the iteration
  print(paste("Difference at time",t,"is",diff)) #output the difference on each iteration
  Vt=rbind(Vt,cbind(V0,rep(t,N_grid),W_grid,rep("Random",N_grid))) #store the value function from the current iteration. rbind (row bind) stacks vectors/matrices on top of each other
}
#Store Value Function
V_random=V0

#Simple Plot
plot(W_grid,V_random)



# --------Value Iteration: Greedy Policy ---------
V0 = rep(0,N_grid)        #Initialize V0
V1 = rep(0,N_grid)        #Initialize V1

#This is extra
t=0 #I would like to store value function on each successive iteration 

diff = 1 #Initialize difference between value function to be large
while(diff>eps){ #While the difference is greater than our covergence criteria (eps), keep iterating
  for(i in 1:N_grid){ 
    if(i==1){consumption=0; index1=1;}else{consumption=1:W_grid[i];index1=i-consumption;}    
    V1[i]=max((utility_vec(consumption)+b*V0[index1]))
  }
  diff = max(abs(V1-V0))  #calculate the new difference
  V0 = V1 #update our approximation V0 to be V1
  
  #This is extra
  t=t+1
  print(paste("Difference at time",t,"is",diff))
  Vt=rbind(Vt,cbind(V0,rep(t,N_grid),W_grid,rep("Greedy",N_grid)))  
}
#Store Value Function
V_greedy=V0

#Simple Plot
plot(W_grid,V_greedy)



# -------- Output ---------
#More complicated but nicer looking plot using ggplot2 library
library(ggplot2)

#Organize data into data frames
data=as.data.frame( rbind( cbind(W_grid,V_half,rep("Half",N_grid)),
                           cbind(W_grid,V_random,rep("Random",N_grid)),
                           cbind(W_grid,V_greedy,rep("Greedy",N_grid))))
colnames(data)=c("W","Value","Policy")#rename the columns

#Change variables from 'factor' to 'numeric'
data$Value=as.numeric(as.character(data$Value))
data$W=as.numeric(as.character(data$W))

#Output to PDF. Note: Cairo pdf does not work when you source the file. 
#Don't forget to check/set the current directory: Session/Set Working Directory->To Source File Location
cairo_pdf(filename=paste("V1.pdf",sep=""),width=6,height=4,family="CMU Serif",onefile = TRUE)

ggplot(data, aes(x=W, y=Value))+  
  # type of graph
  geom_line(aes(color=Policy),size=1) +
  #theme elements
  theme(legend.text = element_text(size = 11), axis.title= element_text(size = 11), legend.title= element_text(size = 11),axis.text= element_text(size = 11))+
  theme_minimal()+
  theme(legend.position = "bottom",legend.direction="horizontal",legend.box="horizontal")+
  theme(axis.text= element_text(size = 11), axis.title= element_text(size = 11),axis.title.x=element_text(vjust=-.25),axis.title.y=element_text(vjust=1),title= element_text(size = 11))+   
  #Labels
  guides(size=FALSE,fill=FALSE,text=FALSE)+
  labs(x = paste("W",sep=""),y = paste("Value",sep=""))

dev.off()


# -------- Neat ---------
library(manipulate)# Note: Manipulate works only in R-studio
library(ggplot2)

Vt=as.data.frame(Vt)                  #Organize data into data frames
colnames(Vt)=c("V","k","W","Policy")  #Rename the columns
Vt$V=as.numeric(as.character(Vt$V))   #change variables from 'factor' to 'numeric'
Vt$W=as.numeric(as.character(Vt$W))   #change variables from 'factor' to 'numeric'


manipulate( ggplot(subset(Vt,k==x), aes(x=W, y=V))+  
              # type of graph
              geom_point(aes(color=Policy),size=2) +
              #theme elements
              theme(legend.text = element_text(size = 11), axis.title= element_text(size = 11), legend.title= element_text(size = 11),axis.text= element_text(size = 11))+
              theme_minimal()+
              theme(legend.position = "bottom",legend.direction="horizontal",legend.box="horizontal")+
              theme(axis.text= element_text(size = 11), axis.title= element_text(size = 11),axis.title.x=element_text(vjust=-.25),axis.title.y=element_text(vjust=1),title= element_text(size = 11))+   
              #Labels
              guides(size=FALSE,fill=FALSE,text=FALSE)+
              ylim(0,20)+
              labs(x = paste("W",sep=""),y = paste("Value",sep="")),
              x = slider(0, 16))


#Diplay value function for "Eat-Half" policy for the first iteration
print(Vt$V[Vt$k==1 & Vt$Policy=="Half"])
