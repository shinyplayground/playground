######adding text to facet_wrap plots
# df <- data.frame(w=c(1:20),up=c(3:22),pp=c(4:23))
# mdf <- melt(df,id.vars='w')
# tdf <- data.frame(w=c(5,15),value=c(10,20),lab=c('first','second'),variable=factor(c('up','pp')))
# ggplot(mdf,aes(x=w,y=value))+geom_point()+facet_wrap(~variable)+geom_text(data=tdf,aes(x=w,y=value,label=lab))
# 
# 


prod_cobd <- function(l,k,alpha,beta){
	(k^alpha)*(l^beta)
}

prod_lin <- function(l,k,alpha,beta){
	(alpha*k)+(beta*l)
}

prod_ces <- function(l,k,ces_alpha,rho,gamma){
	(ces_alpha*(k^rho)+(1-ces_alpha)*(l^rho))^(gamma/rho)
}

prod_fixed <- function(l,k,alpha,beta){
	min(alpha*k,beta*l)
}

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


ProductCurvesK <- function(type,l,k,alpha,beta,gamma,rho,ces_alpha,l_fixed){
	#Testing
	#k <- seq(0,50,by=0.1)
	#l <- seq(0,50,by=0.1)

	if(type=='Linear'){
 	  new.tot.prod <- prod_lin(l_fixed,k,alpha,beta)
	  new.avg.prod <- new.tot.prod/k
	  new.mar.prod <- rep(alpha,length(k))
	  new.df <- data.frame(k=k,new.tot=new.tot.prod,new.avg=new.avg.prod,new.mar=new.mar.prod)
	  ylim_max <- max(new.tot.prod)
  } else if(type=='Cobb Douglas'){
			new.tot.prod <- prod_cobd(l=l_fixed,k=k,alpha,beta)
	  	new.avg.prod <- new.tot.prod/k
	  	new.mar.prod <- (alpha*(k^(alpha-1)))*(l_fixed^beta)
	  	new.df <- data.frame(k=k,new.tot=new.tot.prod,new.avg=new.avg.prod,new.mar=new.mar.prod)
	  	ylim_max <- max(new.tot.prod)
  	} else if(type=='Constant Elasticity Of Substitution'){
  			new.tot.prod <- prod_ces(l=l_fixed,k=k,ces_alpha,rho,gamma)
	  		new.avg.prod <- new.tot.prod/k
	  		new.mar.prod <- ((gamma/rho)*((ces_alpha*(k^rho))+(1-ces_alpha)*(l_fixed^rho))^((gamma/rho)-1))*(rho*(ces_alpha)*(k^(rho-1)))
	  		new.df <- data.frame(k=k,new.tot=new.tot.prod,new.avg=new.avg.prod,new.mar=new.mar.prod)
	  		ylim_max <- max(new.tot.prod)
  		} else if(type=='Fixed Proportion'){
     			new.tot.prod <- alpha*k
     			new.avg.prod <- new.tot.prod/k
     			new.mar.prod <- rep(alpha,length(k))
     			new.df <- data.frame(k=k,new.tot=new.tot.prod,new.avg=new.avg.prod,new.mar=new.mar.prod)
	  		  ylim_max <- max(new.tot.prod)
       	}

gg_tot_prod <- ggplot(data=new.df)+
									geom_line(aes(x=k,y=new.tot),size=1,col='black')+
									theme(plot.title=element_text(size=12*0.9))+ 								
  								labs(title=paste('Capital Product Curves For :',type,'Production Function\n{with labour fixed at l=',l_fixed,'}'))+
									xlab('')+
									ylab('Production')+
									xlim(0,max(k))

melted <- melt(new.df,id.vars='k')

gg_legend <- ggplot(data=melted)+
							geom_line(aes(x=k,col=variable,y=value),size=1)+
							scale_colour_manual(values=c('black','darkred','blue'),name="Product Curves",labels=c('Total','Average','Marginal'))+
	            theme(legend.position='top')
	            
leg_strip <- get_legend(gg_legend)

gg_deriv_prod <- ggplot(data=new.df)+
								 	geom_line(aes(x=k,y=new.avg),size=1,col='darkred',linetype='dashed')+
									geom_line(aes(x=k,y=new.mar),size=1,col='blue',linetype='dotted')+
  								theme(plot.title=element_text(size=12*0.9))+ 								
	                xlab('Capital (k)')+
									ylab('Production')+
									xlim(0,max(k))

grid.arrange(heights=c(3,1.5,0.5),gg_tot_prod,gg_deriv_prod,leg_strip,nrow=3)
}



ProductCurvesL <- function(type,l,k,alpha,beta,gamma,rho,ces_alpha,k_fixed){
	#Testing
	#k <- seq(1,50,by=0.1)
	#l <- seq(1,50,by=0.1)
  
	if(type=='Linear'){
 	  new.tot.prod <- prod_lin(l,k_fixed,alpha,beta)
	  new.avg.prod <- new.tot.prod/l
	  new.mar.prod <- rep(beta,length(l))
	  new.df <- data.frame(l=l,new.tot=new.tot.prod,new.avg=new.avg.prod,new.mar=new.mar.prod)
	  ylim_max <- max(new.tot.prod)
  } else if(type=='Cobb Douglas'){
			new.tot.prod <- prod_cobd(l=l,k=k_fixed,alpha,beta)
	  	new.avg.prod <- new.tot.prod/l
	  	new.mar.prod <- (beta*(l^(beta-1)))*(k_fixed^alpha)
	  	new.df <- data.frame(l=l,new.tot=new.tot.prod,new.avg=new.avg.prod,new.mar=new.mar.prod)
	  	ylim_max <- max(new.tot.prod)
  	} else if(type=='Constant Elasticity Of Substitution'){
  			new.tot.prod <- prod_ces(l=l,k=k_fixed,ces_alpha,rho,gamma)
	  		new.avg.prod <- new.tot.prod/l
	  		new.mar.prod <- ((gamma/rho)*((ces_alpha*(k_fixed^rho))+(1-ces_alpha)*(l^rho))^((gamma/rho)-1))*(rho*(1-ces_alpha)*(l^(rho-1)))
	  		new.df <- data.frame(l=l,new.tot=new.tot.prod,new.avg=new.avg.prod,new.mar=new.mar.prod)
	  		ylim_max <- max(new.tot.prod)
  		} else if(type=='Fixed Proportion'){
     			new.tot.prod <- beta*l
     			new.avg.prod <- new.tot.prod/l
     			new.mar.prod <- rep(beta,length(l))
     			new.df <- data.frame(l=l,new.tot=new.tot.prod,new.avg=new.avg.prod,new.mar=new.mar.prod)
	  		  ylim_max <- max(new.tot.prod)
       	}

gg_tot_prod <- ggplot(data=new.df)+
									geom_line(aes(x=l,y=new.tot),size=1,col='black')+
									theme(plot.title=element_text(size=12*0.9))+ 								
  								labs(title=paste('Labour Product Curves For :',type,'Production Function\n{with capital fixed at k=',k_fixed,'}'))+
									xlab('')+
									ylab('Production')+
									xlim(0,max(l))

melted <- melt(new.df,id.vars='l')

gg_legend <- ggplot(data=melted)+
							geom_line(aes(x=l,col=variable,y=value),size=1)+
							scale_colour_manual(values=c('black','darkred','blue'),name="Product Curves",labels=c('Total','Average','Marginal'))+
	            theme(legend.position='top')
	            
leg_strip <- get_legend(gg_legend)

gg_deriv_prod <- ggplot(data=new.df)+
								 	geom_line(aes(x=l,y=new.avg),size=1,col='darkred',linetype='dotted')+
									geom_line(aes(x=l,y=new.mar),size=1,col='blue',linetype='dotted')+
  								theme(plot.title=element_text(size=12*0.9))+ 								
	                xlab('Labour (l)')+
									ylab('Production')+
									xlim(0,max(l))

grid.arrange(heights=c(3,1.5,0.5),gg_tot_prod,gg_deriv_prod,leg_strip,nrow=3)
}



Isoquant.plot <- function(type,l,k,alpha,beta,gamma,rho,ces_alpha,q1,q2,q3,kl_ratio){
  #Testing
# 	k <- seq(0,50,by=0.1)
# 	l <- seq(0,50,by=0.1)
#   alpha <- beta <- 0.5
#   kl_ratio <- 11
#   q1 <-10
#   q2 <-20
#   q3 <-30
#   
	planes.height <- c(q1,q2,q3)
  isq_k <- isq_k_v <- isq_k_h <- matrix(data=NA,nrow=length(l),ncol=length(planes.height))
  isq_col <-c(adjustcolor("red", alpha.f = 1),adjustcolor("green", alpha.f = 1),adjustcolor("blue", alpha.f = 1))
  mrts <- NULL
  
	if(type=='Linear'){
		for(i in 1:length(planes.height)){
			isq_k[(1:length(l)),i] <- (planes.height[i]-(beta*l))/alpha
			mrts[i] <- -(beta/alpha)
		}
		
		} else if(type=='Cobb Douglas'){
				for(i in 1:length(planes.height)){
					isq_k[(1:length(l)),i] <- (planes.height[i]/(l^beta))^(1/alpha)
					mrts[i] <- -(beta/alpha)*(kl_ratio)
				}
			  
			} else if(type=='Constant Elasticity Of Substitution'){
			 		for(i in 1:length(planes.height)){
						isq_k[(1:length(l)),i] <- (((planes.height[i]^(rho/gamma))-((1-ces_alpha)*(l^rho)))/ces_alpha)^(1/rho)
			 			mrts[i] <- -((1-ces_alpha)/ces_alpha)*(kl_ratio)*((1/kl_ratio)^rho)
			 		}
			 	} else if(type=='Fixed Proportion'){
				 		for(i in 1:length(planes.height)){
				 			isq_k_h[1:(i/beta)-1,i] <- NA
				 			isq_k_h[(i/beta):length(l),i] <- i/alpha
				 			isq_k_v[1:(i/alpha)-1,i] <- NA
				 			isq_k_v[(i/alpha):length(k),i] <- i/beta
				 		}	
				
			}

   #Plot the isoquants
   if(type=='Fixed Proportion'){
   		isq <- data.frame(cbind(l,isq_k_h))
			colnames(isq) <- c('l','k_q1','k_q2','k_q3')
			melt_isq <- melt(isq,id.vars='l')
			
		  gg_fixed <- ggplot(data=melt_isq,aes(x=l,col=variable,y=value))+
								 		geom_line(size=1)
										
		  isq <- data.frame(cbind(l,isq_k_v))
			colnames(isq) <- c('l','k_q1','k_q2','k_q3')
			melt_isq <- melt(isq,id.vars='l')
			
			gg_fixed + geom_line(data=melt_isq,aes(x=value,col=variable,y=l),size=1)+
										geom_hline(yintercept=0,size=1)+
										geom_vline(xintercept=0,size=1)+
										theme(legend.position='bottom',plot.title=element_text(size=12*0.9))+ 								
	    							labs(title=paste(type,'Production Function','Isoquants\nfor chosen {q1,q2,q3}'))+
		  							xlab('Labour(l)')+
		  							ylab('Capital(k)')+
									  xlim(0,max(l))+
										ylim(0,max(k))+
										scale_colour_manual(values=isq_col,name="Output",labels=paste('q=f(k,l)=',planes.height))
   	
   } else {
   	      kl_coord <- kl_slope <- NULL
  				isq <- data.frame(cbind(l,isq_k))
					colnames(isq) <- c('l','k_q1','k_q2','k_q3')
					melt_isq <- melt(isq,id.vars='l')
					
					kl_ratio_q <- isq_k/l
					colnames(kl_ratio_q) <- c('kl1','kl2','kl3')
					temp <- round(cbind(isq,kl_ratio_q),3)
					
					
					for(i in 5:7){
						kl_coord<-rbind(kl_coord,tail(temp[temp[,i]>=kl_ratio,c('l','k_q1','k_q2','k_q3')],1))
					}
					
					kl_temp <- diag(as.matrix(kl_coord)[,-1])
					kl_points <- data.frame(kl_coord[,1],kl_temp)
					kl_slope <- kl_coord[,2]/kl_coord[,1]
					
					kl_coord <- cbind(kl_points,kl_slope,mrts)
					colnames(kl_coord) <- c('l_coord','k_coord','slope','mrts')
					
					ggplot(data=melt_isq,aes(x=l,col=variable,y=value))+
						geom_line(size=1)+
						geom_hline(yintercept=0,size=1)+
						geom_vline(xintercept=0,size=1)+
						geom_point(data=kl_coord,aes(x=l_coord,y=k_coord),col='black',size=3)+
						geom_text(data=kl_coord,aes(x=l_coord-3,y=k_coord,label=paste('MRTS = ',round(mrts,2),sep='')),col='black',size=3)+
						geom_abline(intercept=0,slope=kl_slope[1],linetype='dashed',size=0.5,col='black')+
						theme(legend.position='bottom',plot.title=element_text(size=12*0.9))+ 								
	    			labs(title=paste(type,'Production Function','Isoquants\nfor chosen {q1,q2,q3}'))+
		  			xlab('Labour(l)')+
		  			ylab('Capital(k)')+
						xlim(0,max(l))+
						ylim(0,max(k))+
						scale_colour_manual(values=isq_col,name="Output",labels=paste('q=f(k,l)=',planes.height))
 					}
		  
	}


Production.3D <- function(type,l,k,alpha,beta,gamma,rho,ces_alpha,q1,q2,q3){
	#Testing
# 	k <- c(0:50)
# 	l <- c(0:50)
#   alpha<-beta<-0.5
#   q1<-20
#   q2<-30
#   q3<-10
#   type='gg'
  
  plane.heights <- c(q1,q2,q3)

  if(type=='Fixed Proportion'){
  	for(i in 1:length(l)){
  		q[i] <- prod_fixed(l[i],k[i],alpha,beta)
  	}
  	mat <- matrix(data=0,nrow=length(l),ncol=length(k))
  	
  	for(i in 1:length(l)){
  		mat[i,i:length(l)] <- q[i]
  		mat[i:length(l),i] <- q[i]
  	}
   q<-mat

  }else if(type=='Constant Elasticity Of Substitution'){
  	q <- outer(l,k,prod_ces,ces_alpha=ces_alpha,rho=rho,gamma=gamma)

  } else if(type=='Linear'){
  	q <- outer(l,k,prod_lin,alpha=alpha,beta=beta)

  } else if(type=='Cobb Douglas'){
		q <- outer(l,k,prod_cobd,alpha=alpha,beta=beta)
  }
  
  #Contours and Colour options
  	colours <-colorRampPalette(c("darkblue", "white"))(20)
		contour.list <- list(nlevels=20,drawlabels=F,lty=1,lwd=1,col=colours)
  	colkey.list <- list(cex.axis=0.7,col.clab = "black", line.clab = 1, side.clab = 3,cex.clab=0.65,side=2,length=1,dist=-0.1)
	  imgcol <-c(adjustcolor("red", alpha.f = 0.2),adjustcolor("green", alpha.f = 0.2),adjustcolor("blue", alpha.f = 0.2))
  
  #Hill plot
  	persp3D(plot=TRUE,colkey=FALSE,zlab='Production[q]',ylab='Capital [k]',xlab='Labour [l]',main=paste(type,'Production Function'),bty='u',col.axis='black',col.panel=adjustcolor('lightgrey',alpha.f=0.2),lwd.panel=0.3,lwd.grid=0.5,col.grid='grey',box=T,ticktype='detailed',axes=TRUE, r = 10, d = 2,shade=0, theta =70, phi = 10,contour=contour.list,border=NA,l,k,q, colvar = q,col=colours,cex.axis=0.7,cex.main=0.8,cex.lab=0.7)
      
	#Horizontal planes
  	col=1
		for(i in plane.heights){
    	image3D(x=l,y=k,z=i,col= imgcol[col], zlim = range(q),add=TRUE)
	    text3D(x =max(l), y = max(k), z = i,labels = paste('f(k,l)=q=',i),add = TRUE, adj = 0,cex=0.7)
      col=col+1
	   }
   
}

quadHelper <- function(a,b,w){
	return((a*w)-(b*(w^2)))
}

	TraceText <- function(input.list,xmin,ymin){
		ll <- input.list
		df <- data.frame(x=ll$x,y=ll$y,c=ll$col)
		if(ll$x_text=='Initial Wealth'  || ll$x_text=='Fair Insurance' || ll$x_text=='Ceq'){
			vadj <- 1
		}else{
			vadj <- 0
		}
		if(ll$x_text=='Average1' || ll$x_text=='Average2'){
			adjtxt <- 'Average'
		}else{
			adjtxt <- ll$x_text
		}
		
		if(ll$y_text=='U[w]' || ll$y_text=='U[F.Ins]'){
			xadj <- ll$x
			hadj <- 1.2
		}else if(ll$y_text=='EU[Gamble]'){
			xadj <- ll$x
			hadj <- -0.1
		}
		else{
			xadj <- ll$xmin
			hadj <- 0
		}
		
		p_layer <- geom_point(data=df,aes(x=x,y=y),colour=(ll$c),size=3)
		
		df <- data.frame(x=c(rep(ll$x,2)),y=c(ll$ymin,ll$y),c=ll$col)
		tr_vlayer <- geom_line(data=df,aes(x=x,y=y),colour=(ll$c),size=0.5)
		
		df <- data.frame(x=c(-10,ll$x),y=c(rep(ll$y,2)),c=ll$col)
		tr_hlayer <- geom_line(data=df,aes(x=x,y=y),colour=(ll$c),size=0.5)
		
		txt_layer1 <- geom_text(data=NULL,x=xadj,y=ll$y,label=ll$y_text,size=3,hjust=hadj,vjust=-0.1,colour=ll$col)
	  txt_layer2 <- geom_text(data=NULL,x=ll$x,y=ll$ymin,label=adjtxt,size=3,hjust=0,vjust=vadj,colour=ll$col,angle=90)
		
		trace_text_layer <-ggplot()+ p_layer+tr_vlayer+tr_hlayer+txt_layer1+txt_layer2
		
		return(trace_text_layer)
	}


coeffPlotter <- function(coeff_df){
		coeff_mdf <- melt(coeff_df,id.vars='w')
  	levels(coeff_mdf$variable) <- c("U'[w]", "U''[w]", "Abs Risk Aversion[w]",'Derivative Abs Risk Aversion[w]','Rel Risk Aversion[w]','Deriv Rel Risk Aversion[w]')
		coeff_plot <- ggplot(coeff_mdf,aes(w,round(value,2),colour=variable))+
								  	theme_bw()+
		              	theme(legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    							labs(title='Derivatives and Risk Aversion Functions')+
		  							xlab('Wealth')+
		  							ylab('Values')+
		              	geom_line(size=1)+
		 		  			  	scale_colour_manual(values=c("purple", "orange", "blue",'lightblue','darkgreen','green'),name="Guide",breaks=c("uprime", "udprime", "ra","radiff", "rr", "rrdiff"),labels=c("U'[w]", "U''[w]", "Absolute RA[w]",'Diff Absolute RA[w]','Relative RA[w]','Diff Relative RA[w]'))+
   									facet_wrap(~variable,scales='free_y',ncol=2)
	return(coeff_plot)
}

coeffPower <- function(a,b,r,w_initial) {
	w_max <- abs(a/(2*b))
	w <- c(0:w_max)
	u_prime <-w^(-r)
	u_dprime <- -r*(w^(-r-1))
	r_a <- r/w
	r_a_diff <- -r/(w^2)
	
	r_r <- r
	r_r_diff <- 0
	
	coeff_df <- data.frame(w=w,uprime=u_prime,udprime=u_dprime,ra=r_a,radiff=r_a_diff,rr=r_r,rrdiff=r_r_diff)
	print(coeffPlotter(coeff_df))
}

coeffExp <- function(rho,w_initial,w_gain,w_loss){
	w_max <- 20
	w <- seq(0,w_max,0.2)
	
	w_low <- w_initial-w_loss
	w_high <- w_initial+w_gain

  scale_a <- exp(-w_low/rho)/(exp(-w_low/rho)-exp(-w_high/rho))
	scale_b <- 1/(exp(-w_low/rho)-exp(-w_high/rho))
	
	u_w <- expHelper(rho=rho,w=w,a=scale_a,b=scale_b)
	
	u_prime <- (-scale_b/rho)*exp(-w/rho)
	u_dprime <- (scale_b/(rho^2))*exp(-w/rho)
	
	r_a <- -1/rho
	r_a_diff <- 0
	
	r_r <- (-1/rho)*w
	r_r_diff <- -1/rho
	
	coeff_df <- data.frame(w=w,uprime=u_prime,udprime=u_dprime,ra=r_a,radiff=r_a_diff,rr=r_r,rrdiff=r_r_diff)
	print(coeffPlotter(coeff_df))
}

coeffQuad <- function(a,b,w_initial){
	w_max <- abs(a/(2*b))
	w <- c(0:w_max)
	u_prime <-a-(2*b*w)
	u_dprime <- -2*b
	r_a <- (2*b)/(a-(2*b*w))
	r_a_diff <- (4*(b^2))/((a-(2*b*w))^2)
	
	r_r <- (2*b)/(((a/w))-(2*b))
	r_r_diff <- r_a_diff+r_a
	
	coeff_df <- data.frame(w=w,uprime=u_prime,udprime=u_dprime,ra=r_a,radiff=r_a_diff,rr=r_r,rrdiff=r_r_diff)
	print(coeffPlotter(coeff_df))		     
}

coeffLog <- function(w_initial){
	w_max <- 100
	w <- c(1:w_max)
	u_prime <- 1/w
	u_dprime <- -1/(w^2)
	r_a <- 1/w
	r_a_diff <- -1/(w^2)
	
	r_r <- 1
	r_r_diff <- 0
	
	coeff_df <- data.frame(w=w,uprime=u_prime,udprime=u_dprime,ra=r_a,radiff=r_a_diff,rr=r_r,rrdiff=r_r_diff)
	print(coeffPlotter(coeff_df))		     

}

coeffHyp <- function (hara_gamma,hara_a,hara_b){
  
	w_max <- 100
	w <- seq(0,w_max,length.out=100)
	
	u_prime <- hara_a*(((hara_a*w)/(1-hara_gamma))+hara_b)^(hara_gamma-1)
	u_dprime <- (hara_a^2)*(((hara_a*w)/(1-hara_gamma))+hara_b)^(hara_gamma-2)
	r_a <- (hara_a*(1-hara_gamma))/((hara_a*w)+hara_b*(1-hara_gamma))
	r_a_diff <- -1*(hara_a^2*(1-hara_gamma))/((hara_a*w+hara_b*(1-hara_gamma))^2)
	
	r_r <- r_a*w
	r_r_diff <- r_a+w*r_a_diff
	
	coeff_df <- data.frame(w=w,uprime=u_prime,udprime=u_dprime,ra=r_a,radiff=r_a_diff,rr=r_r,rrdiff=r_r_diff)
	print(coeffPlotter(coeff_df))		     
}


coeffsPlot <- function(type,a,b,r,rho,hara_gamma,hara_a,hara_b,w_initial,prob_loss,w_gain,w_loss,w_slidermax){
  if(type=='Quadratic Utility Function'){
			coeffQuad(a,b,w_initial)
	}else if(type=='Power Utility Function (CRRA)'){
			coeffPower(a,b,r,w_initial)
	}else if(type=='General Exponential Utility (CARA)'){
			coeffExp(rho,w_initial,w_gain,w_loss)
	}else if(type=='Log Utility Function'){
			coeffLog(w_initial)
	}else if(type=='Hyperbolic Absolute Risk Aversion (HARA)'){
			coeffHyp(hara_gamma,hara_a,hara_b)
}
}

	
basePlot <- function(w_max,u_wmax,w,u_w,w_initial,w_low,w_high,w_gamble,w_ceq,w_fins,u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,u_w_fins){
	
		points_df <- data.frame(x=c(w_initial,w_low,w_high,w_gamble,w_gamble,w_ceq,w_fins),y=c(u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,eu_w_gamble,u_w_fins))

    rect_xmin <- ifelse(length(min(points_df$x))==0,0,min(points_df$x))
	  rect_ymin <- ifelse(length(min(points_df$y))==0,0,min(points_df$y))
	  rect_xmax <- ifelse(length(max(points_df$x))==0,0,max(points_df$x))
	  rect_ymax <- ifelse(length(max(points_df$y))==0,0,max(points_df$y))
	
	  sub_df <- data.frame(w=w,u=u_w)
    seg_df <- subset(sub_df,(w>=rect_xmin)&(w<=rect_xmax))
		xmax <- w_max
		ymax <- u_wmax+u_wmax/20
	
	  base.layer <- ggplot(data=sub_df)+
		              	geom_line(aes(x=w,y=u),colour='brown',size=0.5)+
										geom_line(data=seg_df,aes(x=w,y=u),colour='black',size=0.5)+		   
										geom_line(data=data.frame(x=c(w_low,w_high),y=c(u_w_low,u_w_high)),aes(x=x,y=y),colour='darkblue',size=0.5,linetype='dashed')+						
										geom_point(x=w_fins,y=u_w_fins,colour='orange',size=3)+
		  							geom_point(x=w_low,y=u_w_low,colour='black',size=3)+
		  							geom_point(x=w_high,y=u_w_high,colour='black',size=3)+
		  							geom_point(x=w_gamble,y=u_w_gamble,colour='darkgreen',size=3)+
										geom_point(x=w_gamble,y=eu_w_gamble,colour='purple',size=3)+
		 								geom_point(x=w_ceq,y=eu_w_gamble,colour='blue',size=3)+
		  							geom_point(x=w_initial,y=u_w_initial,colour='red',size=3)
  
	
	return(list(base.layer=base.layer,zoom.coords=c(rect_xmin,rect_ymin,rect_xmax,rect_ymax)))
}

utilQuad <- function(a,b,w_initial,prob_loss,w_gain,w_loss){
	  w_max <- abs(a/(2*b))
		w <- seq(0,w_max,1)
		u_w <- quadHelper(a=a,b=b,w=w)
	  u_wmax <- quadHelper(a=a,b=b,w=w_max)

	#initial wealth coords
	  w_initial
		u_w_initial <- quadHelper(a=a,b=b,w=w_initial)
	
	#loss coords
		w_low <- w_initial-w_loss
		u_w_low <- quadHelper(a=a,b=b,w=w_low)
  
	#gain coords
	  w_high <- w_initial+w_gain
		u_w_high <- quadHelper(a=a,b=b,w=w_high)
	
	#gamble coords
		w_gamble <- prob_loss*w_low+(1-prob_loss)*w_high
		u_w_gamble <- quadHelper(a=a,b=b,w=w_gamble)
	  eu_w_gamble <- prob_loss*u_w_low+(1-prob_loss)*u_w_high
	
	#CE coords
		w_ceq<- (a-sqrt((a^2-4*b*eu_w_gamble)))/(2*b) 
	
	#Fair insurance coords
	  w_fins <- w_initial-(prob_loss*w_loss)
		u_w_fins <- quadHelper(a=a,b=b,w=w_fins)
	
	#Insurance
		min_insurance <- w_initial-w_fins
	 	risk_premium <- w_gamble-w_ceq
	 	max_insurance <- min_insurance+risk_premium
		
	  df <- data.frame(r=1,Min_insurance=min_insurance,Risk_premium=risk_premium,Max_insurance=max_insurance)
	 	mdf <- melt(df,id.vars='r')

  	 
	#insurance layer
	  insurance.layer <- ggplot(mdf)+
		  										theme_bw()+
													geom_bar(aes(x=variable,y=value,fill=variable),stat='identity',width=0.5)+
													theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
													labs(title='Insurance')+
		               				xlab('')+
				            			ylab('')	                

	#subplot layer
		proc <- basePlot(w_max,u_wmax,w,u_w,w_initial,w_low,w_high,w_gamble,w_ceq,w_fins,u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,u_w_fins)
	  	base.layer <- proc$base.layer
	    zoom.coords <- proc$zoom.coords
				rect_xmin <- zoom.coords[1]
	      rect_ymin <- zoom.coords[2]
	   		rect_xmax <- zoom.coords[3]
	      rect_ymax <- zoom.coords[4]
	   	 
		subplot.layer <- base.layer+
												theme_grey()+
		                  	annotate("rect", xmin=rect_xmin, xmax=rect_xmax, ymin=rect_ymin, ymax=rect_ymax, alpha=0.1, fill="green",color='brown')+
												theme(panel.grid.major = element_line(colour = "lightgrey"),panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    									labs(title='Full Range{Zoom Area}')+
		  									xlab('[w]')+
		  									ylab('U[w]')+
 		  									scale_x_continuous(limits = c(0,w_max),expand=c(0,0))+
 		  									scale_y_continuous(limits = c(-0.5, u_wmax),expand=c(0,0))
			
	#trace&text layer
		input.list <- list()
	    xadj <- w_max/20
  		xmin <- rect_xmin-xadj
	  	yadj <- u_wmax/30
	  	ymin <- rect_ymin-yadj
	   	xmax <- rect_xmax+xadj
	    ymax <- rect_ymax+yadj

			x_vec <- c(w_initial,w_low,w_high,w_gamble,w_gamble,w_ceq,w_fins)
			y_vec <- c(u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,eu_w_gamble,u_w_fins)
	    x_text <- c('Initial Wealth','Loss','Gain','Average1','Average2','Ceq','Fair Insurance')
			y_text <- c('U[w]','U[Loss]','U[Gain]','U[Gamble]','EU[Gamble]','U[Ceq]','U[F.Ins]')
	
	  	col_vec <- c('red','black','black','darkgreen','purple','blue','orange')
	  	n_element <- length(x_vec)
	  
	  	for(i in 1:n_element){
	  		input.list[[i]] <- list(ymin=ymin,xmin=xmin,x=x_vec[i],y=y_vec[i],col=col_vec[i],x_text=x_text[i],y_text=y_text[i])
	  	}
		trace_text_layer <- lapply(input.list,TraceText)
			
	
	 	zoom.layer <- base.layer+
										theme_bw()+
                  	theme(panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    							labs(title='Quadratic Utility Function {Zoomed In}')+
		  							xlab('wealth')+
		  							ylab('Utility [wealth]')+
                    coord_cartesian(xlim = c(xmin, xmax),ylim = c(ymin, ymax)) 
	
	for(i in 1:n_element){
		zoom.layer <- zoom.layer+trace_text_layer[[i]]$layers
	}
	

	#table
  row.vec <- paste(x_text,':')
	table.df <- data.frame(Wealth=round(x_vec,2),Utility=round(y_vec,2))
	rownames(table.df) <- row.vec
	
	table.spec <- ggTableSpec(columns.exist=T,columns.txt=colnames(table.df),columns.font=rep('bold',2),columns.col='black',columns.fill='white',columns.alpha=0.3,
                          rows.exist=T,rows.txt=row.vec,rows.font=rep('bold',n_element),rows.col='black',rows.fill='lightgrey',rows.alpha=0.3,
                          data.obj=table.df,data.col='white',data.title='',
                          hlt.col.exist=T,hl.col.which=1:n_element,hl.col.fill=col_vec,hl.col.alpha=rep(1,n_element),
                          hlt.row.exist=F,hl.row.which=c(2,3,4),hl.row.fill=c('white','red','red'),hl.row.alpha=rep(0.4,)
                )    
	gg.table <- ggTableDrawer(table.spec)

		
	
	
	#Probability Layer
   	prob_df <- data.frame(rank=1,prob_loss=prob_loss,prob_gain=(1-prob_loss))
		prob.layer <- ggplot(melt(prob_df,id.var='rank'),aes(x=rank,y=value))+
  									theme_tufte()+										
										geom_bar(stat='identity',position='stack',fill=c('navy','blue'))+
										theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
										labs(title='Probabilities')+
		                xlab('')+
				            ylab('')+
  	                coord_flip()+
		                coord_polar(theta='y')
                   
	 grid.newpage()
			pushViewport(viewport(layout=grid.layout(3,5)))
	    	print(zoom.layer,vp=viewport(layout.pos.row=1:3,layout.pos.col=1:3))
				print(subplot.layer,vp=viewport(layout.pos.row=1,layout.pos.col=4))
				print(prob.layer,vp=viewport(layout.pos.row=1,layout.pos.col=5))
	      print(insurance.layer,vp=viewport(layout.pos.row=2,layout.pos.col=4:5))
				print(gg.table,vp=viewport(layout.pos.row=3,layout.pos.col=4:5))
}

powerHelper <- function(r,w){
	return((w^(1-r))/(1-r))
}


utilPower <- function(r,w_initial,prob_loss,w_gain,w_loss,w_slidermax){
	  w_max <- w_slidermax
		w <- c(0:w_max)
		u_w <- powerHelper(r=r,w=w)
	  u_wmax <- powerHelper(r=r,w=w_max)

	#initial wealth coords
		u_w_initial <- powerHelper(r=r,w=w_initial)
	
	#loss coords
		w_low <- w_initial-w_loss
		u_w_low <- powerHelper(r=r,w=w_low)
  
	#gain coords
	  w_high <- w_initial+w_gain
		u_w_high <- powerHelper(r=r,w=w_high)
	
	#gamble coords
		w_gamble <- prob_loss*w_low+(1-prob_loss)*w_high
		u_w_gamble <- powerHelper(r=r,w=w_gamble)
	  eu_w_gamble <- prob_loss*u_w_low+(1-prob_loss)*u_w_high
	
	#CE coords
		w_ceq<- ((1-r)*eu_w_gamble)^(1/(1-r))
	
	#Fair insurance coords
	  w_fins <- w_initial-(prob_loss*w_loss)
		u_w_fins <- powerHelper(r=r,w=w_fins)
	
		#Insurance
		min_insurance <- w_initial-w_fins
	 	risk_premium <- w_gamble-w_ceq
	 	max_insurance <- min_insurance+risk_premium
		
	  df <- data.frame(r=1,Min_insurance=min_insurance,Risk_premium=risk_premium,Max_insurance=max_insurance)
	 	mdf <- melt(df,id.vars='r')

  	 
	#insurance layer
	  insurance.layer <- ggplot(mdf)+
		  										theme_bw()+
													geom_bar(aes(x=variable,y=value,fill=variable),stat='identity',width=0.5)+
													theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
													labs(title='Insurance')+
		               				xlab('')+
				            			ylab('')	                

	
	#subplot layer
		proc <- basePlot(w_max,u_wmax,w,u_w,w_initial,w_low,w_high,w_gamble,w_ceq,w_fins,u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,u_w_fins)
	  	base.layer <- proc$base.layer
	    zoom.coords <- proc$zoom.coords
				rect_xmin <- zoom.coords[1]
	      rect_ymin <- zoom.coords[2]
	   		rect_xmax <- zoom.coords[3]
	      rect_ymax <- zoom.coords[4]
	   	 
			subplot.layer <- base.layer+
												theme_grey()+
		                  	annotate("rect", xmin=rect_xmin, xmax=rect_xmax, ymin=rect_ymin, ymax=rect_ymax, alpha=0.1, fill="green",color='brown')+
												theme(panel.grid.major = element_line(colour = "lightgrey"),panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    									labs(title='Full Range{Zoom Area}')+
		  									xlab('[w]')+
		  									ylab('U[w]')+
 	  								   	scale_x_continuous(limits = c(0,w_max),expand=c(0,0))+
 		  									scale_y_continuous(limits = c(-0.5, u_wmax),expand=c(0,0))
				
	#trace&text layer
		input.list <- list()
	    xadj <- w_max/20
  		xmin <- rect_xmin-xadj
	  	yadj <- u_wmax/30
	  	ymin <- rect_ymin-yadj

			x_vec <- c(w_initial,w_low,w_high,w_gamble,w_gamble,w_ceq,w_fins)
			y_vec <- c(u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,eu_w_gamble,u_w_fins)
	    x_text <- c('Initial Wealth','Loss','Gain','Average1','Average2','Ceq','Fair Insurance')
			y_text <- c('U[w]','U[Loss]','U[Gain]','U[Gamble]','EU[Gamble]','U[Ceq]','U[F.Ins]')
	
	  	col_vec <- c('red','black','black','darkgreen','purple','blue','orange')
	  	n_element <- length(x_vec)
	  
	  	for(i in 1:n_element){
	  		input.list[[i]] <- list(ymin=ymin,xmin=xmin,x=x_vec[i],y=y_vec[i],col=col_vec[i],x_text=x_text[i],y_text=y_text[i])
	  	}
		trace_text_layer <- lapply(input.list,TraceText)
			
	#Zoomed Plot
	  xadj <- w_max/20
	  yadj <- abs(u_wmax/30)
	  xmin <- rect_xmin-xadj
	  ymin <- rect_ymin-yadj
	  xmax <- rect_xmax+xadj
	  ymax <- rect_ymax+yadj
	
	 	zoom.layer <- base.layer+
										theme_bw()+
                  	theme(panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    							labs(title='Power Utility Function {Zoomed In}')+
		  							xlab('wealth')+
		  							ylab('Utility [wealth]')+
                    coord_cartesian(xlim = c(xmin, xmax),ylim = c(ymin, ymax)) 
	
	for(i in 1:n_element){
		zoom.layer <- zoom.layer+trace_text_layer[[i]]$layers
	}
	

	#Table layer
  row.vec <- paste(':',x_text,':')
	table.df <- data.frame(Wealth=round(x_vec,2),Utility=round(y_vec,2))
	rownames(table.df) <- row.vec
	
	table.spec <- ggTableSpec(columns.exist=T,columns.txt=colnames(table.df),columns.font=rep('bold',2),columns.col='black',columns.fill='white',columns.alpha=0.3,
                          rows.exist=T,rows.txt=row.vec,rows.font=rep('bold',n_element),rows.col='black',rows.fill='lightgrey',rows.alpha=0.3,
                          data.obj=table.df,data.col='white',data.title='',
                          hlt.col.exist=T,hl.col.which=1:n_element,hl.col.fill=col_vec,hl.col.alpha=rep(1,n_element),
                          hlt.row.exist=F,hl.row.which=c(2,3,4),hl.row.fill=c('white','red','red'),hl.row.alpha=rep(0.4,)
                )    
	gg.table <- ggTableDrawer(table.spec)

		#Probability Layer
   	prob_df <- data.frame(rank=1,prob_loss=prob_loss,prob_gain=(1-prob_loss))
		prob.layer <- ggplot(melt(prob_df,id.var='rank'),aes(x=rank,y=value))+
	  								theme_tufte()+
										geom_bar(stat='identity',position='stack',fill=c('navy','blue'))+
										theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
										labs(title='Probabilities')+
		                xlab('')+
				            ylab('')+
  	                coord_flip()+
		                coord_polar(theta="y")
	                 
	 grid.newpage()
			pushViewport(viewport(layout=grid.layout(3,5)))
	    	print(zoom.layer,vp=viewport(layout.pos.row=1:3,layout.pos.col=1:3))
				print(subplot.layer,vp=viewport(layout.pos.row=1,layout.pos.col=4))
				print(prob.layer,vp=viewport(layout.pos.row=1,layout.pos.col=5))
	      print(insurance.layer,vp=viewport(layout.pos.row=2,layout.pos.col=4:5))
				print(gg.table,vp=viewport(layout.pos.row=3,layout.pos.col=4:5))

}

expHelper<- function(rho,w,a,b){
	return(if(rho==0){
		w
	}else((a-(b*exp(-w/rho)))))
}

utilExp <- function(rho,w_initial,prob_loss,w_gain,w_loss,w_slidermax){
	  w_max <- w_slidermax
		w <- seq(0,w_max,0.2)
	
	  w_low <- w_initial-w_loss
	  w_high <- w_initial+w_gain

	  scale_a <- exp(-w_low/rho)/(exp(-w_low/rho)-exp(-w_high/rho))
	  scale_b <- 1/(exp(-w_low/rho)-exp(-w_high/rho))
	
		u_w <- expHelper(rho=rho,w=w,a=scale_a,b=scale_b)
	  u_wmax <- expHelper(rho=rho,w=w_max,a=scale_a,b=scale_b)

	#initial wealth coords
		u_w_initial <- expHelper(rho=rho,w=w_initial,a=scale_a,b=scale_b)
	
	#loss coords
		u_w_low <- expHelper(rho=rho,w=w_low,a=scale_a,b=scale_b)
  
	#gain coords
		u_w_high <- expHelper(rho=rho,w=w_high,a=scale_a,b=scale_b)
	
	#gamble coords
		w_gamble <- prob_loss*w_low+(1-prob_loss)*w_high
		u_w_gamble <- expHelper(rho=rho,w=w_gamble,a=scale_a,b=scale_b)
	  eu_w_gamble <- prob_loss*u_w_low+(1-prob_loss)*u_w_high
	
	#CE coords
	  temp <- -rho*log((scale_a-eu_w_gamble)/scale_b)
		w_ceq<- ifelse(is.na(temp),w_initial,temp)

	#Fair insurance coords
	  w_fins <- w_initial-(prob_loss*w_loss)
		u_w_fins <- expHelper(rho=rho,w=w_fins,a=scale_a,b=scale_b)

		#Insurance
		min_insurance <- w_initial-w_fins
	 	risk_premium <- w_gamble-w_ceq
	 	max_insurance <- min_insurance+risk_premium
		
	  df <- data.frame(r=1,Min_insurance=min_insurance,Risk_premium=risk_premium,Max_insurance=max_insurance)
	 	mdf <- melt(df,id.vars='r')

  	 
	#insurance layer
	  insurance.layer <- ggplot(mdf)+
		  										theme_bw()+
													geom_bar(aes(x=variable,y=value,fill=variable),stat='identity',width=0.5)+
													theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
													labs(title='Insurance')+
		               				xlab('')+
				            			ylab('')	                

	#subplot layer
	proc <- basePlot(w_max,u_wmax,w,u_w,w_initial,w_low,w_high,w_gamble,w_ceq,w_fins,u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,u_w_fins)
	  	base.layer <- proc$base.layer
	    zoom.coords <- proc$zoom.coords
				rect_xmin <- zoom.coords[1]
	      rect_ymin <- zoom.coords[2]
	   		rect_xmax <- zoom.coords[3]
	      rect_ymax <- zoom.coords[4]
	   	 
		subplot.layer <- base.layer+
												theme_grey()+
		                  	annotate("rect", xmin=rect_xmin, xmax=rect_xmax, ymin=rect_ymin, ymax=rect_ymax, alpha=0.1, fill="green",color='brown')+
												theme(panel.grid.major = element_line(colour = "lightgrey"),panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    									labs(title='Full Range{Zoom Area}')+
		  									xlab('[w]')+
		  									ylab('U[w]')+
 		 										scale_x_continuous(limits = c(0,21),expand=c(0,0))+
 		  									scale_y_continuous(limits = c(-0.1, 1.2),expand=c(0,0))
			
	#trace&text layer
		input.list <- list()
	    xadj <- w_max/20
  		xmin <- rect_xmin-xadj
	  	#yadj <- u_wmax/30
	  	ymin <- rect_ymin-0.2

			x_vec <- c(w_initial,w_low,w_high,w_gamble,w_gamble,w_ceq,w_fins)
			y_vec <- c(u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,eu_w_gamble,u_w_fins)
	    x_text <- c('Initial Wealth','Loss','Gain','Average1','Average2','Ceq','Fair Insurance')
			y_text <- c('U[w]','U[Loss]','U[Gain]','U[Gamble]','EU[Gamble]','U[Ceq]','U[F.Ins]')
	
	  	col_vec <- c('red','black','black','darkgreen','purple','blue','orange')
	  	n_element <- length(x_vec)
	  
	  	for(i in 1:n_element){
	  		input.list[[i]] <- list(ymin=ymin,xmin=xmin,x=x_vec[i],y=y_vec[i],col=col_vec[i],x_text=x_text[i],y_text=y_text[i])
	  	}
		trace_text_layer <- lapply(input.list,TraceText)
			
	#Zoomed Plot
 	  xmax <- rect_xmax+0.1
	  ymax <- rect_ymax+0.2
	
	 	zoom.layer <- base.layer+
										theme_bw()+
                  	theme(panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    							labs(title='General Exponential Utility Function {Zoomed In}')+
		  							xlab('wealth')+
		  							ylab('Utility [wealth]')+
                    coord_cartesian(xlim = c(xmin, xmax),ylim = c(ymin, ymax)) 
	
	for(i in 1:n_element){
		zoom.layer <- zoom.layer+trace_text_layer[[i]]$layers
	}

	
	#table
  row.vec <- paste(':',x_text,':')
	table.df <- data.frame(Wealth=round(x_vec,2),Utility=round(y_vec,2))
	rownames(table.df) <- row.vec
	
	table.spec <- ggTableSpec(columns.exist=T,columns.txt=colnames(table.df),columns.font=rep('bold',2),columns.col='black',columns.fill='white',columns.alpha=0.3,
                          rows.exist=T,rows.txt=row.vec,rows.font=rep('bold',n_element),rows.col='black',rows.fill='lightgrey',rows.alpha=0.3,
                          data.obj=table.df,data.col='white',data.title='',
                          hlt.col.exist=T,hl.col.which=1:n_element,hl.col.fill=col_vec,hl.col.alpha=rep(1,n_element),
                          hlt.row.exist=F,hl.row.which=c(2,3,4),hl.row.fill=c('white','red','red'),hl.row.alpha=rep(0.4,)
                )    
	gg.table <- ggTableDrawer(table.spec)

		#Probability Layer
   	prob_df <- data.frame(rank=1,prob_loss=prob_loss,prob_gain=(1-prob_loss))
		prob.layer <- ggplot(melt(prob_df,id.var='rank'),aes(x=rank,y=value))+
						  			theme_tufte()+			
										geom_bar(stat='identity',position='stack',fill=c('navy','blue'))+
										theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
										labs(title='Probabilities')+
		                xlab('')+
				            ylab('')+
  	                coord_flip()+
		                coord_polar(theta="y")
                   
	                 
	 grid.newpage()
			pushViewport(viewport(layout=grid.layout(3,5)))
	    	print(zoom.layer,vp=viewport(layout.pos.row=1:3,layout.pos.col=1:3))
				print(subplot.layer,vp=viewport(layout.pos.row=1,layout.pos.col=4))
				print(prob.layer,vp=viewport(layout.pos.row=1,layout.pos.col=5))
	      print(insurance.layer,vp=viewport(layout.pos.row=2,layout.pos.col=4:5))
				print(gg.table,vp=viewport(layout.pos.row=3,layout.pos.col=4:5))
}


logHelper <- function(w){
	return(log(w))
}

utilLog <- function(w_initial,prob_loss,w_gain,w_loss){
	  w_max <- 100
		w <- c(1:w_max)
		u_w <- logHelper(w=w)
	  u_wmax <- logHelper(w=w_max)

	#initial wealth coords
		u_w_initial <- logHelper(w=w_initial)
	
	#loss coords
		w_low <- w_initial-w_loss
		u_w_low <- logHelper(w=w_low)
  
	#gain coords
	  w_high <- w_initial+w_gain
		u_w_high <- logHelper(w=w_high)
	
	#gamble coords
		w_gamble <- prob_loss*w_low+(1-prob_loss)*w_high
		u_w_gamble <- logHelper(w=w_gamble)
	  eu_w_gamble <- prob_loss*u_w_low+(1-prob_loss)*u_w_high
	
	#CE coords
		w_ceq<- exp(eu_w_gamble)
	
	#Fair insurance coords
	  w_fins <- w_initial-(prob_loss*w_loss)
		u_w_fins <- logHelper(w=w_fins)
	#Insurance
		min_insurance <- w_initial-w_fins
	 	risk_premium <- w_gamble-w_ceq
	 	max_insurance <- min_insurance+risk_premium
		
	  df <- data.frame(r=1,Min_insurance=min_insurance,Risk_premium=risk_premium,Max_insurance=max_insurance)
	 	mdf <- melt(df,id.vars='r')

  	 
	#insurance layer
	  insurance.layer <- ggplot(mdf)+
		  										theme_bw()+
													geom_bar(aes(x=variable,y=value,fill=variable),stat='identity',width=0.5)+
													theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
													labs(title='Insurance')+
		               				xlab('')+
				            			ylab('')	                
	
	#subplot layer
	proc <- basePlot(w_max,u_wmax,w,u_w,w_initial,w_low,w_high,w_gamble,w_ceq,w_fins,u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,u_w_fins)
	  	base.layer <- proc$base.layer
	    zoom.coords <- proc$zoom.coords
				rect_xmin <- zoom.coords[1]
	      rect_ymin <- zoom.coords[2]
	   		rect_xmax <- zoom.coords[3]
	      rect_ymax <- zoom.coords[4]
	   	 
		subplot.layer <- base.layer+
												theme_grey()+
		                  	annotate("rect", xmin=rect_xmin, xmax=rect_xmax, ymin=rect_ymin, ymax=rect_ymax, alpha=0.1, fill="green",color='brown')+
												theme(panel.grid.major = element_line(colour = "lightgrey"),panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    									labs(title='Full Range{Zoom Area}')+
		  									xlab('[w]')+
		  									ylab('U[w]')+
 	  									  scale_x_continuous(limits = c(0,w_max),expand=c(0,0))+
 		  									scale_y_continuous(limits = c(-0.5, u_wmax),expand=c(0,0))
				
	#trace&text layer
		input.list <- list()
	    xadj <- w_max/20
  		xmin <- rect_xmin-xadj
	  	yadj <- u_wmax/30
	  	ymin <- rect_ymin-yadj

			x_vec <- c(w_initial,w_low,w_high,w_gamble,w_gamble,w_ceq,w_fins)
			y_vec <- c(u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,eu_w_gamble,u_w_fins)
	    x_text <- c('Initial Wealth','Loss','Gain','Average1','Average2','Ceq','Fair Insurance')
			y_text <- c('U[w]','U[Loss]','U[Gain]','U[Gamble]','EU[Gamble]','U[Ceq]','U[F.Ins]')
	
	  	col_vec <- c('red','black','black','darkgreen','purple','blue','orange')
	  	n_element <- length(x_vec)
	  
	  	for(i in 1:n_element){
	  		input.list[[i]] <- list(ymin=ymin,xmin=xmin,x=x_vec[i],y=y_vec[i],col=col_vec[i],x_text=x_text[i],y_text=y_text[i])
	  	}
		trace_text_layer <- lapply(input.list,TraceText)
			
	#Zoomed Plot
	  xadj <- w_max/20
	  yadj <- abs(u_wmax/30)
	  xmin <- rect_xmin-xadj
	  ymin <- rect_ymin-yadj
	  xmax <- rect_xmax+xadj
	  ymax <- rect_ymax+yadj
	
	 	zoom.layer <- base.layer+
										theme_bw()+
                  	theme(panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    							labs(title='Logarithmic Utility Function {Zoomed In}')+
		  							xlab('wealth')+
		  							ylab('Utility [wealth]')+
                    coord_cartesian(xlim = c(xmin, xmax),ylim = c(ymin, ymax)) 
	
	for(i in 1:n_element){
		zoom.layer <- zoom.layer+trace_text_layer[[i]]$layers
	}

	
	#table
  row.vec <- paste(':',x_text,':')
	table.df <- data.frame(Wealth=round(x_vec,2),Utility=round(y_vec,2))
	rownames(table.df) <- row.vec
	
	table.spec <- ggTableSpec(columns.exist=T,columns.txt=colnames(table.df),columns.font=rep('bold',2),columns.col='black',columns.fill='white',columns.alpha=0.3,
                          rows.exist=T,rows.txt=row.vec,rows.font=rep('bold',n_element),rows.col='black',rows.fill='lightgrey',rows.alpha=0.3,
                          data.obj=table.df,data.col='white',data.title='',
                          hlt.col.exist=T,hl.col.which=1:n_element,hl.col.fill=col_vec,hl.col.alpha=rep(1,n_element),
                          hlt.row.exist=F,hl.row.which=c(2,3,4),hl.row.fill=c('white','red','red'),hl.row.alpha=rep(0.4,)
                )    
	gg.table <- ggTableDrawer(table.spec)

		#Probability Layer
   	prob_df <- data.frame(rank=1,prob_loss=prob_loss,prob_gain=(1-prob_loss))
		prob.layer <- ggplot(melt(prob_df,id.var='rank'),aes(x=rank,y=value))+
	  								theme_tufte()+					
										geom_bar(stat='identity',position='stack',fill=c('navy','blue'))+
										theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
										labs(title='Probabilities')+
		                xlab('')+
				            ylab('')+
  	                coord_flip()+
		                coord_polar(theta="y")
                   
	                 
	 grid.newpage()
			pushViewport(viewport(layout=grid.layout(3,5)))
	    	print(zoom.layer,vp=viewport(layout.pos.row=1:3,layout.pos.col=1:3))
				print(subplot.layer,vp=viewport(layout.pos.row=1,layout.pos.col=4))
				print(prob.layer,vp=viewport(layout.pos.row=1,layout.pos.col=5))
	      print(insurance.layer,vp=viewport(layout.pos.row=2,layout.pos.col=4:5))
				print(gg.table,vp=viewport(layout.pos.row=3,layout.pos.col=4:5))
}

hypHelper <- function(w,hara_gamma,hara_a,hara_b){
	return(((1-hara_gamma)/hara_gamma)*(((hara_a*w)/(1-hara_gamma))+hara_b)^hara_gamma)
}

utilHyp <- function(w_initial,prob_loss,w_gain,w_loss,hara_gamma,hara_a,hara_b,w_slidermax,w_slidermin){

	
	  w_min <- w_slidermin
	  w_max <- w_slidermax
		w <- seq(w_min,w_max,(w_max-abs(w_min))/100)
		u_w <- hypHelper(w=w,hara_gamma=hara_gamma,hara_a=hara_a,hara_b=hara_b)
	  u_wmax <- hypHelper(w=w_max,hara_gamma=hara_gamma,hara_a=hara_a,hara_b=hara_b)

	#initial wealth coords
		u_w_initial <- hypHelper(w=w_initial,hara_gamma=hara_gamma,hara_a=hara_a,hara_b=hara_b)
	
	#loss coords
		w_low <- w_initial-w_loss
		u_w_low <- hypHelper(w=w_low,hara_gamma=hara_gamma,hara_a=hara_a,hara_b=hara_b)
  
	#gain coords
	  w_high <- w_initial+w_gain
		u_w_high <- hypHelper(w=w_high,hara_gamma=hara_gamma,hara_a=hara_a,hara_b=hara_b)
	
	#gamble coords
		w_gamble <- prob_loss*w_low+(1-prob_loss)*w_high
		u_w_gamble <- hypHelper(w=w_gamble,hara_gamma=hara_gamma,hara_a=hara_a,hara_b=hara_b)
	  eu_w_gamble <- prob_loss*u_w_low+(1-prob_loss)*u_w_high
	
	#CE coords
		w_ceq<- (((((hara_gamma/(1-hara_gamma))*eu_w_gamble)^(1/hara_gamma))-hara_b)*(1-hara_gamma))/hara_a
	
	#Fair insurance coords
	  w_fins <- w_initial-(prob_loss*w_loss)
		u_w_fins <- hypHelper(w=w_fins,hara_gamma=hara_gamma,hara_a=hara_a,hara_b=hara_b)
	
		#Insurance
		min_insurance <- w_initial-w_fins
	 	risk_premium <- w_gamble-w_ceq
	 	max_insurance <- min_insurance+risk_premium
		
	  df <- data.frame(r=1,Min_insurance=min_insurance,Risk_premium=risk_premium,Max_insurance=max_insurance)
	 	mdf <- melt(df,id.vars='r')

  	 
	#insurance layer
	  insurance.layer <- ggplot(mdf)+
		  										theme_bw()+
													geom_bar(aes(x=variable,y=value,fill=variable),stat='identity',width=0.5)+
													theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
													labs(title='Insurance')+
		               				xlab('')+
				            			ylab('')	                

	#subplot layer
	proc <- basePlot(w_max,u_wmax,w,u_w,w_initial,w_low,w_high,w_gamble,w_ceq,w_fins,u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,u_w_fins)
	  	base.layer <- proc$base.layer
	    zoom.coords <- proc$zoom.coords
				rect_xmin <- zoom.coords[1]
	      rect_ymin <- zoom.coords[2]
	   		rect_xmax <- zoom.coords[3]
	      rect_ymax <- zoom.coords[4]
	   	 
		subplot.layer <- base.layer+
												theme_grey()+
		                  	annotate("rect", xmin=rect_xmin, xmax=rect_xmax, ymin=rect_ymin, ymax=rect_ymax, alpha=0.1, fill="green",color='brown')+
												theme(panel.grid.major = element_line(colour = "lightgrey"),panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    									labs(title='Full Range{Zoom Area}')+
		  									xlab('[w]')+
		  									ylab('U[w]')+
 	  									  scale_x_continuous(limits = c(w_min,w_max),expand=c(0,0))+
 		  									scale_y_continuous(limits = c(min(u_w[!is.infinite(u_w)]),ifelse(u_wmax>=0,u_wmax,max(u_w))),expand=c(0,0))
				
	#trace&text layer
		input.list <- list()
	  xadj <- w_max/20
	  yadj <- ifelse(u_wmax==0,0.1,abs(u_wmax/20))
	  xmin <- rect_xmin-xadj
	  ymin <- rect_ymin-yadj
	
			x_vec <- c(w_initial,w_low,w_high,w_gamble,w_gamble,w_ceq,w_fins)
			y_vec <- c(u_w_initial,u_w_low,u_w_high,u_w_gamble,eu_w_gamble,eu_w_gamble,u_w_fins)
	    x_text <- c('Initial Wealth','Loss','Gain','Average1','Average2','Ceq','Fair Insurance')
			y_text <- c('U[w]','U[Loss]','U[Gain]','U[Gamble]','EU[Gamble]','U[Ceq]','U[F.Ins]')
	
	  	col_vec <- c('red','black','black','darkgreen','purple','blue','orange')
	  	n_element <- length(x_vec)
	  
	  	for(i in 1:n_element){
	  		input.list[[i]] <- list(ymin=ymin,xmin=xmin,x=x_vec[i],y=y_vec[i],col=col_vec[i],x_text=x_text[i],y_text=y_text[i])
	  	}
		trace_text_layer <- lapply(input.list,TraceText)
			
	#Zoomed Plot
	  xadj <- w_max/20
	  yadj <- ifelse(u_wmax==0,0.1,abs(u_wmax/20))
	  xmin <- rect_xmin-xadj
	  ymin <- rect_ymin-yadj
	  xmax <- rect_xmax+xadj
	  ymax <- rect_ymax+yadj
	
	 	zoom.layer <- base.layer+
										theme_bw()+
                  	theme(panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	    							labs(title='Hyperbolic Absolute Risk Aversion Utility Function {Zoomed In}')+
		  							xlab('wealth')+
		  							ylab('Utility [wealth]')+
                    coord_cartesian(xlim = c(xmin, xmax),ylim = c(ymin, ymax)) 
	
	for(i in 1:n_element){
		zoom.layer <- zoom.layer+trace_text_layer[[i]]$layers
	}

	
	#table
  row.vec <- paste(':',x_text,':')
	table.df <- data.frame(Wealth=round(x_vec,2),Utility=round(y_vec,2))
	rownames(table.df) <- row.vec
	
	table.spec <- ggTableSpec(columns.exist=T,columns.txt=colnames(table.df),columns.font=rep('bold',2),columns.col='black',columns.fill='white',columns.alpha=0.3,
                          rows.exist=T,rows.txt=row.vec,rows.font=rep('bold',n_element),rows.col='black',rows.fill='lightgrey',rows.alpha=0.3,
                          data.obj=table.df,data.col='white',data.title='',
                          hlt.col.exist=T,hl.col.which=1:n_element,hl.col.fill=col_vec,hl.col.alpha=rep(1,n_element),
                          hlt.row.exist=F,hl.row.which=c(2,3,4),hl.row.fill=c('white','red','red'),hl.row.alpha=rep(0.4,)
                )    
	gg.table <- ggTableDrawer(table.spec)
		
	#Probability Layer
   	prob_df <- data.frame(rank=1,prob_loss=prob_loss,prob_gain=(1-prob_loss))
		prob.layer <- ggplot(melt(prob_df,id.var='rank'),aes(x=rank,y=value))+
			  						theme_tufte()+
										geom_bar(stat='identity',position='stack',fill=c('navy','blue'))+
										theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),panel.grid.minor = element_line(colour = "grey", linetype = "dashed"),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
										labs(title='Probabilities')+
		                xlab('')+
				            ylab('')+
  	                coord_flip()+
		                coord_polar(theta="y")
	 grid.newpage()
			pushViewport(viewport(layout=grid.layout(3,5)))
	    	print(zoom.layer,vp=viewport(layout.pos.row=1:3,layout.pos.col=1:3))
				print(subplot.layer,vp=viewport(layout.pos.row=1,layout.pos.col=4))
				print(prob.layer,vp=viewport(layout.pos.row=1,layout.pos.col=5))
	      print(insurance.layer,vp=viewport(layout.pos.row=2,layout.pos.col=4:5))
				print(gg.table,vp=viewport(layout.pos.row=3,layout.pos.col=4:5))
}

uncertainty.plot <- function(type,a,b,r,rho,hara_gamma,hara_a,hara_b,w_initial,prob_loss,w_gain,w_loss,w_slidermax,w_slidermin){
	if(type=='Quadratic Utility Function'){
		utilQuad(a,b,w_initial,prob_loss,w_gain,w_loss)
	}else if(type=='Power Utility Function (CRRA)'){
		utilPower(r,w_initial,prob_loss,w_gain,w_loss,w_slidermax)
	}else if(type=='General Exponential Utility (CARA)'){
		utilExp(rho,w_initial,prob_loss,w_gain,w_loss,w_slidermax)
	}else if(type=='Log Utility Function'){
		utilLog(w_initial,prob_loss,w_gain,w_loss)
	}else if(type=='Hyperbolic Absolute Risk Aversion (HARA)'){
		utilHyp(w_initial,prob_loss,w_gain,w_loss,hara_gamma,hara_a,hara_b,w_slidermax,w_slidermin)
}
}


ggTableSpec <- function(columns.exist,columns.txt,columns.font,columns.col,columns.fill,columns.alpha,
                            rows.exist,rows.txt,rows.font,rows.col,rows.fill,rows.alpha,
                            data.obj,data.col,data.title,
                            hlt.col.exist,hl.col.which,hl.col.fill,hl.col.alpha,
                            hlt.row.exist,hl.row.which,hl.row.fill,hl.row.alpha
                            ){
#Construct the Title Layer
    Title.Layer <- list()
 
        Title.Layer$Columns <- list()
            if(columns.exist){
                Title.Layer$Columns$Exist <- TRUE
                Title.Layer$Columns$Txt  <- columns.txt
            Title.Layer$Columns$Font <- columns.font
            Title.Layer$Columns$Col <- columns.col
            Title.Layer$Columns$Fill <- columns.fill
            Title.Layer$Columns$Alpha <- columns.alpha
            }else{
                Title.Layer$Columns$Exist <- FALSE
 
          }
 
      Title.Layer$Rows <- list()
        if(rows.exist){
            Title.Layer$Rows$Exist <- TRUE
                Title.Layer$Rows$Txt  <- rows.txt
            Title.Layer$Rows$Font <- rows.font
            Title.Layer$Rows$Col <- rows.col
            Title.Layer$Rows$Fill <- rows.fill
            Title.Layer$Rows$Alpha <- rows.alpha
        }else{
            Title.Layer$Rows$Exist <- FALSE
      }
 
#Construct Data Layer
    Data.Layer <- list()
     Data.Layer$Txt <- data.obj
     Data.Layer$Col <- data.col
     Data.Layer$Title <- data.title
 
#Construct Highlight Layer
    Highlight.Layer <- list()
 
        Highlight.Layer$Columns <- list()
            if(hlt.col.exist){
                Highlight.Layer$Columns$Exist <- TRUE
                Highlight.Layer$Columns$Which <- hl.col.which
                Highlight.Layer$Columns$Fill  <- hl.col.fill
                Highlight.Layer$Columns$Alpha <- hl.col.alpha
            }else{
                Highlight.Layer$Columns$Exist <- FALSE
            }
 
        Highlight.Layer$Rows <- list()
            if(hlt.row.exist){
                Highlight.Layer$Rows$Exist <- TRUE
                Highlight.Layer$Rows$Which <- hl.row.which
                Highlight.Layer$Rows$Fill  <- hl.row.fill
                Highlight.Layer$Rows$Alpha <- hl.row.alpha
            }else{
                Highlight.Layer$Rows$Exist <- FALSE
            }
 
gg.table.spec <- list(Title.Layer=Title.Layer,Data.Layer=Data.Layer,Highlight.Layer=Highlight.Layer)
return(gg.table.spec)
}
########################################################################################################
 

ggTableDrawer <- function(gg.table.spec){
    #Data Coordinates & Dataframe
      e<- environment()
      data.obj <- apply(gg.table.spec$Data.Layer$Txt,2,rev)
      data.col <- gg.table.spec$Data.Layer$Col
 
        xmin <- 1
      ymin <- 1
 
      xmax <- ncol(gg.table.spec$Data.Layer$Txt)
      ymax <- nrow(gg.table.spec$Data.Layer$Txt)
 
      x.adj <-1
      y.adj <-1
        lab.adj <- 0.05
 
     if(gg.table.spec$Title.Layer$Rows$Exist){
        txt.temp <- gg.table.spec$Title.Layer$Rows$Txt
        max.char <- max(nchar(txt.temp))
        empty.adj.min<-0.1*max.char
 
        }else{
            empty.adj.min<-0
        }
 
      empty.layer.adj <- 1
      temp.seq <- seq(xmin,xmax,length=xmax)
 
      DataLayer.df <- data.frame(data.obj,ycoord=1:ymax,stringsAsFactors=F)
            for(i in 1:length(temp.seq)){
                DataLayer.df <- cbind(DataLayer.df,rep(temp.seq[i]+5*lab.adj,ymax))
            }
      colnames(DataLayer.df)[(xmax+2):(xmax+2+length(temp.seq)-1)] <- paste('xcoord',1:xmax,sep='')
 
      parse.temp <- colnames(DataLayer.df)[(xmax+2):(ncol(DataLayer.df))]
      parse.coord <- paste('c(',paste(parse.temp,collapse=','),')',sep='')
 
      parse.temp <- colnames(gg.table.spec$Data.Layer$Txt)
        parse.lbl <- paste('c(',paste(parse.temp,collapse=','),')',sep='')
 
      parse.ycoord <- paste('c(',paste(rep('ycoord',xmax),collapse=','),')',sep='')
 
      EmptyLayer <- ggplot(data=DataLayer.df)+
                        geom_blank()+
                        xlim(xmin-empty.adj.min,xmax+empty.layer.adj)+
                        labs(title=gg.table.spec$Data.Layer$Title)+
                    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),axis.ticks = element_blank())
 
      DataLayer <- geom_text(data=DataLayer.df,aes_string(y=parse.ycoord,x=parse.coord,label=parse.lbl,hjust=0,vjust=1),size=rel(3),colour=data.col)
 
    #Title coordinates & Dataframe
    #Columns
            if(!gg.table.spec$Title.Layer$Columns$Exist){
                Title.Column.Layer <- NULL
                Rect.Column.Layer <- NULL
                Rect.Column.df <- data.frame(xmin=xmin,xmax=xmax+empty.layer.adj,ymin=ymax,ymax=ymax+0.5)
            }else{
                col.title.adj <- 0.5
                col.title.xmin <- xmin
                col.title.xmax <- xmax
                col.title.ymin <- ymax
                col.title.ymax <- col.title.ymin+col.title.adj
                col.y <- (col.title.ymax+col.title.ymin)/2
 
                col.lbls <- gg.table.spec$Title.Layer$Columns$Txt
                col.font <- gg.table.spec$Title.Layer$Columns$Font
                fill <- gg.table.spec$Title.Layer$Columns$Fill
                alpha<- gg.table.spec$Title.Layer$Columns$Alpha
                col.colour <- gg.table.spec$Title.Layer$Columns$Col
 
              Title.Column.df <- data.frame(lab.x=seq(col.title.xmin,col.title.xmax,length=length(col.lbls)),lab.y=rep(col.y,length(col.lbls)),Text=col.lbls,Font=col.font)
              Rect.Column.df <- data.frame(xmin=col.title.xmin,xmax=col.title.xmax+empty.layer.adj,ymin=col.title.ymin,ymax=col.title.ymax,fill=fill,alpha=alpha)
 
                Title.Column.Layer <- geom_text(data=Title.Column.df,aes(x=lab.x,y=lab.y,label=Text,fontface=Font,hjust=0,vjust=0),size=rel(3),colour=col.colour)
                Rect.Column.Layer <- geom_rect(data=Rect.Column.df,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=+Inf),alpha=alpha,fill=fill)
 
            }
 
#Rows
            if(!gg.table.spec$Title.Layer$Rows$Exist){
                Title.Row.Layer <- NULL
                Rect.Row.Layer <- NULL
                Rect.Row.df <- data.frame(xmax=1,ymax=Rect.Column.df$ymin)
            }else{
                row.lbls <- rev(gg.table.spec$Title.Layer$Rows$Txt)
                row.font <- gg.table.spec$Title.Layer$Rows$Font
                fill <- gg.table.spec$Title.Layer$Rows$Fill
                alpha<- gg.table.spec$Title.Layer$Rows$Alpha
                row.colour <- gg.table.spec$Title.Layer$Rows$Col
 
                quo <- 1/15
                row.title.adj <- max(nchar(row.lbls))*quo
                row.title.xmin <- xmin-row.title.adj
                row.title.xmax <- xmin
                row.title.ymin <- ymin-1
                row.title.ymax <- Rect.Column.df$ymin
 
              Title.Row.df <- data.frame(lab.y=DataLayer.df$ycoord,lab.x=rep(row.title.xmin,length(row.lbls)),Text=row.lbls,Font=row.font,stringsAsFactors=F)
                Rect.Row.df <- data.frame(xmin=row.title.xmin,xmax=row.title.xmax,ymin=row.title.ymin,ymax=row.title.ymax,fill=fill,alpha=alpha,stringsAsFactors=F)
 
                Title.Row.Layer <- geom_text(data=Title.Row.df,aes(x=lab.x,y=lab.y,label=Text,fontface=Font,hjust=0,vjust=1),size=rel(3),colour=row.colour)
                Rect.Row.Layer <- geom_rect(data=Rect.Row.df,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=alpha,fill=fill)
            }
 
#column highlights
            if(!gg.table.spec$Highlight.Layer$Columns$Exist){
            Highlight.Column.Layer <- NULL
            }else{
                hl.col.dyn <- 0
              hl.col.ymin <- ymax-gg.table.spec$Highlight.Layer$Columns$Which+hl.col.dyn
                hl.col.adj <- Rect.Column.df$ymin-ymax
 
                hl.col.xmin <- Rect.Column.df$xmin
              hl.col.xmax <- Rect.Column.df$xmax
          hl.col.ymax <- hl.col.ymin+1
 
                n.hl <- length(hl.col.ymin)
 
                fill <- gg.table.spec$Highlight.Layer$Columns$Fill
                alpha<- gg.table.spec$Highlight.Layer$Columns$Alpha
 
                Highlight.Column.df <- data.frame(xmin=rep(hl.col.xmin,n.hl),xmax=rep(hl.col.xmax,n.hl),ymin=hl.col.ymin,ymax=hl.col.ymax,fill=fill,alpha=alpha,stringsAsFactors=F)
          Highlight.Column.Layer <- geom_rect(data=Highlight.Column.df,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=fill,alpha=alpha)
 
            }
 
#row highlights
            if(!gg.table.spec$Highlight.Layer$Rows$Exist){
            Highlight.Row.Layer <- NULL
            }else{
                hl.row.adj <- 1-Rect.Row.df$xmax
                hl.row.xmin <- rev(gg.table.spec$Highlight.Layer$Rows$Which)-hl.row.adj
              hl.row.xmax <- rev(gg.table.spec$Highlight.Layer$Rows$Which)+1-hl.row.adj
                hl.row.ymin <- 0
          hl.row.ymax <- Rect.Row.df$ymax
                n.hl <- length(hl.row.xmin)
 
                fill <- rev(gg.table.spec$Highlight.Layer$Rows$Fill)
                alpha<- gg.table.spec$Highlight.Layer$Rows$Alpha
 
                Highlight.Row.df <- data.frame(xmin=hl.row.xmin,xmax=hl.row.xmax,ymin=hl.row.ymin,ymax=hl.row.ymax,fill=fill,alpha=alpha,stringsAsFactors=F)
          Highlight.Row.Layer <- geom_rect(data=Highlight.Row.df,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill=fill,alpha=alpha)
 
            }   
 
ggtbl <- EmptyLayer +
                        Rect.Column.Layer +
                        Title.Column.Layer +
                        Rect.Row.Layer +
                        Title.Row.Layer +
                        Highlight.Column.Layer +
                        Highlight.Row.Layer +
                        DataLayer
 
return(ggtbl)
}
########################################################################################################


ggplots.align <- function(...){
	g_list <- list(...)
	g<-do.call(rbind,c(lapply(g_list,ggplotGrob),list(size='first')))
	grid.newpage()
	grid.draw(g)
}

marshall.df.gen <- function(inc,alpha,px,py,n_curves){
		opt_qx_vec <- NULL
		opt_px_vec <- NULL
		opt_qy_vec <- NULL
	  opt_py_vec <- NULL
		
		for(i in seq(1,n_curves,0.5)){
	 		opt_qx_vec <- c(opt_qx_vec,cobd.max(inc=inc,p_x=0+i,p_y=py,alpha=alpha)$marshall_x)
	 		opt_px_vec <- c(opt_px_vec,0+i)
			opt_qy_vec <- c(opt_qy_vec,cobd.max(inc=inc,p_x=px,p_y=0+i,alpha=alpha)$marshall_y)
	 		opt_py_vec <- c(opt_py_vec,0+i)
			}
	return(data.frame(qx=opt_qx_vec,px=opt_px_vec,qy=opt_qy_vec,py=opt_py_vec))
}


inc.consumption <- function(alpha,px,py,inc){
	min_inc <- 100
	inc_vec <- seq(min_inc,inc,50)
	inc_num <- length(inc_vec)
	zero_vec <- rep(0,inc_num)

	j<-1
	
	new.optim.x <- new.optim.y <- new.optim.u <- NULL
	op <- u_spec_df <-  bc <-  bc_df <- list()
		
	x<-seq(from=0,to=400,by=1)
  y<-seq(from=0,to=400,by=1)
	z<-outer(x,y,cobd,alpha=alpha)
	melted <- melt(z)
	names(melted) <- c('x','y','z')
	
	for(i in inc_vec){
		utility.max <- cobd.max(inc=i,p_x=px,p_y=py,alpha=alpha,x,y)
			new.optim.x[j] <- round(utility.max$marshall_x)
			new.optim.y[j] <- round(utility.max$marshall_y)
			new.optim.u[j]<- round(utility.max$indirect_u)
		op[[j]] <- data.frame(optx=new.optim.x[j],opty=new.optim.y[j])
		u_spec <- cobd.y(u=new.optim.u[j],x=x,alpha=alpha)
		u_spec_df[[j]] <- data.frame(x=x,y=u_spec)

		bc[[j]] <- cobd.bc(Inc=i,px=px,py=py,x=x,y=y)
 		 	maxx <- max(x)-20
  		tempy <- bc[[j]][which(bc[[j]][,1]==maxx),2]
			maxy <- subset(u_spec_df[[j]],x==maxx)[2]
	
			xtemp <- bc[[j]][which(bc[[j]][,2]==0),1]
			ytemp <- u_spec_df[[j]][xtemp,2]
			
		bc_df[[j]] <- data.frame(x=xtemp,y=ytemp)

		j=j+1
	}

	inc_curve <- data.frame(x=new.optim.x,y=new.optim.y)
	engel_curve <- data.frame(x=new.optim.x,y=inc_vec)	

	#plotting contours
	ymax <- 400
	xmax <- 400
	plot.contours <- ggplot(melted, environment = environment())+
 		    								theme_tufte()+
												geom_tile(aes(x,y,z=z,fill=z))+
												stat_contour(aes(x,y,z=z),size=0.3,bins=20,linetype='dashed',alpha=0.3)+
										    scale_fill_gradientn(colours=jet.col(n=nrow(z)))+
					 						  theme(legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	                      labs(title='The Income-Consumption Curve')+
		                    xlab('Quantity [x]')+
		    								ylab('Quantity [y]')+
                        expand_limits(x = 0, y = 0)+
		                    scale_x_continuous(limits = c(0, xmax),expand=c(0,0))+
		                    scale_y_continuous(limits = c(0, ymax),expand=c(0,0))

	#Two for loops to ensure red points are above line plots
	for(i in 1:inc_num){
		plot.contours <- plot.contours +
			            geom_line(data=bc[[i]],aes(x=bc_x,y=bc_y),colour='black',size=0.5)+
 									geom_line(data=u_spec_df[[i]],aes(x=x,y=y),colour='black',size=0.5)+
			            geom_line(data=inc_curve,aes(x=x,y=y),colour='green',size=1)+
								  geom_vline(xintercept=op[[i]]$optx,colour='gold',size=0.5,linetype='dashed')
	}

 for(i in 1:inc_num){
		plot.contours <- plot.contours +
	  		            	geom_point(data=op[[i]],aes(x=optx,y=opty),colour='red',size=3)+
 											geom_text(data=op[[i]],aes(x=optx,y=opty),colour='red',size=4,label=letters[i],hjust=0,vjust=1)
}
	
#Demand Curves
marshall_df_list <- list()
dem.curves.plot <- ggplot()

h<-1
for(i in inc_vec){
	marshall_df_list[[h]] <- marshall.df.gen(i,alpha=alpha,px=px,py=py,n_curves=10)
	dem.curves.plot <- dem.curves.plot+
											geom_line(data=marshall_df_list[[h]],aes(x=qx,y=px),colour='black',size=0.5)+
											geom_vline(xintercept=new.optim.x,colour='gold',size=0.5,linetype='dashed')
	h=h+1
}	

dem.curves.plot <- dem.curves.plot+
		                geom_hline(yintercept=px,colour='black',size=0.5,linetype='dashed')+
										geom_point(data=data.frame(x=new.optim.x,y=rep(px,length(new.optim.x))),aes(x=x,y=y),colour='red',size=3)+
 										geom_text(data=data.frame(x=new.optim.x,y=rep(px-0.5,length(new.optim.x))),aes(x=x,y=y),colour='red',size=4,label=letters[1:length(new.optim.x)])+
										labs(title='Marshallian Demand for good x')+
		            		ylab('Price [x]')+
		    						xlab('Quantity [x]')+
							 	    theme(panel.background = element_rect(fill = NA),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
                		expand_limits(x = 0, y = 0)+
		            		scale_x_continuous(limits = c(0, xmax),expand=c(0,0))+
		            		scale_y_continuous(limits = c(0, 6),expand=c(0,0))	

#Engel Curve
xmax <- 400
ymax <- 410
engel.plot <- ggplot(engel_curve, environment = environment())+
								geom_line(aes(x=x,y=y),colour='green',size=0.5)+
 								geom_text(aes(x=x,y=y),colour='red',size=4,label=letters[1:inc_num],vjust=1,hjust=0)+
								geom_vline(xintercept=new.optim.x,colour='gold',size=0.5,linetype='dashed')+
		 		  		  geom_point(aes(x=x,y=y),colour='red',size=3)+
								labs(title='The Engel curve for good x')+
		            ylab('Income')+
		    				xlab('Quantity [x]')+
	 						  theme(panel.background = element_rect(fill = NA),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
                expand_limits(x = 0, y = 0)+
		            scale_x_continuous(limits = c(0, xmax),expand=c(0,0))+
		            scale_y_continuous(limits = c(0, ymax),expand=c(0,0))
	return(list(inc.consumption.plot=plot.contours,engel.plot=engel.plot,dem.curves.plot=dem.curves.plot))

}


welfare.compare <- function(alpha,px){
	x<-seq(from=0,to=300,by=1)
  y<-seq(from=0,to=300,by=1)
	z<-outer(x,y,cobd,alpha=alpha)
	melted <- melt(z)
	names(melted) <- c('x','y','z')
	inc <- 200
	py_fixed <- 2
	
	#contours
	bc_orig <- cobd.bc(Inc=inc,px=1,py=py_fixed,x=x,y=y)
	opt_x_orig <- cobd.max(inc=inc,p_x=1,p_y=py_fixed,alpha=alpha)$marshall_x
	opt_y_orig <-cobd.max(inc=inc,p_x=1,p_y=py_fixed,alpha=alpha)$marshall_y
	opt_util_orig <- cobd.max(inc=inc,p_x=1,p_y=py_fixed,alpha=alpha)$indirect_u
 	opt_spec_orig <- cobd.y(u=opt_util_orig,x=x,alpha=alpha)
  u_orig_df <- data.frame(x=x,y=opt_spec_orig)

	bc_new <- cobd.bc(Inc=inc,px=1+px,py=py_fixed,x=x,y=y)
	opt_util_new <- cobd.max(inc=inc,p_x=1+px,p_y=py_fixed,alpha=alpha)$indirect_u
	opt_x_new <- cobd.max(inc=inc,p_x=1+px,p_y=py_fixed,alpha=alpha)$marshall_x
	opt_y_new <-cobd.max(inc=inc,p_x=1+px,p_y=py_fixed,alpha=alpha)$marshall_y
	opt_spec_new <- cobd.y(u=opt_util_new,x=x,alpha=alpha)
	u_new_df <- data.frame(x=x,y=opt_spec_new)
	
	optim_exp <- expenditure.min(target.u=opt_util_orig,alpha=alpha,px=1+px,py=py_fixed)$minimal_exp
  hicks_y <- expenditure.y(Inc=optim_exp,px=1+px,py=py_fixed,x=x)
 	hicks_df <- data.frame(x=x,y=hicks_y)
	op_hicks_x <- expenditure.min(target.u=opt_util_orig,alpha=alpha,px=1+px,py=py_fixed)$hicksian_x
	op_hicks_y <- expenditure.min(target.u=opt_util_orig,alpha=alpha,px=1+px,py=py_fixed)$hicksian_y

	optim_ev <- expenditure.min(target.u=opt_util_new,alpha=alpha,px=1,py=py_fixed)$minimal_exp
  ev_y <- expenditure.y(Inc=optim_ev,px=1,py=py_fixed,x=x)
 	ev_df <- data.frame(x=x,y=ev_y)
	op_ev_x <- expenditure.min(target.u=opt_util_new,alpha=alpha,px=1,py=py_fixed)$hicksian_x
	op_ev_y <- expenditure.min(target.u=opt_util_new,alpha=alpha,px=1,py=py_fixed)$hicksian_y
		
	#demand curves
	m_slope <- (px-1)/(opt_x_new-opt_x_orig)
	m_int <- m_slope*(-1)*opt_x_orig+1
	
	price_hicks <- (inc-(py_fixed*op_hicks_y))/op_hicks_x
	h_slope <- (px-1)/(op_hicks_x-opt_x_orig)
	h_int <- h_slope*(-1)*opt_x_orig+1
	
	m_y <- m_slope*x+m_int
	h_y <- h_slope*x+h_int
	
	demand_df <- data.frame(x=x,my=m_y,hy=h_y)
	
	#points
  op_orig <- data.frame(optx=opt_x_orig,opty=opt_y_orig)
	op_new <- data.frame(optx=opt_x_new,opty=opt_y_new)
	op_hicks <- data.frame(x=op_hicks_x,y=op_hicks_y)
	op_ev <- data.frame(x=op_ev_x,y=op_ev_y)
	dem_points <- data.frame(x=c(opt_x_orig,op_hicks_x,opt_x_new),y=c(1,px,px))
	
	#text
	dem_text <- data.frame(x=c(opt_x_orig,op_hicks_x,opt_x_new),y=c(-2,-2,-2),lab=c('original','compensated','new'))
	dem_ctext <- data.frame(x=c(7,7),y=c(m_y[1],h_y[1]),lab=c('Marshallian','Hicksian'),angles=c(atan((m_slope))*360/2/pi*9,atan((h_slope))*360/2/pi*9))
	p_text_df <- data.frame(x=rep(200,2),y=c(1,px),lab=c('original price = 1',paste('new price = ',px)))
  leg.df <- data.frame(x=rep(290,4),y=seq(290,200,length.out=5)[1:4])
	
	#trace
	op_orig_trace <- data.frame(x=rep(opt_x_orig,2),y=c(0,opt_y_orig))
	op_new_trace <- data.frame(x=rep(opt_x_new,2),y=c(0,opt_y_new))
  op_hicks_trace <- data.frame(x=rep(op_hicks_x,2),y=c(0,op_hicks_y))
	op_ev_trace <- data.frame(x=rep(op_ev_x,2),y=c(0,op_ev_y))
	ymax <- 200
  xmax <- 200
	
	#welfare changes
	cv_line <- data.frame(x=rep(0,2),y=c(as.numeric(subset(bc_orig,bc_x==0)[2]),as.numeric(subset(hicks_df,x==0)[2]))) 
	ev_line <- data.frame(x=rep(0,2),y=c(as.numeric(subset(ev_df,x==0)[2]),as.numeric(subset(bc_orig,bc_x==0)[2]))) 
	cv <- round(as.numeric(subset(hicks_df,x==0)[2])-as.numeric(subset(bc_orig,bc_x==0)[2]))
	ev <- round(as.numeric(subset(bc_orig,bc_x==0)[2])-as.numeric(subset(ev_df,x==0)[2]))
	
	#table
	case.vec <- c('original','new','a','b')
	table.df <- data.frame(price_x=c(1,1+px,1+px,1),price_y=rep(py_fixed,4),optimal_x=round(c(opt_x_orig,opt_x_new,op_hicks_x,op_ev_x)),optimal_y=round(c(opt_y_orig,opt_y_new,op_hicks_y,op_ev_y)),Utility=round(c(opt_util_orig,opt_util_new,opt_util_orig,opt_util_new)),C_Variation=c('-',cv,'-','-'),Eq_Variation=c('-',ev,'-','-'))
	rownames(table.df) <- case.vec
	
	table.spec <- ggTableSpec(columns.exist=T,columns.txt=colnames(table.df),columns.font=rep('bold',7),columns.col='black',columns.fill='white',columns.alpha=0.3,
                          rows.exist=T,rows.txt=rownames(table.df),rows.font=rep('bold',4),rows.col='black',rows.fill='white',rows.alpha=0.3,
                          data.obj=table.df,data.col='black',data.title='',
                          hlt.col.exist=T,hl.col.which=c(1,2,3,4),hl.col.fill=c('white','green','red','red'),hl.col.alpha=rep(0.3,4),
                          hlt.row.exist=F,hl.row.which=c(2,3,4),hl.row.fill=c('white','red','red'),hl.row.alpha=rep(0.4,)
                )
	

	plot.contours <- ggplot(melted, environment = environment())+
 		    								theme_tufte()+
												geom_tile(aes(x,y,z=z,fill=z))+
												stat_contour(aes(x,y,z=z),size=0.3,linetype='dashed',bins=20,alpha=0.3)+
										    scale_fill_gradientn(colours=jet.col(n=nrow(z)))+
		                    geom_line(data=bc_new,aes(x=bc_x,y=bc_y),colour='green',size=0.5)+
		    								geom_line(data=u_new_df,aes(x=x,y=y),colour='green',size=0.5)+
				                geom_line(data=hicks_df,aes(x=x,y=y),colour='gold',size=0.5,linetype='dashed')+
												geom_line(data=op_new_trace,aes(x=x,y=y),colour='green',size=0.5,linetype='dashed')+
												geom_line(data=op_hicks_trace,aes(x=x,y=y),colour='red',size=0.5,linetype='dashed')+
												geom_line(data=ev_df,aes(x=x,y=y),colour='gold',size=0.5,linetype='dashed')+
												geom_line(data=op_ev_trace,aes(x=x,y=y),colour='red',size=0.5,linetype='dashed')+
		                    geom_line(data=bc_orig,aes(x=bc_x,y=bc_y),colour='white',size=0.5)+
		    								geom_line(data=u_orig_df,aes(x=x,y=y),colour='white',size=0.5)+
		                 		geom_line(data=op_orig_trace,aes(x=x,y=y),colour='white',size=0.5,linetype='dashed')+
					  						geom_point(data=op_ev,aes(x=x,y=y),colour='red',size=3)+
  											geom_point(data=op_new,aes(x=optx,y=opty),colour='green',size=3)+
			  								geom_point(data=op_hicks,aes(x=x,y=y),colour='red',size=3)+           	
												geom_point(data=op_orig,aes(x=optx,y=opty),colour='white',size=3)+
												geom_line(data=cv_line,aes(x=x,y=y),colour='cyan',size=2)+
												geom_line(data=ev_line,aes(x=x,y=y),colour='purple',size=2)+
												geom_text(data=leg.df,aes(x=x,y=y),label=c(paste('Original\n{x=',round(opt_x_orig),'| y=',round(opt_y_orig),'}'),paste('New optimum\n{x=',round(opt_x_new),'| y=',round(opt_y_new),'}'),paste('Compensating Variation\n{CV=',round(cv),'}'),paste('Equivalent Variation\n{EV=',round(ev),'}')),size=3,colour=c('white','green','cyan','purple'),vjust=1,hjust=1,fontface='bold')+
												geom_text(data=op_hicks,aes(x=x,y=y),label='a',size=4,colour='red',hjust=0,vjust=0)+
 												geom_text(data=op_ev,aes(x=x,y=y),label='b',size=4,colour='red',hjust=0,vjust=0)+
		                    labs(title='Welfare effects of a price increase in good x')+
			 						      theme(legend.position='none',plot.title=element_text(size=12*0.9))+ 								
		                    xlab('Quantity [x]')+
		    								ylab('Quantity [y]')+
                        expand_limits(x = 0, y = 0)+
		                    scale_x_continuous(limits = c(0, 300),expand=c(0,0))+
		                    scale_y_continuous(limits = c(0, 300),expand=c(0,0))
	
	gg.table <- ggTableDrawer(table.spec)

	return(list(plot.contours=plot.contours,gg.table=gg.table))

}

demand.compare <- function(alpha,inc,py_fixed,px){
	x<-seq(from=0,to=200,by=1)
  y<-seq(from=0,to=200,by=1)
	z<-outer(x,y,cobd,alpha=alpha)
	melted <- melt(z)
	names(melted) <- c('x','y','z')
	
	bc_orig <- cobd.bc(Inc=inc,px=1,py=py_fixed,x=x,y=y)
	opt_x_orig <- cobd.max(inc=inc,p_x=1,p_y=py_fixed,alpha=alpha)$marshall_x
	opt_y_orig <-cobd.max(inc=inc,p_x=1,p_y=py_fixed,alpha=alpha)$marshall_y
	opt_util_orig <- cobd.max(inc=inc,p_x=1,p_y=py_fixed,alpha=alpha)$indirect_u
 	opt_spec_orig <- cobd.y(u=opt_util_orig,x=x,alpha=alpha)
  u_orig_df <- data.frame(x=x,y=opt_spec_orig)

	bc_new <- cobd.bc(Inc=inc,px=px,py=py_fixed,x=x,y=y)
	opt_util_new <- cobd.max(inc=inc,p_x=px,p_y=py_fixed,alpha=alpha)$indirect_u
	opt_x_new <- cobd.max(inc=inc,p_x=px,p_y=py_fixed,alpha=alpha)$marshall_x
	opt_y_new <-cobd.max(inc=inc,p_x=px,p_y=py_fixed,alpha=alpha)$marshall_y
	opt_spec_new <- cobd.y(u=opt_util_new,x=x,alpha=alpha)
	u_new_df <- data.frame(x=x,y=opt_spec_new)
	
	optim_exp <- expenditure.min(target.u=opt_util_orig,alpha=alpha,px=px,py=py_fixed)$minimal_exp
  hicks_y <- expenditure.y(Inc=optim_exp,px=px,py=py_fixed,x=x)
 	hicks_df <- data.frame(x=x,y=hicks_y)
	op_hicks_x <- expenditure.min(target.u=opt_util_orig,alpha=alpha,px=px,py=py_fixed)$hicksian_x
	op_hicks_y <- expenditure.min(target.u=opt_util_orig,alpha=alpha,px=px,py=py_fixed)$hicksian_y

	#demand curves
	m_slope <- (px-1)/(opt_x_new-opt_x_orig)
	m_int <- m_slope*(-1)*opt_x_orig+1
	
	price_hicks <- (inc-(py_fixed*op_hicks_y))/op_hicks_x
	h_slope <- (px-1)/(op_hicks_x-opt_x_orig)
	h_int <- h_slope*(-1)*opt_x_orig+1
	
	m_y <- m_slope*x+m_int
	h_y <- h_slope*x+h_int
	
	demand_df <- data.frame(x=x,my=m_y,hy=h_y)
	
	#points
  op_orig <- data.frame(optx=opt_x_orig,opty=opt_y_orig)
	op_new <- data.frame(optx=opt_x_new,opty=opt_y_new)
	op_hicks <- data.frame(x=op_hicks_x,y=op_hicks_y)
	dem_points <- data.frame(x=c(opt_x_orig,op_hicks_x,opt_x_new),y=c(1,px,px))
	
	#text
	dem_text <- data.frame(x=c(opt_x_orig,op_hicks_x,opt_x_new),y=c(-2,-2,-2),lab=c('original','compensated','new'))
	dem_ctext <- data.frame(x=c(7,7),y=c(m_y[1],h_y[1]),lab=c('Marshallian','Hicksian'),angles=c(atan((m_slope))*360/2/pi*9,atan((h_slope))*360/2/pi*9))
	p_text_df <- data.frame(x=rep(200,2),y=c(1,px),lab=c('original price = 1',paste('new price = ',px)))

	#trace
	op_orig_trace <- data.frame(x=rep(opt_x_orig,2),y=c(0,opt_y_orig))
	op_new_trace <- data.frame(x=rep(opt_x_new,2),y=c(0,opt_y_new))
  op_hicks_trace <- data.frame(x=rep(op_hicks_x,2),y=c(0,op_hicks_y))
	
	ymax <- 200
  xmax <- 200
	
	plot.contours <- ggplot(melted, environment = environment())+
 		    								theme_tufte()+
												geom_tile(aes(x,y,z=z,fill=z))+
												stat_contour(aes(x,y,z=z),size=0.3,linetype='dashed',bins=20,alpha=0.3)+
										    scale_fill_gradientn(colours=jet.col(n=nrow(z)))+
		                    geom_line(data=bc_orig,aes(x=bc_x,y=bc_y),colour='black',size=0.5)+
		    								geom_line(data=u_orig_df,aes(x=x,y=y),colour='black',size=0.5)+
		                    geom_line(data=bc_new,aes(x=bc_x,y=bc_y),colour='white',size=0.5)+
		    								geom_line(data=u_new_df,aes(x=x,y=y),colour='white',size=0.5)+
				                geom_line(data=hicks_df,aes(x=x,y=y),colour='gold',size=0.5,linetype='dashed')+
		                    geom_line(data=op_orig_trace,aes(x=x,y=y),colour='black',size=0.5,linetype='dashed')+
												geom_line(data=op_new_trace,aes(x=x,y=y),colour='black',size=0.5,linetype='dashed')+
												geom_line(data=op_hicks_trace,aes(x=x,y=y),colour='red',size=0.5,linetype='dashed')+
		  									geom_point(data=op_orig,aes(x=optx,y=opty),colour='black',size=3)+
		  									geom_point(data=op_new,aes(x=optx,y=opty),colour='black',size=3)+
			  								geom_point(data=op_hicks,aes(x=x,y=y),colour='red',size=3)+
	                      labs(title='Comparing the marshallian and hicksian demand curves')+
			 						      theme(legend.position='none',plot.title=element_text(size=12*0.9))+ 								
		                    xlab('Quantity [x]')+
		    								ylab('Quantity [y]')+
                        expand_limits(x = 0, y = 0)+
		                    scale_x_continuous(limits = c(0, xmax),expand=c(0,0))+
		                    scale_y_continuous(limits = c(0, ymax),expand=c(0,0))
	
	plot.demand.comp <- ggplot(demand_df)+
						 			     geom_line(aes(x=x,y=my),colour='black',size=0.5)+
											 geom_line(aes(x=x,y=hy),colour='darkgreen',size=0.5)+
		 									 labs(title='Comparing the marshallian and hicksian demand curves')+
			 						     theme(panel.background = element_rect(fill = NA),legend.position='none',plot.title=element_text(size=12*0.9))+ 								
		                   geom_vline(xintercept=opt_x_orig,colour='black',size=0.5,linetype='dashed')+
		                   geom_hline(yintercept=1,colour='black',size=0.5,linetype='dashed')+
		                   geom_text(data=p_text_df,aes(x=x,y=y,label=lab),colour=c('black','red'),size=4,hjust=1,vjust=1)+
		                   geom_vline(xintercept=opt_x_new,colour='black',size=0.5,linetype='dashed')+
		                   geom_hline(yintercept=px,colour='black',size=0.5,linetype='dashed')+
		                   geom_vline(xintercept=op_hicks_x,colour='red',size=0.5,linetype='dashed')+
		                   geom_hline(yintercept=px,colour='red',size=0.5,linetype='dashed')+
		                   geom_point(data=dem_points,aes(x=x,y=y),colour=c('black','red','black'),size=3)+
		                   geom_text(data=dem_text,aes(x=x,y=y,label=lab),colour=c('black','red','black'),size=4,angle=90,hjust=0)+
				               geom_text(data=dem_ctext,aes(x=x,y=y,label=lab),colour=c('black','darkgreen'),size=4,vjust=1)+
		                   xlab('Quantity [x]')+
		    							 ylab('Price [x]')+
                       expand_limits(x = 0, y = 0)+
		                   scale_x_continuous(expand = c(0, 0),limits=c(0,200)) + 
		                   scale_y_continuous(expand = c(0, 0),limits=c(-2,NA))
	
	
	return(list(plot.contours=plot.contours,plot.demand.comp=plot.demand.comp))
}
	
cobd <- function(x,y,alpha){
	(x^alpha)*(y^(1-alpha))
}

cobd.y <- function(u,x,alpha){
	return((u/(x^alpha))^(1/(1-alpha)))
}

cobd.bc <- function(Inc,px,py,x,y){
	bc_x <- x
	bc_y <- (Inc-(px*x))/py
	return(bc=data.frame(bc_x=bc_x,bc_y=bc_y))
}

cobd.max <- function(inc,p_x,p_y,alpha,x,y){
	beta <- 1-alpha
	marshall_x <- round((inc/p_x)*(alpha/(alpha+beta)))
	marshall_y <- round((inc/p_y)*(beta/(alpha+beta)))
	indirect_u <-  round(((alpha^alpha)*(beta^beta))/((alpha+beta)^(alpha+beta)) *(inc^(alpha+beta))/((p_x^alpha)*(p_y^beta)))
	return(list(marshall_x=marshall_x,marshall_y=marshall_y,indirect_u=indirect_u))
}

expenditure.min <- function(target.u,alpha,px,py){
	beta <- 1-alpha
	hicksian_x <- target.u*((alpha/beta)*(py/px))^(beta)
	hicksian_y <- target.u*((beta/alpha)*(px/py))^(alpha)
	minimal_exp <- (px^alpha)*(py^beta)*target.u*((1/alpha)^alpha)*((1/beta)^beta)
	return(list(hicksian_x=hicksian_x,hicksian_y=hicksian_y,minimal_exp=minimal_exp))
}

expenditure.y <- function(Inc,px,py,x){
	return((Inc-(px*x))/py)
}

inputs.transform <- function(alpha,Inc,mininc,minpx,minpy,maxpx,maxpy,px,py){
	maxinc <- maxpx*((mininc/minpx)+10)+ maxpy*((mininc/minpy)+10)
	x<-seq(from=0,to=(maxinc/minpx)+10,by=1)
  y<-seq(from=0,to=(maxinc/minpy)+10,by=1)
  z<-outer(x,y,cobd,alpha=alpha)
	melted <- melt(z)
	names(melted) <- c('x','y','z')
	return(list(Inc=Inc,alpha=alpha,mininc=mininc,minpx=minpx,minpy=minpy,maxpx=maxpx,maxpy=maxpy,px=px,py=py,maxinc=maxinc,x=x,y=y,z=z,melted=melted))
}

hicksian.demand.curve <- function(alpha,py_fixed,target.u,col_low,col_high,num){

	opt_q_vec <- NULL
	opt_px_vec <- NULL
	optim_exp <- NULL
	opt_qy_fixed <- NULL
	hicks_y <- list()
	hicks_df <- list()
	
	orig_q_vec <- NULL
	orig_px_vec <- NULL
	
	opt_spec_list <- list()
	opt_spec_df <- list()
	trace_df <- list()
	trace_demand_df <- list()
	
	trace_orig_demand_df<- list()
	trace_orig_df<- list()
	orig_qy_fixed <- NULL
	x<-seq(from=0,to=(400/1),by=1)
  y<-seq(from=0,to=(400/1),by=1)
	z<-outer(x,y,cobd,alpha=alpha)
	melted <- melt(z)
	names(melted) <- c('x','y','z')
	n_curves <- num
	
	col_ramp <- colorRampPalette(c(col_low,col_high),alpha=TRUE)(n_curves)
	
	#original contours
		optim_exp_orig<- expenditure.min(target.u=29,alpha=0.5,px=1,py=3)$minimal_exp
    hicks_y_orig <- expenditure.y(Inc=optim_exp_orig,px=1,py=3,x=x)
 		hicks_df_orig <- data.frame(x=x,y=hicks_y_orig)
	 	opt_spec_orig<- cobd.y(u=29,x=x,alpha=0.5)
 	  opt_spec_orig_df <- data.frame(x=x,y=opt_spec_orig)
	
	
	#demand curve & budget constraint & utility
	for(i in 1:n_curves){
	 	orig_q_vec <- c(orig_q_vec,expenditure.min(target.u=29,alpha=0.5,px=0+i,py=3)$hicksian_x)
	 	orig_px_vec <- c(orig_px_vec,0+i)
		orig_qy_fixed <- c(orig_qy_fixed,expenditure.min(target.u=29,alpha=0.5,px=0+i,py=3)$hicksian_x)
		
	 	opt_q_vec <- c(opt_q_vec,expenditure.min(target.u=target.u,alpha=alpha,px=0+i,py=py_fixed)$hicksian_x)
	 	opt_px_vec <- c(opt_px_vec,0+i)
		opt_qy_fixed <- c(opt_qy_fixed,expenditure.min(target.u=target.u,alpha=alpha,px=0+i,py=py_fixed)$hicksian_y)
		optim_exp <- c(optim_exp,expenditure.min(target.u=target.u,alpha=alpha,px=0+i,py=py_fixed)$minimal_exp)
    hicks_y[[i]] <- expenditure.y(Inc=optim_exp[i],px=0+i,py=py_fixed,x=x)
 		hicks_df[[i]] <- data.frame(x=x,y=hicks_y[[i]])
	  trace_df[[i]] <- data.frame(x=c(rep(opt_q_vec[i],2)),y=c(0,opt_qy_fixed[i])) 
		trace_demand_df[[i]] <- data.frame(x=c(opt_q_vec[i],opt_q_vec[i]),y=c(0,opt_px_vec[i]))
	  trace_orig_df[[i]] <- data.frame(x=c(rep(orig_q_vec[i],2)),y=c(0,orig_qy_fixed[i])) 
		trace_orig_demand_df[[i]] <- data.frame(x=c(orig_q_vec[i],orig_q_vec[i]),y=c(0,orig_px_vec[i]))

	}
	opt_spec_list <- cobd.y(u=target.u,x=x,alpha=alpha)
 	opt_spec_df <- data.frame(x=x,y=opt_spec_list)

	#plotting contours
	xmax <- 200
  ymax <- 400
	
	plot.contours <- ggplot(melted, environment = environment())+
 		    								theme_tufte()+
												geom_tile(aes(x,y,z=z,fill=z))+
												stat_contour(aes(x,y,z=z),size=0.3,bins=20,linetype='dashed',alpha=0.3)+
										    scale_fill_gradientn(colours=jet.col(n=nrow(z)))+
					 						  theme(legend.position='none',plot.title=element_text(size=12*0.9))+ 								
	                      labs(title=paste('Deriving the Hicksian Compensated Demand Curve for good x\n','{Price of good x increases from 1 to ',n_curves,' | ','Price of good y is fixed at : ',py_fixed,' | Utility is fixed at :',target.u,'}'))+
		                    xlab('Quantity [x]')+
		    								ylab('Quantity [y]')+
                        expand_limits(x = 0, y = 0)+
		                    scale_x_continuous(limits = c(0, xmax),expand=c(0,0))+
		                    scale_y_continuous(limits = c(0, ymax),expand=c(0,0))

	for(i in 1:n_curves){
		plot.contours <- plot.contours +
 			                 geom_line(data=opt_spec_df,aes(x=x,y=y),colour=col_ramp[i],size=0.5)+
			                 geom_line(data=subset(hicks_df[[i]],y>=0),aes(x=x,y=y),colour=col_ramp[i],size=0.5)+
			                 geom_line(data=trace_df[[i]],aes(x=x,y=y),colour=col_ramp[i],linetype='dashed',size=0.5)

	}
	plot.contours <- plot.contours+
 											 geom_line(data=hicks_df_orig,aes(x=x,y=y),colour='black',size=0.5)+
		    							 geom_line(data=opt_spec_orig_df,aes(x=x,y=y),colour='black',size=0.5)
		    
	#Hicksian Demand Curve
	hicksian_df <- data.frame(qx=opt_q_vec,px=opt_px_vec)
  hicks_text <- data.frame(x=max(opt_q_vec),y=min(opt_px_vec))
	
	orig_df <- data.frame(qx=orig_q_vec,px=orig_px_vec)
	orig_text <- data.frame(x=max(orig_q_vec),y=min(orig_px_vec))
  xmax <- 200
	ymax <- 30
	
	plot.demand.curve <- ggplot(data=orig_df,aes(x=qx,y=px))+
                       	geom_line(colour='black',size=0.5,alpha=1)+
		                    geom_line(data=hicksian_df,aes(x=qx,y=px),colour='red',size=0.5,alpha=1)+
		                    geom_text(data=hicks_text,aes(x=x,y=y),label='new',size=3,colour='red',vjust=1,hjust=1)+
		                    geom_text(data=orig_text,aes(x=x,y=y),label='original',size=3,colour='black',vjust=1,hjust=1)+
		         						labs(title=paste('The Hicksian Demand Curve For Good x\n original{U=29,price_y=3,alpha=0.5}'))+
		                    xlab('Quantity [x]')+
		    								ylab('Price [x]')+
			 						      theme(panel.background = element_rect(fill = NA),legend.position='none',plot.title=element_text(size=12*0.9))+
                        expand_limits(x = 0, y = 0)+
		                    scale_x_continuous(limits = c(0, xmax),expand=c(0,0))+
		                    scale_y_continuous(limits = c(0, n_curves+2),expand=c(0,0))
		
		                    
  for(i in 1:n_curves){
  	plot.demand.curve <- plot.demand.curve +
  		                    geom_line(data=trace_demand_df[[i]],aes(x=x,y=y),colour=col_ramp[i],linetype='dashed',size=0.5)+
  		  		              geom_point(data=trace_demand_df[[i]][2,],aes(x=x,y=y),colour='red',size=3)+
  					              geom_point(data=trace_orig_demand_df[[i]][2,],aes(x=x,y=y),colour='black',size=3)

  }
	return(list(plot.contours=plot.contours,plot.demand.curve=plot.demand.curve))
}


marshallian.demand.curve <- function(inc,alpha,py_fixed,col_low,col_high,num){
	opt_q_vec <- NULL
	opt_px_vec <- NULL
	opt_util_vec <- NULL
	opt_qy_fixed <- NULL
	opt_spec_list <- list()
	opt_spec_df <- list()
	bc_list <-list()
	trace_df <- list()
	trace_demand_df <- list()
	trace_orig_demand_df<- list()
	trace_orig_df<- list()
	orig_qy_fixed <- NULL

	orig_q_vec <- NULL
	orig_px_vec <- NULL
	x<-seq(from=0,to=(200/1),by=1)
  y<-seq(from=0,to=(200/1),by=1)
	z<-outer(x,y,cobd,alpha=alpha)
	melted <- melt(z)
	names(melted) <- c('x','y','z')
	n_curves <- num
	
	col_ramp <- colorRampPalette(c(col_low,col_high),alpha=TRUE)(n_curves)

	#original contours
		bc_orig <- cobd.bc(Inc=100,px=1,py=3,x=x,y=y)
		opt_util_orig <- cobd.max(inc=100,p_x=1,p_y=3,alpha=0.5)$indirect_u
 		opt_spec_orig <- cobd.y(u=opt_util_orig,x=x,alpha=0.5)
 	  opt_spec_orig_df <- data.frame(x=x,y=opt_spec_orig)
	 
	#demand curve & budget constraint & utility
	for(i in 1:n_curves){
	 	orig_q_vec <- c(orig_q_vec,cobd.max(inc=100,alpha=0.5,p_x=0+i,p_y=3)$marshall_x)
	 	orig_px_vec <- c(orig_px_vec,0+i)
	 	orig_qy_fixed <- c(orig_qy_fixed,cobd.max(inc=100,p_x=0+i,p_y=3,alpha=0.5)$marshall_y)

		opt_q_vec <- c(opt_q_vec,cobd.max(inc=inc,p_x=0+i,p_y=py_fixed,alpha=alpha)$marshall_x)
	 	opt_px_vec <- c(opt_px_vec,0+i)
		opt_qy_fixed <- c(opt_qy_fixed,cobd.max(inc=inc,p_x=0+i,p_y=py_fixed,alpha=alpha)$marshall_y)
		opt_util_vec <- c(opt_util_vec,cobd.max(inc=inc,p_x=0+i,p_y=py_fixed,alpha=alpha)$indirect_u)
		bc_list[[i]] <- cobd.bc(Inc=inc,px=0+i,py=py_fixed,x=x,y=y)
 		opt_spec_list[[i]] <- cobd.y(u=opt_util_vec[i],x=x,alpha=alpha)
 	  opt_spec_df[[i]] <- data.frame(x=x,y=opt_spec_list[[i]])
	  trace_df[[i]] <- data.frame(x=c(rep(opt_q_vec[i],2)),y=c(0,opt_qy_fixed[i])) 
		trace_demand_df[[i]] <- data.frame(x=c(opt_q_vec[i],opt_q_vec[i]),y=c(0,opt_px_vec[i]))
	  trace_orig_df[[i]] <- data.frame(x=c(rep(orig_q_vec[i],2)),y=c(0,orig_qy_fixed[i])) 
		trace_orig_demand_df[[i]] <- data.frame(x=c(orig_q_vec[i],orig_q_vec[i]),y=c(0,orig_px_vec[i]))
	}
	
	
	#plotting contours
	xmax <- 200
	ymax <- (inc/py_fixed)+10
	
	plot.contours <- ggplot(melted, environment = environment())+
 		    								theme_tufte()+
												geom_tile(aes(x,y,z=z,fill=z))+
												stat_contour(aes(x,y,z=z),size=0.3,linetype='dashed',bins=20,alpha=0.3)+
										    scale_fill_gradientn(colours=jet.col(n=nrow(z)))+
	                      labs(title=paste('Deriving the Marshallian Demand Curve\n','{Income changes from 100 to : ',inc,' | ','Price of good x increases from 1 to ',n_curves,' | ','Price of good y changes from 3 to : ',py_fixed,' | ','Alpha changes from 0.5 to : ',alpha,'}'))+
			 						      theme(legend.position='none',plot.title=element_text(size=12*0.9))+ 								
		                    xlab('Quantity [x]')+
		    								ylab('Quantity [y]')+
                        expand_limits(x = 0, y = 0)+
		                    scale_x_continuous(limits = c(0, xmax),expand=c(0,0))+
		                    scale_y_continuous(limits = c(0, ymax),expand=c(0,0))
	
	for(i in 1:n_curves){
		plot.contours <- plot.contours +
			                 geom_line(data=subset(bc_list[[i]],bc_y>=0),aes(x=bc_x,y=bc_y),colour=col_ramp[i],size=0.5) +
			                 geom_line(data=opt_spec_df[[i]],aes(x=x,y=y),colour=col_ramp[i],size=0.5)+
			                 geom_line(data=trace_df[[i]],aes(x=x,y=y),colour=col_ramp[i],linetype='dashed',size=0.5)
			                 
	}
	  
	plot.contours <- plot.contours+
 											 geom_line(data=bc_orig,aes(x=bc_x,y=bc_y),colour='black',size=0.5)+
		    							 geom_line(data=opt_spec_orig_df,aes(x=x,y=y),colour='black',size=0.5)
		                   
	#Marshallian Demand Curve
	marshall_df <- data.frame(qx=opt_q_vec,px=opt_px_vec)
  marshall_text <- data.frame(x=max(opt_q_vec),y=min(opt_px_vec))
	orig_df <- data.frame(qx=orig_q_vec,px=orig_px_vec)
	orig_text <- data.frame(x=max(orig_q_vec),y=min(orig_px_vec))
  xmax <- 200
	ymax <- 30
	
	plot.demand.curve <- ggplot(data=orig_df,aes(x=qx,y=px))+
                       	geom_line(colour='black',size=0.5,alpha=1)+
		                    geom_line(data=marshall_df,aes(x=qx,y=px),colour='red',size=0.5,alpha=1)+
		                    geom_text(data=marshall_text,aes(x=x,y=y),label='new',size=3,colour='red',vjust=1,hjust=1)+
		                    geom_text(data=orig_text,aes(x=x,y=y),label='original',size=3,colour='black',vjust=1,hjust=1)+
		 										labs(title=paste('The Marshallian Demand Curve For Good x\n original{inc=100,price_y=3,alpha=0.5}',' | ','new{inc=',inc,',price_y=',py_fixed,',alpha=',alpha,'}'))+
		                    xlab('Quantity [x]')+
		    								ylab('Price [x]')+
			 						      theme(panel.background = element_rect(fill = NA),legend.position='none',plot.title=element_text(size=12*0.9))+
                        expand_limits(x = 0, y = 0)+
		                    scale_x_continuous(limit = c(0, xmax),expand=c(0,0))+
		                    scale_y_continuous(limit = c(0, n_curves+2),expand=c(0,0))
		
  for(i in 1:n_curves){
  	plot.demand.curve <- plot.demand.curve +
  		                    geom_line(data=trace_demand_df[[i]],aes(x=x,y=y),colour=col_ramp[i],linetype='dashed',size=0.5)+
  		  		              geom_point(data=trace_demand_df[[i]][2,],aes(x=x,y=y),colour='red',size=3)+
  		  		  		        geom_point(data=trace_orig_demand_df[[i]][2,],aes(x=x,y=y),colour='black',size=3)

  }

return(list(plot.contours=plot.contours,plot.demand.curve=plot.demand.curve))
}	
		


Utility.Max2d <- function(inputs.list,cobd.list){

		maxinc <- inputs.list$maxinc
	  x <- inputs.list$x
		y <- inputs.list$y
	  z <- inputs.list$z
	  alpha <- inputs.list$alpha
	  Inc <- inputs.list$Inc
	  mininc <- inputs.list$mininc
	  minpx <- inputs.list$minpx
	  minpy <- inputs.list$minpy
	  maxpx <- inputs.list$maxpx
	  maxpy <- inputs.list$maxpy
	  px <- inputs.list$px
	  py <- inputs.list$py
	  melted <- inputs.list$melted
	
	  optim.x <- cobd.list$marshall_x
	  optim.y <- cobd.list$marshall_y
	  optim.u <- cobd.list$indirect_u
	
	  bc <- cobd.bc(Inc=Inc,px=px,py=py,x=x,y=y)
	  op <- data.frame(optx=optim.x,opty=optim.y)
	  u_spec <- cobd.y(u=optim.u,x=x,alpha=alpha)
	  u_spec_df <- data.frame(x=x,y=u_spec)
		
	  maxx <- 200
   	tempy <- bc[which(bc[,1]==maxx),2]
	  maxy <- subset(u_spec_df,x==maxx)[2]

	  trace_op <- data.frame(opx=c(rep(op$optx,2)),opy=c(0,op$opty))
	
		#plot limits
	  ytemp <- tail(u_spec_df[which(u_spec_df[,2]>=bc[1,2]),2])	
 	  bc_ymax <- ytemp[end(ytemp)][1]+20
	  xmax<-ymax<-200
	 
	  ytemp <- min(u_spec_df[,2])+5
		bc_df <- data.frame(x=xmax-10,y=ytemp)
	
	  plot.original <- ggplot(data=melted,environment = environment())+
 		    								theme_tufte()+
												geom_tile(data=melted,aes(x,y,z=z,fill=z))+
												stat_contour(aes(x,y,z=z),size=0.3,bins=20,linetype='dashed',alpha=0.3)+
										    scale_fill_gradientn(colours=jet.col(n=nrow(z)))+
        								geom_line(data=bc,aes(x=bc_x,y=bc_y),colour='black',size=0.5)+
		    								geom_line(data=u_spec_df,aes(x=x,y=y),colour='black',size=0.5)+
		                    geom_line(data=trace_op,aes(x=opx,y=opy),colour='black',size=0.5,linetype='dashed')+
	                   		geom_point(data=op,aes(x=optx,y=opty),colour='gold',size=3)+
                    		geom_text(data=op,aes(x=optx+10,y=opty+5,label=paste('x*=',round(optx),'\n','y*=',round(opty))),colour='black',size=3)+
				                geom_text(data=bc_df,aes(x=x,y=y),label=paste('U*=',round(optim.u)),colour='black',size=3)+
												labs(title='Level Curves For Utility Maximisation Problem')+
			 						      theme(legend.position='none',plot.title=element_text(size=12*0.9))+ 								
		                    xlab('Quantity [x]')+
		    								ylab('Quantity [y]')+
                        expand_limits(x = 0, y = 0)+
		                    scale_x_continuous(limits = c(0, xmax),expand=c(0,0))+
		                    scale_y_continuous(limits = c(0, ymax),expand=c(0,0))
	return(list(orig.xmax=xmax,orig.ymax=ymax,orig.bc=bc,orig.px=px,orig.py=py,inc=Inc,x=x,y=y,alpha=alpha,plot.original=plot.original,optim.x=optim.x,optim.y=optim.y,optim.u=optim.u))
}




Slutsky.Decomp <- function(orig.pricex,orig.pricey,new.pricex,new.pricey,orig.inc,new.inc,orig.alpha,orig.x,orig.y){
	#original 
	utility.max <- cobd.max(inc=orig.inc,p_x=orig.pricex,p_y=orig.pricey,alpha=orig.alpha,x=orig.x,y=orig.y)
	orig.optim.x <- utility.max$marshall_x
	#sub
	utility.max <- cobd.max(inc=new.inc,p_x=new.pricex,p_y=new.pricey,alpha=orig.alpha,x=orig.x,y=orig.y)
	sub.optim.x <- utility.max$marshall_x
	#inc
	utility.max <- cobd.max(inc=orig.inc,p_x=new.pricex,p_y=new.pricey,alpha=orig.alpha,x=orig.x,y=orig.y)
	inc.optim.x <- utility.max$marshall_x
	#substitution effect
  subx <- 	sub.optim.x-orig.optim.x
	#income effect
  incx <- 	round(inc.optim.x)-round(sub.optim.x)
	return(list(subx=subx,incx=incx))
}


Price.Change <- function(original2d,px_change,new_Inc){
	orig.plot <- original2d$plot.original
	orig.optimx <- original2d$optim.x
	orig.optimy <- original2d$optim.y
	orig.optimu <- original2d$optim.u
	orig.alpha <- original2d$alpha
	orig.x <- original2d$x
	orig.y <- original2d$y
	orig.inc <- original2d$inc
	orig.px <- original2d$orig.px
	orig.bc <- original2d$orig.bc
	orig.xmax <- original2d$orig.xmax
	orig.ymax <- original2d$orig.ymax
	
	new_py <- orig.py <- original2d$orig.py
	new_px <- px_change+orig.px
	
  #new optimisation
  utility.max <- cobd.max(inc=new_Inc,p_x=new_px,p_y=new_py,alpha=orig.alpha,orig.x,orig.y)
	new.optim.x <- round(utility.max$marshall_x)
	new.optim.y <- round(utility.max$marshall_y)
	new.optim.u <- round(utility.max$indirect_u)
	op <- data.frame(optx=new.optim.x,opty=new.optim.y)
	u_spec <- cobd.y(u=new.optim.u,x=orig.x,alpha=orig.alpha)
	u_spec_df <- data.frame(x=orig.x,y=u_spec)

	#new budget constraint
	bc <- cobd.bc(Inc=new_Inc,px=new_px,py=new_py,x=orig.x,y=orig.y)
  maxx <- max(orig.x)-20
  tempy <- bc[which(bc[,1]==maxx),2]
	maxy <- subset(u_spec_df,x==maxx)[2]
	
	xtemp <- bc[which(bc[,2]==0),1]
	ytemp <- u_spec_df[xtemp,2]
	bc_df <- data.frame(x=xtemp,y=ytemp)

	#hicksian
	exp.min <- expenditure.min(target.u=orig.optimu,alpha=orig.alpha,px=new_px,py=new_py)
  hicks.x <- round(exp.min$hicksian_x)
	hicks.y <- round(exp.min$hicksian_y)
	optim.exp <- round(exp.min$minimal_exp)
	hicks_y <- round(expenditure.y(Inc=optim.exp,px=new_px,py=new_py,x=orig.x))
	hicks_df <- data.frame(x=orig.x,y=hicks_y)

	#Slutsky
	slutsky <- Slutsky.Decomp(orig.px,orig.py,new_px,new_py,orig.inc,optim.exp,orig.alpha,orig.x,orig.y)
	 subx <- slutsky$subx
	 x.subx <- round(orig.optimx+subx)
	 y.subx <- round(subset(hicks_df,x==x.subx)[2])
	 x.incx <- new.optim.x-x.subx
	 slutsky_df <- data.frame(x=x.subx,y=y.subx)
	 slutsky_lines_df <- data.frame(subx=c(orig.optimx,x.subx),coordy=c(0,0),incx=c(x.subx,new.optim.x))

	
	#plot limits
   #xmax <- min(orig.xmax,(u_spec_df[which(u_spec_df[,2]==min(u_spec_df[,2]))[1],1])+20)
    ytemp1 <- tail(u_spec_df[which(u_spec_df[,2]>=bc[1,2]),2])	
	 #ymax <- min(orig.ymax,ytemp1[end(ytemp1)][1]+30)
		xmax<-ymax<-200

	  ytemp <- min(u_spec_df[,2])+5
		bc_df <- data.frame(x=xmax-40,y=ytemp)

	  leg.df <- data.frame(x=rep(xmax-10,5),y=seq(ymax-10,round(ymax/2),length.out=8)[1:5])
 		#leg.df <- data.frame(x=seq(2,xmax,length.out=6)[1:5],y=rep(ymax,5))
    trace_df1 <- data.frame(x=c(rep(slutsky_lines_df$incx[2],2)),y=c(0,bc[slutsky_lines_df$incx[2],2]))
	  trace_df2 <- data.frame(x=c(rep(slutsky_lines_df$incx[1],2)),y=c(0,hicks_df[slutsky_lines_df$incx[1],2]))
	
	#bringing the lower layers up
		orig_bc <- orig.plot$layers[[3]]
		orig_uspec <- orig.plot$layers[[4]]
		orig_tr <- orig.plot$layers[[5]]
		orig_p <- orig.plot$layers[[6]]

	#add new bc & contour to original plot

	orig.plot+
 	      geom_line(data=bc,aes(x=bc_x,y=bc_y),colour='white',size=0.5)+
		    geom_line(data=hicks_df,aes(x=x,y=y),colour='red',size=0.5,linetype='dashed')+
 				geom_line(data=u_spec_df,aes(x=x,y=y),colour='white',size=0.5)+
				geom_line(data=slutsky_lines_df,aes(x=subx,y=coordy),colour='green',size=2)+
 				geom_line(data=slutsky_lines_df,aes(x=incx,y=coordy),colour='purple',size=2)+
		    geom_line(data=trace_df1,aes(x=x,y=y),colour='white',size=0.5,linetype='dashed')+
				geom_line(data=trace_df2,aes(x=x,y=y),colour='red',size=0.5,linetype='dashed')+    
		    geom_point(data=op,aes(x=optx,y=opty),colour='gold',size=3)+
		  	orig_bc+
		    orig_uspec+
		    orig_tr+
		    orig_p+
		    geom_text(data=leg.df,aes(x=x,y=y),label=c(paste('Original {U=',round(orig.optimu),' | x*=',round(orig.optimx),';y*=',round(orig.optimy),'}'),paste('New optimum {U=',round(new.optim.u),' | x*=',round(new.optim.x),';y*=',round(new.optim.y),'}'),paste('Substitution effect {U=',round(orig.optimu),' | SE=',round(subx,3),'}'),paste('Income effect {U=',round(new.optim.u),' | IE=',round(x.incx,3),'}'),'Compensated Income'),size=3,colour=c('black','white','green','purple','red'),vjust=1,hjust=1,fontface='bold')+
		    ggtitle(paste('Slutsky Decomposition Of A Price Change In Good X\n{ Total effect : Substitution effect(',round(subx,3),') + Income effect (',round(x.incx,3),') =',round(subx+x.incx,3),' }'))+
		    theme(legend.position='none',plot.title=element_text(size=12*0.9))+								
        expand_limits(x = 0, y = 0)+
		    scale_x_continuous(limit = c(0, xmax),expand=c(0,0))+
		    scale_y_continuous(limit = c(0, ymax),expand=c(0,0))
}	
	
param.table <- function(inputs.list,cobd.list){
	  alpha <- inputs.list$alpha
	  Inc <- inputs.list$Inc
	  px <- inputs.list$px
	  py <- inputs.list$py
	  optim.x <- cobd.list$marshall_x
	  optim.y <- cobd.list$marshall_y
	  optim.u <- cobd.list$indirect_u
	
	  table.df <- data.frame(Original=c(0.5,0.5,1,1,200,100,100,100),New=c(alpha,1-alpha,px,py,Inc,optim.x,optim.y,optim.u))
	  rownames(table.df) <- c('Alpha','Beta','Price[x]','Price[y]','Income','Quantity[x]','Quantity[y]','Utility')
	
	  table.spec <- ggTableSpec(columns.exist=T,columns.txt=colnames(table.df),columns.font=rep('bold',1),columns.col='black',columns.fill='grey',columns.alpha=0.3,
                          rows.exist=T,rows.txt=rownames(table.df),rows.font=rep('bold',8),rows.col='black',rows.fill='grey',rows.alpha=0.3,
                          data.obj=table.df,data.col='black',data.title='Parameters & Optimum',
                          hlt.col.exist=T,hl.col.which=c(1,2,3,4,5,6,7,8),hl.col.fill=c(rep('red',5),rep('green',3)),hl.col.alpha=rep(0.3,8),
                          hlt.row.exist=F,hl.row.which=c(2,3,4),hl.row.fill=c('white','red','red'),hl.row.alpha=rep(0.4,)
                )
	gg2.table <- ggTableDrawer(table.spec)
	return(gg2.table)
}


Utility.Max3d <- function(inputs.list,cobd.list,phi=10,theta=70){

	#Inputs
		maxinc <- inputs.list$maxinc
	  x <- inputs.list$x
		y <- inputs.list$y
	  z <- inputs.list$z
	  alpha <- inputs.list$alpha
	  Inc <- inputs.list$Inc
	  mininc <- inputs.list$mininc
	  minpx <- inputs.list$minpx
	  minpy <- inputs.list$minpy
	  maxpx <- inputs.list$maxpx
	  maxpy <- inputs.list$maxpy
	  px <- inputs.list$px
	  py <- inputs.list$py
	  melted <- inputs.list$melted
	
	  optim.x <- cobd.list$marshall_x
	  optim.y <- cobd.list$marshall_y
	  optim.u <- cobd.list$indirect_u
	
	#Utility function with contours
	contour.list <- list(nlevels=20,drawlabels=F,lty=1,lwd=1,col=jet.col(n=20,alpha=1))
	colkey.list <- list(cex.axis=0.65,col.clab = "black", line.clab = 1, side.clab = 3,cex.clab=0.65,side=2,length=1,dist=-0.1)
	pmat<-persp3D(plot=TRUE,zlab='Utility',ylab='Quantity [y]',xlab='Quantity [x]',main='Cobb-Douglas Utility Function',bty='u',col.axis='black',col.panel='white',lwd.panel=0.3,lwd.grid=0.5,col.grid='grey',box=T,ticktype='detailed',axes=TRUE, r = 10, d = 2,shade=0, theta =theta, phi = phi,contour=contour.list,border=NA,x,y,z, colvar = z,col=jet.col (n = nrow(z), alpha = 1),clab='Utility', colkey = colkey.list,clim=range(z),cex.axis=0.65,cex.main=0.8,cex.lab=0.65)

	#horizontal plane
	height.col <- jet.col(n=nrow(z),alpha=0)[optim.u]
	plane.matrix <- matrix(optim.u,nrow=length(x),ncol=length(y))
	persp3D(add=TRUE,x=x,y=y,z=plane.matrix,colvar=z,col=height.col,colkey=FALSE,contour=list(levels=optim.u,col='black',lwd=2,lty=1))

	#budget plane
	bdg_x <- c(0,Inc/px,Inc/px,0)
	bdg_y <- c(Inc/py,0,0,Inc/py)
	bdg_z <- c(0,0,optim.u+10,optim.u+10)
	polygon(trans3D(x=bdg_x,y=bdg_y,z=bdg_z,pmat=pmat),col=alpha.col('grey',alpha=0.5),border='black')

	#Locate optimum
		#base
		lines(trans3D(x=c(optim.x,optim.x),y=c(0,max(y)),z=c(0,0),pmat=pmat),col='black',lwd=1,lty=5)
		lines(trans3D(x=c(max(x),0),y=c(optim.y,optim.y),z=c(0,0),pmat=pmat),col='black',lwd=1,lty=5)
		points(trans3D(x=c(optim.x),y=c(optim.y),z=c(0),pmat=pmat),col='red',pch=21,cex=1,lwd=1,bg='black')

		#height
		lines(trans3D(x=c(optim.x,optim.x),y=c(0,max(y)),z=c(optim.u,optim.u),pmat=pmat),col='black',lwd=1,lty=5)
		lines(trans3D(x=c(max(x),0),y=c(optim.y,optim.y),z=c(optim.u,optim.u),pmat=pmat),col='black',lwd=1,lty=5)
		points(trans3D(x=c(optim.x),y=c(optim.y),z=c(optim.u),pmat=pmat),col='red',pch=21,cex=1,lwd=1,bg='black')

		#link
		lines(trans3D(x=c(optim.x,optim.x),y=c(optim.y,optim.y),z=c(0,optim.u),pmat=pmat),col='black',lwd=1,lty=5)

	}

tax.helper <- function(inc,px,py,alpha,x,y){
	utility.max <- cobd.max(inc=inc,p_x=px,p_y=py,alpha=alpha,x,y)
	optim.x <- utility.max$marshall_x
	optim.y <- utility.max$marshall_y
	optim.u <- utility.max$indirect_u	
	
	op <- data.frame(optx=optim.x,opty=optim.y)
	u_spec <- cobd.y(u=optim.u,x=x,alpha=alpha)
	u_spec_df <- data.frame(x=x,y=u_spec)
	
	bc <- cobd.bc(Inc=inc,px=px,py=py,x=x,y=y)
  maxx <- max(x)-20
  tempy <- bc[which(bc[,1]==maxx),2]
	maxy <- subset(u_spec_df,x==maxx)[2]
	bc_df <- data.frame(x=ifelse(tempy<0,bc[which(bc[,2]==0),1]+5,maxx),y=maxy)

	return(list(optim_y=optim.y,optim_x=optim.x,optim_u=optim.u,op=op,u_spec=u_spec,bc=bc,bc_df=bc_df,u_spec_df=u_spec_df))
}

Lumpsum.Principle <- function(original2d,x_tax){
	
	#original plot and optimum
	orig.plot <- original2d$plot.original
	orig.optimx <- original2d$optim.x
	orig.optimy <- original2d$optim.y
	orig.optimu <- original2d$optim.u
	orig.alpha <- original2d$alpha
	orig.x <- original2d$x
	orig.y <- original2d$y
	orig.inc <- original2d$inc
	orig.px <- original2d$orig.px
	orig.py <- original2d$orig.py
	orig.bc <- original2d$orig.bc
	orig.xmax <- original2d$orig.xmax
	orig.ymax <- original2d$orig.ymax

	#tax variables
	new_px <- orig.px+x_tax
	
	#hold income const
	px_tax_list <- tax.helper(inc=orig.inc,px=new_px,py=orig.py,alpha=orig.alpha,x=orig.x,y=orig.y)
   px_op <- px_tax_list$op
   px_u_spec <- px_tax_list$u_spec
   px_bc <- px_tax_list$bc
   px_bc_df <- px_tax_list$bc_df
   px_u_spec_df <- px_tax_list$u_spec_df
	 px_optim_u <- px_tax_list$optim_u
	 px_optim_x <- px_tax_list$optim_x
	 px_optim_y <- px_tax_list$optim_y
	
	 px_trace <-data.frame(opx=c(px_op$optx,px_op$optx),opy=c(0,px_op$opty)) 
	
	
	#Tax Revenue from good x
	px_rev <- px_optim_x*x_tax

	#Equivalent Income tax 
	new_inc <- orig.inc-px_rev 
	
	#hold px const
	inc_tax_list <- tax.helper(inc=new_inc,px=orig.px,py=orig.py,alpha=orig.alpha,x=orig.x,y=orig.y)
   inc_op <- inc_tax_list$op
   inc_u_spec <- inc_tax_list$u_spec
   inc_bc <- inc_tax_list$bc
   inc_bc_df <- inc_tax_list$bc_df
   inc_u_spec_df <- inc_tax_list$u_spec_df
	 inc_optim_u <- inc_tax_list$optim_u
   inc_optim_x <- inc_tax_list$optim_x
   inc_optim_y <- inc_tax_list$optim_y
	
	 inc_trace <-data.frame(opx=c(inc_op$optx,inc_op$optx),opy=c(0,inc_op$opty)) 

	#Income Tax Revenue
	inc_rev <- inc_optim_x*x_tax
	
	#plot limits
#	xmax <- min(orig.xmax,(px_u_spec_df[which(px_u_spec_df[,2]==min(px_u_spec_df[,2]))[1],1])+20,(inc_u_spec_df[which(inc_u_spec_df[,2]==min(inc_u_spec_df[,2]))[1],1])+20)
  ytemp1 <- tail(px_u_spec_df[which(px_u_spec_df[,2]>=px_bc[1,2]),2])	
	ytemp2 <- tail(inc_u_spec_df[which(inc_u_spec_df[,2]>=inc_bc[1,2]),2])	
#	ymax <- min(orig.ymax,ytemp1[end(ytemp1)][1]+30,ytemp2[end(ytemp2)][1]+30)
	xmax<-ymax<-200

	#makeshift legend
	#leg.df <- data.frame(x=seq(2,xmax,length.out=4)[1:3],y=rep(ymax,3))
	leg.df <- data.frame(x=rep(190,3),y=seq(190,100,length.out=8)[1:3])

	#bringing the lower layers up
	orig_bc <- orig.plot$layers[[3]]
	orig_uspec <- orig.plot$layers[[4]]
	orig_tr <- orig.plot$layers[[5]]
	orig_p <- orig.plot$layers[[6]]

	#ggplot

	orig.plot+
	      	geom_line(data=px_bc,aes(x=bc_x,y=bc_y),colour='green',size=0.5)+
					geom_line(data=px_u_spec_df,aes(x=x,y=y),colour='green',size=0.5)+
			  	geom_line(data=px_trace,aes(x=opx,y=opy),colour='green',size=0.5,linetype='dashed')+	
		    	geom_line(data=inc_bc,aes(x=bc_x,y=bc_y),colour='cyan',size=0.5)+
					geom_line(data=inc_u_spec_df,aes(x=x,y=y),colour='cyan',size=0.5)+ 
 			  	geom_line(data=inc_trace,aes(x=opx,y=opy),colour='cyan',size=0.5,linetype='dashed')+
		      orig_bc+
		      orig_uspec+
		      orig_tr+
		      orig_p+
		      geom_point(data=inc_op,aes(x=optx,y=opty),colour='gold',size=3)+ 
			  	geom_point(data=px_op,aes(x=optx,y=opty),colour='gold',size=3)+
		    	ggtitle(paste('The Lumpsum Principle\n{ Sales Tax Revenue = ',round(px_rev,3),' :: ','Income Tax Revenue = ',round(inc_rev,3),'}'))+
		    	geom_text(data=leg.df,aes(x=x,y=y),label=c(paste('Original : U{',round(orig.optimu),'}'),paste('Tax on good X : U{',round(px_optim_u),'}'),paste('Income Tax : U{',round(inc_optim_u),'}')),size=3,colour=c('black','green','cyan'),hjust=1,vjust=1,fontface='bold')+
		    	theme(legend.position='none',plot.title=element_text(size=12*0.9))+						
        	expand_limits(x = 0, y = 0)+
		    	scale_x_continuous(limit = c(0, xmax),expand=c(0,0))+
		    	scale_y_continuous(limit = c(0, ymax),expand=c(0,0))
}

