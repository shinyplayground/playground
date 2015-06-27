source('helpers.R')
library(plot3D)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(reshape2)
library(shiny)
library(gtable)
library(animation)
library(grDevices)



shinyServer(function(input, output) {
	
	dyn_slider_mat <- matrix(cbind(c(1,2,3,4,5),c(0,-1,-2,-3,-4),c(4,3,2,1,0)),ncol=3)	
	formula_df <- data.frame(selected=c('Quadratic Utility Function','Power Utility Function (CRRA)','General Exponential Utility (CARA)','Log Utility Function','Hyperbolic Absolute Risk Aversion (HARA)'),formula=c('$$U(w)=aw-bw^2$$','$$U(w)=\\frac{w^\\left(1-r\\right)}{1-r}\\!$$','$$U(w)=A-B*\\exp\\left(-\\rho/w\\right)$$','$$U(w)=\\ln\\left(w\\right)$$','$$U(w)=\\frac{1-\\gamma}{\\gamma}\\left(\\frac{aw}{1-\\gamma}+b\\right)^\\gamma\\!$$'))
	prod_df <- data.frame(selected=c('Linear','Fixed Proportion','Cobb Douglas','Constant Elasticity Of Substitution'),formula=c('$$q=f(k,l)=\\alpha k+\\beta l$$','$$q=f(k,l)=min(\\alpha k,\\beta l)$$','$$q=f(k,l)=Ak^\\alpha l^\\beta$$','$$q=f(k,l)=(\\alpha k^\\rho +(1-\\alpha) l^\\rho )^\\left(\\gamma/\\rho\\right)$$'))

output$formula_text <- renderUI({ 
 		withMathJax(h6(subset(formula_df,selected==input$utility_choice)[2]))
	})
	
output$formula_production <- renderUI({ 
 		withMathJax(h6(subset(prod_df,selected==input$production_choice)[2]))
	})
	
output$AnimPlot <- renderImage({
  filename <- normalizePath(file.path('C:/University of Warwick/R experiments/ShinyIO/images/','Quadratic Utility Function.gif'))
    list(src = filename,alt = paste("Image"))
  }, deleteFile = FALSE)

				
output$slider_wealth <- renderUI({
	if(input$utility_choice=='Quadratic Utility Function'){
    sliderInput("slider_w",h6('Initial wealth :'),min = 0,max = abs(input$slider_quad_a/(2*input$slider_quad_b)),value =abs(input$slider_quad_a/(2*input$slider_quad_b)/2))
  }else if(input$utility_choice=='Power Utility Function (CRRA)'){
  	sliderInput("slider_w",h6('Initial wealth :'),min = 0,max = 100,value =50)
  }else if(input$utility_choice=='General Exponential Utility (CARA)'){
  	sliderInput("slider_w",h6('Initial wealth :'),min = 0,max = 20,value =10)
  }else if(input$utility_choice=='Log Utility Function'){
  	sliderInput("slider_w",h6('Initial wealth :'),min = 1,max = 100,value =50)
  }else if(input$utility_choice=='Hyperbolic Absolute Risk Aversion (HARA)'){
  	#Quotient
	 			quotient <- (-input$text_h_b*(1-input$text_h_gamma))/input$text_h_a
			#Different cases [ui.r] later
				if(quotient<0){
					w_min <- quotient
					w_max <- abs(w_min)*2
				}else if(quotient>0){
					w_max <- quotient
					w_min <- 0
				}
  	calc_wealth <- (w_max-w_min)/2
  	sliderInput("slider_w",h6('Initial wealth :'),min = 0,max = w_max,value =calc_wealth)
  }
})
	
output$slider_loss <- renderUI({
	if(input$utility_choice=='Hyperbolic Absolute Risk Aversion (HARA)'){
			#Quotient
	 			quotient <- (-input$text_h_b*(1-input$text_h_gamma))/input$text_h_a
			#Different cases [ui.r] later
				if(quotient<0){
					w_min <- quotient
					w_max <- abs(w_min)*2
					w_initial<-(w_max-w_min)/2
					w_gain=(w_max-w_initial)/2
	 				w_loss=(w_initial-w_min)/2
				}else if(quotient>0){
					w_max <- quotient
					w_min <- 0
			   	w_initial<-(w_max-w_min)/2
					w_gain=(w_max-w_initial)/2
	 				w_loss=(w_initial-w_min)/2
				}
    sliderInput("slider_wloss",h6('Size of loss :'),min = 0,max = w_loss,value =w_loss/2)
}else{
	   sliderInput("slider_wloss",h6('Size of loss :'),min = 0,max = input$slider_w/2,value =input$slider_w/4)
}
})
	
output$slider_gain <- renderUI({
 if(input$utility_choice=='Quadratic Utility Function'){
 			choice_maxw <- (abs(input$slider_quad_a/(2*input$slider_quad_b))-input$slider_w)/2
 }else if(input$utility_choice=='Power Utility Function (CRRA)'){
			choice_maxw <- (100-input$slider_w)/2
 }else if(input$utility_choice=='General Exponential Utility (CARA)'){
			choice_maxw <- (20-input$slider_w)/2
 }else if(input$utility_choice=='Log Utility Function'){
			choice_maxw <- (100-input$slider_w)/2
 }else if(input$utility_choice=='Hyperbolic Absolute Risk Aversion (HARA)'){
			#Quotient
	 			quotient <- (-input$text_h_b*(1-input$text_h_gamma))/input$text_h_a
			#Different cases [ui.r] later
				if(quotient<0){
					w_min <- quotient
					w_max <- abs(w_min)*2
					w_initial<-(w_max-w_min)/2
					w_gain=(w_max-w_initial)/2
	 				w_loss=(w_initial-w_min)/2
	
				}else if(quotient>0){
					w_max <- quotient
					w_min <- 0
					w_initial<-(w_max-w_min)/2
					w_gain=(w_max-w_initial)/2
	 				w_loss=(w_initial-w_min)/2
				}
  	choice_maxw <- w_gain
 }
    sliderInput("slider_wgain",h6('Size of gain :'),min = 0,max =choice_maxw,value =choice_maxw/2)
})
	
	
	
output$slider <- renderUI({
		sliderInput("slider_pxchange",h6('Change price of good x by the following amount :'),min = dyn_slider_mat[input$slider_px,2],max = dyn_slider_mat[input$slider_px,3],value = 0)
})


output$IsoquantPlot <- renderPlot({
	  l_range <- k_range <- seq(1,50,by=0.1)
		if(input$production_choice=='Linear'){
		Isoquant.plot(type='Linear',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_lin_a,
									 beta=input$slider_lin_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
									 q1=input$slider_pd_q1,
			             q2=input$slider_pd_q2,
									 q3=input$slider_pd_q3,
	 			           kl_ratio=input$slider_kl
				)
	} else if(input$production_choice=='Cobb Douglas'){
	 		Isoquant.plot(type='Cobb Douglas',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_cd_a,
									 beta=input$slider_cd_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
									 q1=input$slider_pd_q1,
			             q2=input$slider_pd_q2,
									 q3=input$slider_pd_q3,
	 			           kl_ratio=input$slider_kl
				)
	} else if (input$production_choice=='Fixed Proportion'){
	 		Isoquant.plot(type='Fixed Proportion',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_fx_a,
									 beta=input$slider_fx_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
									 q1=input$slider_pd_q1,
			             q2=input$slider_pd_q2,
									 q3=input$slider_pd_q3,
	 			           kl_ratio=input$slider_kl
				)
	} else if (input$production_choice=='Constant Elasticity Of Substitution'){
	 		Isoquant.plot(type='Constant Elasticity Of Substitution',
									 l=l_range,
		               k=k_range,
									 alpha=0,
									 beta=0,
									 gamma=input$slider_ces_gamma,
			             rho=input$slider_ces_rho,
			             ces_alpha=input$slider_ces_alpha,
									 q1=input$slider_pd_q1,
			             q2=input$slider_pd_q2,
									 q3=input$slider_pd_q3,
	 			           kl_ratio=input$slider_kl
				)
	 }
})
output$ProductCurveCapital <- renderPlot({
	l_range <- k_range <- seq(1,50,by=0.1)
	if(input$production_choice=='Linear'){
		ProductCurvesK(type='Linear',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_lin_a,
									 beta=input$slider_lin_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
                   l_fixed=input$slider_pd_l
				)
	} else if(input$production_choice=='Cobb Douglas'){
	 		ProductCurvesK(type='Cobb Douglas',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_cd_a,
									 beta=input$slider_cd_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
                   l_fixed=input$slider_pd_l

				)
	} else if (input$production_choice=='Fixed Proportion'){
	 	ProductCurvesK(type='Fixed Proportion',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_fx_a,
									 beta=input$slider_fx_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
                   l_fixed=input$slider_pd_l

				)
	} else if (input$production_choice=='Constant Elasticity Of Substitution'){
	 	ProductCurvesK(type='Constant Elasticity Of Substitution',
									 l=l_range,
		               k=k_range,
									 alpha=0,
									 beta=0,
									 gamma=input$slider_ces_gamma,
			             rho=input$slider_ces_rho,
			             ces_alpha=input$slider_ces_alpha,
                   l_fixed=input$slider_pd_l
				)
	 }
})


output$ProductCurveLabour <- renderPlot({
	l_range <- k_range <- seq(1,50,by=0.1)
	if(input$production_choice=='Linear'){
		ProductCurvesL(type='Linear',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_lin_a,
									 beta=input$slider_lin_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
                   k_fixed=input$slider_pd_k
				)
	} else if(input$production_choice=='Cobb Douglas'){
	 		ProductCurvesL(type='Cobb Douglas',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_cd_a,
									 beta=input$slider_cd_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
                   k_fixed=input$slider_pd_k

				)
	} else if (input$production_choice=='Fixed Proportion'){
	 	ProductCurvesL(type='Fixed Proportion',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_fx_a,
									 beta=input$slider_fx_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
                   k_fixed=input$slider_pd_k

				)
	} else if (input$production_choice=='Constant Elasticity Of Substitution'){
	 	ProductCurvesL(type='Constant Elasticity Of Substitution',
									 l=l_range,
		               k=k_range,
									 alpha=0,
									 beta=0,
									 gamma=input$slider_ces_gamma,
			             rho=input$slider_ces_rho,
			             ces_alpha=input$slider_ces_alpha,
                   k_fixed=input$slider_pd_k
				)
	 }
})

output$ProductionPlot <- renderPlot({
	l_range <- k_range <- c(0:50)
	if(input$production_choice=='Linear'){
		Production.3D(type='Linear',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_lin_a,
									 beta=input$slider_lin_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
									 q1=input$slider_pd_q1,
			             q2=input$slider_pd_q2,
									 q3=input$slider_pd_q3
				)
	} else if(input$production_choice=='Cobb Douglas'){
	 		Production.3D(type='Cobb Douglas',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_cd_a,
									 beta=input$slider_cd_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
									 q1=input$slider_pd_q1,
			             q2=input$slider_pd_q2,
									 q3=input$slider_pd_q3
				)
	} else if (input$production_choice=='Fixed Proportion'){
	 	Production.3D(type='Fixed Proportion',
									 l=l_range,
		               k=k_range,
									 alpha=input$slider_fx_a,
									 beta=input$slider_fx_b,
									 gamma=0,
			             rho=0,
			             ces_alpha=0,
									 q1=input$slider_pd_q1,
			             q2=input$slider_pd_q2,
									 q3=input$slider_pd_q3
				)
	} else if (input$production_choice=='Constant Elasticity Of Substitution'){
	 	Production.3D(type='Constant Elasticity Of Substitution',
									 l=l_range,
		               k=k_range,
									 alpha=0,
									 beta=0,
									 gamma=input$slider_ces_gamma,
			             rho=input$slider_ces_rho,
			             ces_alpha=input$slider_ces_alpha,
									 q1=input$slider_pd_q1,
			             q2=input$slider_pd_q2,
									 q3=input$slider_pd_q3
				)
	 }
})


output$FullPlot <- renderPlot({
 if(input$utility_choice=='Quadratic Utility Function'){
 	choice_maxw <- abs(input$slider_quad_a/(2*input$slider_quad_b))
 	 uncertainty.plot(type=input$utility_choice,a=input$slider_quad_a,b=input$slider_quad_b,r=input$slider_power_r,rho=input$slider_exp_rho,hara_gamma=input$text_h_gamma,hara_a=input$text_h_a,hara_b=input$text_h_b,w_initial=input$slider_w,prob_loss=input$slider_lossprob,w_gain=input$slider_wgain,w_loss=input$slider_wloss,w_slidermax=choice_maxw)
 }else  if(input$utility_choice=='Power Utility Function (CRRA)'){
	choice_maxw <- 100
 	 uncertainty.plot(type=input$utility_choice,a=input$slider_quad_a,b=input$slider_quad_b,r=input$slider_power_r,rho=input$slider_exp_rho,hara_gamma=input$text_h_gamma,hara_a=input$text_h_a,hara_b=input$text_h_b,w_initial=input$slider_w,prob_loss=input$slider_lossprob,w_gain=input$slider_wgain,w_loss=input$slider_wloss,w_slidermax=choice_maxw)
 }else if(input$utility_choice=='General Exponential Utility (CARA)'){
	choice_maxw <- 20
 	 uncertainty.plot(type=input$utility_choice,a=input$slider_quad_a,b=input$slider_quad_b,r=input$slider_power_r,rho=input$slider_exp_rho,hara_gamma=input$text_h_gamma,hara_a=input$text_h_a,hara_b=input$text_h_b,w_initial=input$slider_w,prob_loss=input$slider_lossprob,w_gain=input$slider_wgain,w_loss=input$slider_wloss,w_slidermax=choice_maxw)
 }else if(input$utility_choice=='Log Utility Function'){
	 choice_maxw <- NA
 	 uncertainty.plot(type=input$utility_choice,a=input$slider_quad_a,b=input$slider_quad_b,r=input$slider_power_r,rho=input$slider_exp_rho,hara_gamma=input$text_h_gamma,hara_a=input$text_h_a,hara_b=input$text_h_b,w_initial=input$slider_w,prob_loss=input$slider_lossprob,w_gain=input$slider_wgain,w_loss=input$slider_wloss,w_slidermax=choice_maxw)
 }else if(input$utility_choice=='Hyperbolic Absolute Risk Aversion (HARA)'){
			#Quotient
	 			quotient <- (-input$text_h_b*(1-input$text_h_gamma))/input$text_h_a
			#Different cases [ui.r] later
				if(quotient<0){
					w_min <- quotient
					w_max <- abs(w_min)*2
					w_initial<-(w_max-w_min)/2
					w_gain=(w_max-w_initial)/2
	 				w_loss=(w_initial-w_min)/2
				}else if(quotient>0){
					w_max <- quotient
					w_min <- 0
					w_initial<-(w_max-w_min)/2
					w_gain=(w_max-w_initial)/2
	 				w_loss=(w_initial-w_min)/2
				}
  uncertainty.plot(type=input$utility_choice,a=input$slider_quad_a,b=input$slider_quad_b,r=input$slider_power_r,rho=input$slider_exp_rho,hara_gamma=input$text_h_gamma,hara_a=input$text_h_a,hara_b=input$text_h_b,w_initial=input$slider_w,prob_loss=input$slider_lossprob,w_gain=input$slider_wgain,w_loss=input$slider_wloss,w_slidermax=w_max,w_slidermin=w_min)
 }
})

output$CoeffsPlot <- renderPlot({
 coeffsPlot(type=input$utility_choice,a=input$slider_quad_a,b=input$slider_quad_b,r=input$slider_power_r,rho=input$slider_exp_rho,hara_gamma=input$text_h_gamma,hara_a=input$text_h_a,hara_b=input$text_h_b,w_initial=input$slider_w,prob_loss=input$slider_lossprob,w_gain=input$slider_wgain,w_loss=input$slider_wloss)
})

output$combCurves <- renderPlot({
	proc.store <- 	inc.consumption(alpha=input$slider6_alpha,px=input$slider6_px,py=input$slider6_py,inc=input$slider6_inc)
  	contour.curves <- proc.store$inc.consumption.plot
	  dem.curves <- proc.store$dem.curves.plot
	  engel.curves <- proc.store$engel.plot
	ggplots.align(gg1=contour.curves,gg2=dem.curves,gg3=engel.curves)
})
	
output$tablePlot <- renderPlot({
	welfare.compare(alpha=input$slider5_alpha,px=input$slider5_px)$gg.table
})
	
output$welfarePlot <- renderPlot({
	welfare.compare(alpha=input$slider5_alpha,px=input$slider5_px)$plot.contours
})	

output$demandCombined <- renderPlot({
	proc.store <- demand.compare(alpha=input$slider4_alpha,inc=input$slider4_inc,py_fixed=input$slider4_py,px=input$slider4_px)
	  dem.cont <- proc.store$plot.contours
	  dem.comp <- proc.store$plot.demand.comp
	ggplots.align(gg1=dem.cont,gg2=dem.comp)
})

output$marshallCombined <- renderPlot({
proc.store <- marshallian.demand.curve(inc=input$slider2_inc,alpha=input$slider2_alpha,py_fixed=input$slider2_py,num=input$slider2_num,col_low=input$drop2_col1,col_high=input$drop2_col2)
	  dem.cont <- proc.store$plot.contours
	  dem.comp <- proc.store$plot.demand.curve
		ggplots.align(gg1=dem.cont,gg2=dem.comp)
})
	
output$hicksianCombined <- renderPlot({
proc.store <- hicksian.demand.curve(target.u=input$slider3_util,alpha=input$slider3_alpha,py_fixed=input$slider3_py,num=input$slider3_num,col_low=input$drop3_col1,col_high=input$drop3_col2)
	  dem.cont <- proc.store$plot.contours
	  dem.comp <- proc.store$plot.demand.curve
		ggplots.align(gg1=dem.cont,gg2=dem.comp)
})


output$uplot <- renderPlot({
	inp.list<-inputs.transform(alpha=input$slider_alpha,Inc=input$slider_inc,mininc=10,minpx=1,minpy=1,maxpx=5,maxpy=5,px=input$slider_px,py=input$slider_py)
	utility.max <- cobd.max(inc=input$slider_inc,p_x=input$slider_px,p_y=input$slider_py,alpha=input$slider_alpha,x,y)
  Utility.Max3d(inputs.list=inp.list,cobd.list=utility.max,phi=input$slider_phi,theta=input$slider_theta)
	})
	
	
output$paramtbl <- renderPlot({
	inp.list <-inputs.transform(alpha=input$slider_alpha,Inc=input$slider_inc,mininc=10,minpx=1,minpy=1,maxpx=5,maxpy=5,px=input$slider_px,py=input$slider_py)
	utility.max <- cobd.max(inc=input$slider_inc,p_x=input$slider_px,p_y=input$slider_py,alpha=input$slider_alpha,x,y)
  param.table(inputs.list=inp.list,cobd.list=utility.max)
})
	
output$levelcurve <- renderPlot({
	inp.list<-inputs.transform(alpha=input$slider_alpha,Inc=input$slider_inc,mininc=10,minpx=1,minpy=1,maxpx=5,maxpy=5,px=input$slider_px,py=input$slider_py)
	utility.max <- cobd.max(inc=input$slider_inc,p_x=input$slider_px,p_y=input$slider_py,alpha=input$slider_alpha,x,y)
  orig <- Utility.Max2d(inputs.list=inp.list,cobd.list=utility.max)
	orig$plot.original
})
	
output$taxplot <- renderPlot({
	inp.list<-inputs.transform(alpha=input$slider_alpha,Inc=input$slider_inc,mininc=10,minpx=1,minpy=1,maxpx=5,maxpy=5,px=input$slider_px,py=input$slider_py)
	utility.max <- cobd.max(inc=input$slider_inc,p_x=input$slider_px,p_y=input$slider_py,alpha=input$slider_alpha,x,y)
  orig <- Utility.Max2d(inputs.list=inp.list,cobd.list=utility.max)
	Lumpsum.Principle(original2d=orig,x_tax=input$slider_tax)
})
	
output$slutskyplot <- renderPlot({
	inp.list<-inputs.transform(alpha=input$slider_alpha,Inc=input$slider_inc,mininc=10,minpx=1,minpy=1,maxpx=5,maxpy=5,px=input$slider_px,py=input$slider_py)
	utility.max <- cobd.max(inc=input$slider_inc,p_x=input$slider_px,p_y=input$slider_py,alpha=input$slider_alpha,x,y)
  orig <- Utility.Max2d(inputs.list=inp.list,cobd.list=utility.max)
	Price.Change(original2d=orig,px_change=input$slider_pxchange,new_Inc=input$slider_inc)
})
})
