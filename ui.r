library(shiny)

shinyUI(navbarPage('Shiny',
	 tabPanel('Home', 
        fluidPage(
  				fluidRow(
    				column(12,headerPanel(h1(":: { King Arthur } ::",style = "font-family: 'Times New Roman', cursive;
        		font-size: 125%; line-height: 0; color: white;"), windowTitle = 'Consumer theory'),div(style = "height:30px;background-color: darkblue;"),
      		fluidRow(
          		column(12,wellPanel(        	
        			helpText(h5("King Arthur",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  	helpText(h6('King Arthur is a legendary British leader of the late 5th and early 6th centuries, who, according to medieval histories and romances, led the defence of Britain against Saxon invaders in the early 6th century. The details of Arthurs story are mainly composed of folklore and literary invention, and his historical existence is debated and disputed by modern historians.[2] The sparse historical background of Arthur is gleaned from various sources, including the Annales Cambriae, the Historia Brittonum, and the writings of Gildas. Arthurs name also occurs in early poetic sources such as Y Gododdin.[3]
Arthur is a central figure in the legends making up the so-called Matter of Britain. The legendary Arthur developed as a figure of international interest largely through the popularity of Geoffrey of Monmouths fanciful and imaginative 12th-century Historia Regum Britanniae (History of the Kings of Britain).[4] In some Welsh and Breton tales and poems that date from before this work, Arthur appears either as a great warrior defending Britain from human and supernatural enemies or as a magical figure of folklore, sometimes associated with the Welsh Otherworld, Annwn.[5] How much of Geoffreys Historia was adapted from such earlier sources, rather than invented by Geoffrey himself, is unknown.
Although the themes, events and characters of the Arthurian legend varied widely from text to text, and there is no one canonical version, Geoffreys version of events often served as the starting point for later stories. Geoffrey depicted Arthur as a king of Britain who defeated the Saxons and established an empire over Britain, Ireland, Iceland, Norway and Gaul. Many elements and incidents that are now an integral part of the Arthurian story appear in Geoffreys Historia, including Arthurs father Uther Pendragon, the wizard Merlin, Arthurs wife Guinevere, the sword Excalibur, Arthurs conception at Tintagel, his final battle against Mordred at Camlann, and final rest in Avalon. The 12th-century French writer Chrétien de Troyes, who added Lancelot and the Holy Grail to the story, began the genre of Arthurian romance that became a significant strand of medieval literature. In these French stories, the narrative focus often shifts from King Arthur himself to other characters, such as various Knights of the Round Table. Arthurian literature thrived during the Middle Ages but waned in the centuries that followed until it experienced a major resurgence in the 19th century. In the 21st century, the legend lives on, not only in literature but also in adaptations for theatre, film, television, comics and other media.',style='font-size=65%'))
        ))))),
  				fluidRow(
    				column(12,headerPanel(h1(":: { Name } ::",style = "font-family: 'Times New Roman', cursive;
        		font-size: 125%; line-height: 0; color: white;"), windowTitle = 'Consumer theory'),div(style = "height:30px;background-color: darkred;"),
      		fluidRow(
          		column(12,wellPanel(        	
        			helpText(h5("Name",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  	helpText(h6("The origin of the Welsh name Arthur remains a matter of debate. Some suggest it is derived from the Roman nomen gentile (family name) Artorius, of obscure and contested etymology[22] (but possibly of Messapic[23][24][25] or Etruscan origin).[26][27][28] Some scholars have suggested it is relevant to this debate that the legendary King Arthur's name only appears as Arthur, or Arturus, in early Latin Arthurian texts, never as Artōrius (though it should be noted that Classical Latin Artōrius became Arturius in some Vulgar Latin dialects). However, this may not say anything about the origin of the name Arthur, as Artōrius would regularly become Art(h)ur when borrowed into Welsh.[29]
Another possibility is that it is derived from a Brittonic patronym *Arto-rīg-ios (the root of which, *arto-rīg- bear-king is to be found in the Old Irish personal name Art-ri) via a Latinized form Artōrius.[30] Less likely is the commonly proposed derivation from Welsh arth bear + (g)wr man (earlier *Arto-uiros in Brittonic); there are phonological difficulties with this theory—notably that a Brittonic compound name *Arto-uiros should produce Old Welsh *Artgur and Middle/Modern Welsh *Arthwr and not Arthur (in Welsh poetry the name is always spelled Arthur and is exclusively rhymed with words ending in -ur – never words ending in -wr – which confirms that the second element cannot be [g]wr man).[31][32]
An alternative theory, which has gained only limited acceptance among professional scholars, derives the name Arthur from Arcturus, the brightest star in the constellation Boötes, near Ursa Major or the Great Bear.[33] Classical Latin Arcturus would also have become Art(h)ur when borrowed into Welsh, and its brightness and position in the sky led people to regard it as the guardian of the bear (which is the meaning of the name in Ancient Greek) and the leader of the other stars in Boötes.[34]
A similar first name is Old Irish Artúr, which is believed to be derived directly from an early Old Welsh or Cumbric Artur.[35] The earliest historically attested bearer of the name is a son or grandson of Áedán mac Gabráin (d. AD 609).[36]",style='font-size=65%'))
        ))))))),
navbarMenu('Consumer Theory',
	
 tabPanel('Preferences',
	fluidPage(
  fluidRow(
    column(12,headerPanel(h1(":: { Playground } ::",style = "font-family: 'Times New Roman', cursive;
        font-size: 125%; line-height: 0; color: white;"), windowTitle = 'Consumer theory'),div(style = "height:30px;background-color: darkblue;"),

      fluidRow(
          column(3,wellPanel(        	
        		helpText(h5("Cobb-Douglas Inputs",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  helpText(h6('Inputs for the Utility maximisation problem with equality contraints.Minimum of income constrained to 100 since values lower than this generate strange plots.For some reason the line plots cross the axes sometimes.Have not tested whether the numbers are correct.',style='font-size=65%')),
          	    sliderInput("slider_alpha",h6('Alpha :'),min = 0.1,max = 0.90,value = c(0.5),step=0.1),
        				sliderInput("slider_inc", h6("Income :"),min = 100, max = 200, value = 200),
        				sliderInput("slider_px", h6("Price of good x :"),min = 1, max = 5, value = 1),
        				sliderInput("slider_py", h6("Price of good y :"),min = 1, max = 5, value = 1)),
      	   	wellPanel(
      	   		helpText(h5("Plotting options",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
      	   		sliderInput("slider_phi",h6('Perspective Phi :'),min = 0,max = 360,value = 10),
          		sliderInput("slider_theta",h6('Perspective Theta :'),min = 0,max = 360,value = 70)
            ),
          	wellPanel(        	
        			helpText(h5("Slutsky Decomposition & Lumpsum principle",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
						  helpText(h6('The Slutsky equation decomposes the change in the demand for a good,as a result of a price change,into a substitution effect and an income effect.The lump sum principle illustrates the superiority of taxes on general purchasing power to taxes on specific goods.',style='font-size=65%')),
          		uiOutput("slider"),
          		helpText(h6('The substituion effect considers the change in the relative price, with a sufficient change in income to keep the consumer on the same utility isoquant.Income effect is the change in consumption resulting from the change in income. It adjusts for the loss of (gain in) purchasing power arising from the price increase (decrease).',style='font-size=65%')),
          		sliderInput("slider_tax",h6('Tax per unit of good x :'),min = 0,max = 5,value = 0),
          		helpText(h6('A tax on good x rotates the original budget contraint (black) to a new one (green).An equivalent-revenue income tax shifts the budget contraint (cyan) resulting in higher consumption & utility',style='font-size=65%'))
          )
          ),
      	 column(9,
      	 		fluidRow(
      	 			 column(4,plotOutput('paramtbl')),column(8,plotOutput("uplot"))
      	 		),
      	 		fluidRow(	
      	 			 column(12,plotOutput("levelcurve"))
      	 		),
      	 	  fluidRow(
      	  		column(12,plotOutput("slutskyplot"))
      	 	  ),
      	 	  fluidRow(
      	 	  	column(12,plotOutput("taxplot"))
      	 	  )
     	 )
      )
    )
   )
  ) 
 ),
	tabPanel('Marshallian Demand Curves',
		fluidPage(
     fluidRow(
      column(12,headerPanel(h1(":: { Playground } ::",style = "font-family: 'Times New Roman', cursive;
        font-size: 125%; line-height: 0; color: white;"), windowTitle = 'Consumer theory'),div(style = "height:30px;background-color: darkblue;"),
       fluidRow(
          column(3,wellPanel(        	
        		helpText(h5("Marshallian Demand Curve",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  helpText(h6('The marshallian demand curve for x summarises the relationship between price and quantity demanded under the assumption that (a)income,(b)price of good y,and (c) preferences remain constant as the price of x varies.',style='font-size=65%')),
          	    sliderInput("slider2_alpha",h6('Alpha :'),min = 0.1,max = 0.90,value = c(0.5),step=0.1),
        				sliderInput("slider2_inc", h6("Income :"),min = 100, max = 200, value = 100),
        				sliderInput("slider2_num", h6("Number of price points :"),min = 1, max = 30, value = 1),
        				sliderInput("slider2_py", h6("Fixed price of good y :"),min = 1, max = 5, value = 3)),
          	wellPanel(
          	 helpText(h5("Colour options for good x",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
      	        selectInput('drop2_col1',h6('Low price colour :'),colors(),selected='red'),
      	        selectInput('drop2_col2',h6('High price colour :'),colors(),selected='gold'))),
          	column(9,plotOutput('marshallCombined',height=1000))
       )	
      )	
     )  	
		)
),
	tabPanel('Hicksian Demand Curves',
		fluidPage(
     fluidRow(
      column(12,headerPanel(h1(":: { Playground } ::",style = "font-family: 'Times New Roman', cursive;
        font-size: 125%; line-height: 0; color: white;"), windowTitle = 'Consumer theory'),div(style = "height:30px;background-color: darkblue;"),
       fluidRow(
          column(3,wellPanel(        	
        		helpText(h5("Hicksian Demand Curve",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  helpText(h6('The hicksian demand curve for x summarises the relationship between price and quantity demanded under the assumption that (a)utility,(b)price of good y,and (c) preferences remain constant as the price of x varies.The hicksian compensated demand curve illustrates only substitution effects. ',style='font-size=65%')),
          	    sliderInput("slider3_alpha",h6('Alpha :'),min = 0.1,max = 0.90,value = c(0.5),step=0.1),
        				sliderInput("slider3_util", h6("Utility :"),min = 20, max = 100, value = 29),
        				sliderInput("slider3_num", h6("Number of price points :"),min = 1, max = 30, value = 1),
        				sliderInput("slider3_py", h6("Fixed price of good y :"),min = 1, max = 5, value = 3)),
          	wellPanel(
          	 helpText(h5("Colour options for good x",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
      	        selectInput('drop3_col1',h6('Low price colour :'),colors(),selected='red'),
      	        selectInput('drop3_col2',h6('High price colour :'),colors(),selected='gold'))),
          	column(9,plotOutput('hicksianCombined',height=1000))
       )	
      )
     )  	
		)
),
	tabPanel('Comparing Demand Curves',
		fluidPage(
     fluidRow(
      column(12,headerPanel(h1(":: { Playground } ::",style = "font-family: 'Times New Roman', cursive;
        font-size: 125%; line-height: 0; color: white;"), windowTitle = 'Consumer theory'),div(style = "height:30px;background-color: darkblue;"),
       fluidRow(
          column(3,wellPanel(        	
        		helpText(h5("Comparing Demand Curves",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  helpText(h6('The marshallian demand curve is more price-responsive than the hicksian compensated demand curve since the former reflects both income and substution effects while the latter is derived by holding utility constant and therefore captures only the substitution effect.The demand curves intersect at the price where income is just sufficient to attain the original indifference curve',style='font-size=65%')),
          	    sliderInput("slider4_alpha",h6('Alpha :'),min = 0.1,max = 0.90,value = c(0.5),step=0.1),
        				sliderInput("slider4_inc", h6("Income :"),min = 100, max = 200, value = 100),
        				sliderInput("slider4_py", h6("Fixed price of good y :"),min = 1, max = 5, value = 3),
       	        sliderInput("slider4_px", h6("Change price of good x to :"),min = 2, max = 5, value = 3)),
          	wellPanel(
          		helpText(h6('The orginal price of good x is 1.The corresponding contour/budget set is black.Increasing the price of good x using the slider rotates the budget constraint inward around the original y intercept.The corresponding contour/budget set is white.Parallel shifting this new (white) budget constraint yields the compensated case (yellow). The demand curves are derived using only two points according to the point-point formula.The marshallian curve is constructed from the two black points (original & new optimum).The hicksian curve is constructed from the red and first black points (compensated & original optimum).',style='font-size=65%'))
              )),
        	column(9,plotOutput('demandCombined',height=1000))
       )	
      )
     )  	
		)
),
	tabPanel('Welfare Changes',
		fluidPage(
     fluidRow(
      column(12,headerPanel(h1(":: { Playground } ::",style = "font-family: 'Times New Roman', cursive;
        font-size: 125%; line-height: 0; color: white;"), windowTitle = 'Consumer theory'),div(style = "height:30px;background-color: darkblue;"),
       fluidRow(
          column(3,wellPanel(        	
        		helpText(h5("Comparing Welfare changes",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  helpText(h6('Compensating variation (CV) is a measure of utility change that refers to the additional amount of money an individual would require to reach the initial utility after a change in prices.It can be used to find the effect of a price change on net welfare,reflecting new prices and old utility levels. Equivalent variation (EV) measures the additional amount of money a consumer would pay to avert the price increase. This measure uses old prices and the new utility level.When there are no income effects for the good then both metrics are equivalent.',style='font-size=65%')),
          	    sliderInput("slider5_alpha",h6('Alpha :'),min = 0.1,max = 0.70,value = c(0.3),step=0.1,animate=animationOptions(interval=300, loop=F)),
       	        sliderInput("slider5_px", h6("Increase price of good x by :"),min = 0, max = 3, value = 0,animate=animationOptions(interval=300, loop=F))),
          	wellPanel(
          		helpText(h6('The orginal price of good x is 1.The corresponding contour/budget set is black.Increasing the price of good x using the slider rotates the budget constraint inward around the original y intercept.The corresponding contour/budget set is white.Parallel shifting this new (white) budget constraint yields the compensated case (yellow) and allows us to calculate CV.The equivalent variation (EV) metric measures the monetary amount by which income must fall to generate the same disutility that would otherwise be caused by the price increase in good x.',style='font-size=65%'))
              )),
        	column(9,plotOutput('welfarePlot',height=550,width=900),plotOutput('tablePlot',height=200,width=900))
       )	
      )
     )  	
		)
),
	tabPanel('Income-Consumption Curve',
		fluidPage(
     fluidRow(
      column(12,headerPanel(h1(":: { Playground } ::",style = "font-family: 'Times New Roman', cursive;
        font-size: 125%; line-height: 0; color: white;"), windowTitle = 'Consumer theory'),div(style = "height:30px;background-color: darkblue;"),
       fluidRow(
          column(3,wellPanel(        	
        		helpText(h5("Income effect",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  helpText(h6('The income effect is the change in consumption resulting from a change in real income.This change in income can either be internal as illustrated using the slutsky decomposition of a price change, or external.The effect of the latter type of change in available income is captured by the income-consumption curve which is the set of tangency points between indifference curves and associated budget constraints with (a) prices held constant and (b) income permitted to vary.The curve is the locus of points showing optimal consumption bundles at different levels of income.',style='font-size=65%')),
          	    sliderInput("slider6_alpha",h6('Alpha :'),min = 0.1,max = 0.90,value = c(0.5),step=0.1,animate=animationOptions(interval=300, loop=F)),
       	        sliderInput("slider6_px", h6("Change price of good x to :"),min = 1, max = 5, value = 1),
          	    sliderInput("slider6_py", h6("Change price of good y to :"),min = 1, max = 5, value = 1),
        				sliderInput("slider6_inc", h6("Income :"),min = 100, max = 400, value = 100,step=50,animate=animationOptions(interval=300, loop=F))),
            wellPanel(
          		helpText(h6('The income consumption curve can be used to derive the Engel curve which captures the relationship between income and quantity demanded,with all other influences (ie.prices,consumer preferences) held constant.As income increases,the budget line shifts outward resulting in new tangency points on successively higher indiferrence curves,providing correspondingly superior utility levels from successively greater consumption levels.',style='font-size=65%'))
              )),
       	  column(9,plotOutput('combCurves',height=1000))

       )	
      )
     )  	
		)
),
	tabPanel('Choice Under Uncertainty',
		fluidPage(
     fluidRow(
      column(12,headerPanel(h1(":: { Playground } ::",style = "font-family: 'Times New Roman', cursive;
        font-size: 125%; line-height: 0; color: white;"), windowTitle = 'Consumer theory'),div(style = "height:30px;background-color: darkblue;"),
       fluidRow(
          column(3,wellPanel(        	
        		helpText(h5("Choice under uncertainty",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  helpText(h6('Choosing different utility functions from the dropdown box changes the shiny UI inputs dynamically.Different functional forms along with input choices specified below imply different (a) absolute/relative coefficients of risk aversion,(b) certainty equivalents,(c) risk premia.',style='font-size=65%')),
						selectInput('utility_choice',h6('Choice of utility function :',style='font-size=65%'),c('Quadratic Utility Function','Power Utility Function (CRRA)','General Exponential Utility (CARA)','Log Utility Function','Hyperbolic Absolute Risk Aversion (HARA)'),  selected='Quadratic Utility Function', multiple=FALSE)
           )),
       	 	column(3,wellPanel(        	
        		helpText(h5("Inputs to the gamble",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					  withMathJax(),
         	  helpText(h6('An individual starts with \\(W_0\\) and is faced with a probability \\(\\pi_L\\) of loosing an amount \\(L\\) and a probability \\(1-\\pi_L\\) of winning an amount \\(G\\).Together with the chosen utility specification,these inputs help determine (a)the amount of fair insurance,(b)CEQ,(c)Risk premia and associated utility values.The loss/gain slider values are updated dynamically depending on the chosen initial wealth.The \\(Gain\\) and \\(Loss\\) chords in the resulting plot are calculated by adding \\(W+G\\) or subtracting \\(W-L\\) the gains & losses to or from \\(W_0\\).',style='font-size=65%'))
       	 	)),
       	 	column(3,wellPanel(        	
       	 		uiOutput("slider_wealth"),
         	  sliderInput("slider_lossprob",h6('Probability of loss :'),min = 0,max = 1,value = 0.5)
       	 )),
       	 column(3,wellPanel(        	
       	 		uiOutput("slider_loss"),
          	uiOutput("slider_gain")
         ))),
       fluidRow(
       	column(3,wellPanel(
        	helpText(h5("Inputs for chosen functional form",style = "font-family: 'Times New Roman',font-size: 100%;color: darkblue;")),
					uiOutput("formula_text"),
           	  #Quadratic Utility Function
           		conditionalPanel(condition="input.utility_choice=='Quadratic Utility Function' ",
        			  sliderInput("slider_quad_a",h6('parameter a :'),min = 100,max = 200,value = 100),
        			  sliderInput("slider_quad_b",h6('parameter b :'),min = -5,max = 5,value = 1)
           ),
           		#Power Utility Function (CRRA)
              conditionalPanel(condition="input.utility_choice=='Power Utility Function (CRRA)' ",
        			  sliderInput("slider_power_r",h6('parameter r :'),min = -3,max = 0.90,value = 0,step=0.1)
      		 ),
           	  #General Exponential Utility (CARA)
              conditionalPanel(condition="input.utility_choice=='General Exponential Utility (CARA)' ",
        			  sliderInput("slider_exp_rho",h6('parameter rho :\n(Choose value other than 0)'),min = -1,max = 1,value = 1,step=0.1)
      		),
           	  #Log Utility Function
           		conditionalPanel(condition="input.utility_choice=='Log Utility Function' ",
					 			helpText(h6('Log utility functions do not require parameter inputs',style='font-size=65%'))
      		 ),
           	  #Hyperbolic Absolute Risk Aversion (HARA)          	
           		conditionalPanel(condition="input.utility_choice=='Hyperbolic Absolute Risk Aversion (HARA)' ",
        			  numericInput("text_h_gamma",h6('parameter gamma :'),value = 0.9,min = NA, max = 0.9,step=0.05),
           		  numericInput("text_h_a",h6('parameter a :'),value = 1,min = 0.1, max = NA),
           		  numericInput("text_h_b",h6('parameter b :'),value = 1,min = NA, max = NA)
     		  ),
           	withMathJax(),
         	  helpText(h6('The utility function can be used to derive the Arrow-Pratt coefficients of absolute/relative risk aversion aswell as the risk tolerance function.',style='font-size=65%')),
           	hr(),
         	  helpText(h6('The coefficient of absolute risk aversion is given by \\(A(w)=-\\frac{U^2(w)}{U^1(w)}\\),The coefficient of relative risk aversion is given by \\(R(w)=w*A(w)\\).',style='font-size=65%')),
           	hr(),
         	  helpText(h6('The absolute coefficient of risk aversion tracks how preferences change when wealth increases.The relative coefficient of risk aversion measures aversion to gambles that are proportional to wealth levels.These measures of curvature can be used to classify individuals as risk neutral/loving/averse',style='font-size=65%'))
         )),
       	  column(9,plotOutput('FullPlot',height=550),plotOutput('CoeffsPlot',height=500))
       	 	)
         
        )
     )  	
		)
)
)
)
)
