#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#                   BUILD GUI BASED ON GTK FRAMEWORK
# 
#         This GUI is fully functional but been replaced by a 
#         browser-based interface. The code is kept here in commented form 
#         for documentation. 
#         Depenencies: 
#             GTK+ (>= 2.24) framework
#             R packages: RGtk2, cairoDevice
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# modifier_cwidget <- function(min=1, max=100, xlab="default xlab")
# {
#   vbox <- gtkVBox()
#   vbox.0.range <- gtkHBox()
#   vbox.1.draw <- gtkVBox()
#   vbox.2.scale <- gtkVBox()   
#   hbox.3.mod.prob <- gtkHBox()   
#   
#   align.0.range <- gtkAlignment(0,0,0,0)
#   align.0.range$add(vbox.0.range)
#   align.3.mod.prob <- gtkAlignment(0,.5,0,0)
#   align.3.mod.prob$add(hbox.3.mod.prob) 
#     
#   vbox$packStart(align.0.range, padding=10)   
#   vbox$packStart(vbox.1.draw, padding=10)
#   vbox$packStart(vbox.2.scale, padding=0)
#   vbox$packStart(align.3.mod.prob, padding=10) 
#   
#   # 0. spin button for min and max number   
#   spin.min <- gtkSpinButton(min=min, max=max, step=1)  
#   spin.max <- gtkSpinButton(min=min, max=max, step=1)
#   spin.min$setValue(min)
#   spin.max$setValue(min) 
#   label.min <- gtkLabel("Minimum")   
#   vbox.0.range$packStart(label.min, expand=F, fill=T, padding=5) 
#   vbox.0.range$packStart(spin.min, expand=F, fill=T, padding=10)
#   label.max <- gtkLabel("Maximum")
#   vbox.0.range$packStart(label.max, expand=F, fill=T, padding=5) 
#   vbox.0.range$packStart(spin.max, expand=T, fill=F, padding=10)
#   
#   # 1. visualization of probabilities for number of attributes
#   #vbox.1.draw$packStart(gtkLabel("Probabilities"))
#   da <- gtkDrawingArea()   
#   da$setSizeRequest(width=200, height=200) 
#   vbox.1.draw$add(da)
#   
#   # 2. scale selector
#   chk <- gtkCheckButton("Fix y-axis to [0, 1]")
#   vbox.2.scale$add(chk)
#   
#   # 3. manual modification of probabilities  
#   spin.select <- gtkSpinButton(min=min, max=max, step=1)  
#   spin.prob <- gtkSpinButton(min=0, max=1, step=.01)
#   label.attribute <- gtkLabel(xlab)  # counts or category
#   hbox.3.mod.prob$packStart(label.attribute, expand=F, fill=T, padding=5)
#   hbox.3.mod.prob$packStart(spin.select, expand=F, fill=T, padding=5) 
#   label.probability <- gtkLabel("Probability")
#   hbox.3.mod.prob$packStart(label.probability, expand=F, fill=T, padding=5) 
#   hbox.3.mod.prob$packStart(spin.prob, expand=F, fill=T, padding=5)            
#   
#   gset(vbox, "spin.min", spin.min) 
#   gset(vbox, "spin.max", spin.max)
#   gset(vbox, "da", da)
#   gset(vbox, "chk", chk) 
#   gset(vbox, "spin.select", spin.select) 
#   gset(vbox, "spin.prob", spin.prob) 
#   gset(vbox, "probs", rep(1/(max - min + 1), (max - min + 1))) 
#   gset(vbox, "cw_class", "cw_modifier") 
#   
#   gSignalConnect(spin.min, "value-changed", 
#                  handler_adjust_min_max_spinners, 
#                  data = list(cw=vbox, widget="spin.min"))
#   gSignalConnect(spin.max, "value-changed", 
#                  handler_adjust_min_max_spinners, 
#                  data = list(cw=vbox, widget="spin.max"))
#   gSignalConnect(spin.min, "value-changed", 
#                  handler_adjust_active_attribute_spinner, data = list(cw=vbox))
#   gSignalConnect(spin.max, "value-changed", 
#                  handler_adjust_active_attribute_spinner, data = list(cw=vbox)) 
# 
#   gSignalConnect(chk, "clicked", handler_modifier_draw_probs,
#                  data = list(cw=vbox, xlab=xlab))
#   gSignalConnect(spin.select, "value-changed", handler_modifier_draw_probs,
#                  data = list(cw=vbox, xlab=xlab))
#   gSignalConnect(spin.min, "value-changed", handler_modifier_draw_probs, 
#                  data = list(cw=vbox, xlab=xlab))
#   gSignalConnect(spin.max, "value-changed", handler_modifier_draw_probs,
#                  data = list(cw=vbox, xlab=xlab))   
#   
#   # manipulation of probabilties                                                
#   gSignalConnect(spin.select, "value-changed", 
#                  handler_update_active_att_probability_value,
#                  data=list(cw=vbox))
#   gSignalConnect(spin.prob, "value-changed", 
#                  handler_change_active_probability,
#                  data=list(cw=vbox, xlab=xlab))                                                  
#   vbox
# }
# 
#   
# modifier_da_as_cairo_plus_device_number <- function(cw)
# {
#   cairoDevice::asCairoDevice(gget(cw, "da"))
#   Sys.sleep(.1)
#   gset(gget(cw, "da"), "device.number", dev.cur()) 
# }
# 
# 
# get_modifier_object_device_number <- function(cw)
# {
#   da <- gget(cw, "da")
#   gget(da, "device.number")    
# }
# 
# 
# set_cairo_device <- function(cw)
# {
#   dev.no <- get_modifier_object_device_number(cw) 
#   dev.set(dev.no)
# } 
# 
# 
# handler_adjust_min_max_spinners <- function(w, data=list(widget="NA"))
# {
#   cw <- data$cw
#   spin.min <- gget(cw, "spin.min")
#   spin.max <- gget(cw, "spin.max")
#   spin.min.val <- spin.min$getValueAsInt()
#   spin.max.val <- spin.max$getValueAsInt()
#   if (data$widget == "spin.min" & spin.min.val > spin.max.val)
#       spin.max$setValue(spin.min.val)   
#   if (data$widget == "spin.max" & spin.max.val < spin.min.val)
#       spin.min$setValue(spin.max.val)  
#   return(TRUE)    
# }
# 
# 
# handler_adjust_active_attribute_spinner <- function(w, data=list(), ...)
# {
#   cw <- data$cw
#   spin.min <- gget(cw, "spin.min")
#   spin.max <- gget(cw, "spin.max")  
#   spin.min.val <- spin.min$getValueAsInt()
#   spin.max.val <- spin.max$getValueAsInt()  
#   gget(cw, "spin.select")$setRange(spin.min.val, spin.max.val)
#   return(TRUE) 
# }
# 
# 
# handler_update_active_att_probability_value <- function(w, data=list(), ...)
# {
#   cw <- data$cw
#   cur.index <- gget(cw, "spin.select")$getValueAsInt()
#   probs <- gget(cw, "probs")
#   gget(cw, "spin.prob")$setValue(probs[cur.index])
#   return(TRUE)     
# }
# 
#  
# handler_change_active_probability <- function(w, data=list(), ...)
# {
#   cw <- data$cw
#   cur.index <- gget(cw, "spin.select")$getValueAsInt()
#   probs <- gget(cw, "probs")  
#   probs[cur.index] <- gget(cw, "spin.prob")$getValue()
#   gset(cw, "probs", probs)   
#   modifier_draw_probs(cw, xlab=data$xlab)
#   return(TRUE)
# }  
# 
#  
# # norm visible probabilities within window so they add up to 1
# modifier_normalize_modifier_probs <- function(cw)
# {
#   att.probs <- gget(cw, "probs")
#   min <- gget(cw, "spin.min")$getValueAsInt()
#   max <- gget(cw, "spin.max")$getValueAsInt() 
#   att.interval <- att.probs[min:max] 
#   att.probs <- rep(0, length(att.probs)) 
#   if (sum(att.interval) != 0)     # prevent error in case all probs are zero by making probs uniform
#     new.vals <- att.interval / sum(att.interval)
#   else
#     new.vals <- rep(1 / length(att.interval), length(att.interval))
#   att.probs[min:max] <- new.vals
#   gset(cw, "probs", att.probs)
# }      
# 
# 
# # visualize probabilities (drawing function)
# modifier_draw_probs <- function(cw, xlab="test")
# {
#   modifier_normalize_modifier_probs(cw) 
#   probs <- gget(cw, "probs")
#   min <- gget(cw, "spin.min")$getValueAsInt()
#   max <- gget(cw, "spin.max")$getValueAsInt()
#   active.att.index <- gget(cw, "spin.select")$getValueAsInt() - min + 1     
#   n <- max - min + 1
#   att.color <- rep("black", n)
#   att.color[active.att.index] <- "red" 
#   check <- gget(cw, "chk")$getActive()
#   if (check)
#     ylim <- c(0,1.05)
#   else
#     ylim <- c(0, max(probs)*1.1)   
#   set_cairo_device(cw)    
#   par(oma=c(0,0,0,0), mar=c(4.5,4.5,0.8,1)) 
#   plot(min:max, probs[min:max], 
#        col=att.color, type="h", las=1, 
#        cex.axis=.9, cex.lab=.9, xaxt="n", lwd=2, 
#        ylim=ylim, 
#        ylab="Probability", xlab=xlab) 
#   Axis(at=min:max, side=1, cex.axis=.9)
#   text(min:max, probs[min:max], round(probs[min:max], 2), pos=3, cex=.8)
# }
# 
# 
# handler_modifier_draw_probs <- function(w, data, ...)
# {
#   modifier_draw_probs(data$cw, xlab=data$xlab)
# }      
#   
# 
# cw <- function(cw)
# {
#   #if (gget(cw, "cw_class") == )
#   cat("references: spin.min, spin.max, da, chk, spin.select, spin.prob")
#   cat("\nprobs:", gget(cw, "probs")) 
#   cat("\ncw_class:", gget(cw, "cw_class"))
# } 
# 
# 
# 
# probability_preset <- function(cw, xlab="undefined")
# {
#    # probability presets expander (uniform, normal, poisson)
#    btn.width <- 90
#    btn.height <- 25
#    
#    hbox.uniform <- gtkHBox()
#    btn.prob.uniform <- gtkButton("Uniform")    
#    btn.prob.uniform$setSizeRequest(btn.width, btn.height)
#    hbox.uniform$packStart(btn.prob.uniform, expand=FALSE, fill=FALSE, padding=5)
# 
#    hbox.normal <- gtkHBox()
#    btn.prob.normal <- gtkButton("Normal")
#    btn.prob.normal$setSizeRequest(btn.width, btn.height)
#    hbox.normal$packStart(btn.prob.normal, expand=FALSE, fill=FALSE, padding=5)
#    hbox.normal$packStart(gtkLabel("mean"), padding=5)
#    normal.spin.mean <- gtkSpinButton(min=1, max=20, step=.5)  
#    normal.spin.mean$setValue(6)
#    hbox.normal$packStart(normal.spin.mean, padding=5)
#    hbox.normal$packStart(gtkLabel("sd"), padding=5 )
#    normal.spin.sd <- gtkSpinButton(min=1, max=20, step=.2)  
#    hbox.normal$packStart(normal.spin.sd, padding=5)
# 
#    hbox.poisson <- gtkHBox()
#    btn.prob.poisson <- gtkButton("Poisson")
#    btn.prob.poisson$setSizeRequest(btn.width, btn.height)
#    hbox.poisson$packStart(btn.prob.poisson, expand=FALSE, fill=FALSE, padding=5)
#    hbox.poisson$packStart(gtkLabel("lambda"), expand=FALSE, fill=FALSE, padding=5)
#    poisson.spin.lambda <- gtkSpinButton(min=1, max=20, step=.5)  
#    poisson.spin.lambda$setValue(6)
#    hbox.poisson$packStart(poisson.spin.lambda, expand=FALSE, fill=FALSE, padding=5)
#    
#    hbox.exponential <- gtkHBox()
#    btn.prob.exponential <- gtkButton("Exponential")
#    btn.prob.exponential$setSizeRequest(btn.width, btn.height)
#    hbox.exponential$packStart(btn.prob.exponential, expand=FALSE, fill=FALSE, padding=5)
#    hbox.exponential$packStart(gtkLabel("rate"), expand=FALSE, fill=FALSE, padding=5)
#    exponential.spin.rate <- gtkSpinButton(min=0, max=5, step=.01)  
#    exponential.spin.rate$setValue(.1)
#    hbox.exponential$packStart(exponential.spin.rate, expand=FALSE, fill=FALSE, padding=5)
#   
#    align.uniform <- gtkAlignment(0, .5,0,0)
#    align.normal <- gtkAlignment(0,.5,0,0)
#    align.poisson <- gtkAlignment(0,.5,0,0) 
#    align.exponential  <- gtkAlignment(0,.5,0,0)
#    
#    align.uniform$add(hbox.uniform)
#    align.normal$add(hbox.normal)
#    align.poisson$add(hbox.poisson)
#    align.exponential$add(hbox.exponential)   
#    
#    vbox.gen.probs <- gtkVBox(T, 4)
#    vbox.gen.probs$add(align.uniform)
#    vbox.gen.probs$add(align.normal)
#    vbox.gen.probs$add(align.poisson) 
#    vbox.gen.probs$add(align.exponential)  
#    vbox.outer <- gtkVBox()
#    vbox.outer$packStart(vbox.gen.probs, padding=15)
#    valign <- gtkAlignment(0,0,1,0)
#   
#    probs.frame <- gtkFrame("Probability presets") 
#    probs.frame$add(vbox.outer)
#    valign$add(probs.frame) 
#    exp <- valign
#    
#    gset(exp, "btn.prob.uniform", btn.prob.uniform) 
#    gset(exp, "btn.prob.normal", btn.prob.normal)
#    gset(exp, "btn.prob.poisson", btn.prob.poisson) 
#    gset(exp, "btn.prob.poisson", btn.prob.exponential)  
#    gset(exp, "normal.spin.mean", normal.spin.mean) 
#    gset(exp, "normal.spin.sd", normal.spin.sd) 
#    gset(exp, "poisson.spin.lambda", poisson.spin.lambda) 
#    gset(exp, "exponential.spin.rate", exponential.spin.rate) 
#    
#    gset(exp, "cw_class", "cw_preset")
#    
#    # calculate probability presets
#    gSignalConnect(btn.prob.uniform, "clicked", 
#                   handler_prob_preset,
#                   data=list(cw=cw, cw_preset=exp, preset="uniform", xlab=xlab))    
#    gSignalConnect(btn.prob.normal, "clicked", 
#                   handler_prob_preset,
#                   data=list(cw=cw, cw_preset=exp, preset="normal", xlab=xlab))
#    gSignalConnect(btn.prob.poisson, "clicked", 
#                   handler_prob_preset,
#                   data=list(cw=cw, cw_preset=exp, preset="poisson", xlab=xlab))                                                   
#    gSignalConnect(btn.prob.exponential, "clicked", 
#                  handler_prob_preset,
#                  data=list(cw=cw, cw_preset=exp, preset="exponential", xlab=xlab))  
#    exp 
# }  
# 
# 
# # handler for probability presets 
# handler_prob_preset <- function(w, data, ...)
# {
#    cw <- data$cw   
#    pr <- data$cw_preset
#    probs <- gget(cw, "probs") 
#    spin.min.val <- gget(cw, "spin.min")$getValueAsInt()
#    spin.max.val <- gget(cw, "spin.max")$getValueAsInt()
#    if (data$preset == "uniform"){
#       probs <- rep(1, length(probs))    
#       gset(cw, "probs", probs)
#    }
#    if (data$preset == "normal"){
#      mean <- gget(pr, "normal.spin.mean")$getValue() 
#      sd <- gget(pr, "normal.spin.sd")$getValue()
#      probs <- dnorm(seq_along(probs), mean, sd)
#      gset(cw, "probs", probs)
#    }
#    if (data$preset == "poisson"){
#      lambda <- gget(pr, "poisson.spin.lambda")$getValue() 
#      probs <- dpois(seq_along(probs), lambda)
#      gset(cw, "probs", probs)
#    }  
#    if (data$preset == "exponential"){
#      rate <- gget(pr, "exponential.spin.rate")$getValue() 
#      probs <- dexp(seq_along(probs), rate)
#      gset(cw, "probs", probs)
#    }
#    modifier_draw_probs(cw, xlab=data$xlab)
#    handler_update_active_att_probability_value(NULL, data=data) 
#    return(TRUE) 
# }   
# 
# 
# cw_draw_one_sample <- function(cw1, cw2)
# {
#   vbox.3 <- gtkVBox()   
#   
#   # draw one random sample
#   spin.sample.size <- gtkSpinButton(min=1, max=1000, step=1)  
#   spin.sample.size$setValue(50)
#   btn.one.sample <- gtkButton("Random sample")
#   btn.one.sample$setSizeRequest(150, 25)  
#   hbox.one.sample.1a <- gtkHBox()   
#   hbox.one.sample.1a$packStart(gtkLabel("Sample size (N)"), padding=10)
#   hbox.one.sample.1a$packStart(spin.sample.size, padding=5)   
#   align.1a <- gtkAlignment(0,.5,0,0)
#   align.1a$add(hbox.one.sample.1a)
#   align.1b <- gtkAlignment(1,.5,0,0)  
#   align.1b$add(btn.one.sample)  
#   hbox.one.sample.1 <- gtkHBox()
#   hbox.one.sample.1$packStart(align.1a)
#   hbox.one.sample.1$packStart(align.1b)  
#   
#   # run simulation n times
#   spin.runs <- gtkSpinButton(min=100, max=10000, step=100)
#   btn.runs <- gtkButton("Run")
#   btn.runs$setSizeRequest(150, 25) 
#   hbox.one.sample.2a <- gtkHBox()  
#   hbox.one.sample.2a$packStart(gtkLabel("Run simulation n times"), padding=10)
#   hbox.one.sample.2a$packStart(spin.runs, padding=5)  
#   align.2a <- gtkAlignment(0,.5,0,0)
#   align.2a$add(hbox.one.sample.2a)
#   align.2b <- gtkAlignment(1,.5,0,0)  
#   align.2b$add(btn.runs)
#   hbox.one.sample.2 <- gtkHBox()
#   hbox.one.sample.2$packStart(align.2a)
#   hbox.one.sample.2$packStart(align.2b)
#   
#   # drawing area  
#   da.one.sample <- gtkDrawingArea()
#   da.one.sample$setSizeRequest(400, 250)
#   vbox.one.sample <- gtkVBox()
#   vbox.one.sample$add(gtkLabel(""))
#   vbox.one.sample$add(hbox.one.sample.1)    
#   vbox.one.sample$add(hbox.one.sample.2)
#   vbox.one.sample$add(da.one.sample)
#   vbox.3$add(vbox.one.sample)    
#   
#   # getters for internal pointers
#   gset(vbox.3, "da", da.one.sample)
#   gset(vbox.3, "spin.sample.size", spin.sample.size)    
#   gset(vbox.3, "spin.runs", spin.runs) 
#   
#   # signals
#   gSignalConnect(btn.one.sample , "clicked", handler_btn_one_sample,
#                  data=list(cw1=cw1, cw2=cw2, cw3=vbox.3, plot="one.sample"))                 
#   gSignalConnect(btn.runs, "clicked", handler_btn_one_sample,
#                  data=list(cw1=cw1, cw2=cw2, cw3=vbox.3, plot="quantiles"))   
#   vbox.3
# }  
# 
# get_probs_from_modifier <- function(cw)
# {
#   probs <- gget(cw, "probs")
#   min <- gget(cw, "spin.min")$getValueAsInt()
#   max <- gget(cw, "spin.max")$getValueAsInt()  
#   res <- probs[min:max] 
#   names(res) <- min:max
#   res
# }
# 
# 
# handler_btn_one_sample <- function(w, data, ...)
# {  
#   probs.no.attributes <- get_probs_from_modifier(data$cw1)
#   no.attributes <- as.integer(names(probs.no.attributes))
#   probs.categories <- get_probs_from_modifier(data$cw2)
#   size <- gget(data$cw3, "spin.sample.size")$getValueAsInt() 
#   runs <- gget(data$cw3, "spin.runs")$getValueAsInt() 
#   set_cairo_device(data$cw3) 
#   if (data$plot == "one.sample"){
#     g <- draw_n_person_sample(probs.categories, n=size, a=no.attributes, 
#                           ap=probs.no.attributes)    
#   } else {
#     r <- sim_n_persons_x_times(probs.categories, n=size, a=no.attributes, 
#                                ap=probs.no.attributes, times=runs, progress="tk")  
#     g <- expected_frequencies(r)    
#   }    
#   print(g)
# }
# 
# 
# cw_draw_many_samples <- function(cw1, cw2, cw3)
# {
#   vbox.4 <- gtkVBox()
#   
#   #simulate many samples    
#   lbl.ns <- gtkLabel("Sample size (N)")
#   ent.ns <- gtkEntry()
#   ent.ns$setSizeRequest(200, 15)
#   ent.ns$setText("10, 20, 30, 40, 50, 60, 70, 80") 
#   lbl.runs <- gtkLabel("Simulation runs for each N")
#   spin.runs <- gtkSpinButton(min=100, max=2000, step=100)  
#   spin.runs$setValue(100)
#   btn.simulate <- gtkButton("Simulate")
#   btn.simulate$setSizeRequest(150, 40)
#      
#   lbl.m <- gtkLabel("Minimum count (M)")  
#   ent.m <- gtkEntry()
#   ent.m$setSizeRequest(90, 15)  
#   ent.m$setText("4, 5, 6")
#   lbl.prop <- gtkLabel("Proportion (K)")   
#   ent.prop <- gtkEntry() 
#   ent.prop$setSizeRequest(90, 15)    
#   ent.prop$setText("0.9, 0.95, 1")  
#   btn.redraw <- gtkButton("Redraw")
#   btn.redraw$setSizeRequest(150, 40) 
#   btn.redraw$setSensitive(F)
#   
#   # Sampling properties (Simulation)
#   hbox.a <- gtkHBox()
#   hbox.a$packStart(lbl.ns, padding=10) 
#   hbox.a$packStart(ent.ns, padding=5)  
#   align.hbox.a <- gtkAlignment(0, .5, 0, 0)
#   align.hbox.a$add(hbox.a)
#   hbox.b <- gtkHBox()
#   hbox.b$packStart(lbl.runs, padding=10)
#   hbox.b$packStart(spin.runs, padding=5)
#   align.hbox.b <- gtkAlignment(0, .5, 0, 0)
#   align.hbox.b$add(hbox.b)
#   vbox.ab <- gtkVBox() 
#   vbox.ab$packStart(align.hbox.a, padding=0)
#   vbox.ab$packStart(align.hbox.b, padding=0)  
#   align.2 <- gtkAlignment(1, .5, 0, 0)
#   align.2$add(btn.simulate) 
#   hbox.1.simulate <- gtkHBox()
#   hbox.1.simulate$add(vbox.ab)
#   hbox.1.simulate$add(align.2)  
#   
#   # Drawing properties (Redraw)
#   hbox.a <- gtkHBox()
#   hbox.a$packStart(lbl.m, padding=10) 
#   hbox.a$packStart(ent.m, padding=5)  
#   align.hbox.a <- gtkAlignment(0, .5, 0, 0)
#   align.hbox.a$add(hbox.a)
#   hbox.b <- gtkHBox()
#   hbox.b$packStart(lbl.prop, padding=10)
#   hbox.b$packStart(ent.prop, padding=5)
#   align.hbox.b <- gtkAlignment(0, .5, 0, 0)
#   align.hbox.b$add(hbox.b)
#   vbox.ab <- gtkVBox() 
#   vbox.ab$packStart(align.hbox.a, padding=0)
#   vbox.ab$packStart(align.hbox.b, padding=0)  
#   align.2 <- gtkAlignment(1, .5, 0, 0)
#   align.2$add(btn.redraw) 
#   hbox.2.simulate <- gtkHBox()
#   hbox.2.simulate$add(vbox.ab)
#   hbox.2.simulate$add(align.2)
#   
#   # drawing area  
#   da.simulate <- gtkDrawingArea()
#   da.simulate$setSizeRequest(400, 250)
#   
#   # aggregate components
#   vbox.simulate <- gtkVBox()
#   vbox.simulate$add(gtkLabel(""))
#   vbox.simulate$add(hbox.1.simulate)
#   vbox.simulate$packStart(hbox.2.simulate, padding=4)      
#   vbox.simulate$add(da.simulate)
#   vbox.4$add(vbox.simulate)    
#   
#   gset(vbox.4, "da", da.simulate)
#   gset(vbox.4, "spin.runs", spin.runs) 
#   gset(vbox.4, "ent.m", ent.m)
#   gset(vbox.4, "ent.ns", ent.ns)  
#   gset(vbox.4, "ent.prop", ent.prop)    
#   gset(vbox.4, "btn.simulate", btn.simulate)       
#   gset(vbox.4, "btn.redraw", btn.redraw)  
#   
#   gSignalConnect(btn.simulate , "clicked", handler_btn_simulate,
#                  data=list(cw1=cw1, cw2=cw2, cw3=cw3, cw4=vbox.4))                 
#   gSignalConnect(btn.redraw , "clicked", handler_btn_redraw,
#                  data=list(cw1=cw1, cw2=cw2, cw3=cw3, cw4=vbox.4)) 
#   vbox.4    
# }
# 
# 
# handler_btn_simulate <- function(w, data, ...)
# {  
#   probs.no.attributes <- get_probs_from_modifier(data$cw1)
#   no.attributes <- as.integer(names(probs.no.attributes))
#   probs.categories <- get_probs_from_modifier(data$cw2)
#   size <- gget(data$cw3, "spin.sample.size")$getValueAsInt() 
#   runs <- gget(data$cw4, "spin.runs")$getValueAsInt()   
#   ns.text <- gget(data$cw4, "ent.ns")$getText()
#   #TODO ns.text # check if pattern is correct using regex
#   ns <- eval(parse(text=paste("c(", ns.text, ")")))
#   
#   # working horse part: conducting the  simulation
#   res <- sim_n_persons_x_times_many_n(probs.categories, n=ns,
#                                       a=no.attributes, ap=probs.no.attributes,
#                                       times=runs, progress="tk")
#   gset(data$cw4, "simulation.data", res)
#   gget(data$cw4, "btn.redraw")$setSensitive(TRUE)
#   handler_btn_redraw(NULL, data)
#   return(TRUE)
# }
# 
# 
# handler_btn_redraw <- function(w, data, ...)
# {
#   ns.text <- gget(data$cw4, "ent.ns")$getText()
#   #TODO ns.text # check if pattern in not number, comma etc.    
#   ns <- eval(parse(text=paste("c(", ns.text, ")")))
#   m.text <- gget(data$cw4, "ent.m")$getText()
#   #TODO m.text # check if pattern in not number, comma etc.
#   ms <- eval(parse(text=paste("c(", m.text, ")")))
#   prop.text <- gget(data$cw4, "ent.prop")$getText()
#   #TODO prop.text # check if pattern in not number, comma etc. 
#   min.props <- eval(parse(text=paste("c(", prop.text, ")")))
#   # drawing of simulated data   
#   
#   res <- gget(data$cw4, "simulation.data") 
#   if (is.null(res))
#     warning("You need to simulate data first")   # should never actually happen
#   dd <- calc_probabilities(r=res, n=ns, ms=ms, min.props=min.props)
#   dd$m <- as.factor(dd$m) 
#   dd$min.prop.k <- paste0("K=", dd$min.prop)
#   g <- ggplot(dd, aes_string(x="n", y="prob", group="m", shape="m", color="m")) +  
#               geom_line() + 
#               geom_point() +
#               scale_y_continuous("Probability", limits=c(0,1)) + 
#               scale_x_continuous("Sample size N") + 
#               facet_grid(. ~ min.prop.k) +
#               scale_color_discrete(name="M: Min.\ncount") + 
#               scale_shape_discrete(name="M: Min.\ncount")
#   set_cairo_device(data$cw4)
#   print(g)    
#   return(TRUE)
# }
# 
# 
# #### MAIN ####
# 
# 
# #' Start gridsampler GUI
# #' 
# #' @export
# #' @author  Mark Heckmann
# #' @examples \dontrun{
# #' gridsampler()
# #' } 
# #'
# gridsampler <- function()
# {  
#   win <- gtkWindow(show = F)
#   win$setBorderWidth(10)
#   win$setDefaultSize(1100, 700)
#   hbox <- gtkHBox()
#     
#   ### left pane
#   mod.1 <- modifier_cwidget(max=30, xlab="No. of attributes")
#   gget(mod.1, "da")$setSizeRequest(200, 250)
#   preset.1 <- probability_preset(mod.1, xlab="No. of attributes")
#   vbox.1 <- gtkVBox()
#   vbox.1$packStart(mod.1, padding=5)
#   vbox.1$packStart(preset.1, padding=20) 
#   lbl.text <- '<span font_weight = "bold">  1. Number of attributes per person  </span>'
#   lbl.1 <- gtkLabel()
#   lbl.1$setMarkup(lbl.text)
#   frame.left <- gtkFrame("") # 1. Number of attributes per person   
#   frame.left$setLabelWidget(lbl.1)  
#   valign.1 <- gtkAlignment(0,0,1,0)
#   valign.1$add(vbox.1)
#   frame.left$add(valign.1)
#   
#   ### middle pane
#   mod.2 <- modifier_cwidget(max=100, xlab="Category")
#   gget(mod.2, "da")$setSizeRequest(350, 250) 
#   preset.2 <- probability_preset(mod.2, xlab="Category") 
#   gget(mod.2, "spin.min")$setSensitive(F)  # minimal number set to 1
#   vbox.2 <- gtkVBox() 
#   vbox.2$packStart(mod.2, padding=5)
#   vbox.2$packStart(preset.2, padding=20)
#   lbl.text <- '<span font_weight = "bold">  2. Probability of each category  </span>'
#   lbl.2 <- gtkLabel()
#   lbl.2$setMarkup(lbl.text)   
#   frame.middle <- gtkFrame("")
#   frame.middle$setLabelWidget(lbl.2)
#   valign.2 <- gtkAlignment(0,0,1,0)
#   valign.2$add(vbox.2)
#   frame.middle$add(valign.2) 
#   
#   ### right pane
#   vbox.3 <- cw_draw_one_sample(mod.1, mod.2) 
#   vbox.4 <- cw_draw_many_samples(mod.1, mod.2, vbox.3)  
#   vbox.right <- gtkVBox()
#   vbox.right$add(vbox.3) 
#   vbox.right$add(gtkHSeparator())     
#   vbox.right$add(vbox.4) 
#   lbl.text <- '<span font_weight = "bold">  3. Simulate </span>'
#   lbl.3 <- gtkLabel()
#   lbl.3$setMarkup(lbl.text)   
#   frame.right <- gtkFrame("")
#   frame.right$setLabelWidget(lbl.3)
#   frame.right$add(vbox.right)
#   
#   ### aggregate frames in panes
#   hpane.2 <- gtkHPaned()
#   hpane.2$setPosition(300) # position of seperator  
#   hpane.2$pack1(frame.middle, resize=T, shrink=F)
#   hpane.2$pack2(frame.right, resize=T, shrink=F)  
#   hpane.1 <- gtkHPaned()   
#   hpane.1$setPosition(200) # position of seperator
#   hpane.1$pack1(frame.left, resize=T, shrink=F)
#   hpane.1$pack2(hpane.2, resize=T, shrink=F)
#   win$add(hpane.1) 
#   
#   # add information box
#   # info.image <- gtkImageNewFromStock(GTK_STOCK_INFO, "GTK_ICON_SIZE_LARGE_TOOLBAR")
#   # info.btn <- gtkButton()
#   # info.btn$setImage(info.image)
#   # info.btn$setRelief("GTK_RELIEF_NONE")   
#   # info.btn$setSizeRequest(60,60)
#   # info.align <- gtkAlignment(0,1,0,1)
#   # info.align$add(info.btn)
#   # vbox.1$packStart(info.align, padding=0)   
#   
#   # show all
#   win$showAll()
# 
#   # create cairo contexts
#   modifier_da_as_cairo_plus_device_number(mod.1)
#   modifier_da_as_cairo_plus_device_number(mod.2)  
#   modifier_da_as_cairo_plus_device_number(vbox.3)  
#   modifier_da_as_cairo_plus_device_number(vbox.4)  
#   
#   # initial drawing
#   modifier_draw_probs(mod.1, xlab="Number of attributes")
#   modifier_draw_probs(mod.2, xlab="Category")
#   
#   # set initial values
#   gget(mod.1, "spin.min")$setValue(4) 
#   gget(mod.1, "spin.max")$setValue(8) 
#   gget(mod.2, "spin.max")$setValue(20)
#   handler_prob_preset(NULL, data=list(cw=mod.1, cw_preset=preset.1, 
#       preset="normal"))
#   handler_prob_preset(NULL, data=list(cw=mod.2, cw_preset=preset.2, 
#       preset="exponential"))  
# }
 










