#' Loads packages and installs those that are not in the library
#' @param  vector of package names
#' @export

using<-function(...) {
    libs<-unlist(list(...))
    req<-unlist(lapply(libs,require,character.only=TRUE))
    need<-libs[req==FALSE]
    if(length(need)>0){ 
        install.packages(need)
        lapply(need,require,character.only=TRUE)
    }
}


# load/install packages
  packages = c('anytime','arm','data.table', 'effects', 'foreach', 'ggplot2', 'ggthemes', 'glue',  'grid', 'here', 'htmlTable', 'lattice', 'lubridate', 'magrittr', 'multcomp', 'plyr','raster','stringr','xlsx','zoo')
  sapply(packages, function(x) suppressPackageStartupMessages(using(x)) )

# Customized ggplot theme
    theme_MB = theme(  
              axis.line=element_blank(),
              #axis.line = element_line(colour="grey70", size=0.25),
              axis.title = element_text(size=7, colour="grey30"),
              axis.title.y = element_text(vjust=1),
              axis.title.x = element_text(vjust=1),
              axis.text=element_text(size=6),#, vjust = 0.5, hjust=1),# margin=units(0.5,"mm")),
              axis.ticks.length=unit(0.5,"mm"),
              #axis.ticks.margin,
              
              strip.text.x = element_text(size = 6, color="grey30",  margin=margin(1,1,1,1,"mm")), #grey50
              strip.background = element_rect(fill="grey99",colour="grey70", size=0.25),
                #strip.background = element_blank(), 
                #strip.text = element_blank(),
              panel.spacing = unit(0, "mm"),
              panel.background=element_blank(),
                #panel.border=element_blank(),
              panel.border = element_rect(colour="grey70", size=0.25, fill = NA),
              panel.grid = element_blank(),
              legend.text=element_text(size=6),
              legend.title=element_text(size=7)
              )


# model output function
  m_out = function(name = "define", model = m, round_ = 3, nsim = 5000, aic = TRUE, N = NA){
    bsim <- sim(model, n.sim=nsim)  
     v = apply(bsim@fixef, 2, quantile, prob=c(0.5))
     ci = apply(bsim@fixef, 2, quantile, prob=c(0.025,0.975))   
     oi=data.frame(model=name,type='fixed',effect=rownames(coef(summary(model))),estimate=v, lwr=ci[1,], upr=ci[2,])
            rownames(oi) = NULL
            oi$estimate_r=round(oi$estimate,round_)
            oi$lwr_r=round(oi$lwr,round_)
            oi$upr_r=round(oi$upr,round_)
    oii=oi[c('model','type',"effect", "estimate_r","lwr_r",'upr_r')]    
    
     l=data.frame(summary(model)$varcor)
     l = l[is.na(l$var2),]
     l$var1 = ifelse(is.na(l$var1),"",l$var1)
     l$pred = paste(l$grp,l$var1)
    ri=data.frame(model=name,type='random %',effect=l$pred, estimate_r=round(100*l$vcov/sum(l$vcov)), lwr_r=NA, upr_r=NA)
    ri$estimate_r = paste(ri$estimate_r,'%',sep='')
    x = rbind(oii,ri)
    x$N = NA
    x$N[1] = N
    if (aic == TRUE){   
        x$AIC = NA
        x$AIC[1]=AIC(update(model,REML = FALSE))
        x$delta = NA
        x$prob = NA
        x$ER = NA
        }
    return(x)
  } 
# model assumption function
  m_ass = function(name = 'define', mo = m0, dat = d, fixed = NULL, categ = NULL, trans = NULL, spatial = TRUE, temporal = TRUE, PNG = TRUE, outdir = 'outdir'){
   l=data.frame(summary(mo)$varcor)
   l = l[is.na(l$var2),]
   if(PNG == TRUE){
    png(paste(outdir,name, ".png", sep=""), width=6,height=9,units="in",res=600)
     }else{dev.new(width=6,height=9)}
   
   n = nrow(l)-1+length(fixed)+length(categ) + 4 + if(temporal==TRUE){1}else{0} + if(spatial==TRUE){1}else{0} 
   par(mfrow=c(ceiling(n/3),3))
   
   scatter.smooth(fitted(mo),resid(mo),col='grey');abline(h=0, lty=2, col ='red')
   scatter.smooth(fitted(mo),sqrt(abs(resid(mo))), col='grey')
   qqnorm(resid(mo), main=list("Normal Q-Q Plot: residuals", cex=0.8),col='grey');qqline(resid(mo))
   #unique(l$grp[l$grp!="Residual"])
   for(i in unique(l$grp[l$grp!="Residual"])){
    #i = "mean_year"
    ll=ranef(mo)[names(ranef(mo))==i][[1]]
    if(ncol(ll)==1){
     qqnorm(ll[,1], main = paste(i,names(ll)[1]),col='grey');qqline(ll[,1], col ='red')
     }else{
      qqnorm(ll[,1], main = paste(i,names(ll)[1]),col='grey');qqline(ll[,1], col ='red')
      qqnorm(ll[,2], main = paste(i,names(ll)[2]),col='grey');qqline(ll[,2], col ='red')
     }
    }
    
   # variables
   scatter={} 
   for (i in rownames(summary(mo)$coef)) {
        #i = "lat_abs"
      j=sub("\\).*", "", sub(".*\\(", "",i)) 
      scatter[length(scatter)+1]=j
    }
    x = data.frame(scatter=unique(scatter)[2:length(unique(scatter))],
                    log_ = grepl("log",rownames(summary(mo)$coef)[2:length(unique(scatter))]), stringsAsFactors = FALSE)
    for (i in 1:length(fixed)){
        jj =fixed[i]
        variable=dat[, ..jj][[1]]
        if(trans[i]=='log'){
        scatter.smooth(resid(mo)~log(variable),xlab=paste('log(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
        }else if(trans[i]=='abs'){
        scatter.smooth(resid(mo)~abs(variable),xlab=paste('abs(',jj,')',sep=''), col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
        }else{
        scatter.smooth(resid(mo)~variable,xlab=jj,col = 'grey');abline(h=0, lwd=1, lty = 2, col ='red')
      }
     }
    
    if(length(categ)>0){
      for(i in categ){
         variable=dat[, ..i][[1]]
          boxplot(resid(mo)~variable, medcol='grey', whiskcol='grey', staplecol='grey', boxcol='grey', outcol='grey');abline(h=0, lty=3, lwd=1, col = 'red')
         }
    }     
          
    if(temporal == TRUE){
        acf(resid(mo), type="p", main=list("Temporal autocorrelation:\npartial series residual",cex=0.8))
        }
    if(spatial == TRUE){    
    spdata=data.frame(resid=resid(mo), x=dat$Longitude, y=dat$Latitude)
        spdata$col=ifelse(spdata$resid<0,rgb(83,95,124,100, maxColorValue = 255),ifelse(spdata$resid>0,rgb(253,184,19,100, maxColorValue = 255), 'red'))
        #cex_=c(1,2,3,3.5,4)
        cex_=c(1,1.5,2,2.5,3)
        spdata$cex=as.character(cut(abs(spdata$resid), 5, labels=cex_))
      plot(spdata$x, spdata$y,col=spdata$col, cex=as.numeric(spdata$cex), pch= 16, main=list('Spatial distribution of residuals', cex=0.8))
        legend("topleft", pch=16, legend=c('>0','<0'), ,col=c(rgb(83,95,124,100, maxColorValue = 255),rgb(253,184,19,100, maxColorValue = 255)), cex=0.8)
      plot(spdata$x[spdata$resid<0], spdata$y[spdata$resid<0],col=spdata$col[spdata$resid<0], cex=as.numeric(spdata$cex[spdata$resid<0]), pch= 16, main=list('Spatial distribution of residuals (<0)', cex=0.8))
      plot(spdata$x[spdata$resid>=0], spdata$y[spdata$resid>=0],col=spdata$col[spdata$resid>=0], cex=as.numeric(spdata$cex[spdata$resid>=0]), pch= 16, main=list('Spatial distribution of residuals (>=0)', cex=0.8))
        }
   
   mtext(paste(slot(mo,"call")[1],'(',slot(mo,"call")[2],sep=''), side = 3, line = -1, cex=0.7,outer = TRUE)
  if(PNG==TRUE){dev.off()}
  }
    

