# BOOTSTRAPPING INCREMENTAL REVENUE CIs
# The purpose of this code is to extract confidence interval bands for incremental revenue derived from a marketing campaign 
# where a control group cannot be held out and a artificial control group was formed.
# The input dataset contains a list of enrolled and control customers with incremental spend metrics for each 30 day increments and a unique id connecting
# the enrolled and control customer

rm(list=ls())
getwd()
setwd('C:/Users/avenkata/Desktop/')

# READING THE DATA SET IN WIDE FORM
# for total spend
ts_rev= read.csv('penfed_total.csv', header=TRUE, na.strings='.')

# str(ts_rev)
ts_rev$control_flag= factor(ts_rev$control_flag)
levels(ts_rev$control_flag)= c('Enrollee','Twin')
table(ts_rev$control_flag)
dim(ts_rev)
# head(ts_rev)
# tail(ts_rev)

# INCREMENTAL REVENUE CALCULATOR
n_enrollees= dim(ts_rev)[1]/2

# REORDERING OBSERVATIONS TO EACH ID/SUBJECT'S MEASURES/OCCASIONS ARE GROUPED TOGETHER
ts_rev= ts_rev[order(ts_rev$control_flag, ts_rev$twin_pair), ]
ts_rev= ts_rev[, -c(1:2,12)]
ts_rev= ts_rev[c('spend_pre090','spend_pre060','spend_pre030','spend_post030','spend_post060','spend_post090','spend_post120','spend_post150','spend_post180')]


# CALCULATING OBSERVED INCREMENTAL REVENUE
enrollee_total_spend=  apply(ts_rev[1:n_enrollees,], 2, sum, na.rm=TRUE)  
# enrollee_total_active= apply((ts_rev[1:n_enrollees,]>0), 2, sum, na.rm=TRUE)          
twin_total_spend=      apply(ts_rev[(n_enrollees+1):(2*n_enrollees),], 2, sum, na.rm=TRUE)
# twin_total_active=     apply((ts_rev[(n_enrollees+1):(2*n_enrollees),]>0), 2, sum, na.rm=TRUE)
inc_rev= (enrollee_total_spend - twin_total_spend)
inc_rev_enrollee_obs= inc_rev/n_enrollees

# MERSENE-TWISTER - Random number generator
RNGkind(kind="Mersenne")
set.seed(1971)
B=10000
#B= 100
B_inc_rev= matrix(0, nrow=B, ncol=dim(ts_rev)[2])

t_beg= Sys.time()
for (i in 1:B) {
   R_I= round(runif(n_enrollees, 1, n_enrollees), 0) # BOOTSTRAP ROW INDICES
   B_ts_rev= ts_rev[c(R_I,(R_I+n_enrollees)),]       # BOOTSTRAP DATA MATRIX   

   enrollee_total_spend=  apply(B_ts_rev[1:n_enrollees,], 2, sum, na.rm=TRUE)  
   # enrollee_total_active= apply((B_ts_rev[1:n_enrollees,]>0), 2, sum, na.rm=TRUE)          
   twin_total_spend=      apply(B_ts_rev[(n_enrollees+1):(2*n_enrollees),], 2, sum, na.rm=TRUE)
   # twin_total_active=     apply((B_ts_rev[(n_enrollees+1):(2*n_enrollees),]>0), 2, sum, na.rm=TRUE)

   inc_rev= (enrollee_total_spend - twin_total_spend)
   inc_rev_enrollee= inc_rev/n_enrollees

   B_inc_rev[i,]= inc_rev_enrollee
}
t_end= Sys.time()
t_iter= as.numeric((t_end - t_beg)/B)
cat('Number of seconds per iteration is:', t_iter, '\n')

colnames(B_inc_rev)= 
   c('spend_pre90','spend_pre060','spend_pre030','spend_post030','spend_post060','spend_post090','spend_post120','spend_post150','spend_post180')

write.table(B_inc_rev, "B_inc_rev.txt", row.names=FALSE, col.names=TRUE)

CIs <- function(x, lower=0.025, upper=0.975) {
   CI_bounds= as.numeric(quantile(x, c(lower, upper) ))
   return(CI_bounds)
} # END FUNCTION CIs

CIs_95= apply(B_inc_rev, 2, CIs)
CIs_95= matrix(c(t(CIs_95), inc_rev_enrollee_obs), nrow=3, byrow=T)
colnames(CIs_95)= 
   c('spend_pre90','spend_pre060','spend_pre030','spend_post030','spend_post060','spend_post090','spend_post120','spend_post150','spend_post180')
rownames(CIs_95)= c('95% CI: Lower','95% CI: Upper','Average Value')
write.table(CIs_95, "CIs_95.txt", row.names=TRUE, col.names=TRUE)
#CIs_95= read.table('CIs_95.txt', header=TRUE, na.strings='.')

# INCREMENTAL REVENUE PLOT
windows(9,6)
par(xaxs='i', yaxs='i')
time_points= seq(from=1, to=9, by=1)
plot(time_points, CIs_95[3,1:9], type='o', lwd=4, col='blue',
     ylim= c(-10, 40),
     xlab='Days Since Enrollment', ylab='Incremental Total Spend Per Enrollee($)',
     axes=FALSE)

polygon(c(time_points, time_points[length(time_points):1]), 
        c(CIs_95[1,1:9], CIs_95[2,9:1]), col='lightgray', border=NA)

axis(1, at=time_points, labels=c('-90','-60','-30','30','60','90','120','150','180') )
axis(2, seq(from=-10, to=40, by=10))
box()

grid(nx=8, ny=5, col='darkgray')
# abline(v=3.5, lwd=2, col='black')
abline(h=0, lwd=2, col='black')
lines(time_points, CIs_95[3,1:9], type='o', lwd=3, col='blue')
lines(time_points, CIs_95[1,1:9], col='darkgray')
lines(time_points, CIs_95[2,1:9], col='darkgray')

legend('topleft', c('Avg. Inc. Total Spend','95% Confidence Interval'), 
       cex=1, lty=c(1,0), pch=c(20,15), 
       col=c('blue','white'), bty='n')
