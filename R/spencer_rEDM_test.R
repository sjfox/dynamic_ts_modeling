library(rEDM)
data(tentmap_del)
head(tentmap_del)

lib <- c(1, 100) 
pred <- c(201, 500)

ts <- tentmap_del
simplex_output <- simplex(ts, lib, pred)

par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)",ylab = "Forecast Skill (rho)")


simplex_output <- simplex(ts, lib, pred, E = 2, tp = 1:10)

par(mar = c(4, 4, 1, 1))
plot(simplex_output$tp, simplex_output$rho, type = "l", xlab = "Time to Prediction (tp)",
     ylab = "Forecast Skill (rho)")

smap_output <- s_map(ts, lib, pred, E = 2)
 
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(smap_output$theta, smap_output$rho, type = "l", xlab = "Nonlinearity (theta)",
     ylab = "Forecast Skill (rho)")


data(block_3sp) 
head(block_3sp)
lib <- c(1, NROW(block_3sp)) 
pred <- c(1, NROW(block_3sp))
block_lnlp_output <- block_lnlp(block_3sp, 
                                lib = lib, 
                                pred = pred, 
                                columns = c(1, 2, 4), 
                                target_column = 1, 
                                stats_only = FALSE, 
                                first_column_time = TRUE)

block_lnlp_output <- block_lnlp(block_3sp,tp = 1,
                                lib = lib, 
                                pred = pred, 
                                columns = c("x_t", "x_t-1", "y_t"), 
                                target_column = "x_t", 
                                stats_only = FALSE, 
                                first_column_time = TRUE)

observed <- block_lnlp_output[[1]]$model_output$obs
predicted <- block_lnlp_output[[1]]$model_output$pred

par(mar = c(4, 4, 1, 1), pty = "s")
plot_range <- range(c(observed, predicted), na.rm = TRUE)
plot(observed, predicted, xlim = plot_range, ylim = plot_range, xlab = "Observed",
     ylab = "Predicted")
abline(a = 0, b = 1, lty = 2, col = "blue")


## Try with flu data
library(cdcfluview)
library(tidyverse)
ili <- get_flu_data(region = "national", data_source = "ilinet", years = 1997:2017)
who <- get_flu_data(region = "national", data_source = "who", years= 1997:2017)
who <- who[[1]]
ili <- ili[1:nrow(who),]

flu_ts <- as.numeric(ili$`% WEIGHTED ILI`) #* as.numeric(who$`PERCENT POSITIVE`)
flu_ts <- flu_ts[which(ili$YEAR == 2002 & ili$WEEK == 40):length(flu_ts)]
lib<-c(1, 400)
pred<-c(401, length(flu_ts))
flu_simplex <- simplex(flu_ts, lib, pred, tau = 10)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(flu_simplex$E, flu_simplex$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")

flu_simplex <- simplex(flu_ts, lib, pred, E = 3, tau = 10, tp = 4, stats_only = F)
plot(flu_simplex[[1]]$model_output$pred, type="l")
lines(x = flu_ts, col="red")
plot(flu_simplex[[1]]$model_output$obs, flu_simplex[[1]]$model_output$pred)



flu_df <- data.frame(time=seq(from=1, to=length(flu_ts)-1), 
                         xt=diff(flu_ts),
                         xtm1=c(NA, diff(flu_ts[-length(flu_ts)])),
                         xtm2=c(NA, NA, diff(flu_ts[-((length(flu_ts)-1):length(flu_ts))])))

# flu_df <- data.frame(time=seq(from=1, to=length(flu_ts)), 
#                      xt=flu_ts,
#                      xtm1=c(NA, flu_ts[-length(flu_ts)]),
#                      xtm2=c(NA, NA, flu_ts[-((length(flu_ts)-1):length(flu_ts))]))

lib <- c(1, 500)
pred <- c(501, 678)
block_lnlp_output <- block_lnlp(flu_df, lib = lib, pred = pred, tp = 3, target_column = 1,
                                  first_column_time = TRUE, stats_only = FALSE)

block_lnlp_output[[1]]$stats

observed <- block_lnlp_output[[1]]$model_output$obs
predicted <- block_lnlp_output[[1]]$model_output$pred

par(mar = c(4, 4, 1, 1), pty = "s")
plot_range <- range(c(observed, predicted), na.rm = TRUE)
plot(observed, predicted, xlim = plot_range, ylim = plot_range, xlab = "Observed", 
     ylab = "Predicted")
abline(a = 0, b = 1, lty = 2, col = "blue")

plot(predicted, type = "l")
plot(flu_df$xt, type="l", col="red")


####################################################
## Simulated data test
####################################################
sim_data <- read_csv("produced_data/sirs_sim_100yrs.csv")
sim_data <- as.numeric(unlist(sim_data))
lib<-c(1, 2000)
pred<-c(2001, length(sim_data))
flu_simplex <- simplex(sim_data, lib, pred, E = 1:10)
par(mar = c(4, 4, 1, 1), mgp = c(2.5, 1, 0))
plot(flu_simplex$E, flu_simplex$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")

flu_simplex <- simplex(sim_data, lib, pred, E = 5, tp = 1:20)
par(mar = c(4, 4, 1, 1))
plot(flu_simplex$tp, flu_simplex$rho, type = "l", xlab = "Time to Prediction (tp)", 
     ylab = "Forecast Skill (rho)")

flu_simplex <- simplex(sim_data, lib, pred, E = 5, tp = 4, stats_only = F)
plot(flu_simplex[[1]]$model_output$pred,flu_simplex[[1]]$model_output$obs)

