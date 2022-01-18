
BASELINE <- c(0.0625, 0.0625, 0.0625, 0.0625, 0.06369426751592357)

log_all_vals <- c(0.3312, 0.3844, 0.4844, 0.4156, 0.2389)
log_not_y <- c(0.3875, 0.3875, 0.5062, 0.4219, 0.3089)
log_not_z <- c(0.1375, 0.2625, 0.2812, 0.2125, 0.1783)
log_not_x <- c(0.2156, 0.2344, 0.3219, 0.2969, 0.2197)
neur_all_vals <- c(0.328125, 0.409375, 0.478125, 0.359375, 0.2898089171974522)
neur_not_y <- c(0.4, 0.375, 0.54375, 0.321875, 0.3375796178343949)
neur_not_z <- c(0.18125, 0.309375, 0.253125, 0.275, 0.16878980891719744)
neur_not_x <- c(0.253125, 0.2875, 0.328125, 0.290625, 0.2070063694267516)

mean(log_all_vals)
var(log_all_vals)
mean(log_all_vals_raw)
var(log_all_vals_raw)
mean(neur_all_vals)
var(neur_all_vals)
mean(neur_all_vals_raw)
var(neur_all_vals_raw)
mean(BASELINE)
var(BASELINE)

t.test(log_all_vals, log_all_vals_raw)
t.test(log_all_vals, neur_all_vals)
t.test(log_all_vals, neur_all_vals_raw)
t.test(log_all_vals, BASELINE)
t.test(log_all_vals_raw, neur_all_vals)
t.test(log_all_vals_raw, neur_all_vals_raw)
t.test(log_all_vals_raw, BASELINE)
t.test(neur_all_vals, neur_all_valls_raw)
t.test(neur_all_vals, BASELINE)
t.test(neur_all_vals_raw, BASELINE)


t.test(neur_all_vals, neur_not_y)
t.test(neur_all_vals, neur_not_z)
t.test(neur_all_vals, neur_not_x)
t.test(neur_all_vals_raw, neur_not_y)
t.test(neur_all_vals_raw, neur_not_z)
t.test(neur_all_vals_raw, neur_not_x)

p_vals = c(0.8509,0.9698,0.5413,1.719e-3,0.8650,0.6464,1.609e-03,0.5365,6.954e-04,3.776e-03,0.6707, 0.01352, 0.0378, 0.7728, 0.03423, 0.06896, 0.8286, 0.00926)

p_vals_adjusted <- p.adjust(c(0.8509,0.9698,0.5413,1.719e-3,0.8650,0.6464,1.609e-03,0.5365,6.954e-04,3.776e-03,0.6707, 0.01352, 0.0378, 0.7728, 0.03423, 0.06896, 0.8286, 0.00926),method="BH")
p_vals_adjusted-p_vals
p_vals<0.05
p_vals_adjusted<0.05

p_vals_adjusted

log_all_vals_raw <- c(0.3312, 0.3781, 0.5406, 0.3656, 0.2962)
log_not_y_raw <- c(0.3781, 0.3812, 0.5531, 0.4312, 0.3185)
log_not_z_raw <- c(0.2219, 0.2312, 0.2188, 0.2281, 0.1497)
log_not_x_raw <- c(0.2219, 0.2312, 0.2188, 0.2281, 0.1497)
neur_all_vals_raw <- c(0.4, 0.45, 0.621875, 0.2875, 0.3248407643312102)
neur_not_y_raw <- c(0.484375, 0.340625, 0.665625, 0.415625, 0.2929936305732484)
neur_not_z_raw <- c(0.2219, 0.2312, 0.2188, 0.2281, 0.1497)
neur_not_x_raw <- c(0.259375, 0.196875, 0.290625, 0.234375, 0.17197452229299362)
t.test(log_all_vals_raw, neur_all_vals_raw)

t.test(log_all_vals, log_all_vals_raw)
t.test(neur_all_vals, neur_all_vals_raw)




log_all_vals_LEFT <- c(0.3188, 0.3938, 0.4688, 0.4188, 0.2739)
neur_all_vals_LEFT <-c(0.365625, 0.378125, 0.525, 0.359375, 0.2929936305732484)
t.test(log_all_vals_LEFT, log_all_vals)
t.test(neur_all_vals_LEFT, neur_all_vals)

log_all_vals_raw_LEFT <- c(0.1656, 0.1812, 0.2062, 0.1781, 0.1465)
neur_all_vals_raw_LEFT <- c(0.1625, 0.190625, 0.234375, 0.078125, 0.1624203821656051)
t.test(log_all_vals_raw_LEFT, log_all_vals_LEFT)
t.test(neur_all_vals_raw_LEFT, neur_all_vals_LEFT)


t.test(neur_all_vals, neur_all_vals_LEFT)
t.test(neur_all_vals_raw, neur_all_vals_raw_LEFT)

mean(neur_all_vals)
var(neur_all_vals)
