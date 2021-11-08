# Libraries ---------------------------------------------------------------
library(sizeMat)
library(ggpubr)
library(patchwork)



# 1) Size at morphimetric maturity ----------------------------------------


# 1.1. Data to estimate size at morphometric maturity
data(crabdata)

# 1.2. Classify technique
classify <- classify_mature(crabdata, varNames = c("carapace_width", "chela_height"), 
                            varSex = "sex_category", selectSex = NULL, method = "ld")

data <- classify
juv  <- data[data$mature == 0, ]
adt  <- data[data$mature == 1, ]
fit_juv <- glm(y ~ x, data = juv)
fit_adt <- glm(y ~ x, data = adt)
eq_juv <- paste0("Juveniles: Y = ", 
                 round(as.numeric(coef(fit_juv)[1]), 2), " + ", 
                 round(as.numeric(coef(fit_juv)[2]), 2), " *X", 
                 sep = "")
eq_adt <- paste0("Adults: Y = ", 
                 round(as.numeric(coef(fit_adt)[1]), 2), " + ", 
                 round(as.numeric(coef(fit_adt)[2]), 2), " *X", 
                 sep = "")


# 1.3. Plot classification 
classif <- data.frame(y = data$y, 
                      x = data$x,
                      group = data$mature)

p <- ggplot(classif,
            aes(x, y, group = group, color = factor(group)))+
  geom_point(size = 3)+
  geom_smooth(method = "lm", se = FALSE)+
  theme_bw() +
  labs(x = "Carapace width (mm)", y = "Chela height (mmm)") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(40, 170, 10)) +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95", colour = "grey95")) +
  theme(legend.position = c(0.23, 0.9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12), 
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  scale_colour_manual(values = c('red','blue'), 
                      labels = c(eq_juv, eq_adt))



# 1.4. Estimation and plot
my_ogive_fq <- morph_mature(classify, method = "fq", niter = 1000)

ogive <- data.frame(x        = my_ogive_fq$out$x, 
                    y        = my_ogive_fq$out$fitted,
                    ci_lower = my_ogive_fq$out$CIlower,
                    ci_upper = my_ogive_fq$out$CIupper)

m <- ggplot(ogive,
            aes(x))+
  geom_line(aes(y = y), color = "steelblue", size = 3) + 
  geom_line(aes(y = ci_lower), color="steelblue", linetype = "dashed", size = 2) +
  geom_line(aes(y = ci_upper), color="steelblue", linetype = "dashed", size = 2) +
  geom_label(label=expression(L[50]~ "= 118.7"),
             x = 50,
             y = 0.95,
             label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.35,
             color = "black",
             fill = "#69b3a2")+
  geom_segment(aes(x = 118.7, y = 0, xend = 118.7, yend = 0.5), size = 1.5, col = "red", linetype = "dashed")+
  geom_segment(aes(x = 40, y = 0.5, xend = 118.7, yend = 0.5), size = 1.5, col = "red", linetype = "dashed")+
  geom_point(x = 118.17, y = 0.5, size = 2.5, col = "red")+
  theme_bw() +
  labs(x = "Carapace width (mm)", y = "Proportion mature") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(40, 170, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95", colour = "grey95")) +
  theme(legend.position = c(0.23, 0.9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12), 
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))





# 2) Size at gonadic maturity (size at sexual maturity) -------------------

# 2.1. Data to estimate size at gonadic maturity
data(matFish)


# 2.2. Estimation and plot
my_ogive_fq <- gonad_mature(matFish, varNames = c("total_length", "stage_mat"), inmName = "I",
                            matName = c("II", "III", "IV" ), method = "fq", niter = 999)

ogive <- data.frame(x        = my_ogive_fq$out$x, 
                    y        = my_ogive_fq$out$fitted,
                    ci_lower = my_ogive_fq$out$CIlower,
                    ci_upper = my_ogive_fq$out$CIupper)


n <- ggplot(ogive,
            aes(x))+
  geom_line(aes(y = y), color = "steelblue") + 
  geom_line(aes(y = ci_lower), color="steelblue", linetype = "dashed") +
  geom_line(aes(y = ci_upper), color="steelblue", linetype = "dashed") +
  geom_label(label=expression(L[50]~ "= 24.2"),
             x=55,
             y=0.1,
             label.padding = unit(0.55, "lines"), # Rectangle size around label
             label.size = 0.35,
             color = "black",
             fill = "#69b3a2")+
  geom_segment(aes(x = 24.2, y = 0, xend = 24.2, yend = 0.5), size = 1.5, col = "red", linetype = "dashed")+
  geom_segment(aes(x = 10, y = 0.5, xend = 24.2, yend = 0.5), size = 1.5, col = "red", linetype = "dashed")+
  geom_point(x = 24.2, y = 0.5, size = 2.5, col = "red")+
  theme_bw() +
  labs(x = "Total length (cm)", y = "Proportion mature") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15)) +
  scale_x_continuous(breaks = seq(10, 70, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95", colour = "grey95")) +
  theme(legend.position = c(0.23, 0.9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12), 
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))


# 2.3 Some statistics data
statistics <- data.frame(A   = my_ogive_fq$A_boot, 
                         B   = my_ogive_fq$B_boot,
                         L50 = my_ogive_fq$L50_boot)

alpha <- ggplot(statistics,
                aes(A))+
  geom_histogram(color="darkblue", fill="lightblue")+
  geom_vline(data=statistics, aes(xintercept = mean(A)), colour="red", size = 1.25)+
  geom_vline(data=statistics, aes(xintercept = quantile(A, prob = 0.025)), colour="red", size = 1.25, linetype = "dashed")+
  geom_vline(data=statistics, aes(xintercept = quantile(A, prob = 0.975)), colour="red", size = 1.25, linetype = "dashed")+
  theme_bw() +
  labs(x = expression(alpha), y = "Frequency") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95", colour = "grey95")) +
  theme(legend.position = c(0.23, 0.9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12), 
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))


beta <- ggplot(statistics,
                aes(B))+
  geom_histogram(color="darkblue", fill="lightblue")+
  geom_vline(data=statistics, aes(xintercept = mean(B)), colour="red", size = 1.25)+
  geom_vline(data=statistics, aes(xintercept = quantile(B, prob = 0.025)), colour="red", size = 1.25, linetype = "dashed")+
  geom_vline(data=statistics, aes(xintercept = quantile(B, prob = 0.975)), colour="red", size = 1.25, linetype = "dashed")+
  theme_bw() +
  labs(x = expression(beta), y = "Frequency") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95", colour = "grey95")) +
  theme(legend.position = c(0.23, 0.9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12), 
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))


L50 <- ggplot(statistics,
               aes(L50))+
  geom_histogram(color="darkblue", fill="lightblue")+
  geom_vline(data=statistics, aes(xintercept = mean(L50)), colour="red", size = 1.25)+
  geom_vline(data=statistics, aes(xintercept = quantile(L50, prob = 0.025)), colour="red", size = 1.25, linetype = "dashed")+
  geom_vline(data=statistics, aes(xintercept = quantile(L50, prob = 0.975)), colour="red", size = 1.25, linetype = "dashed")+
  theme_bw() +
  labs(x = expression(L[50]), y = "Frequency") +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 15)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "grey95", colour = "grey95")) +
  theme(legend.position = c(0.23, 0.9), 
        legend.title = element_blank(),
        legend.text = element_text(size = 12), 
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent"))



# 3) Final plots ----------------------------------------------------------

(p | m) / (alpha | beta | L50 | n)
