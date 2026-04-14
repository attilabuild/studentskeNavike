podaci <- read.csv(
  "student_navike.csv",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8-BOM"
)

cat("----- Struktura podataka -----\n")
str(podaci)

cat("\n----- Prvih 6 redova -----\n")
print(head(podaci))

cat("\n----- Dimenzije: ", nrow(podaci), "redova x", ncol(podaci), "kolona -----\n")

numericka_polja <- c("sati_ucenja", "sati_sna", "prosecna_ocena", "nivo_stresa")

cat("\n----- Deskriptivne statistike -----\n")
statistike <- data.frame(
  varijabla = numericka_polja,
  sredina   = sapply(numericka_polja, function(p) mean(podaci[[p]],   na.rm = TRUE)),
  medijana  = sapply(numericka_polja, function(p) median(podaci[[p]], na.rm = TRUE)),
  minimum   = sapply(numericka_polja, function(p) min(podaci[[p]],    na.rm = TRUE)),
  maksimum  = sapply(numericka_polja, function(p) max(podaci[[p]],    na.rm = TRUE)),
  sd        = sapply(numericka_polja, function(p) sd(podaci[[p]],     na.rm = TRUE))
)
print(statistike, row.names = FALSE)

cat("\n----- summary() pregled -----\n")
print(summary(podaci[, numericka_polja]))


hist(
  podaci$sati_ucenja,
  main = "Raspodela sati učenja dnevno",
  xlab = "Sati učenja",
  ylab = "Broj studenata",
  col  = "7dd3fc",
  border = "white",
  breaks = 10
)

broj_po_smeru <- table(podaci$smer)
barplot(
  broj_po_smeru,
  main = "Broj studenata po smeru",
  xlab = "Smer",
  ylab = "Broj studenata",
  col  = "a78bfa",
  border = "white"
)

boxplot(
  prosecna_ocena ~ smer,
  data = podaci,
  main = "Raspodela prosečne ocene po smeru",
  xlab = "Smer",
  ylab = "Prosečna ocena",
  col  = "86efac",
  border = "0d0f12"
)

plot(
  podaci$sati_ucenja, podaci$prosecna_ocena,
  main = "Odnos sati učenja i prosečne ocene",
  xlab = "Sati učenja dnevno",
  ylab = "Prosečna ocena",
  pch  = 19,
  col  = "fca5a5"
)
abline(lm(prosecna_ocena ~ sati_ucenja, data = podaci), col = "7dd3fc", lwd = 2)


najbolji <- podaci[podaci$prosecna_ocena >= 8, ]
cat("\n----- Studenti sa ocenom >= 8 -----\n")
cat("Broj:", nrow(najbolji), "\n")
print(najbolji)

pod_stresom <- podaci[podaci$nivo_stresa >= 4, ]
cat("\n----- Studenti sa stresom >= 4 -----\n")
cat("Broj:", nrow(pod_stresom), "\n")
print(pod_stresom)


cat("\n----- Prosečna ocena po smeru -----\n")
prosek_po_smeru <- aggregate(
  prosecna_ocena ~ smer,
  data = podaci,
  FUN  = mean
)
print(prosek_po_smeru, row.names = FALSE)

cat("\n----- Prosečni sati učenja po smeru -----\n")
print(aggregate(sati_ucenja ~ smer, data = podaci, FUN = mean), row.names = FALSE)

cat("\n----- Prosečni nivo stresa po polu -----\n")
print(aggregate(nivo_stresa ~ pol, data = podaci, FUN = mean), row.names = FALSE)

cat("\n----- Analiza završena -----\n")
