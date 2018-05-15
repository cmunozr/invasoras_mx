evaluation <- function(spmo_path, occ_path, type = NA,
                       pattern.models = NA,
                       pattern.occ = NA,
                       pattern.thresholds = "other.csv",
                       write = T, path.write) {
  name_model <- sub(
    pattern = "(.*)\\..*$", replacement = "\\1",
    basename(spmo_path)
  )

  models <- list.files(spmo_path,
    pattern = pattern.models,
    full.names = T
  )

  thresholds <- list.files(spmo_path,
    pattern = pattern.thresholds,
    full.names = T
  )
  if (type == "proy") {
    occ <- list.files(occ_path, pattern = pattern.occ, full.names = T)
    occ <- read.csv(occ)[, c("longitude", "latitude")]
    proc <- matrix(nrow = length(models), ncol = 3)
    for (i in 1:length(models)) {
      proc[i, ] <- PartialROC(
        ModelFile = models[i], Occur = occ,
        OmissionVal = 1 - 0.1, RandomPercent = 50, NoOfIteration = 500
      )
    }
    proc <- data.frame(proc)
    names(proc) <- c("ModelFile", "pRoc", "p_valor")

    bin <- data.frame(matrix(nrow = length(models), ncol = 8))
    for (i in 1:length(models)) {
      bin[i, ] <- Eval_bin(
        path.thresholds = thresholds[i], Test_Occur = occ,
        Model = models[i]
      )
    }
    names(bin) <- c(
      "MTP_area", "MTP_bin", "MTP_OR", "MTP",
      "TTP_area", "TTP_bin", "TTP_OR", "TTP"
    )
    results <- cbind(proc, bin)
  }

  if (type == "cal") {
    occ <- list.files(occ_path, pattern = pattern.occ, full.names = T)
    occ <- read.csv(occ)[, c("longitude", "latitude", "bin")]
    proc_1 <- matrix(nrow = length(models), ncol = 3)
    for (i in 1:length(models)) {
      proc_1[i, ] <- PartialROC(
        ModelFile = models[i], Occur = occ[which(occ$bin == 1), c("longitude", "latitude") ],
        OmissionVal = 1 - 0.1, RandomPercent = 50, NoOfIteration = 500
      )
    }
    proc_2 <- matrix(nrow = length(models), ncol = 3)
    for (i in 1:length(models)) {
      proc_2[i, ] <- PartialROC(
        ModelFile = models[i], Occur = occ[which(occ$bin == 2), c("longitude", "latitude") ],
        OmissionVal = 1 - 0.1, RandomPercent = 50, NoOfIteration = 500
      )
    }
    options(digits = 6)
    proc_array <- array(c(
      as.numeric(proc_1[, 2:3]),
      as.numeric(proc_2[, 2:3])
    ),
    dim = c(30, 2, 2)
    )
    proc_mean <- apply(proc_array, 1:2, mean)

    bin_1 <- data.frame(matrix(nrow = length(models), ncol = 8))
    for (i in 1:length(models)) {
      bin_1[i, ] <- Eval_bin(
        path.thresholds = thresholds[i],
        Test_Occur = occ[which(occ$bin == 1), c("longitude", "latitude") ],
        Model = models[i]
      )
    }
    bin_2 <- data.frame(matrix(nrow = length(models), ncol = 8))
    for (i in 1:length(models)) {
      bin_2[i, ] <- Eval_bin(
        path.thresholds = thresholds[i],
        Test_Occur = occ[which(occ$bin == 2), c("longitude", "latitude") ],
        Model = models[i]
      )
    }
    bin_array <- array(c(as.matrix(bin_1), as.matrix(bin_2)),
      dim = c(30, 8, 2)
    )
    bin_mean <- data.frame(apply(bin_array, 1:2, mean))
    results <- cbind(proc_1[, 1], proc_mean, bin_mean)
    names(results) <- c(
      "ModelFile", "pROC_mean", "p_valor_mean",
      "MTP_area", "MTP_bin", "MTP_OR", "MTP",
      "TTP_area", "TTP_bin", "TTP_OR", "TTP"
    )
  }


  if (write == TRUE) {
    write.csv(results, paste0(path.write, name_model, "_", type, "_eval", ".csv"),
      row.names = F
    )
    return("ok")
  } else {
    return(results)
  }
  rm(list = ls())
  gc()
}