assign_levels <-
  # Takes UKB raw data and assigns the right labels to vactors
  function(bd) {
    # Reported sex
    lvl.0009 <- c(0, 1)
    lbl.0009 <- c("Female", "Male")
    bd$f.31.0.0 <- ordered(bd$f.31.0.0, levels = lvl.0009, labels = lbl.0009)

    # Genetic sex
    bd$f.22001.0.0 <- ordered(bd$f.22001.0.0, levels = lvl.0009, labels = lbl.0009)

    # Breastfed or not
    lvl.100349 <- c(-3, -1, 0, 1)
    lbl.100349 <- c("Prefer not to answer", "Do not know", "No", "Yes")
    bd$f.1677.0.0 <- ordered(bd$f.1677.0.0, levels = lvl.100349, labels = lbl.100349)
    bd$f.1677.1.0 <- ordered(bd$f.1677.1.0, levels = lvl.100349, labels = lbl.100349)
    bd$f.1677.2.0 <- ordered(bd$f.1677.2.0, levels = lvl.100349, labels = lbl.100349)

    # Qualifications
    lvl.100305 <- c(-7, -3, 1, 2, 3, 4, 5, 6)
    lbl.100305 <- c("None of the above", "Prefer not to answer", "College or University degree", "A levels/AS levels or equivalent", "O levels/GCSEs or equivalent", "CSEs or equivalent", "NVQ or HND or HNC or equivalent", "Other professional qualifications eg: nursing, teaching")
    bd$f.6138.0.0 <- ordered(bd$f.6138.0.0, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.0.1 <- ordered(bd$f.6138.0.1, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.0.2 <- ordered(bd$f.6138.0.2, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.0.3 <- ordered(bd$f.6138.0.3, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.0.4 <- ordered(bd$f.6138.0.4, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.0.5 <- ordered(bd$f.6138.0.5, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.1.0 <- ordered(bd$f.6138.1.0, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.1.1 <- ordered(bd$f.6138.1.1, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.1.2 <- ordered(bd$f.6138.1.2, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.1.3 <- ordered(bd$f.6138.1.3, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.1.4 <- ordered(bd$f.6138.1.4, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.1.5 <- ordered(bd$f.6138.1.5, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.2.0 <- ordered(bd$f.6138.2.0, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.2.1 <- ordered(bd$f.6138.2.1, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.2.2 <- ordered(bd$f.6138.2.2, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.2.3 <- ordered(bd$f.6138.2.3, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.2.4 <- ordered(bd$f.6138.2.4, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.2.5 <- ordered(bd$f.6138.2.5, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.3.0 <- ordered(bd$f.6138.3.0, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.3.1 <- ordered(bd$f.6138.3.1, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.3.2 <- ordered(bd$f.6138.3.2, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.3.3 <- ordered(bd$f.6138.3.3, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.3.4 <- ordered(bd$f.6138.3.4, levels = lvl.100305, labels = lbl.100305)
    bd$f.6138.3.5 <- ordered(bd$f.6138.3.5, levels = lvl.100305, labels = lbl.100305)

    return(bd)
  }
