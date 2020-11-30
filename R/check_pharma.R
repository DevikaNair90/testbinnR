


check_pharma <- function(word_vec, mode) {
  pharma_terms <- stringr::str_to_lower(c("FDCA", "HIPAA", "USPTO", "FDASIA", "ANDA",  "anda", "Anda", "NDA", "pharmacokinetic", "bioequivalen(t|ce|cy)",  "biosimilar", "biologic", "biopharmaceutical", "biopharma", "therapeutic", "pharmaceutical", "science", "bio", "bioscience", "investigational", "Pharma", "prescriber", "preclinical", "formulary", "formularies", "rheumatology", "homozygous", "infective", "adjunctive", "intranasal", "chewable", "unobservable", "injectable", "pharmacodynamic", "recombinant", "vitro", "intraocular", "comparator", "recoverability", "macular", "Materiality", "immunotherapy", "mRNA", "multidrug", "dystrophy", "colorectal", "antisense", "fractionation", "biomarker", "parenteral", "neurodegenerative", "ulcerative", "antipsychotic", "neuropathic", "gastroenterology", "liposomal", "neuromuscular", "intra", "bioavailability", "monotherapy", "ligand", "oculus", "submental", "cytotoxic", "chaperone", "benchmarking", "responder", "intracellular", "neuroscience", "radiological", "subpopulation", "tumours", "excipient", "titration", "genotype", "genomic", "ophthalmic", "genericized?", "CLIA"))
  pharma_manual <- c("flml", "pharmacovigilance", "pharm")
  pharma_terms <- c(stringr::str_to_lower(pharma_terms), stringr::str_to_lower(pharma_manual))
  pharm_patt <- paste0(paste0("\\b", pharma_terms, "s?\\b"), collapse = "|")

  if(mode == "TF") {
    pharma_TF <- stringr::str_detect(word_vec, pharm_patt)
  }

  if(mode == "extract") {
    pharma_TF <- stringr::str_extract_all(word_vec, pharm_patt)
  }

  pharma_TF

}


