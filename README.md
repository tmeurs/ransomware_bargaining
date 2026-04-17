# Ransomware Bargaining — Replication Package

This repository contains replication artifacts for the paper:

**“You Can’t Talk Your Way Out of Cyber Extortion: An Empirical Study of Ransomware Negotiations”**

## Overview

The repository provides data and code to reproduce the main analyses in the paper, covering:

- Police and incident response (IR) data (synthetic version)
- Public ransomware negotiation transcripts (ransomware.live)
- Statistical models and figures

The goal is to ensure transparency and reproducibility while respecting legal and privacy constraints.

---

## Repository Structure

- `synthetic_police_ir_data.csv`  
  Synthetic dataset mirroring the structure and statistical properties of the original police/IR data.

- `data_neg_flat.csv`  
  Publicly available ransomware negotiation transcript data obtained via the ransomware.live API.

- `police_ir_code.R`  
  R script for preprocessing, modeling, and analysis of the police/IR dataset.

- `public_transcripts_code.R`  
  R script for preprocessing, modeling, and analysis of the negotiation transcript dataset.

---

## Data Description

### Police / IR Data (Synthetic)
The original police and IR datasets cannot be shared due to confidentiality and legal restrictions.  
Instead, we provide a synthetic dataset that preserves:

- Variable structure
- Distributions and relationships
- Compatibility with all analysis scripts

All results can be reproduced using this dataset.

### Public Negotiation Transcripts
The transcript dataset is obtained via the public ransomware.live API and contains:

- Message volume
- Initial ransom demand
- Discount indicators and size
- Payment outcomes

These data are publicly available and included to enable full replication of transcript-based analyses.

---

## Reproducibility

All analyses are implemented in R.

To reproduce the results:

1. Open the R scripts:
   - `police_ir_code.R`
   - `public_transcripts_code.R`

2. Run the scripts in order:
   - Data preprocessing
   - Model estimation
   - Figure and table generation

The scripts are self-contained and reproduce the main results of the paper.

---

## Codebook

A detailed codebook describing variable definitions, coding rules, and decision criteria is provided in the paper (Appendix A).

---

## Limitations

- The synthetic police/IR dataset does not contain real incident data.
- Some analyses (e.g., post-payment case studies) were conducted manually and are documented in the paper rather than reproduced in code.

---

## Contact

For questions or clarifications, please contact the authors.

---

## License

This repository is provided for academic and research purposes only.
