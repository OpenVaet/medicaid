# medicaid
Misc. Statistics on Medicaid Datasets

The scripts assume that [medicaid-provider-spending.csv](https://opendata.hhs.gov/datasets/medicaid-provider-spending/) is placed in the "data" folder.

- parse_raw_to_hcpcs_month.pl -> Converts the raw data to aggregates by months & HCPCS codes.
- hcpcs_month_to_literal.pl -> Enriches the data with the literal description of the HCPCS codes (when available)
- hcpcs_plots.R -> Generates plots by codes & plots of aggregates.