# Current Development Status

**Last Updated:** 2026-02-20 by @mpdougherty

## Active Work

### In Progress: Data Validation Module
- **Branch:** `feature/data-validation`
- **Goal:** Robust validation of site data before analysis
- **Status:** ~40% complete

### Recently Completed
- Basic validation functions (`validate_site_data()`)
- Test coverage for common edge cases
- AI transparency documentation

### Up Next
- Batch validation for multiple sites
- Performance optimization
- Integration with Quarto reports

## Context for AI Sessions

### Project Background
This is an R package for USACE EQRI (Engineering Quality Risk Indicators) analysis.
It analyzes survey data to assess quality metrics across USACE projects.

### Key Dependencies
- `{dplyr}` v1.1.4 - data manipulation
- `{pointblank}` v0.12.1 - data validation
- `{ggplot2}` v3.5.0 - visualization
- `{quarto}` v1.4 - reporting

### Known Constraints
- Zero-variance fields common (watershed always "OHIO" in MVR data)
- Site IDs must match pattern `MVR_SITE_YYYY_NNN`
- All analysis must validate against EQRI questionnaire schema

### Current Architecture
- **Data ingestion:** `data-raw/*.R` scripts
- **Core functions:** `R/*.R` 
- **Validation:** `R/validate_*.R`
- **Reports:** `*.qmd` files (Quarto website)
- **Development:** `dev/` folder (excluded from package build)

## Quick Reference Links
- [EQRI App](https://egis-app.mvr.usace.army.mil/ords/cm2/r/qri/home)
- [Pointblank Docs](https://rstudio.github.io/pointblank/)
- [Package DESCRIPTION](./DESCRIPTION)