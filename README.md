# USACE Engineering Quality Risk Indicators Analysis

[View the report](https://mvr-gis.github.io/EQRIanalysis/)

![](logos/EQRI-logo.png)

## Introduction
The Engineering Quality Risk Indicators (EQRI) initiative aims to measure risk to engineering quality as a counterpoint to existing cost and schedule metrics and to give PDTs and Technical Leads a tool to help them leverage their voice for quality. 

* [Visit the EQRI app (USACE employees only)](https://egis-app.mvr.usace.army.mil/ords/cm2/r/qri/home)
* [Read More on the USACE KM Portal (USACE employees only)](https://usace.dps.mil/sites/KMP-EC/SitePages/QRI.aspx)

## FY25 Question Analysis
The purpose of this website is to provide a detailed Exploratory Data Analysis (EDA) summary of the FY25 version of the EQRI questionnaire. To explore these results, use the items on the top menu:

- **Questions** - Results for each question are presented and summarized by key USACE organizational units. 
- **Indicators** - Results for each indicator (a metric derived from a topical grouping of questions) summarized by key organizational units. 

## Development Practices

### AI Assistance

This project uses GitHub Copilot to assist with:
- R function development and refactoring
- Test case generation
- Documentation writing
- Package structure and best practices

**Transparency:** Unfiltered AI session logs are maintained in `dev/sessions/` for colleague review and audit purposes. All AI suggestions undergo human review, testing, and validation before integration.

**Accountability:** The project maintainer (@mpdougherty) is responsible for all code quality, correctness, and scientific validity regardless of AI assistance.

**Human Oversight:** AI-generated code is:
- Validated against official package documentation (see `DESCRIPTION` for dependencies)
- Tested using `{testthat}` framework
- Reviewed for alignment with project goals and data requirements
- Modified as needed to meet USACE data quality standards
