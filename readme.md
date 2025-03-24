# Example Data Set for Teaching

This repository contains an R Project designed to refresh knowledge on data analysis.  
For additional context, please refer to the teaching syllabus.  

## Overview of the Data

The `/data` directory contains all relevant data files, while the `/analysis` directory includes scripts for performing analyses.  

The virtual experiment (based on simulated data) models a **three-factor within-subjects design** with multiple covariates. It simulates a reaction-time experiment and consists of three measurement points: **T0 (baseline), Post (after intervention), and Follow-Up**.  

The primary dependent variable in this study is the **BDI-II (Beck Depression Inventory-II) score**. Between T0 and Post, an intervention was introduced, involving an **AI-based consultation process** for participants with depression (`p`) and a **non-depressed control group** (`c`).  

The central research question is:  

*"Do individuals with depression respond differently to [specific intervention] compared to healthy controls in terms of changes in depressive symptoms over time?"*  

---

## Data Structure

### **T0 (Baseline Assessment)**  
- Contains all **BDI-II items** separately (must be aggregated).  
- Includes **four items** from a fictional hopelessness questionnaire (Items 2 and 3 require recoding).  
- Includes **GAD-7** as a measure of anxiety.  
- Demographic variables: **Age and Sex**.  
- No missing data.  
- **Participant code** included for data merging.  

### **Experiment (Reaction Time Task)**  
- Separate data file for each participant.  
- Participants provided **speeded responses** to happy and sad facial stimuli, classifying them as **happy** or **sad**.  
- Data includes:  
  - **Reaction times**  
  - **Correct/incorrect responses**  
  - **Stimulus type**  
- **Participant code** included for data matching.  
- Missing data present.  

### **Follow-Up (Post & Follow-Up Measurements)**  
- Includes **BDI-II scores** collected **post-intervention** and at **follow-up (three months later)**.  
- **Participant code** included for data matching.  
- Missing data present.  

---

## **Data Summary**  

| **T0 (Baseline)** | **Post-Measurement & Follow-Up** | **Experiment** |
|------------------|---------------------------------|--------------|
| - Participant Code  | - Participant Code  | --> Carried out at T0 |
| - BDI-II | - BDI-II (Post-Intervention) | - Participant Code |
| - GAD-7 | - BDI-II (Follow-Up, 3 Months Later) | - Reaction Times |
| - Hopelessness Questionnaire | | - Correct/Incorrect Responses |
| - Age & Sex | | - Stimulus Type |

