# Appendix – *The Kurdish Case for Democracy*  

This repository contains the appendix materials for my book *The Kurdish Case for Democracy*, published with **Palgrave Macmillan** that is based on my PhD thesis submitted and defended at the Friedrich Alexander University of Erlangen-Nürnberg.  

## Structure  

### Scripts  
- **`scripts/raw/Kurd_merge.R`**  
  - This is the **main script** if you want to run everything in **RStudio**.  
  - It is structured with RStudio-style **Code headings and subheadings**, so you can easily navigate the workflow in one file.  

- **`scripts/processed/`**  
  - For those working with **VS Code** or other environments, the same script is divided into **modular subfiles** (`00_setup.R`, `01_data_import.R`, etc.).  
  - This makes it easier to run and edit step by step.  

### Data  
- The scripts harmonize my **own survey data** (not publicly available) with **World Values Survey (WVS) 7** data.  
- The WVS code can be reused. 
- To portray the maps use the shapefiles
- ⚠️ If you use or adapt this code, please **cite the book and refer to me**.  

### Notebooks  
- The folder `notebooks/` contains:  
  - **`00_appendix.md`** – an appendix with explanations of:  
    - Imputation model diagnostics  
    - Comparisons of models with and without imputation  
  - Figures are provided in `notebooks/figs/`.  

## Citation  
If you use this repository or adapt the scripts, please cite:  

> Jasim, Dastan. *The Kurdish Case for Democracy*. Palgrave Macmillan, 2026 (forthcoming).  

---
📠 *Questions or ideas? Drop me a message under jasim.dastan@gmail.com*
📌 *Note: Survey data used in this project is not published. Only code and WVS-related scripts are available for reproducibility.*  
