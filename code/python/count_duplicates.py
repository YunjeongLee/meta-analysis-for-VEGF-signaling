import pandas as pd
import numpy as np

# Import list of papers found in Google Scholar
df_adip_size = pd.read_csv("data/papers/adipocyte_diameter.csv")
df_adip_v = pd.read_csv("data/papers/adipose_vessel.csv")
df_tumor_vsize = pd.read_csv("data/papers/tumor_vessel_size.csv")
df_tumor_vdensity = pd.read_csv("data/papers/tumor_vessel_density.csv")
df_cbm = pd.read_csv("data/papers/CBM_thickness.csv")
df_vegfr12 = pd.read_csv("data/papers/Kd_for_VEGFR1_and_VEGFR2.csv")
df_nrp1 = pd.read_csv("data/papers/Kd_for_NRP1.csv")

# Import list of papers selected for full-text assessment
df_adip_size_select = pd.read_excel("data/papers/paper_list_MetaAnalysis.xlsx", sheet_name="adipocyte_diameter")
df_adip_v_select = pd.read_excel("data/papers/paper_list_MetaAnalysis.xlsx", sheet_name="adipose_vessel_size")
df_tumor_vsize_select = pd.read_excel("data/papers/paper_list_MetaAnalysis.xlsx", sheet_name="tumor_vessel_size")
df_tumor_vdensity_select = pd.read_excel("data/papers/paper_list_MetaAnalysis.xlsx", sheet_name="tumor_vessel_density")
df_cbm_mice_select = pd.read_excel("data/papers/paper_list_MetaAnalysis.xlsx", sheet_name="CBM_mice")
df_cbm_rats_select = pd.read_excel("data/papers/paper_list_MetaAnalysis.xlsx", sheet_name="CBM_rats")
df_vegfr12_select = pd.read_excel("data/papers/paper_list_MetaAnalysis.xlsx", sheet_name="Kd_for_VEGFR1_and_VEGFR2")
df_nrp1_select = pd.read_excel("data/papers/paper_list_MetaAnalysis.xlsx", sheet_name="Kd_for_NRP1")

# Merge two dataframes `df_cbm_mice_select` and `df_cbm_rats_select`
# Search list
df_tumor_v = pd.concat([df_tumor_vsize, df_tumor_vdensity])
df_kd = pd.concat([df_vegfr12, df_nrp1])

# Used papers list
df_tumor_v_select = pd.concat([df_tumor_vsize_select, df_tumor_vdensity_select])
df_cbm_select = pd.concat([df_cbm_mice_select, df_cbm_rats_select])
df_kd_select = pd.concat([df_vegfr12_select, df_nrp1_select])

# Check if the used papers are in the Google Scholar list
print(all(df_adip_size_select["Title"].isin(df_adip_size["Title"])))
print(all(df_adip_v_select["Title"].isin(df_adip_v["Title"])))
print(all(df_adip_v_select["Title"].isin(df_adip_v["Title"])))
print(all(df_adip_v_select["Title"].isin(df_adip_v["Title"])))
print(all(df_adip_v_select["Title"].isin(df_adip_v["Title"])))

# Count the total number of papers
df_search = pd.concat([df_adip_size[["Authors", "Year", "Title"]], df_adip_v[["Authors", "Year", "Title"]], df_tumor_v[["Authors", "Year", "Title"]], df_cbm[["Authors", "Year", "Title"]], df_kd[["Authors", "Year", "Title"]]])
df_select = pd.concat([df_adip_size_select, df_adip_v_select, df_tumor_v_select, df_cbm_select, df_kd_select])

print("The number of searched papers (including duplicates): ", len(df_search))
print("The number of selected papers (including duplicates): ", len(df_select))

# Check the number of searched papers is same as the sum of dataframes' length
print("Is the number of rows in `df_search` is same as the sum of rows of all dataframes?", (len(df_search) == (len(df_adip_size)+len(df_adip_v)+len(df_tumor_v)+len(df_cbm)+len(df_kd))))

# Count the number of duplicates
print("The number of duplicates in searched papers list: ", len(df_search) - len(df_search.drop_duplicates()))
print("The number of duplicates in selected papers list: ", len(df_select) - len(df_select.drop_duplicates()))

print("The number of searched papers (excluding duplicates): ", len(df_search.drop_duplicates()))
print("The number of selected papers (excluding duplicates): ", len(df_select.drop_duplicates()))