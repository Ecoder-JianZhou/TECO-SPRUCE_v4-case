import pandas as pd
import datetime
import os

path = "/mnt/d/3_case_SPRUCE_data_analysis/2_TECO-SPRUCE_data_assimilation/2_code_based_on_Ma/inputs/SPRUCE/"
outdir = "/mnt/d/3_case_SPRUCE_data_analysis/2_TECO-SPRUCE_data_assimilation/2_code_based_on_Ma/inputs/SPRUCE_new/"

plot_names = {"P04": "f2p4",
              "P06": "f1p2", 
              "P07": "f1p1", 
              "P08": "f1p5", 
              "P10": "f2p6",
              "P11": "f2p3", 
              "P13": "f1p4", 
              "P16": "f2p5", 
              "P17": "f1p6", 
              "P19": "f2p2", 
              "P20": "f1p3"} 


def get_year_doy(days_since_2011 ):
    start_date = datetime.date(2011, 1, 1)  # 2011年的第1天
    target_date = start_date + datetime.timedelta(days=days_since_2011 - 1)
    year = target_date.year
    doy = target_date.timetuple().tm_yday
    return year, doy


filename = "obs_cflux_"
column = ['days', 'GPP', 'GPP_sd', 'NEE', 'NEE_sd', 'Reco', 'Rec_sd']
for ikey, item in plot_names.items():
    file = path+ item + "/"+filename+ item+".txt"
    df = pd.read_csv(file, delimiter='\t')
    df['Year'], df['DOY'] = zip(*df['days'].apply(get_year_doy))
    df_to_save = df[['Year', 'DOY', "GPP",  "GPP_sd"]]
    df_to_save["hour"] = 24
    df_to_save = df_to_save[['Year', 'DOY',"hour", "GPP",  "GPP_sd"]]
    print(df_to_save)
    outdir4file = outdir + ikey
    if not os.path.exists(outdir4file):
        os.makedirs(outdir4file)
    df_to_save.to_csv(outdir + ikey + "/obsfile_GPP_d.txt",sep='\t', index=False )

    df_to_save = df[['Year', 'DOY', "Reco",  "Rec_sd"]]
    df_to_save["hour"] = 24
    df_to_save = df_to_save[['Year', 'DOY',"hour", "Reco",  "Rec_sd"]]
    print(df_to_save)
    outdir4file = outdir + ikey
    if not os.path.exists(outdir4file):
        os.makedirs(outdir4file)
    df_to_save.to_csv(outdir + ikey + "/obsfile_Reco_d.txt",sep='\t', index=False )

    df_to_save = df[['Year', 'DOY', "NEE",  "NEE_sd"]]
    df_to_save["hour"] = 24
    df_to_save = df_to_save[['Year', 'DOY',"hour", "NEE",  "NEE_sd"]]
    print(df_to_save)
    outdir4file = outdir + ikey
    if not os.path.exists(outdir4file):
        os.makedirs(outdir4file)
    df_to_save.to_csv(outdir + ikey + "/obsfile_NEE_d.txt",sep='\t', index=False)