import pandas as pd

# Merges two CSV files on a common column.
def merge_csv_files(csv_file_path1, csv_file_path2, common_column, output_file_path ):

    # Reading the CSV files into DataFrames
    df1 = pd.read_csv(csv_file_path1)
    df2 = pd.read_csv(csv_file_path2)

    # Merging the DataFrames on the specified common column
    merged_df = pd.merge(df1, df2, on=common_column, how='inner')
	
    merged_df.to_csv(output_file_path, index=False)

# Example usage
# Replace 'path_to_first_csv.csv' and 'path_to_second_csv.csv' with your actual file paths
# Replace 'common_column' with the actual column name you want to merge on

merged_df = merge_csv_files('dept0713_Funk.csv', 'dept0713_LDAP.csv', 'surname' , 'dept0713_common_entries.csv')

