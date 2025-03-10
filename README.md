# SDD - HIES Household economic activity processing

## Usage

The R scripts in this project is used on processing HIES household primary economic activity involvement. The data file is supplied by the Data Collection team.

Any future updates will be processed using the script.

# Folder structure

There are four main folders in this repository:
- `docs`: Contains the documentation of the project.
- `src`: Contains the source code of the project.
- `raw_data`: Contains temporary local copies of the raw data used in the project. This folder won't be uploaded to the repository.
- `output`: Contains the temporary output files generated by the project (png, pdfs, small data units). This folder won't be uploaded to the repository.

# gitignore

The `.gitignore` file is configured to ignore the most common development temporary files for Python, R, and Stata. It also ignore most file formats in the `/temp/` sub directories.