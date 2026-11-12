# Chart24MS Local Windows App

This folder contains a local Shiny app for generating the Lake Powell and Lake Mead 24-Month Study plots from USBR HDB data.

## First-time setup

1. Install **R for Windows** if it is not already installed.
2. Make sure the computer can reach CRAN, GitHub, and the USBR HDB API.
3. Double-click `Setup_Chart24MS.bat`.
4. Wait for the package installation to finish successfully.

The setup installs packages into your **personal Windows R library** under `%LOCALAPPDATA%\R\win-library\<R version>`. It does not write to `C:\Program Files\R`, so Administrator rights are normally not required.

RStudio is not required.

## Monthly use

1. Double-click `Start_Chart24MS.bat`.
2. The app opens in the default web browser. It runs only on the local computer (`127.0.0.1`).
3. Enter:
   - Study month (`YYYY-MM`)
   - MRID and run month for Probable Minimum 6.0 maf
   - MRID and run month for Most Probable 6.0 maf
   - MRID and run month for Most Probable 7.0 maf
   - MRID and run month for Probable Maximum 7.0 maf
4. Click **Generate Plots**.
5. Review the Powell and Mead previews or use the download buttons.
6. Use **Open Output Folder** to see the files on disk.
7. Close the command window that launched the app when finished.

## Outputs

Each study month is stored separately:

```text
output/
  2026-09/
    Powell24MS.png
    Powell24MS.pdf
    Mead24MS.png
    Mead24MS.pdf
```

This prevents a new month from overwriting a previous month's files.

## Automatic labels

The legend labels and plot subtitle are derived from the entered months. For example, a `2026-08` run month becomes `August 2026` automatically. You no longer need to edit the labels in R code.

## Dynamic date reference lines

The original script contained Powell reference lines fixed to September 2026/2027 and Mead reference lines fixed to December 2025/2026/2027. The app generates September and December reference lines automatically for the years inside each plot's data window.

## Files

- `app.R` - local Shiny user interface
- `generate_plots.R` - shared HDB/plot-generation function
- `setup.R` - installs/checks required R packages
- `user_library.R` - selects the writable per-user R package library used by setup and launch
- `Setup_Chart24MS.bat` - first-time Windows setup launcher
- `Start_Chart24MS.bat` - normal Windows launcher
- `launch_app.R` - starts the local Shiny server
- `data/` - existing logo and elevation-volume lookup CSV files
- `scripts/` - original plotting script retained for reference
- `.github/` - original GitHub Actions workflow retained unchanged

## Notes

- The four MRIDs must be unique because they map to four separate traces.
- The app validates month fields as `YYYY-MM` and MRIDs as positive whole numbers.
- If ImageMagick logo compositing fails, the app still creates PDF outputs from the ggplot objects as a fallback.
- The package setup pins `rhdb` to `BoulderCodeHub/rhdb` tag `v0.3.0`, matching the existing GitHub Actions workflow.

## Preview layout fix

This build includes a responsive preview layout for the Lake Powell and Lake Mead plots. The preview containers use the same 11 x 8.5 aspect ratio as the generated images so one preview does not overflow into the next card when the browser window is resized.

### Open Output Folder

The app opens output folders by launching Windows Explorer directly. This avoids cases where R's `shell.exec()` does nothing for a directory on some Windows installations.
