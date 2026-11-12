# Chart24MS - Lake Powell & Lake Mead 24-Month Study Plot Generator

This repository builds the official Lake Powell and Lake Mead 24-Month Study elevation charts from U.S. Bureau of Reclamation Hydrologic Database (HDB) data. It produces high-resolution PNG (300 DPI) and print-ready PDF (11 x 8.5 in) plots.

It can be run in two ways:
1. **GitHub Actions + GitHub Pages:** Trigger a run directly in the browser via GitHub's web form; plots are automatically saved in the repo and displayed on GitHub Pages.
2. **Local Windows PC (Shiny App or Standalone):** Run locally with the Shiny UI (`Start_Chart24MS.bat`) or the standalone script (`generate_plots_standalone.R`), with automatic ODBC fallback if the HDB API is down.

---

## 1. Running on GitHub (Actions & Pages)

### How to trigger a run
1. Go to the repository's **Actions** tab on GitHub.
2. Select **Generate 24-Month Study Plots** from the left sidebar.
3. Click the **Run workflow** button on the right.
4. Fill in or adjust any fields in the web form:
   - **Study Month:** `YYYY-MM` (leave blank to default to the current month).
   - **Plot End Month:** optional `YYYY-MM` (leave blank for the full 24 months).
   - **Include Most Probable (6.0 maf / 7.0 maf):** checkboxes (uncheck one during May–July when only one scenario is run).
   - **MRIDs & Run Months:** Defaults use the study month for Min-6 and Most Probable, and the latest quarterly month (Jan, Apr, Aug, Oct) for Max-7.
   - **Y-Axis Limits:** Adjustable elevation ranges for Powell (default 3400–3625 ft) and Mead (default 950–1100 ft).
5. Click the green **Run workflow** button.

### What happens automatically:
1. GitHub Actions runs on an Ubuntu runner and queries the USBR HDB HTTP API.
2. Generated charts are saved to `output/<YYYY-MM>/` in the repository:
   - `output/<YYYY-MM>/Powell24MS.png` & `Powell24MS.pdf`
   - `output/<YYYY-MM>/Mead24MS.png` & `Mead24MS.pdf`
3. The latest charts and an interactive dashboard are published to **GitHub Pages** (`https://<owner>.github.io/<repo>/`) with:
   - Clear titles for Lake Powell and Lake Mead
   - Embedded high-resolution PNG previews
   - Download buttons for PNG and PDF
   - A study parameters summary table
   - An archive selector for past study runs

### First-Time GitHub Pages Setup (One Time)
1. Go to repository **Settings** -> **Pages**.
2. Under **Build and deployment** -> **Source**, select **GitHub Actions**.
   *(Alternatively, if deploying from a branch, select `Deploy from a branch`, choose branch `main`, and folder `/docs`).*

---

## 2. Running Locally on Windows

See [README-Windows-App.md](README-Windows-App.md) for full instructions:
- **First-time setup:** Double-click `Setup_Chart24MS.bat` to install packages into your personal user library (no admin rights needed).
- **Run the Shiny App:** Double-click `Start_Chart24MS.bat` (opens `http://127.0.0.1:port` in your browser).
- **Run Standalone:** Edit the `settings` block at the top of `generate_plots_standalone.R` and run in RStudio or via `Rscript generate_plots_standalone.R`.
- **HDB Data Source:** The local app tries the USBR HDB HTTP API first. If the API is down, it seamlessly prompts for your HDB credentials to query `DSN=vblchdb` directly via ODBC.
