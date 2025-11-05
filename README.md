---

# 🐟 Pacific Salmon Biodata Pipeline: A Modular R Data Science Project

This repository outlines a **five-part, end-to-end data science infrastructure** designed in R for the storage, cleaning, time series analysis, and visualization of Pacific Salmon biological metrics. The architecture is modular, promoting reusability and maintainability.

---

## 🏗️ Overall Project Architecture

The pipeline follows a standard professional workflow, moving data from raw storage to interactive dashboards:

1.  **Raw Data Storage:** `kokanee` (Data Lake)
2.  **Transformation Logic:** `biodata-utils` (R Package)
3.  **Clean Data Storage:** `sockeye` (DBMS)
4.  **Analysis Engine:** `biodata-explorer` (R Package)
5.  **Presentation Layer:** `shiny-biodata` (Shiny Application)

---

## 1. 🏞️ Project 1: Data Lake (`kokanee`)

The raw storage layer, focused on preserving unformatted data in its original state.

* **Goal:** Store raw, unformatted biological data.
* **Methodology:** Structured file system (Data Lake philosophy).
* **Key R Recommendations:**
    * **File Format:** Use **Parquet** or **Feather** via the **`arrow`** package for efficient reading/writing, compression, and data type preservation.
    * **Example Code:**
        ````r
        library(arrow)
        write_parquet(raw_data, "kokanee/raw_data/year_2024.parquet")
        ````
    * **Structure:** Implement **Hive-style partitioning** for optimal query performance:
        `kokanee/raw_data/species={SPP}/year={YYYY}/month={MM}/data.parquet`
    * **Tools:** Use **`fs`** for file manipulation and **`here`** for robust path management.

---

## 2. 🗃️ Project 2: DBMS (`sockeye`)

The centralized, clean data store optimized for relational querying and analysis.

* **Goal:** Store clean, formatted, and relational data.
* **DBMS Choice:** **SQLite** (`RSQLite` package). It is serverless, highly portable, and ideal for a dedicated R analysis environment.
* **Data Model:** Define a **Relational Model** (e.g., Star Schema) consisting of:
    * **Fact Table** (e.g., `observations`): Core measurements and foreign keys.
    * **Dimension Tables** (e.g., `locations`, `species`, `sampling_method`): Descriptive attributes to reduce redundancy.

---

## 3. 🛠️ Project 3: Data Utility Package (`biodata-utils`)

The core business logic package that handles data quality and transformation.

* **Goal:** Develop pipe-friendly functions to clean, format, standardize, and load data into the `sockeye` DBMS.
* **Core Logic:** Houses all data wrangling intelligence, standardizing units, handling missing values, and validating data types.
* **Key R Recommendations:**
    * **Packages:** Heavily rely on the **`tidyverse`** (`dplyr`, `stringr`).
    * **Function Design:** Functions should be **pipe-friendly** (`data.frame` $\to$ `data.frame`).
    * **Example Logic Flow:**
        ```
        read_raw_data() -> clean_metrics() -> standardize_units()
        ```
    * **DB Interaction:** Use the **`DBI`** package for consistent database connections. Abstract connections into a helper function, e.g., `connect_sockeye()`.
    * **Quality Control:** **Unit testing** using **`testthat`** is paramount for ensuring data integrity during cleaning.

---

## 4. 📈 Project 4: Explorer Package (`biodata-explorer`)

The analysis engine responsible for querying, visualizing, and conducting time series analysis.

* **Goal:** Provide functions to analyze and visualize time series trends from the `sockeye` DBMS.
* **Key R Recommendations:**
    * **Querying:** Use **`dbplyr`** to write `dplyr` code that translates into efficient SQL, minimizing data transfer by performing calculations directly on the database.
    * **Time Series:** Utilize the modern **`tsibble`** and **`fable`** packages for robust time series modeling and forecasting.
    * **Visualization:** Standardized plots using **`ggplot2`** and optional integration with **`plotly`** for interactivity.

---

## 5. 🖥️ Project 5: Shiny App (`shiny-biodata`)

The final presentation layer, using the explorer package as its analytical backend.

* **Goal:** Create interactive dashboards and reporting interfaces for exploring salmon biodata trends.
* **Engine:** Uses functions from **`biodata-explorer`** to execute queries and generate visualizations.
* **Key R Recommendations:**
    * **Modularity:** Build the UI/Server logic using **Shiny Modules** to keep the application organized and scalable.
    * **Performance:** Manage expensive computations using **`eventReactive()`** and implement **`bindCache()`** for plots and tables to dramatically improve user experience and speed.
    * **User Interface:** Use a modern framework like **`bslib`** or **`shinydashboard`** for a clean, professional aesthetic.

---

### Current Development Status

We are currently focused on **Project 3: `biodata-utils`**, as establishing clean data pipelines and a definitive schema is the foundation for all subsequent analysis.