## Usage Instructions

-   **Performance Considerations:** Operations and visualisations that are computationally intensive, such as generating world maps or heatmap clustering based on very large datasets, may take longer to render. Your patience is appreciated during these processes.

-   **Label Consistency:** We strive for consistent labeling across the application:

    -   **Grouping vs. Subsetting Variables:** Grouping variables are used for comparisons (e.g., groups in boxplots), while subsetting variables specify the data to be visualised.
    -   **Legend Tables:** When traditional figure legends are impractical, they are replaced or supplemented with legend tables. All legends and plots should be easily linkable through their shared colour code.

-   **Boxplot Interpretation:** For boxplots displaying low y-axis values (e.g., abundance, concentration close to zero), the visual distribution of samples might appear misleadingly wide even if all values are zero. To verify the exact values, hover your cursor over the data points of interest.

-   **Outlier Representation in Boxplots:** Due to technical constraints, all outlier values in boxplots are depicted twice: once as black "boxplot dots" and again as "jittered" coloured dots.
