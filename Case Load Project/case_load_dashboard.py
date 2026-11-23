"""
case_load_dashboard.py

Interactive Dash app: a U.S. map of caseload by state, with a filter to show one or more regions.
Hovering over a state shows its current region, suggested region (from the balancing logic in
Case_Load_Balancing_Map.ipynb) and number of cases.

Run from this folder:
    pip install pandas plotly dash openpyxl
    python case_load_dashboard.py
then open http://127.0.0.1:8050 in a browser.
"""

import pandas as pd
import plotly.express as px
from dash import Dash, dcc, html, Input, Output


def build_state_counts(path="SampleAnon.xlsx"):
    df = pd.read_excel(path, sheet_name="Sheet1").rename(columns={"state_region": "Region"})
    counts = df.groupby(["State", "Region"]).size().reset_index(name="CaseCount")

    # move states, smallest first, from overloaded regions to the most underloaded region
    region_cases = counts.groupby("Region")["CaseCount"].sum().reset_index()
    target = region_cases["CaseCount"].sum() / len(region_cases)
    region_cases["CaseLoadDifference"] = region_cases["CaseCount"] - target
    overloaded = region_cases[region_cases["CaseLoadDifference"] > 0]
    underloaded = region_cases[region_cases["CaseLoadDifference"] < 0].sort_values("CaseLoadDifference").copy()
    counts["SuggestedRegion"] = counts["Region"]
    for _, row in overloaded.iterrows():
        excess = row["CaseLoadDifference"]
        for idx, state_row in counts[counts["Region"] == row["Region"]].sort_values("CaseCount").iterrows():
            if excess <= 0 or underloaded.empty:
                break
            counts.loc[idx, "SuggestedRegion"] = underloaded.iloc[0]["Region"]
            underloaded.iloc[0, underloaded.columns.get_loc("CaseLoadDifference")] += state_row["CaseCount"]
            excess -= state_row["CaseCount"]
            if underloaded.iloc[0]["CaseLoadDifference"] >= 0:
                underloaded = underloaded.iloc[1:]
    return counts


state_case_counts = build_state_counts()

app = Dash(__name__)
app.layout = html.Div([
    html.H1("Caseload Count by State and Region"),
    dcc.Dropdown(id="region-filter", multi=True, placeholder="Select region(s)",
                 options=[{"label": r, "value": r} for r in sorted(state_case_counts["Region"].unique())]),
    dcc.Graph(id="choropleth-map"),
])


@app.callback(Output("choropleth-map", "figure"), [Input("region-filter", "value")])
def update_map(selected_regions):
    data = state_case_counts if not selected_regions else state_case_counts[state_case_counts["Region"].isin(selected_regions)]
    return px.choropleth(data, locations="State", locationmode="USA-states", color="CaseCount", scope="usa",
                         hover_name="State", hover_data=["Region", "SuggestedRegion", "CaseCount"],
                         color_continuous_scale="Blues", title="Caseload by state, with suggested region")


if __name__ == "__main__":
    app.run(debug=False)
