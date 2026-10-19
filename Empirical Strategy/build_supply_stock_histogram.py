import csv
import re
import unicodedata
from collections import defaultdict
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


FIGURES = Path(r"C:\Users\Keitaro Ninomiya\Documents\GitHub\Tokyo_Project_Figures")
SOURCE = FIGURES / "MainResults" / "TransferPatternsBy1944Stock" / (
    "tables_full_office_crosswalk_corrected_fu_shi"
) / "stock_by_1944_office.csv"
OUTPUT = Path(__file__).with_name("supply_stock_histogram.png")


def norm(value: str) -> str:
    return re.sub(r"\s+", "", unicodedata.normalize("NFKC", value or ""))


stocks = defaultdict(list)
with SOURCE.open(encoding="utf-8-sig", newline="") as handle:
    for row in csv.DictReader(handle):
        if (
            int(row["drafted_men_1944_name_match"]) > 0
            and row["positive_supply_any_year_all_links"] == "True"
        ):
            stocks[norm(row["1944_office"])].append(
                float(row["mean_supply_stock_all_links"])
            )

values = np.array([np.mean(group) for group in stocks.values()])
q25, median = np.quantile(values, [0.25, 0.5])

fig, ax = plt.subplots(figsize=(10.8, 5.25), dpi=180)
bins = np.arange(0, 181, 20)
ax.hist(values, bins=bins, color="#4472C4", edgecolor="white", linewidth=1.2)
ax.axvline(median, color="#C00000", linewidth=2, linestyle="--",
           label=f"Median = {median:.1f}")
ax.set_xlabel("Mean historical supply stock (workers)", fontsize=13)
ax.set_ylabel("Number of 1944 destination ka", fontsize=13)
ax.set_xticks(bins)
ax.tick_params(labelsize=11)
ax.grid(axis="y", alpha=0.22)
ax.spines[["top", "right"]].set_visible(False)
ax.legend(loc="upper center", frameon=False, fontsize=11)

summary = (
    f"N                 {len(values):.0f}\n"
    f"Mean          {values.mean():.1f}\n"
    f"Std. dev.    {values.std(ddof=1):.1f}\n"
    f"Minimum     {values.min():.1f}\n"
    f"P25             {q25:.1f}\n"
    f"Median       {median:.1f}\n"
    f"Maximum   {values.max():.1f}"
)
ax.text(
    0.975, 0.96, summary, transform=ax.transAxes, ha="right", va="top",
    fontsize=10.5, linespacing=1.25,
    bbox=dict(boxstyle="round,pad=0.6", facecolor="white", edgecolor="#555555", alpha=0.96),
)

fig.tight_layout(pad=0.7)
fig.savefig(OUTPUT, bbox_inches="tight", facecolor="white")
print(f"Wrote {OUTPUT} with {len(values)} observations")
