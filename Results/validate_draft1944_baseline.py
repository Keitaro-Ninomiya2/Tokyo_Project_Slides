"""Independently count historical men from the raw master, without R joins."""
import csv
import os
import re
from collections import defaultdict
from pathlib import Path

master = Path(os.environ['TOKYO_PERSONNEL_MASTER'])
out = Path('Results/draft1944_pre_draft_trajectories')
seen = set()
matched = defaultdict(set)
office = defaultdict(set)

def missing(value):
    return None if value in ('', 'NA') else value

with master.open(encoding='utf-8-sig', newline='') as handle:
    for row in csv.DictReader(handle):
        if row['is_name'].lower() != 'true':
            continue
        year = int(row['year'])
        staff = missing(row['staff_id'])
        staff = float(staff) if staff is not None else None
        staff_year = (staff, year)
        if staff_year in seen:
            continue
        seen.add(staff_year)
        gender = missing(row['gender_modern'])
        if year >= 1944 or gender is None or gender == 'female':
            continue
        position = missing(row['position'])
        position = re.sub(r'\s+', '', position) if position is not None else None
        team = missing(row['kakari'])
        matched[(missing(row['ka']), team, position)].add(staff)
        office_id = missing(row['office_id'])
        office_id = float(office_id) if office_id is not None else None
        office[(office_id, team, position)].add(staff)

for filename, counts, first_key in [
    ('matched_1944_baseline_audit.csv', matched, 'ka'),
    ('replacement_1944_analysis_cells.csv', office, 'office_id'),
]:
    checked = positive = 0
    with (out / filename).open(encoding='utf-8', newline='') as handle:
        for row in csv.DictReader(handle):
            group = missing(row[first_key])
            if first_key == 'office_id' and group is not None:
                group = float(group)
            key = (group, missing(row['kakari']), missing(row['pos_norm']))
            expected = len(counts[key])
            actual = int(float(row['cumul_n_male']))
            assert expected == actual, (filename, key, expected, actual)
            checked += 1
            positive += actual > 0
    assert checked > 0 and positive > 0
    print(f'PASS: {checked} {filename} baselines match raw historical worker counts; {positive} positive.')
