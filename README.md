# Phonetics of Tone in Northern Pomo

Scripts and data accompanying the LSA 2026 presentation on F0 dynamics and tonal coarticulation in Northern Pomo.

## Repository Contents

| File | Description |
|---|---|
| `scripts/data_extraction_and_preprocessing.py` | Extracts F0 measurements from `.wav` + `.TextGrid` files using Parselmouth (Python/Praat). Outputs a segment-level CSV with ~60 contextual features per observation. |
| `scripts/data_visualization_and_analysis.R` | Fits Generalized Additive Models (GAMs) to the F0 time series and produces visualizations of tonal contours and contextual effects. |
| `data/timeseries.csv` | The extracted dataset used in the analysis. |

## Data

**Audio**: Source recordings are available through the [Califormia Language Archive](https://cla.berkeley.edu/) at UC Berkeley (California Language Archive).

**TextGrid annotations**: Available on request.

**Extracted dataset** (`data/timeseries.csv`): Segment-level F0 measurements with tone labels, positional features, and phonological context (preceding/following tone, laryngeal class, vowel height/frontness). This is the direct input to the R modeling script.

## Dependencies

**Python** (data extraction):
The pipeline uses Praat's filtered autocorrelationpitch tracker |`To Pitch (filtered ac)`|, which requires installing Parselmouth from source. The PyPI release does ot include this method:
```
pip install git+https://github.com/YannickJadoul/Parselmouth.git \
  --config-settings="--build-option=--cmake-executable=/usr/bin/cmake"
```
Then install the remaining dependencies:
```
pip install -r requirements.txt
```

**R** (modeling and visualization):
```r
install.packages(c("mgcv", "tidyverse", "dplyr", "gridExtra"))
```

## Citation

Dailey, Brady A. (2026). *[Title]*. Presented at the Annual Meeting of the Linguistic Society of America.

If you use the data or scripts, please also cite the California Language Archive as the source of the original recordings.

## Contact

Questions about the scripts or data: open an issue or contact via GitHub.
