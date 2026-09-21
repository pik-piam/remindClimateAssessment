# Variable profiles

The files in this directory are named profiles that let REMIND report a wider set of MAGICC7 output variables through `climate-assessment`, without changing any Python code. A profile is a set of three config files that are passed to the `ca-clim` command. REMIND turns a profile on by pointing three `climate_assessment_*` keys in its `default.cfg` at these files. When a key is left empty, the built-in AR6 default set is used. Two profiles ship today:

- **`pbo`** adds ocean-surface pCO2 (`PCO2S_CONC`) and equivalent effective stratospheric chlorine (`EESC_CONC`) on top of the AR6 default set. It works with a standard MAGICC7 build.
- **`slr`** adds a sea-level-rise breakdown. It only works with a purpose-built, SLR-capable MAGICC7 version and a matching parameter set. A standard MAGICC7 build does not compute these variables.

## The three files and where each enters the pipeline

The pipeline runs `ca-clim`, which drives openscm-runner, then pymagicc, then the MAGICC7 binary, and reads the results back up the same chain. `ca-clim` reads each profile file through one command-line flag, but each file takes effect at a different point:

| File | Passed via | Where it takes effect | Names it uses |
|---|---|---|---|
| `<profile>_magicc_extra_config.json` | `--magicc-extra-config` | Its `out_dynamic_vars` list is handed down to the MAGICC7 binary and tells MAGICC which raw variables to write. | MAGICC `DAT_*` names |
| `<profile>_output_variables_report.json` | `--output-variables-file` | Its `output_variables` list tells climate-assessment which results to collect back from openscm-runner after the MAGICC run. | openscm-runner names |
| `<profile>_variable_definitions.csv` | `--variable-definitions-file` | Read in the final post-processing step. Every collected variable must have a row here giving its final name and unit, or post-processing stops. | final report names |

So a variable has to appear in all three files, but usually under a different name in each, because MAGICC output is renamed on the way back up (first by pymagicc, then openscm-runner, then climate-assessment).

## Two kinds of variables

- **Standard variables** (for example surface temperature, the forcings, the common concentrations) are already handled by the Python code from end to end. The rename steps connect their three different names for you. The final report name is a normal IAMC name, so it contains a space, a `|`, or a lower-case letter.
- **Pass-through variables** (for example `PCO2S_CONC`, `EESC_CONC`, and the detailed `SLR_*` variables) are not renamed at any step. They keep their raw MAGICC name the whole way: `DAT_X` in the first file, `X` in the second, and `X` in the third. This is what makes it possible to report extra variables without changing the Python code.

## Adding a variable to a profile

1. Add `DAT_<var>` to `<profile>_magicc_extra_config.json` so MAGICC writes it. You can skip this if the variable is already produced by a fixed switch such as `out_temperature=1`, which covers the surface temperatures.
2. Add the variable's report name to `<profile>_output_variables_report.json`. For a pass-through variable this is the raw token `X`. For a standard variable it is the openscm-runner name.
3. Add a row to `<profile>_variable_definitions.csv` with the variable's final name and its unit.
4. Check the result with a short end-to-end run.
