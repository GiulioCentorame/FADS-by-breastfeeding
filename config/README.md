# Configuration option
<!-- Describe how to configure the workflow (using config.yaml and maybe additional files). -->
<!-- All of them need to be present with example entries inside of the config folder. -->

A description of every valid option in `config.yaml`.

Temporary workflow directory (e.g., in the HPC scratch space). It will be linked to `../temp` by the workflow

``` yaml
tempdir: /temp
```

Path to the folder containing the UK Biobank basket.
```yaml
basket_path: /path/to/basket/folder
```

Filename of the UK Biobank basket file set, without the file extension. Assumes the presence of three files with the same name, with extensions `.r`, `.tab`, `.html` (obtained through `ukbunpack` and `ukbconv`)
```yaml
basket_filename: ukbXXXXX
```

Path to the UK Biobank bgen file for chromosome 11. Assumes the presence of `chr11.bgen.bgi` in the same folder (derived through `bgenix -index` or through the UK Biobank).
```yaml
chromosome_11_bgen: /path/chr11.bgen
```

Path to the `.sample` file tied to the UKB application.
``` yaml
sample_file: /path.sample
```

Path to the withdrawals file (the most recent provided by UK Biobank):
``` yaml
withdrawals: /path/to/withdrawals.csv
```

Path the file including the standard exclusions from the genetic data
``` yaml
std_exclusions: /path/to/std_exclusions.txt
```
