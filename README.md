# Overview

The repository contains code and data for:

> Matthew Wilkens, Elizabeth F. Evans, Sandeep Soni, David Bamman, and Andrew Piper. **"Small Worlds: Measuring Mobility in English-Language Fiction."** *Proceedings of the Third Annual Conference on Computational Literary Studies (CCLS'24)*. Vienna, Austria, 13-14 June 2024. (DOI and link to come.)

Content is organized as the folder names suggest:

## `code`

Scripts and notebooks to perform analyses and generate visualizations presented in the article. Code is a mix of Python and R, all drawing on the same input data files from the `data` directory.

## `data`

Input data for analyses performed in the article. Subdirectories describe data types and sources:

  * `booknlp`. Output from BookNLP and the character-place grounding workflow. For details of the pipeline, see [Soni et al. (2023)](https://aclanthology.org/2023.acl-long.655/).
  * `derived`. Data derived from character-place pipeline data.
  * `geo`. Geographic data associated with GPEs.
  * `metadata`. Metadata about books in the corpus.

## `figures`

Figures generated from derived data. Includes all figures from the artcile, as well as others produced in the course of analysis.

## `results`

Pure numeric results produced from derived data.