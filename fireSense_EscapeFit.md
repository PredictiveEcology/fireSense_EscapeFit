---
title: "fireSense_EscapeFit Manual"
subtitle: "v.0.0.1"
date: "Last updated: 2025-04-08"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
bibliography: citations/references_fireSense_EscapeFit.bib
link-citations: true
always_allow_html: true
---

# fireSense_EscapeFit Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:fireSense-EscapeFit) *fireSense_EscapeFit*



[![made-with-Markdown](figures/markdownBadge.png)](https://commonmark.org)

<!-- if knitting to pdf remember to add the pandoc_args: ["--extract-media", "."] option to yml in order to get the badge images -->

#### Authors:

Jean Marchal <jean.d.marchal@gmail.com> [aut], Ian Eddy <ian.eddy@nrcan-rncan.gc.ca> [aut, cre], Eliot McIntire <eliot.mcintire@nrcan-rncan.gc.ca> [aut], Alex M Chubaty <achubaty@for-cast.ca> [ctb]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

<!-- TODO -->
Fit statistical models that can be used to parameterize the fire escape component of landscape fire models (e.g. fireSense [@Marchal:2017a; @Marchal:2017b; @Marchal:2019]).

### Module summary

Provide a brief summary of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

### Module inputs and parameters

Table \@ref(tab:moduleInputs-fireSense-EscapeFit) shows the full list of module inputs.

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-fireSense-EscapeFit)(\#tab:moduleInputs-fireSense-EscapeFit)List of (ref:fireSense-EscapeFit) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> fireSense_escapeCovariates </td>
   <td style="text-align:left;"> data.frame </td>
   <td style="text-align:left;"> table of aggregated covariates with annual ignitions and escapes </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> fireSense_escapeFormula </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> a formula describing the model to be fitted, as character. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Summary of user-visible parameters (Table \@ref(tab:moduleParams-fireSense-EscapeFit))


<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-fireSense-EscapeFit)(\#tab:moduleParams-fireSense-EscapeFit)List of (ref:fireSense-EscapeFit) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> .runInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> when to start this module? By default, the start time of the simulation. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .runInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. Interval between two runs of this module, expressed in units of simulation time. By default, NA, which means that this module only runs once per simulation. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInitialTime </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. When to start saving output to a file. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .saveInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> optional. Interval between save events. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant. </td>
  </tr>
</tbody>
</table>

### Events

<!-- TODO -->
- Module initialization;
- Model fitting;

### Plotting

<!-- TODO -->
Write what is plotted.

### Saving

<!-- TODO -->
Write what is saved.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-fireSense-EscapeFit)).

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-fireSense-EscapeFit)(\#tab:moduleOutputs-fireSense-EscapeFit)List of (ref:fireSense-EscapeFit) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> fireSense_EscapeFitted </td>
   <td style="text-align:left;"> fireSense_EscapeFit </td>
   <td style="text-align:left;"> formula - as a character - describing the model to be fitted. </td>
  </tr>
</tbody>
</table>

### Links to other modules

<!-- TODO: link to other fireSense modules -->
Describe any anticipated linkages to other modules, such as modules that supply input data or do post-hoc analysis.

### Getting help

- <https://github.com/PredictiveEcology/fireSense_EscapeFit/issues>

## References

<!-- autogenerated from bibligraphy -->
