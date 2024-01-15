# ARTICLE TITLE:  Core concept: defining cores and core reduction sequences in Australian archaeology

# AUTHOR: Simon Wyatt-Spratt

# JOURNAL: Journal of Paleolithic Archaeology

# SCRIPT AUTHOR: Simon Wyatt-Spratt

# SCRIPT CONTACT: simon.wyatt-spratt@sydney.edu.au

# ACKNOWLEDGEMENTS:

# LAST EDITED: 11/01/2024

# ABSTRACT
# One of the biggest impediments to the analysis of cores and reduction strategies in the archaeology 
# of Indigenous Australia is the slippery use of terminology. This impares comparative analyses and 
# limits our  ability to extrapolate meaningful information from our lithic analyses. This situation 
# has resulted from a history of poorly articulated theoretical frameworks. These frameworks impact 
# both the way that archaeologists have interpreted the results of lithic analysis, but more
# fundamentally how they practise lithic analysis itself. While there has been some discussion and 
# debate about the interpretative frameworks used to interpret the results of lithic analysis, the 
# theoretical underpinnings of lithic analysis in Australia have only occasionally been interrogated. 
# This has led to a situation where the same terminology for basic concepts in lithic analysis is used
# across Australia, but with different meanings depending on the archaeologist who is using it. This 
# paper reviews the different theoretical approaches archaeologists have  used when interpreting 
# Indigenous Australian stone artefact assemblages. The aim of this paper is to explore how the use of 
# different theoretical frameworks has resulted in 1) fundamentally different interpretations of the 
# lithic record, and 2) the same terminology for cores and sequence models being used in subtly but 
# significantly different ways.

# SYSTEM INFORMATION
# R version 4.3.2 (2023-10-31)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042.928)

# ATTACHED BASE PACKAGES:
# [1] stats     graphics  grDevices utils     datasets 
# [6] methods   base  

# SCRIPT

# KNOWN ISSUES
# None.

# INSTALL AND ACTIVIATE PACKAGES

if(!require("cowplot")) install.packages('cowplot', repos ='http://cran.us.r-project.org') # cowplot 1.1.2
if(!require("egg")) install.packages('egg', repos ='http://cran.us.r-project.org') # egg 0.4.5
if(!require("ggpubr")) install.packages("ggpubr", repos = "http://cran.us.r-project.org") # ggpubr 0.6.0
if(!require("tidyverse")) install.packages("tidyverse", repos = "http://cran.us.r-project.org") # tidyverse 2.0.0
if(!require("viridis")) install.packages("viridis", repos = "http://cran.us.r-project.org") # viridis 0.6.4

library(cowplot)    # load cowplot package
library(egg)        # load egg package
library(ggpubr)     # load ggpubr package
library(tidyverse)  # load tidyverse package
library(viridis)    # load tidyverse package

# GET WORKING DIRECTORY
getwd() # show working directory

# IMPORT DATA

# import *.csv data
core.definitions      <- read.csv("data/core_definitions.csv", 
                                  header = TRUE)
# PREPARE DATA

core.definitions <- core.definitions %>% 
  mutate(decade = as.numeric(year) - as.numeric(year) %% 10) %>% # adds decades
  filter(source_type != "thesis_honours") %>% # removes sources that are Honours theses
  filter(!term %in% c("amorphous flaked piece", 
                      "block",
                      "flaked piece", 
                      "flaked piece A",
                      "flaked piece B",
                      "fragment",
                      "implement",
                      "na",
                      "waste")) # removes unwanted terms from dataset

dataset.australia           <- filter(core.definitions, region != "global") # removes non-Australian definitions from the dataset

dataset.australia.defined   <- filter(dataset.australia, defined == "yes")   # retains only sources with explicit definitions

dataset.australia.cites     <- filter(dataset.australia, defined == "cites") # retains only sources with cited definitions

dataset.australia.undefined <- filter(dataset.australia, defined == "no")   # retains only sources with undefined or uncited terms

dataset.category    <- filter(dataset.australia.defined, type == "category") %>% 
  filter(term %in% c("core",
                     "core or nucleus",
                     "coroid", 
                     "nucleus",
                     "objective piece")) # creates a dataset that exclusively has terms for core or semantically similar equivalent

dataset.subcategory <- filter(dataset.australia.defined, type == "subcategory") # creates a dataset that exclusively has subcategories of artefacts

dataset.cited       <- filter(core.definitions, definitions_cited != "na") %>%
  distinct(reference, decade, term, defined, definitions_cited, .keep_all = FALSE)

# ANALYSIS

n_distinct(dataset.australia$reference)           # number of unique references which refers to cores
n_distinct(dataset.australia.defined$reference)   # number of unique references with a core definition
n_distinct(dataset.australia.cites$reference)     # number of unique references with a definition is cited
n_distinct(dataset.australia.undefined$reference) # number of unique references where a core term is used but not defined

# FIGURE 1a.
# Number of sources published per decade on Indigenous Australian lithic technology, with either a 
# "core" category and subcategory definition.

australia.data         <- select(dataset.australia.defined, "decade", "reference") %>% 
  distinct(reference, decade, .keep_all = FALSE) # keeps only a single instance of each source

australia.count        <- australia.data %>% 
  count(decade, reference) %>% 
  rename(reference_count = n)

australia.count.total  <- australia.data %>% # count overall number of publications per decade
  count(decade) %>%
  rename(decade_count = n)

figure_1a <- ggplot(
  data = australia.count.total,
  aes(x = decade, y = decade_count)) +
  geom_col() +
  theme_minimal() +
  labs(x = "Decade published", y = "Number of publications", size = 8) +
  scale_x_continuous(limits = c(1890, 2030), 
                     breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970, 
                                1980, 1990, 2000, 2010, 2020)) +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5 ),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank())
figure_1a

# FIGURE 1b.
# Number of definitions published per decade. Definitions were divided into categories and 
# subcategories.

data.type        <- select(dataset.australia.defined, "reference", "decade", "defined", "type") %>%
  filter(defined == "yes") %>%
  filter(type != "na") # filters out "na"

type.count       <- data.type %>% count(decade, type) %>% rename(type_count = n)

figure_1b <- ggplot(
  data = type.count,
  aes(x = decade, y = type_count, fill = type)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  theme_minimal() +
  labs(x = "Decade published", y = "Number of definitions", fill = "Definition type", size = 8) +
  scale_x_continuous(limits = c(1890, 2030), 
                     breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970,
                                1980, 1990, 2000, 2010, 2020)) +
  scale_fill_viridis(discrete = TRUE, labels=c("category", "subcategory")) +
  theme(axis.title.x = element_text(size = 8),
        axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5 ),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        panel.grid.minor = element_blank())
figure_1b

figure_1  <- plot_grid(figure_1a, figure_1b, 
                       labels = c('a', 'b'), 
                       label_size = 10,
                       hjust = -0.075,
                       ncol = 1)
figure_1

ggsave2("results/figure_1.tiff", 
        plot = figure_1, 
        scale = 1, 
        dpi = 400)

# FIGURE 2.
# Materialist vs typological over time

term.mattyp  <- select(dataset.category, "decade", "materialist_typological")

count.mattyp <- term.mattyp %>% 
  count(decade, materialist_typological) %>% 
  rename(mattyp_count = n)

figure_2 <- ggplot(
  data = count.mattyp,
  aes(x = decade, y = mattyp_count, fill = materialist_typological)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  theme_minimal() +
  labs(x = "Decade published", y = "Number of definitions", fill = "Definition Type", size = 12) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_continuous(limits = c(1890, 2030), 
                     breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970,
                                1980, 1990, 2000, 2010, 2020)) +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(angle = 90, size = 9, vjust = 0.5 ),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        legend.position = "bottom",
        panel.grid.minor = element_blank())
figure_2

ggsave2("results/figure_2.tiff", 
        plot = figure_2, 
        scale = 1, 
        width = 180, 
        height = 90, 
        units = "mm", 
        dpi = 400)

# FIGURE 3.

# FIGURE 3a.
# Core category definition components per decade. Note that a definition could be composed of multiple 
# components. Only explicit components were included. No category definitions contained an emic or raw
# material component.

count.cat.emic          <- filter(dataset.category, emic_component %in% c("explicit","implicit")) %>% # filters out definitions with no emic component
  count(decade, emic_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% emic_component) "emic" else "na") %>%
  filter(emic_component == "explicit")

count.cat.functional    <- filter(dataset.category, functional_component %in% c("explicit","implicit")) %>% # filters out definitions with no functional component
  count(decade, functional_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% functional_component) "functional" else "na") %>%
  filter(functional_component == "explicit")

count.cat.morphological <- filter(dataset.category, morphological_component %in% c("explicit","implicit")) %>% # filters out definitions with no morphological component
  count(decade, morphological_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% morphological_component) "morphological" else "na") %>%
  filter(morphological_component == "explicit")

count.cat.rm            <- filter(dataset.category, raw_material_component %in% c("explicit","implicit")) %>% # filters out definitions with no raw material component
  count(decade, raw_material_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% raw_material_component) "raw material" else "na") %>%
  filter(raw_material_component == "explicit")

count.cat.technological <- filter(dataset.category, technological_component %in% c("explicit","implicit")) %>% # filters out definitions with no technological component
  count(decade, technological_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% technological_component) "technological" else "na") %>%
  filter(technological_component == "explicit")

count.cat.indcom        <- bind_rows(count.cat.emic, count.cat.functional, count.cat.morphological, 
                                     count.cat.rm, count.cat.technological) %>%
  select(decade, count, component)

figure_3a <- ggplot(
  data = count.cat.indcom,
  aes(x = decade, y = count, fill = component)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  theme_minimal() +
  labs(x = "Decade published", y = "No. components", fill = "Component", size = 8) +
  scale_x_continuous(limits = c(1890, 2030), 
                     breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970,
                                1980, 1990, 2000, 2010, 2020)) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5 ),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank())
figure_3a

# FIGURE 3b.
# Core subcategory definition components per decade. Note that a definition could be  composed of 
# multiple components. Only explicit components were included. No subcategory definitions contained an
# emic component.

count.sub.emic          <- filter(dataset.subcategory, emic_component %in% 
                                    c("explicit", "implicit")) %>% # filters out definitions with no emic component
  count(decade, emic_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% emic_component) "emic" else "na") %>%
  filter(emic_component == "explicit")

count.sub.functional    <- filter(dataset.subcategory, functional_component %in% 
                                    c("explicit", "implicit")) %>% # filters out definitions with no functional component
  count(decade, functional_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% functional_component) "functional" else "na") %>%
  filter(functional_component == "explicit")

count.sub.morphological <- filter(dataset.subcategory, morphological_component %in% 
                                    c("explicit", "implicit")) %>% # filters out definitions with no morphological component
  count(decade, morphological_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% morphological_component) "morphological" else "na") %>%
  filter(morphological_component == "explicit")

count.sub.rm            <- filter(dataset.subcategory, raw_material_component %in% 
                                    c("explicit","implicit")) %>% # filters out definitions with no raw material component
  count(decade, raw_material_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% raw_material_component) "raw material" else "na") %>%
  filter(raw_material_component == "explicit")

count.sub.technological <- filter(dataset.subcategory, technological_component %in% 
                                    c("explicit","implicit")) %>% # filters out definitions with no technological component
  count(decade, technological_component, type) %>%
  rename(count = n) %>%
  mutate(component = if("explicit" %in% technological_component) "technological" else "na") %>%
  filter(technological_component == "explicit")

count.sub.indcom        <- bind_rows(count.sub.emic, count.sub.functional, count.sub.morphological, 
                                     count.sub.rm, count.sub.technological) %>%
  select(decade, count, component)

figure_3b <- ggplot(
  data = count.sub.indcom,
  aes(x = decade, y = count, fill = component)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  theme_minimal() +
  labs(x = "Decade published", y = "No. components", size = 8) +
  scale_x_continuous(limits = c(1890, 2030), 
                     breaks = c(1900, 1910, 1920, 1930, 1940, 1950, 1960, 1970,
                                1980, 1990, 2000, 2010, 2020)) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 8),
        axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5 ),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank())
figure_3b

figure_3 <- ggarrange(figure_3a, figure_3b,
                      labels = c('a', 'b'),
                      ncol = 1,
                      common.legend = TRUE, 
                      legend = "bottom")
figure_3

ggsave2("results/figure_3.tiff", 
        plot = figure_3, 
        scale = 1, 
        dpi = 400)

# TABLE 1.
# Unique sources divided by publication type. Sources had to contain an explicit definition of a core
# category or subcategory.

n_distinct(dataset.australia.defined$reference) # number of unique references in dataset

data.source      <- select(dataset.australia.defined, "reference", "decade", "source_type") %>% 
  distinct(reference, source_type, .keep_all = FALSE)

count.source     <- data.source %>% 
  count(source_type) %>% 
  rename(source_count = n) %>%
  arrange(desc(source_count))

write.csv(count.source, "results/core_concept_table1.csv", row.names = TRUE)

# TABLE 2. 
# Commonly defined categories of cores in the analysis of Indigenous Australian lithic technology.

n_distinct(dataset.category$reference)  # number of unique references in category dataset
n_distinct(dataset.category$definition) # number of unique definitions in category dataset

count.category <- dataset.category %>% 
  count(term) %>%
  rename(cat_count = n) %>%
  arrange(desc(cat_count))

write.csv(count.category, "results/core_concept_table2.csv", row.names = TRUE)

# TABLE 3. 
# Commonly defined subcategories of cores in the analysis of Indigenous Australian lithic 
# technology. All terms with ≥5 occurrences are shown. Terms have been slightly standardised. This was 
# done to ensure consistent spellings and punctuation (e.g. multi-platform and multiplatform), rather 
# than to group similar but different terms (e.g. multiple platform core and multiplatform core).

term.sub  <- select(dataset.subcategory, "reference", "decade", "term", "definition") %>% 
  mutate(term = str_replace_all(term, "\\*|\\(|\\)", "")) %>%
  mutate(term = str_replace_all(term, "bi-", "bi")) %>%
  mutate(term = str_replace_all(term, "core-tool", "core tool")) %>%
  mutate(term = str_replace_all(term, "core-", "core")) %>%
  mutate(term = str_replace_all(term, "flake that has been used as a core", "flake core")) %>%
  mutate(term = str_replace_all(term, "horsehoof core subtype 1b", "horsehoof core")) %>% 
  mutate(term = str_replace_all(term, "multi-", "multi")) %>%
  mutate(term = str_replace_all(term, "single-platform", "single platform")) %>%
  mutate(term = str_replace_all(term, "uni-", "uni")) %>%
  mutate(term = str_replace_all(term, "utilized", "utilised"))

n_distinct(term.sub$reference)  # number of unique references in subcategory dataset
n_distinct(term.sub$definition) # number of unique definitions in subcategory dataset

count.subcategory <- term.sub %>% 
  count(term) %>%
  rename(sub_count = n) %>%
  arrange(desc(sub_count))

write.csv(count.subcategory, "results/core_concept_table3.csv", row.names = TRUE)

# TABLE 4.
# Combination of definition components across the category dataset.

count.component.cat <- dataset.category %>% count(pick(contains("component")), sort = TRUE) %>%
  mutate(percentage = n/sum(n))
write.csv(count.component.cat , "results/core_concept_table4.csv", row.names = TRUE)

# TABLE 5.
# Combination of definition components across the subcategory dataset.

count.component.sub <- dataset.subcategory %>% count(pick(contains("component")), sort = TRUE) %>%
  mutate(percentage = n/sum(n))
write.csv(count.component.sub , "results/core_concept_table5.csv", row.names = TRUE)

