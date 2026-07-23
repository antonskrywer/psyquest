# data_raw/prepare_MLP_from_xlsx.R
# Einmaliges Konvertierungsskript. Liest MLP_Itembank_3Ebenen.xlsx ein und erzeugt
#   data_raw/item_banks/MLP_item_bank.csv
#   data_raw/dicts/MLP_dict.csv
# Danach normal weiter mit Schritt 4 des psyquest-Skills:
#   source("data_raw/item_bank_generator.R"); source("data_raw/dict_generator.R")

library(readxl)
library(dplyr)
library(stringr)
library(readr)

xlsx_path <- "C:/Users/Anton/Nextcloud/Promotion/JMS/MLP/data_raw/MLP_Itembank_3Ebenen.xlsx"  # Pfad ggf. anpassen
raw <- read_excel(xlsx_path, sheet = "MLP_Itembank_3Ebenen")

# Items, die laut MLP_structure.txt tatsächlich in der finalen MLP vorkommen.
# MLP_0008 fehlt in der Itembank (structure.txt nennt "0005 bis 0011") -> übersprungen.
used_items <- c(
  "MLP_0000", "MLP_0001",                                                    # expression
  "MLP_0003", "MLP_0004",                                                    # overall
  "MLP_0005", "MLP_0006", "MLP_0007", "MLP_0009", "MLP_0010", "MLP_0011",    # technique
  "MLP_0012", "MLP_0013", "MLP_0014", "MLP_0015", "MLP_0016", "MLP_0017",    # woodwind
  "MLP_0018", "MLP_0019", "MLP_0020", "MLP_0021",                            # guitar
  "MLP_0022", "MLP_0023", "MLP_0024", "MLP_0025",                            # brass
  "MLP_0026", "MLP_0027", "MLP_0028", "MLP_0029", "MLP_0030",                # strings
  "MLP_0031", "MLP_0032", "MLP_0033",                                        # piano
  "MLP_0035", "MLP_0036", "MLP_0037", "MLP_0038"                             # automatic_controlled
)

# Keys der vier neuen Intro-/Break-Seiten (kein L1/L2/L3, kein PROMPT/CHOICE-Suffix,
# daher separat behandelt statt über used_items/kind-Regex)
intro_keys <- c(
  "MLP_0039_INTRO_OVERALL",
  "MLP_0040_INTRO_TECHNIQUE",
  "MLP_0041_INTRO_EXPRESSION",
  "MLP_0042_INTRO_INSTRUMENT"
)

raw <- raw_all %>%
  mutate(
    item  = str_extract(key, "^MLP_\\d{4}"),
    level = str_extract(key, "L[123]"),
    kind  = str_extract(key, "(PROMPT|CHOICE\\d+)$")
  ) %>%
  filter(item %in% used_items)

item_order <- tibble(item = used_items, item_rank = seq_along(used_items))

# --- 1) Item-Bank-CSV ---------------------------------------------------
# main_id-Schema: TMLP_<item>_<level>  (z.B. TMLP_0003_L1)
# Zusätzliche Spalte "level" (optional, s. psyquest-Skill Abschnitt 2/6) für
# eigenes Postprocessing, falls L1/L2/L3 getrennt ausgewertet werden sollen.
item_bank <- raw %>%
  filter(kind == "PROMPT") %>%
  distinct(item, level, subscale) %>%
  left_join(item_order, by = "item") %>%
  arrange(item_rank, match(level, c("L1", "L2", "L3"))) %>%
  mutate(main_id = paste0("TMLP_", str_remove(item, "MLP_"), "_", level)) %>%
  tidyr::crossing(language = c("de", "en")) %>%
  transmute(
    main_id, language,
    template   = "5-option multiple choice",
    score_func = "function(x) x+0",   # Annahme: CHOICE1 (schlechtester) -> CHOICE5 (bester) durchgehend aufsteigend
    subscales  = subscale,
    layout     = "vertical",
    level
  )
dir.create("data_raw/item_banks", recursive = TRUE, showWarnings = FALSE)
dir.create("data_raw/dicts", recursive = TRUE, showWarnings = FALSE)
write_delim(item_bank, "data_raw/item_banks/MLP_item_bank.csv", delim = ";", quote = "all")

# --- 2) Dictionary-CSV ---------------------------------------------------
item_dict <- raw %>%
  transmute(
    key = paste0("TMLP_", str_remove(item, "MLP_"), "_", level, "_", kind),
    de, en
  )

# Intro-/Break-Texte vor den vier Item-Blöcken (Overall/Technique/Expression/Instrument)
intro_dict <- raw_all %>%
  filter(key %in% intro_keys) %>%
  transmute(
    key = paste0("TMLP_", str_remove(key, "^MLP_"), "_PROMPT"),
    de, en
  )
# Custom-Texte für die nicht-Item-Seiten (Titel, Intro, Meta-/Sozio-Fragen)
custom_dict <- tibble::tribble(
  ~key,                             ~de,                                                                       ~en,
  "TMLP_0000_PROMPT",               "Musical Learning Protocol",                                               "Musical Learning Protocol",
  "TMLP_0001_PROMPT",               "Willkommen zum Musical Learning Protocol (MLP).",                         "Welcome to the Musical Learning Protocol (MLP).",
  "TMLP_FIRSTTIME_PROMPT",          "Ist dies das erste Mal, dass Sie das MLP ausfüllen?",                     "Is this the first time you fill out the MLP?",
  "TMLP_FIRSTTIME_CHOICE1",         "Ja",                                                                       "Yes",
  "TMLP_FIRSTTIME_CHOICE2",         "Nein",                                                                     "No",
  "TMLP_INSTRUMENT_GROUP_PROMPT",   "Welche Instrumentengruppe lernt der/die Schüler/in?",                     "Which instrumental group does the pupil learn?",
  "TMLP_INSTRUMENT_GROUP_CHOICE1",  "Holzblasinstrument",                                                       "Woodwind",
  "TMLP_INSTRUMENT_GROUP_CHOICE2",  "Blechblasinstrument",                                                      "Brass",
  "TMLP_INSTRUMENT_GROUP_CHOICE3",  "Klavier",                                                                  "Piano",
  "TMLP_INSTRUMENT_GROUP_CHOICE4",  "Gitarre",                                                                  "Guitar",
  "TMLP_INSTRUMENT_GROUP_CHOICE5",  "Streichinstrument",                                                        "Strings",
  "TMLP_PRACTICE_PUPIL_PROMPT",     "Frage die Schülerin/den Schüler: Wie viele Minuten wurde in der letzten Woche durchschnittlich pro Tag geübt?", "Ask the pupil: On average, how many minutes per day did he or she practice in the last week?",
  "TMLP_PRACTICE_TEACHER_PROMPT",   "Wie viele Minuten pro Tag, schätzen Sie, hat der/die Schüler/in geübt?", "For how many minutes per day do you think the pupil practiced?",
  "TMLP_AGE_PROMPT",                "Wie alt ist der/die Schüler/in?",                                         "How old is the pupil?",
  "TMLP_INSTRUMENT_PROMPT",         "Welches Instrument lernt der/die Schüler/in?",                            "What instrument is the pupil learning?"
)

dict <- bind_rows(item_dict, custom_dict) %>% filter(!is.na(de), !is.na(en))
write_delim(dict, "data_raw/dicts/MLP_dict.csv", delim = ";", quote = "all")