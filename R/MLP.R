#' MLP
#'
#' Modul-Funktion für das Musical Learning Protocol (MLP) zur Einbindung in
#' eine psychTestR-Timeline oder eine Batterie. Für eigenständige
#' Testsitzungen siehe \code{\link{MLP_standalone}()}.
#' Hand gebaut (nicht \code{main_test()}), da MLP mehrstufige Items
#' (individuums-/norm-/kriterienbezogen: L1/L2/L3), einen bedingten
#' Instrumenten-Block und eine bedingte Sozio-Demografie-Sektion enthält.
#'
#' @param label (Character scalar) Label für die Ergebnisse in der Ausgabedatei.
#' @param dict (i18n_dict) psyquest-Dictionary für Internationalisierung.
#' @param ... Weitere Argumente.
#' @export
MLP <- function(label = "MLP", dict = psyquest::psyquest_dict, ...) {

  # Ein "Item" -> 3 NAFC-Seiten (L1 individuumsbezogen, L2 normbezogen,
  # L3 kriterienbezogen), siehe MLP_structure.txt "item_page logic".
  mlp_item_pages <- function(item_id) {
    lapply(c("L3", "L2", "L1"), function(lvl) {
      prefix <- paste0("TMLP_", item_id, "_", lvl)
      psychTestR::NAFC_page(
        label       = prefix,
        prompt      = psychTestR::i18n(paste0(prefix, "_PROMPT")),
        choices     = paste0("CHOICE", 1:5),
        labels      = sapply(1:5, function(k) psychTestR::i18n(paste0(prefix, "_CHOICE", k))),
        save_answer = TRUE,
        arrange_vertically = TRUE,
        button_style = "white-space: normal; word-wrap: break-word; width: 100%; max-width: 100%; text-align: center; height: auto; padding: 10px 14px; line-height: 1.4;"
      )
    })
  }
  mlp_block_pages <- function(item_ids) {
    do.call(c, lapply(item_ids, mlp_item_pages))
  }

  general_items    <- c("0003", "0004")
  technique_items  <- c("0005", "0006", "0007", "0009", "0010", "0011")
  expression_items <- c("0000", "0001")
  automatic_items  <- c("0035", "0036", "0037", "0038")

  instrument_items <- list(
    Woodwind = c("0012", "0013", "0014", "0015", "0016", "0017"),
    Guitar   = c("0018", "0019", "0020", "0021"),
    Brass    = c("0022", "0023", "0024", "0025"),
    Strings  = c("0026", "0027", "0028", "0029", "0030"),
    Piano    = c("0031", "0032", "0033")
  )

  num_validate <- function(answer, ...) {
    if (grepl("^[0-9]+$", trimws(answer))) TRUE
    else "Bitte eine Zahl eingeben. / Please enter a number."
  }
 psychTestR::new_timeline(
  psychTestR::join(
    psychTestR::begin_module(label = label),

    # 1) Welcome
    psychTestR::one_button_page(psychTestR::i18n("TMLP_0001_PROMPT")),

    # 2) Erstausfüllung ja/nein -> steuert Sozio-Demografie-Block am Ende
    psychTestR::NAFC_page(
      label   = "firsttime",
      prompt  = psychTestR::i18n("TMLP_FIRSTTIME_PROMPT"),
      choices = c("CHOICE1", "CHOICE2"),
      labels  = c(psychTestR::i18n("TMLP_FIRSTTIME_CHOICE1"),
                  psychTestR::i18n("TMLP_FIRSTTIME_CHOICE2")),
      arrange_vertically = TRUE,
      button_style = "white-space: normal; word-wrap: break-word; width: 100%; max-width: 100%; text-align: left; height: auto; padding: 10px 14px; line-height: 1.4;",
      on_complete = function(answer, state, ...) {
        psychTestR::set_local("first_time", answer == "CHOICE1", state)
      }
    ),

    # 13-14) Sozio-Demografie, nur bei Erstausfüllung
    psychTestR::conditional(
      test  = function(state, ...) isTRUE(psychTestR::get_local("first_time", state)),
      logic = psychTestR::join(
        psychTestR::text_input_page(
          label    = "pupil_age",
          prompt   = psychTestR::i18n("TMLP_AGE_PROMPT"),
          validate = num_validate
        ),
        psychTestR::text_input_page(
          label  = "pupil_instrument",
          prompt = psychTestR::i18n("TMLP_INSTRUMENT_PROMPT")
        )
      )
    ),
    psychTestR::one_button_page(psychTestR::i18n("TMLP_0039_INTRO_OVERALL_PROMPT")),
    # general
    mlp_block_pages(general_items),
    psychTestR::one_button_page(psychTestR::i18n("TMLP_0040_INTRO_TECHNIQUE_PROMPT")),
    # technique
    mlp_block_pages(technique_items),
    psychTestR::one_button_page(psychTestR::i18n("TMLP_0041_INTRO_EXPRESSION_PROMPT")),
    # expression
    mlp_block_pages(expression_items),

    # 8) Instrumentengruppe
    psychTestR::NAFC_page(
      label   = "instrument_group",
      prompt  = psychTestR::i18n("TMLP_INSTRUMENT_GROUP_PROMPT"),
      choices = paste0("CHOICE", 1:5),
      labels  = sapply(1:5, function(k) psychTestR::i18n(paste0("TMLP_INSTRUMENT_GROUP_CHOICE", k))),
      arrange_vertically = TRUE,
      button_style = "white-space: normal; word-wrap: break-word; width: 100%; max-width: 100%; text-align: left; height: auto; padding: 10px 14px; line-height: 1.4;",
      on_complete = function(answer, state, ...) {
        group <- c(CHOICE1 = "Woodwind", CHOICE2 = "Brass", CHOICE3 = "Piano",
                   CHOICE4 = "Guitar", CHOICE5 = "Strings")[[answer]]
        psychTestR::set_local("instrument_group", group, state)
      }
    ),

    # 9) instrumentenspezifische Items (nur die gewählte Gruppe)
    psychTestR::conditional(
      test  = function(state, ...) identical(psychTestR::get_local("instrument_group", state), "Woodwind"),
      logic = psychTestR::join(
        psychTestR::one_button_page(psychTestR::i18n("TMLP_0042_INTRO_INSTRUMENT_PROMPT")),
        mlp_block_pages(instrument_items[["Woodwind"]])
      )
    ),
    psychTestR::conditional(
      test  = function(state, ...) identical(psychTestR::get_local("instrument_group", state), "Brass"),
      logic = psychTestR::join(
        psychTestR::one_button_page(psychTestR::i18n("TMLP_0042_INTRO_INSTRUMENT_PROMPT")),
        mlp_block_pages(instrument_items[["Brass"]])
      )
    ),
    psychTestR::conditional(
      test  = function(state, ...) identical(psychTestR::get_local("instrument_group", state), "Piano"),
      logic = psychTestR::join(
        psychTestR::one_button_page(psychTestR::i18n("TMLP_0042_INTRO_INSTRUMENT_PROMPT")),
        mlp_block_pages(instrument_items[["Piano"]])
      )
    ),
    psychTestR::conditional(
      test  = function(state, ...) identical(psychTestR::get_local("instrument_group", state), "Guitar"),
      logic = psychTestR::join(
        psychTestR::one_button_page(psychTestR::i18n("TMLP_0042_INTRO_INSTRUMENT_PROMPT")),
        mlp_block_pages(instrument_items[["Guitar"]])
      )
    ),
    psychTestR::conditional(
      test  = function(state, ...) identical(psychTestR::get_local("instrument_group", state), "Strings"),
      logic = psychTestR::join(
        psychTestR::one_button_page(psychTestR::i18n("TMLP_0042_INTRO_INSTRUMENT_PROMPT")),
        mlp_block_pages(instrument_items[["Strings"]])
      )
    ),

    # 10) automatic-controlled
    mlp_block_pages(automatic_items),

    # 11-12) Übungszeit, Minuten mit Validierung
    psychTestR::text_input_page(
      label    = "practice_pupil_min",
      prompt   = psychTestR::i18n("TMLP_PRACTICE_PUPIL_PROMPT"),
      validate = num_validate
    ),
    psychTestR::text_input_page(
      label    = "practice_teacher_min",
      prompt   = psychTestR::i18n("TMLP_PRACTICE_TEACHER_PROMPT"),
      validate = num_validate
    ),

    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::end_module()
  ),
  dict = dict
 )
}
