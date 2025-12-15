---
title: "Japanese Verb Conjugator"
author: "Michelle Obst"
date: "2025-12-15"
output: html_document
runtime: shiny
---

#```{r setup, include=FALSE}
library(shiny)
library(stringr)

irregular <- c("する", "くる", "ある", "いく", "です")

abnormal <- c("いる", "きる", "かえる")

ruverb <- "[いきしちにひみりぎじぢびえけせてねへめれげぜでべ]る$"

ru_liars <- c("はいる", "はしる", "しる" , "しゃべる", "のこる", "まじる", "なる", "へる", 
"あせる", "ほてる", "ふせる", "ひねる", "ぬれる", "すべる", "せる", "もつ", "もてる")

kana_chart <- data.frame(
    a = c("あ","か","さ","た","な","は","ま","や","ら","わ","が","ざ","だ","ば"),
    i = c("い","き","し","ち","に","ひ","み","","り","","ぎ","じ","ぢ","び"),
    u = c("う","く","す","つ","ぬ","ふ","む","ゆ","る","","ぐ","ず","づ","ぶ"),
    e = c("え","け","せ","て","ね","へ","め","","れ","","げ","ぜ","で","べ"),
    o = c("お","こ","そ","と","の","ほ","も","よ","ろ","を","ご","ぞ","ど","ぼ")
)

#View(kana_chart)

WTFverb <- function(verb)
{if (grepl("する$", verb)) {
    return("irregular")
	}
if (verb %in% irregular)
{return("irregular")}
    if (verb %in% abnormal)
    {return("abnormal")
    	}
if (grepl(ruverb, verb))
    {if (verb %in% ru_liars) {return("uverb")} 
        else {return("ruverb")}}
        
    return("uverb")
}

stem_ru <- function(verb) {
    gsub("る$", "", verb)
}


ruconj <- function(verb) {
    stem <- stem_ru(verb)
    list(
        present = paste0(stem, "ます"),
        past = paste0(stem, "ました"),
        negative = paste0(stem, "ません"),
        past_negative = paste0(stem, "ませんでした")
    )
}

uconj <- function(verb) {
    ending <- str_sub(verb, -1)
    s <- str_sub(verb, 1, -2)
    row <- kana_chart[kana_chart$u == ending, ]
    
    list(
        present = paste0(s, row$i, "ます"),
        past = paste0(s, row$i, "ました"),
        negative = paste0(s, row$i, "ません"),
        past_negative = paste0(s, row$i, "ませんでした")
    )
}

suruconj <- function(verb) {
    noun <- str_sub(verb, 1, -3)
    list( 
        present = paste0(noun,"します"),
        past = paste0(noun, "しました"),
        negative = paste0(noun, "しません"),
        past_negative = paste0(noun, "しませんでした")
        )
}

irrconj <- function(verb) {
    if (grepl("する$", verb)) {
        return(suruconj(verb))
    }
    if (verb == "くる") {
        return(list(
            present = "きます",
            past = "きました",
            negative = "きません",
            past_negative = "きませんでした"
        ))
    }
    if (verb == "いく") {
        return(list(
            present = "いきます",
            past = "いきました",
            negative = "いきません",
            past_negative = "いきませんでした"
        ))
    }
    if (verb == "ある") {
        return(list(
            present = "あります",
            past = "ありました",
            negative = "ありません",
            past_negative = "ありませんでした"
        ))
    }
    if (verb == "です") {
        return(list(
            present = "です",
            past = "でした",
            negative = "ではありません",
            past_negative = "ではありませんでした"
        ))
    }
}

conjugate_kana <- function(verb) {
    type <- WTFverb(verb)
    if (type == "ruverb") {
        return(ruconj(verb))
    } else 
        if (type == "uverb") {
        return(uconj(verb))
    } else 
        if (type == "irregular") {
        return(irrconj(verb))
    } else 
        if (type == "abnormal") {
        return(list(
            message = paste("This verb is ambiguous.",
            "Conjugation depends on meaning."),
            ruverb = ruconj(verb),
            uverb = uconj(verb)
        ))
    }
}

r_irregular <- c("suru", "kuru", "aru", "iku", "desu")
r_abnormal <- c("iru", "kiru", "kaeru")
r_ruverb <- "(i|e)ru$"
r_ru_liars <- c("hairu", "hashiru", "shiru" , "shaberu", "nokoru", "majiru", "naru", "heru", 
"aseru", "hoteru", "fuseru", "hineru", "nureru", "suberu", "seru", "motsu", "moteru")

r_WTFverb <- function(verb)
{if (grepl("suru$", verb)) {
    return("r_irregular")
	}
if (verb %in% r_irregular)
{return("r_irregular")}
    if (verb %in% r_abnormal)
    {return("r_abnormal")
    	}
if (grepl(r_ruverb, verb))
    {if (verb %in% r_ru_liars) {return("r_uverb")} 
        else {return("r_ruverb")}}
        
    return("r_uverb")
    	}
    	
r_kana_chart <- data.frame(
    a = c("a","ka","sa","ta","na","ha","ma","ya","ra","wa","ga","za","da","ba"),
    i = c("i","ki","shi","chi","ni","hi","mi","","ri","","gi","ji","di","bi"),
    u = c("u","ku","su","tsu","nu","fu","mu","yu","ru","","gu","zu","dzu","bu"),
    e = c("e","ke","se","te","ne","he","me","","re","","ge","ze","de","be"),
    o = c("o","ko","so","to","no","ho","mo","yo","ro","wo","go","zo","do","bo")
)
#View(r_kana_chart)

r_stem_ru <- function(verb) {
    gsub("ru$", "", verb)
}

r_ruconj <- function(verb) {
    r_stem <- r_stem_ru(verb)
    list(
        present = paste0(r_stem, "masu"),
        past = paste0(r_stem, "mashita"),
        negative = paste0(r_stem, "masen"),
        past_negative = paste0(r_stem, "masendeshita")
    )
}

r_uconj <- function(verb) {
    r_ending <- str_extract(verb, "(tsu|dzu|ku|su|nu|fu|mu|yu|ru|gu|zu|bu|u)$")
    r_s <- str_remove(verb, "(tsu|dzu|ku|su|nu|fu|mu|yu|ru|gu|zu|bu|u)$")
    r_row <- r_kana_chart[r_kana_chart$u == r_ending, ]
    
    list(
        present = paste0(r_s, r_row$i, "masu"),
        past = paste0(r_s, r_row$i, "mashita"),
        negative = paste0(r_s, r_row$i, "masen"),
        past_negative = paste0(r_s, r_row$i, "masendeshita")
    )
}

r_suruconj <- function(verb) {
    r_noun <- str_sub(verb, 1, -5)
    list( 
        present = paste0(r_noun,"shimasu"),
        past = paste0(r_noun, "shimashita"),
        negative = paste0(r_noun, "shimasen"),
        past_negative = paste0(r_noun, "shimasendeshita")
        )
}

r_irrconj <- function(verb) {
    if (grepl("suru$", verb)) {
        return(r_suruconj(verb))
    }
    if (verb == "kuru") {
        return(list(
            present = "kimasu",
            past = "kimashita",
            negative = "kimasen",
            past_negative = "kimasendeshita"
        ))
    }
    if (verb == "iku") {
        return(list(
            present = "ikimasu",
            past = "ikimashita",
            negative = "ikimasen",
            past_negative = "ikimasendeshita"
        ))
    }
    if (verb == "aru") {
        return(list(
            present = "arimasu",
            past = "arimashita",
            negative = "arimasen",
            past_negative = "arimasendeshita"
        ))
    }
    if (verb == "desu") {
        return(list(
            present = "desu",
            past = "deshita",
            negative = "dewa arimasen",
            past_negative = "dewa arimasendeshita"
        ))
    }
}

conjugate_romaji <- function(verb) {
    r_type <- r_WTFverb(verb)
    if (r_type == "r_ruverb") {
        return(r_ruconj(verb))
    } else 
        if (r_type == "r_uverb") {
        return(r_uconj(verb))
    } else 
        if (r_type == "r_irregular") {
        return(r_irrconj(verb))
    } else 
        if (r_type == "r_abnormal") {
        return(list(
            message = paste("This verb is ambiguous.",
            "Conjugation depends on meaning."),
            ruverb = r_ruconj(verb),
            uverb = r_uconj(verb)
        ))
    }
}

orthography <- function(verb) {
    if (grepl("^[あ-んア-ン]+$", verb)) {
        return("kana")
    }
    if (grepl("^[a-zA-Z]+$", verb)) {
        return("romaji")
    }
    return("unknown")
}

is_valid_uverb <- function(verb, ortho) {
  if (ortho == "romaji") {
    if(str_ends(verb, "ru")) {
      return(TRUE)
  }
    return(str_ends(tolower(verb), "u"))
  }
  if (ortho == "kana") {
    if (str_ends(verb, "る"))
      return(TRUE)
    u_endings <- kana_chart$u[kana_chart$u != ""]
    return(any(str_ends(verb, u_endings)))
  }
  return(FALSE)
}

theultimateconjugator <- function(verb) {
    ortho <- orthography(verb)
    if (ortho == "kana") {
    	if (!is_valid_uverb(verb, "kana")) {
    	return("Cannot conjugate: Imput must be in dictionary form")}
        return(conjugate_kana(verb))
    }
    if (ortho == "romaji") {
    	verb <- tolower(verb)
    	if (!is_valid_uverb(verb, "romaji")) {
    	return("Cannot conjugate: Imput must be in dictionary form")}
        return(conjugate_romaji(verb))
    }
    if (ortho == "unknown") {
        return("Imput incorrect. Please use hiragana or romaji.")
    }
}
#```


#```{r, echo=FALSE}
textInput("verb_input", "Enter verb (kana or romaji):", value = "")

actionButton("conjugate_button", "Conjugate")

verbatimTextOutput("conjugation_output")
#```


#```{r, echo=FALSE}
observeEvent(input$conjugate_button, {
  verb <- input$verb_input
  result <- theultimateconjugator(verb)
  output$conjugation_output <- renderText({
    if (!is.list(result)) {
      return(result)
    }
    if ("message" %in% names(result)) {
      text <- paste0(
        result$message, "\n\n",
        "ru-verb:\n",
        paste(paste(gsub("_", " ", names(result$ruverb)), result$ruverb, sep=": "),collapse="\n"),
        "\n\nu-verb:\n",
        paste(paste(gsub("_", " ", names(result$uverb)), result$uverb, sep=": "),collapse="\n")
      )
        return(text)
      }
        paste(paste(gsub("_", " ", names(result)), result, sep=": "),collapse="\n")
  })
})
#```