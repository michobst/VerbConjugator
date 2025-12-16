#idk why git hates me, but here is the shiny app with my r code in action. 
#https://micho.shinyapps.io/JapaneseVerbConj/

#Not to many Books in this Library
library(stringr)
library(shiny)

#Defining catagories of irregular verbs
#I know you said we didnt have to do this but... my OCD would not let me not do this
#My fingers are crossed that everything will work right. 
#irregular are just weird, like most irregular they just dont follow patterns
irregular <- c("する", "くる", "ある", "いく", "です")

#I am listing abnormal ones because in kanji they would be differenciated between either ru verbs or
#u verbs but with only hiragana they can be conjugated both ways
abnormal <- c("いる", "きる", "かえる")

#Ru verbs end with either -iru or -eru (anything in the i/e column of hiragana chart)
ruverb <- "[いきしちにひみりぎじぢびえけせてねへめれげぜでべ]る$"

#Using my limited knowledge I know the two categories of verbs have some that look like eachother
#A quick google gives the list of ru look alikes 
#AKA those that end in -iru or -eru but are NOT ru verbs
ru_liars <- c("はいる", "はしる", "しる" , "しゃべる", "のこる", "まじる", "なる", "へる", 
"あせる", "ほてる", "ふせる", "ひねる", "ぬれる", "すべる", "せる", "もつ", "もてる")

#When I first wrote this I realized that compound words like benkyousuru did not register as irregular
#added the first if statement to group those with irregulars. I am sure there is a way to minimize this but it works in my brain.
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

#I hate the way my code looks. I mean it works... but just the spacing and everything. Is there a proper spaacing for coding? 
#So this was just a classifier so each classification can have its own rules for conjugating. 

#kana chart is used to conjugate u verbs - Really only need the u and i column for basic present and past tense but future proofing 
# a, e, o columns are used for conditional, volitional, potential, more plain/less formal forms, etc 
kana_chart <- data.frame(
    a = c("あ","か","さ","た","な","は","ま","や","ら","わ","が","ざ","だ","ば"),
    i = c("い","き","し","ち","に","ひ","み","","り","","ぎ","じ","ぢ","び"),
    u = c("う","く","す","つ","ぬ","ふ","む","ゆ","る","","ぐ","ず","づ","ぶ"),
    e = c("え","け","せ","て","ね","へ","め","","れ","","げ","ぜ","で","べ"),
    o = c("お","こ","そ","と","の","ほ","も","よ","ろ","を","ご","ぞ","ど","ぼ")
)

View(kana_chart)

#Time to get into the functions of each type of verb, then we will combine them to make the ULTIMATE FUNCTION!!! mwhahaha
#I appologize, this is the extent of my socilization.... 

#need the stem of ru verbs first - easy, remove the ru
#this is also where i realized i am going to spell "function" without the "c" and have to check everytime. no one said i could spell. 
stem_ru <- function(verb) {
    gsub("る$", "", verb)
}

#conjucation for ru verbs
ruconj <- function(verb) {
    stem <- stem_ru(verb)
    list(
        present = paste0(stem, "ます"),
        past = paste0(stem, "ました"),
        negative = paste0(stem, "ません"),
        past_negative = paste0(stem, "ませんでした")
    )
}

#conjucation for u verbs - 
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

#the more i work on this, the more little things i find that need more work. After thinking about compound suru verbs 
#i realized a needed a suru conjugator in my irrergular conjugator. My initial flow chart didnt cover this 
#it was like "irregular verb" > "yes" > "conjugate" *sigh* now here we are :P 
suruconj <- function(verb) {
    noun <- str_sub(verb, 1, -3)
    list( 
        present = paste0(noun,"します"),
        past = paste0(noun, "しました"),
        negative = paste0(noun, "しません"),
        past_negative = paste0(noun, "しませんでした")
        )
}
 
#All my irregular verbs. This could be easily added too. These forms are easy and could use a paridigm like above but short forms
#and plain forms have more irregular spellings. 
#after the first if verb = statement, i really should have copy and pasted... but it took me till the third one before i decided to make it easier
#I also realized how much i hate coding in hiragana as changing between keyboards is totally do able but is a pain the the... yah.  
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

#We are finally to the real stuff! BTW happy holidays :) 
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

#Now watch me do it all over again with romaji. This is also a good time to take a break, drink some water and maybe get up 
#and stretch those .... old bones of yours. 
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

#When i was testing this I couldnt figure out why some conjugations were coming back with "ra" instead of "ri, then i realized
#I cant type and i just had ra where ri should have been.     	
r_kana_chart <- data.frame(
    a = c("a","ka","sa","ta","na","ha","ma","ya","ra","wa","ga","za","da","ba"),
    i = c("i","ki","shi","chi","ni","hi","mi","","ri","","gi","ji","di","bi"),
    u = c("u","ku","su","tsu","nu","fu","mu","yu","ru","","gu","zu","dzu","bu"),
    e = c("e","ke","se","te","ne","he","me","","re","","ge","ze","de","be"),
    o = c("o","ko","so","to","no","ho","mo","yo","ro","wo","go","zo","do","bo")
)
View(r_kana_chart)

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

#BINGO - we have a problem. str_sub only removes number of characters, works perfect with hiragana but with romaji we have varying numbers
#have to adjust this to make it work right 
#r_uconj <- function(verb) {
#    r_ending <- str_sub(verb, -1)
#    r_s <- str_sub(verb, 1, -2)
#    r_row <- r_kana_chart[r_kana_chart$u == r_ending, ]
#    
#    list(
#        present = paste0(r_s, r_row$i, "masu"),
#        past = paste0(r_s, r_row$i, "mashita"),
#        negative = paste0(r_s, r_row$i, "masen"),
#        past_negative = paste0(r_s, r_row$i, "masendeshita")
#    )
#}

#so I just manually searched for each ending and matched that to the chart making sure that the longest options came first and ending with u 
#otherwise u would just show up first all the time
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

#in this case because it is always a suru we can us the last 4 characters always
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

#this is my favorite part, seeing it all come together everytime. 
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

#This just checks what type of writing it is and returns kana, romaji, or unnknown if its a mix of things
orthography <- function(verb) {
    if (grepl("^[あ-んア-ン]+$", verb)) {
        return("kana")
    }
    if (grepl("^[a-zA-Z]+$", verb)) {
        return("romaji")
    }
    	return("unknown")
}

#realized after a few test runs that if you end in something other then u it spits out weird errors, need to make sure its actually ending in u. 
#this function checks if romaji ends in u, or if kana ends in any in the u column 
#I swear every problem i encountered i just tagged on a new function
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


#this is the function that does it all. 
theultimateconjugator <- function(verb) {
    ortho <- orthography(verb)
    if (ortho == "kana") {
    	if (!is_valid_uverb(verb, "kana")) {
    	return("Cannot conjugate: Input must be in dictionary form")}
        return(conjugate_kana(verb))
    }
    if (ortho == "romaji") {
    	verb <- tolower(verb)
    	if (!is_valid_uverb(verb, "romaji")) {
    	return("Cannot conjugate: Input must be in dictionary form")}
        return(conjugate_romaji(verb))
    }
    if (ortho == "unknown") {
        return("Input incorrect. Please use hiragana or romaji.")
    }
}

#going thru the shiny package functions along with a google of useful functions these are what was needed to make an 
#input box and button for conjugating  
#after someone suggested an "imput" warning :P I wanted to make that a seperate line and if I just used the label of textInput
#it reads it all as one line, so i litterally googled "how to seperate text above a text box in rstudio with shiny package" 
#div() was the best function that worked that didnt crash the program
div("USE DICTIONARY FORM ONLY PLEASE (ending in u/う or ru/る)")
div("Enter verb (kana or romaji):")

textInput("verb_input", label = NULL, value = "")

actionButton("conjugate_button", "Conjugate")

verbatimTextOutput("conjugation_output")


#again most of these functions were found in the shiny package help as well as google. It just runs the code only when the button is clicked
#it takes the input and turns it into the string that is ran thru the function. 
#the output was the most difficult part. 
#if it is a normal paradigm and only has the 4 results, it renders it with proper spacing, ":" and removes the "_" 
#if it is an ambiguous verb and has the potential for 2 paradigms, it renders both with headers for each also making it look pretty. 
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


#This works with hiragana, or romaji. It is easily updated with more conjugations, could easily add te form or short forms etc.. 

##Test verbs include たべる,　かえる,　いく,　きく,　からう,　かく,　くる,　ダンスする, taberu, kaeru, iku, kiku, karau, kaku, kuru, yomu, also tested for capitals, mixed words
#with both hiragana and romaji 

#this is also me saying thank you for a good class, again and happy holidays... again. 