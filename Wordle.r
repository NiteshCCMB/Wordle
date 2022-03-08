file_list_to_guess = readline(prompt = "Enter the file name with words to guess : ")
file_list_of_allowed_words = readline(prompt = "Enter the file name with list of allowed words : ")
start_time <- Sys.time()
library(tidyverse)

#making the word list as a matrix of letters, with each word as row and number of letters in word as column
pans = read_delim(file_list_to_guess, delim = "\t", col_names = F)
pans = str_split_fixed(pans$X1, pattern = '', n = str_length(pans[1,1])) %>% as.data.frame()

#List of allowed words from which the guesses will be made
allowed_words = read_delim(file_list_of_allowed_words, delim = "\t", col_names = F)
allowed_words = str_split_fixed(allowed_words$X1, pattern = '', n = str_length(allowed_words[1,1])) %>% as.data.frame()
#List of allowed words where no letter is repeated. This matrix will be used for initial guesses. This helps in getting more info for first couple of guesses
allowed_words_no_repitions = allowed_words[which(apply(allowed_words, 1, FUN = function(x) max(table(x))) == 1),]

word_length = ncol(pans)
#This function compares second word with first word and returns 4 values as a list.
# (1) It returns letters that are present in 2nd word but absent in 1st word.
# (2) It returns letters that are common in both the words.
# (3) It returns a data frame with letter that was present at correct position and its position.
# (4) It returns a data frame with letter that is present but position is not correct, with its corresponding position.
compare_two_words = function(Word_to_guess, guess_word)
{
  absent_letters = guess_word[which(!(guess_word %in% Word_to_guess))]
  present_letters_position = as.numeric(which((guess_word %in% Word_to_guess)))
  present_letters = guess_word[present_letters_position]
  absent_letters = absent_letters[!(absent_letters %in% present_letters)]
  correct_letters_position = as.numeric(which((guess_word == Word_to_guess)))
  present_letters_wrong_position = present_letters_position[which(!(present_letters_position %in% correct_letters_position))]
  if(!(is_empty(correct_letters_position)))
    {correct_letters_table = tibble(letters = t(guess_word[correct_letters_position]), positions = correct_letters_position)}
  else
    {correct_letters_table = tibble(letters = character(), positions = numeric())}
  if(!(is_empty(present_letters_wrong_position)))
    {present_letters_wrong_table = tibble(letters = t(guess_word[present_letters_wrong_position]), positions = present_letters_wrong_position)}
  else
  {present_letters_wrong_table= tibble(letters = character(), positions = numeric())}
  return(list(absent_letters = absent_letters,
              present_letters = present_letters,
              correct_letters_table = correct_letters_table,
              present_letters_wrong_table = present_letters_wrong_table))
}

#It takes a word matrix and reduces the size based on absent letters, present letters, correct letters table and present letters but at wrong position table
reduce_word_matrix = function(word_matrix, absent_letters, present_letters, correct_letters_table, present_letters_wrong_table)
{
  if(!is_empty(absent_letters))
  {
    word_matrix = word_matrix %>% 
      filter(!(if_any(starts_with("V"), ~ . %in% absent_letters)))
  }
  if(!is_empty(present_letters))
  {
    common_index = seq(1,nrow(word_matrix))
    for (present_letter in present_letters)
    {
      common_index = intersect(common_index, which(apply(word_matrix, 1, function(r) any(r == present_letter))))
    }
    word_matrix = word_matrix[common_index,]
  }
  if((nrow(correct_letters_table)>0))
  {
    for (i in c(1:nrow(correct_letters_table)))
    {
      word_matrix = word_matrix %>% filter(.[[correct_letters_table$positions[i]]] == correct_letters_table$letters[i])
    }
  }
  if((nrow(present_letters_wrong_table)>0))
  {
    for (i in c(1:nrow(present_letters_wrong_table)))
    {
      word_matrix = word_matrix %>% filter(!(.[[present_letters_wrong_table$positions[i]]] == present_letters_wrong_table$letters[i]))
    }
  }
  return(word_matrix)
}

success = c()

for (row_count in c(1:nrow(pans)))
  {
  #Word_to_guess = pans[sample(1:nrow(pans),size = 1),]
  Word_to_guess = pans[row_count,]
  print(paste0("########################################################################"))
  print(paste0("Word to Guess : ", paste(Word_to_guess, collapse = "")))
  guess_counter = 0
  #Making a copy of allowed word lists to work on
  allowed_words_copy = allowed_words
  allowed_words_no_repitions_copy = allowed_words_no_repitions
  
  common_letters = c()
  correct_position_table = tibble(letters = character(), positions = numeric())
  absent_letters = c()
  incorrect_position_table = tibble(letters = character(), positions = numeric())
  skip = FALSE
  while(paste(guess_word,collapse = "") != paste(Word_to_guess,collapse = ""))
  {
    guess_counter = guess_counter + 1
    #If even after 6 guesses, we cant find the right word, we will count this as failure and stop further guessing. 
    if(guess_counter == 7)
    {break}
    #This condition will run until a single word is left in allowed_words_no_repitions_copy is left. Here we are trying to gather maximum information for the first few guesses without trying to predict the word.
    #Once we have enough information we will skip this condition. For each guess it removes words containing any letter of the guess until the matrix is empty.
    #For example, For a 5 letter wordle, for first 3 runs, we will have information about 15 unique alphabets. So the minimum guess for a words will be number of times this condition runs (usually 3 for 5 letter wordle)
    #This condition will run irrespective of the word to guess. The real guessing of words, or matching, will start after this condition is false
    if((skip == FALSE))
    {
      guess_word = allowed_words_no_repitions_copy[sample(1:nrow(allowed_words_no_repitions_copy),size = 1),]
      compare_words = compare_two_words(Word_to_guess, guess_word)
      #This is just gathering of information, its not used at the moment but will be used once the skip is set to TRUE
      absent_letters = unique(c(absent_letters, compare_words$absent_letters))
      common_letters = unique(c(common_letters,compare_words$present_letters))
      correct_position_table = correct_position_table %>% bind_rows(compare_words$correct_letters_table %>% filter(!(positions %in% correct_position_table$positions))) %>% arrange(positions)
      incorrect_position_table = incorrect_position_table %>% bind_rows(compare_words$present_letters_wrong_table)
      #Here we reduce the matrix by removing any letters from the guessed word. This insures that for each guess we are maximizing the information gained.
      allowed_words_no_repitions_copy = allowed_words_no_repitions_copy %>% 
          filter(!(if_any(starts_with("V"), ~ . %in% guess_word)))
      #This condition is set to true when there is no words left after randomly guessing words and removing them from the list of unique letter words
      #Once this is set true, based on information gathered on unqiue letters in each guess, we reduces the size of allowed word list from which to guess the word
      if(nrow(allowed_words_no_repitions_copy) == 0){
        skip = TRUE
        allowed_words_copy = reduce_word_matrix(allowed_words_copy, absent_letters, common_letters, correct_position_table, incorrect_position_table)
        }
    }
    else
    {
      #Here we are guessing the words from a reduced word list
      guess_word = allowed_words_copy[sample(1:nrow(allowed_words_copy),size = 1),]
      compare_words = compare_two_words(Word_to_guess, guess_word)
      absent_letters = unique(c(absent_letters, compare_words$absent_letters))
      common_letters = unique(c(common_letters,compare_words$present_letters))
      correct_position_table = correct_position_table %>% bind_rows(compare_words$correct_letters_table %>% filter(!(positions %in% correct_position_table$positions))) %>% arrange(positions)
      incorrect_position_table = incorrect_position_table %>% bind_rows(compare_words$present_letters_wrong_table)
      allowed_words_copy = reduce_word_matrix(allowed_words_copy, absent_letters, common_letters, correct_position_table, incorrect_position_table)
    }
    # print(paste0("        ",skip, " ", guess_counter, " Guess Word : ", paste(guess_word, collapse = "")))
    # print(paste0("                "," Absent letters : ", paste(absent_letters, collapse = ",")))
    # print(paste0("                "," Present letters : ", paste(common_letters, collapse = ",")))
    # print(paste0("                "," Letters at correct positions : "))
    # print(correct_position_table)
  }
  print(paste0("Last Guess ", paste(guess_word, collapse = ""), " in ", guess_counter, " attempt"))
  print(paste0("########################################################################\n"))
  success = c(success, guess_counter)
}
success_rate = (sum(success <=6)*100)/length(success)
print(paste0("Success rate : ",success_rate))
ggplot() + aes(success)+ geom_histogram(binwidth=1, colour="black", fill="white")

end_time <- Sys.time()
time_taken = difftime(time1 = end_time, time2 = start_time, units = "auto")
print(time_taken)
