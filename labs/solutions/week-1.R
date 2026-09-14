################################################################################
# Lab 1 - R Basics 
# Possible solution for lab
################################################################################

################################################## 
# Question 1 - Basic Calculations 
##################################################

## (a)
15 + 8

# (b) 
20 - 7 

# (c) 
6 * 4 

# (d) 
25 / 5 

# (e) 
10 + 6 / 2 

# (f) 
(10 + 6) / 2


####################################################
# Question 2 - Variables
##################################################
# Create a variable containing an age 
age <- 22 
# Age next year 
age + 1 
# Age in five years 
age + 5

###################################################
# Question 3 - Vectors
####################################################
# Create the vector
scores <- c(45, 67, 82, 91, 56) 
# (a) Number of values 
length(scores) 
# (b) Largest value 
max(scores) 
# (c) Second value 
scores[2] 
# (d) Scores greater than 70 
scores[scores > 70] 
# (e) Scores greater than 50 and less than 90 
scores[scores > 50 & scores < 90] 
# (f) Mean score 
mean(scores)

##################################################
# Question 4 - Lists
#################################################

# Create the list 
student <- list(name = "Alice", 
                age = 22, 
                grades = c(75, 83, 68, 91), 
                passed = TRUE ) 
# (a) Extract the student's name 
student$name 
# (b) Extract the grades 
student$grades 
# (c) Extract the third grade 
student$grades[3] 
# (d) Add a course element 
student$course <- "Statistics" 
# Display the updated list 
student


#####################################################
# Question 5 - Functions
################################################
# Create the function 
square_number <- function(number) { 
    return(number^2) 
    } 
# Test the function 
square_number(5) 
square_number(10)

#######################################################
# Question 6 - For Loops
#####################################################

# Print numbers from 1 to 10
for (i in 1:10) { 
    print(i)
} 
# Print only even numbers 
for (i in 1:10) { 
    if (i %% 2 == 0) { 
        print(i) 
        } 
}

#############################################################
# Question 7 - Data Frames
############################################################

# Create the data frame
student_records <- data.frame( 
    student_id = 1:4, 
    name = c("Alice", "Bob", "Cathy", "David"), 
    age = c("23", "22", "21", "24"), 
    grade = c(75, 88, 79, 91) ) 

# Display the data frame
student_records 

# (a) Display the name column 
student_records$name 

# (b) Find the number of rows 
nrow(student_records) 

# (c) Add a result column 
student_records$result <- ifelse( 
    student_records$grade >= 40,
    "Pass", 
    "Fail" ) 

# Display the updated data frame 
student_records

#############################################################
# Try this question at home
##############################################################

PATH <- './words.txt'
word_list <- read.delim(PATH, sep = '\n', header = FALSE)$V1

# Function to play the hangman game
play_hangman <- function(word_list = c("apple", "banana", "cat", "dog", "horse")) {
    # Pick a random word from the word list and convert it to list.
    word <- sample(word_list, 1)
    word_split <- unlist(strsplit(word, ''))

    # Initialize the hidden word
    hidden_word <- rep('-', length(word_split))
    
    # Initialize the game state
    lives <- 7
    guessed_letters <- c()
    played_letters <- c()
    # Start the game loop
    while (lives > 0) {
        print(paste("You have", lives, "lives."))
        print(paste("You have played the letters:", played_letters))
        print(hidden_word)
        
        # Prompt the user for a letter
        letter <- readline("Guess a letter: ")

        played_letters <- paste0(c(played_letters, letter), collapse = "")
        # Check if the letter is in the word
        if (letter %in% word_split) {
            # The letter is in the word, update the game state
            print("It's in!")
            guessed_letters <- c(guessed_letters, letter)

            hidden_word[which(letter == word_split)] <- letter

            # Remove the letter from the word
            word_split <- gsub(letter, "", word_split)
            # Check if the user has guessed the entire word
            if (length(unique(word_split)) == 1) {
                # The user has won!
                print("Congratulations, you won!")
                print(paste("The word was:", word))
                break
            }
        } else {
            # The letter is not in the word, decrement the lives
            print(paste("The letter", letter , "is not in the word."))
            lives <- lives - 1
        }

        # The game is over
        if (lives == 0) {
            print("You lost!")
            print(paste("The word was:", word))
        }
    }

}

# Play the game
play_hangman()
