!Domenic Bianchi
!CIS 3190 - Assignment #1 Hangman
!February 3, 2016
!This program simulates a game of hangman that is capable of playing 50 rounds

program hangman

character, dimension(12,12) :: hangmanDrawingMatrix
character(len=20), dimension(50) :: wordBank
character(len=20) :: selectedWord
character, dimension(26) :: lettersUsed
character, dimension(20) :: playWord
integer, dimension(50) :: wordsUsed
character(len=20) :: wordGuess
character :: answer, letterGuess
integer :: i, numberOfGamesPlayed, wordLength, counter, game, win, inWord, hangmanStage, alreadyUsed
real :: randomNum
numberOfGamesPlayed = 1
counter = 1
game = 1
win = 0
inWord = 0
hangmanStage = 1
alreadyUsed = 0

!Initialize word bank with 50 words
wordBank = [character(20) :: 'gum','sin','for','cry','lug','bye','fly','ugly', &
    'each','from','work','talk','with','self', &
    'pizza','thing','feign','fiend','elbow','fault', &
    'dirty','budget','spirit','quaint','maiden', &
    'escort','pickax','example','tension','quinine', &
    'kidney','replica','sleeper','triangle', &
    'kangaroo','mahogany','sergeant','sequence', &
    'moustache','dangerous','scientist','different', &
    'quiescent','magistrate','erroneously', &
    'loudspeaker','phytotoxic','matrimonial', &
    'parasympathomimetic','thigmotropism']

!Initilize all elements of the array that will be keeping track of which words have been used to 0 (0 means word hasn't been used; 1 means it has been used)
do i = 1, 50

    wordsUsed(i) = 0

end do

write(*,*) 'THE GAME OF HANGMAN'
write(*,*) '---'

!Main game loop - loop until entire game is over/loop until program end
do while (numberOfGamesPlayed < 51)
    
    numberOfGamesPlayed = numberOfGamesPlayed + 1
    
    !Reset variables that are needed for the next round (round dependent)
    game = 1
    hangmanStage = 1
    counter = 1
    win = 0

    !Reset arrays that are needed for the next round
    call resetGame(lettersUsed, playWord, hangmanDrawingMatrix)
    
    !Generate a random number that will be used to select the word to be used for the round
    call generateRandomNum(randomNum, wordsUsed)
    !Get the word to be used for the round
    selectedWord = wordBank(int(randomNum))
    wordLength = LEN_TRIM(selectedWord)
   
    !Main round loop - loop until round is over
    do while (game == 1)

        !Display all letters already guessed by player
        call displayLettersUsed(lettersUsed, counter)

        !Prompt user to guess a letter
        write (*,*) 'What is your guess' !*********R=0??
        read (*,*) letterGuess

        !Check letter to make sure it has not already been guessed
        call checkLetter(letterGuess, lettersUsed, alreadyUsed)
        
        !If the letter has not been guessed
        if (alreadyUsed == 0) then
            
            !Add letter to array containing all the letters guessed
            lettersUsed(counter) = letterGuess
            counter = counter + 1
            
            !Check if letter is in word
            call isLetterInWord(letterGuess, playWord, selectedWord, wordLength, win, inWord)
        end if

        !If the letter has not been guessed and is not in the word
        if (inWord == 0 .and. alreadyUsed == 0) then

            write(*,*) 'Sorry, that letter is not in the word.'

            !Add the body part to the hangman matrix
            call updateHangman(hangmanDrawingMatrix, hangmanStage)

            !Draw the hangman to the screen
            call drawHangman(hangmanDrawingMatrix)

        !If the letter is in the word and not already guessed but not all letters in the word have been guessed
        else if (inWord == 1 .and. alreadyUsed == 0 .and. win == 0) then

            inWord = 0
           
            !Display the word with the letters that have been guessed correctly but keep the unguessed letters hidden
            write (*,*) playWord(1:wordLength)

            !Prompt user to guess for the complete/full word
            write(*,*) 'What is your guess for the word?'
            read(*,*) wordGuess

            !If the user guessed the word, they win and the round is over
            if (wordGuess == selectedWord) then
                win = 1
            else
                write(*,*) 'Wrong. Try another letter'
            end if
        end if
	
        !User guessed the word or guessed all the letters in the word (win)
        if (win == 1) then
           
            write (*,*) selectedWord(1:wordLength)
            write(*,*) 'Right! It took you ', counter-1, 'guesses.'

            !Prompt user to play again
            write(*,*) 'Do you want another word? (Y/N)'
            call playAgainPrompt(answer)
        
        !If the entire hangman has been drawn
        else if (hangmanStage == 11) then

            write (*,*) 'Sorry, you loose. The word was ', selectedWord
            write (*,*) 'You missed that one'

            !Prompt user to play again
            write (*,*) 'Do you want another word? (Y\N)'
            call playAgainPrompt(answer)

        end if
	
        !If the user does not want to play agin
        if (answer .eq. 'N') then
            numberOfGamesPlayed = 52
            game = 0
            write (*,*) 'Its been fun! Bye for now'
        else if (win == 1 .or. answer .eq. 'Y') then
            game = 0
            answer = ' ' 
        end if
    end do
end do

if (numberOfGamesPlayed == 51) then

    write (*,*) 'You did all the words'

end if

write (*,*) 'Ending...'

end

!This subroutine clears the arrays containing all of the letters guessed, the hangman, and word in order to get ready for the next round
subroutine resetGame(lettersUsed, playWord, hangmanDrawingMatrix)
    
    implicit none
    character, dimension(26), intent(inout) :: lettersUsed
    character, dimension(20), intent(inout) :: playWord
    character, dimension(12,12), intent(inout) :: hangmanDrawingMatrix
    integer :: i, j

    !Set the entire drawing matrix to blanks and draw the pole/hanger
    do i = 1, 12
        do j = 1, 12
            !Print top of hanger
            if (i == 1 .and. j < 8) then
                hangmanDrawingMatrix(1,j) = 'X'
            else
                hangmanDrawingMatrix(i,j) = ' '
            end if
        end do
        !Print vertical portion of hanger
        hangmanDrawingMatrix(i,1) = 'X'
    end do

    !Set the "rope"
    hangmanDrawingMatrix(2,7) = 'X'

    !Set all the array elements of the play word to a dash to represent a hidden letter
    do i = 1, 20
        playWord(i) = '-'
    end do

    !Clear all the letters used to prepare for a new game
    do i = 1, 26
        lettersUsed(i) = ' '
    end do

end subroutine resetGame

!This subroutine draws the hangman to the screen
subroutine drawHangman(hangmanDrawingMatrix)

    implicit none
    integer :: i, j
    character, dimension(12,12), intent(in) :: hangmanDrawingMatrix

    !Loop through the 12 by 12 array and print out each index in order
    do i = 1, 12
        write (*,*) (hangmanDrawingMatrix(i,j), j=1,12)
    end do

end subroutine drawHangman

!This subroutine adds the appropriate body part to the hangman after a incorrect letter is guessed
subroutine updateHangman(hangmanDrawingMatrix, stage)

    implicit none
    integer, intent(inout) :: stage
    integer :: i
    character, dimension(12,12), intent(inout) :: hangmanDrawingMatrix

    select case(stage)
        !Add head
        case(1)
            write(*,*) 'First we draw a head'
            hangmanDrawingMatrix(3,6) = '-'
            hangmanDrawingMatrix(3,7) = '-'
            hangmanDrawingMatrix(3,8) = '-'
            hangmanDrawingMatrix(4,5) = '('
            hangmanDrawingMatrix(4,6) = '.'
            hangmanDrawingMatrix(4,8) = '.'
            hangmanDrawingMatrix(4,9) = ')'
            hangmanDrawingMatrix(5,6) = '-'
            hangmanDrawingMatrix(5,7) = '-'
            hangmanDrawingMatrix(5,8) = '-'
        !Body
        case(2)
            write(*,*) 'Now we draw a body'
            do i = 6, 9
                hangManDrawingMatrix(i,7) = 'X'
            end do
        !Left Arm
        case(3)
        write(*,*) 'Next we draw an arm'
            do i = 4, 7
                hangmanDrawingMatrix(i,i-1) = '\'
            end do
        !Right Arm
        case(4)
            write(*,*) 'This time it is the other arm'
            hangmanDrawingMatrix(7,8) = '/'
            hangmanDrawingMatrix(6,9) = '/'
            hangmanDrawingMatrix(5,10) = '/'
            hangmanDrawingMatrix(4,11) = '/'
        !Left Leg
        case(5)
            write(*,*) 'Now, lets draw the left leg'
            hangmanDrawingMatrix(10,6) = '/'
            hangmanDrawingMatrix(11,5) = '/'
        !Right Leg
        case(6)
            write(*,*) 'This time we draw the right leg'
            hangmanDrawingMatrix(10,8) = '\'
            hangmanDrawingMatrix(11,9) = '\'
        !Right Hand
        case(7)
            write(*,*) 'Now we put up a hand'
            hangmanDrawingMatrix(3,11) = '\'
        !Left Hand
        case(8)
            write(*,*) 'Next the other hand'
            hangmanDrawingMatrix(3,3) = '/'
        !Right Foot
        case(9)
            write(*,*) 'Now we draw one foot'
            hangmanDrawingMatrix(11,10) = '-'
        !Left Foot
        case default
            write(*,*) 'Here is the other foot -- You are hung!!'
            hangmanDrawingMatrix(11,4) = '-'
    end select

    stage = stage + 1

end subroutine updateHangman

!This subroutine prompts the player if they want to play again (new word) or end the game (end the program)
subroutine playAgainPrompt(answer)

    implicit none
    character, intent(inout) :: answer

    !Prompt the user until they enter Y or N
    do while (answer .ne. 'Y' .and. answer .ne. 'N')

        read(*,*) answer
    end do

end subroutine playAgainPrompt

!This subroutine checks if the letter guessed is in the word
subroutine isLetterInWord(letter, word, hiddenWord, length, win, inWord)

    implicit none
    integer :: i
    integer, intent(in) :: length
    integer, intent(inout) :: win
    integer, intent(inout) :: inWord
    character, intent(in) :: letter
    character, intent(in), dimension(20) :: hiddenWord
    character, intent(inout), dimension(20) :: word

    win = 1;
	
    !Loop through all characters of the word and check if the character is equal to letter that was guessed
    do i = 1, length

    if (hiddenWord(i) == letter) then
           !Replace '-' character with the letter (reveal the character to the player)
           word(i) = letter
           inWord = 1
        end if
    end do
        
    !Loop through the array that is used to display the word to the player (letters that haven't been guessed are shown as "-")
    do i = 1, length

        !If a character has not been revealed to the player, then the user has not yet won the round
        if (word(i) .eq. '-') then
             win = 0
        end if
    end do

end subroutine isLetterInWord

!This subroutine determines if the letter inputed has already been guessed
subroutine checkLetter(letter, usedLetters, alreadyUsed)

    implicit none
    integer :: i
    integer, intent(out) :: alreadyUsed
    character, intent(in) :: letter
    character, intent(in), dimension(26) :: usedLetters
    alreadyUsed = 0

    !Loop through all the array containing all the letters the user has already guessed
    do i = 1, 26

        !If the letter at the array index matches the letter that has been guessed, it means the letter has already been guessed
        if (usedLetters(i) == letter) then
            write (*,*) 'You guessed that letter before'
            !Set flag that will be used outside of this subroutine to determine if the letter was guessed before or not
            alreadyUsed = 1
            exit
        end if
    end do

end subroutine checkLetter

!This subroutine displays all letters that the user has already guessed
subroutine displayLettersUsed(letters, numberOfLetters)

    implicit none
    integer :: i
    integer, intent(in) :: numberOfLetters
    character(len=1), intent(in), dimension(26) :: letters

    write(*,*) 'Here are the letters you used:'
    
    !Loop through the array containing the letters the user guessed and print them to the screen
    do i = 1, numberOfLetters-1
        write (*,'(AA$)') letters(i), ','
    end do

    !Print new line
    write (*,*) ' '

end subroutine displayLettersUsed

!This subroutine generates a random number (integer) between 0 and 50
subroutine generateRandomNum(randomNum, wordsUsed)

    implicit none
    real, intent(inout) :: randomNum
    integer, dimension(5), intent(inout) :: wordsUsed
    integer :: k, i

    ! Declare an assumed shape, dynamic array
    integer, dimension(:), allocatable :: seed
    integer, dimension(8) :: values
    
    i = 0

    do while (i == 0)
 
        ! gfortran subroutine to return date and time information from the
        ! real time system clock. Works down to milliseconds and stores
        ! the eight return values in array values.
        call date_and_time(VALUES=values)

        ! restart the state of the pseudorandom number generator
        ! k = minimum size of seed (12 on my system)
        call random_seed(size=k)

        ! allocate memory to seed
        allocate(seed(k))

        ! assign information in values to seed
        seed(:) = values(:)

        ! seed the random number generator
        call random_seed(put=seed)

        call random_number(randomNum)
        randomNum = ceiling(randomNum*50)

        !If the index at wordsUsed equals 1, that means that the word at that index has already been used in a previous round.
        if (wordsUsed(int(randomNum)) == 0) then
            wordsUsed(int(randomNum)) = 1
            exit
        end if
    end do
end subroutine generateRandomNum
