      *DESCRIPTION: This program computes the factorial of the largest 
      *number from an array of 5 single-digit positive integers.
      *AUTHOR: Keith Ginoel S. Gabinete
      *DATE: August 29, 2024 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. gabinete_ex1.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *Variable definitions
      *IntegerArray
      *I used 9(1) instead of 9 only for easier modification of the variable

      *Since the problem only allows positive integers as input,
      *We can initialize the num array's elements to zeros which makes it 
      *easier for us to check later if the array is still empty or not 
      *(for printing its contents)
       01 numRecord.
           02 numArr PIC 9(1) OCCURS 5 TIMES VALUE 0.
    
       77 userChoice PIC X(38).
       77 choice PIC 9(38) VALUE 1.
       77 exited PIC 9(1) VALUE 0.

       77 userInput PIC X(38).
       77 numericInput PIC 9(38).
       77 isValid PIC 9(1) VALUE 0.
       77 validInput PIC 9(1).

       77 largestNum PIC 9(1).
       77 iterator PIC 9(1) VALUE 1.

      *The largest possible factorial of a 1-digit number is 362880 (9!)
      *which is a 6-digit number
       77 nFactorial PIC 9(6).
       77 nFactorialFormatted PIC Z(5)9(1).
       
      *Start of the code
      *NOTE: COBOL indexing starts with 1
       PROCEDURE DIVISION.
           PERFORM displayMenu UNTIL exited = 1.
      *    Terminate the program
           STOP RUN.
      
      *A function to display the main menu
       displayMenu.
           DISPLAY "     MENU     ".
           DISPLAY "[1] Fill Array".
           DISPLAY "[2] Print Array".
           DISPLAY "[3] Factorial of Largest Number".
           DISPLAY "[4] Exit".

           DISPLAY "Choice: " WITH NO ADVANCING.
           ACCEPT userChoice.
           
      *    Use the function trim to remove any leading or tailing whitespaces
      *    <whiespace/s>1<whiespace/s> is accepted
           IF FUNCTION TRIM(userChoice) IS NUMERIC
      *        Use the function numval to get the actual numeric value of the 
      *        given string (since it already passed the IS NUMERIC check)     
               MOVE FUNCTION NUMVAL(userChoice) TO choice
               IF choice = 1
                   PERFORM fillNumArray
               ELSE
                   IF choice = 2
                       PERFORM printNumArray
                   ELSE
                       IF choice = 3
                           PERFORM getFactorialOfLargestNum
                       ELSE
                           IF choice = 4
                               DISPLAY " "
                               DISPLAY "EXITED!"
                               MOVE 1 TO exited
                           ELSE
                               DISPLAY " "
                               DISPLAY "Invalid Input!"
                           END-IF
                       END-IF
                   END-IF
               END-IF
           ELSE
               DISPLAY " "
               DISPLAY "Invalid Input!"
           END-IF.
      
      *    New line
           DISPLAY " ".

      *A function to fill the num array with positive integer inputs
      *ref: https://stackoverflow.com/questions/28167441/how-to-check-
      *valid-numeric-in-number-for-a-given-length
      *ref: https://stackoverflow.com/questions/60457084/how-to-validate-a-
      *negative-value-being-passed-in-as-a-alphanumeric-literal
       fillNumArray.
           DISPLAY " ".
           DISPLAY "FILL ARRAY".

           MOVE 1 TO iterator.
           PERFORM UNTIL iterator > 5
      *        Check if the input entered by the user is a valid 
      *        single digit input
      *        Loop until it encounters a valid input         
               MOVE 0 to isValid
               PERFORM UNTIL isValid = 1
                   DISPLAY "(" iterator ") " WITH NO ADVANCING
                   DISPLAY "Enter a positive integer (1-9): " 
                   WITH NO ADVANCING
                   ACCEPT userInput
                   
      *            Use the function trim to remove any leading or 
      *            tailing whitespaces
      *            <whiespace/s>8<whiespace/s> is accepted
                   IF FUNCTION TRIM (userInput) IS NUMERIC
      *                Use the function numval to get the actual 
      *                numeric value of the given string (since it 
      *                already passed the IS NUMERIC check)
                       MOVE FUNCTION NUMVAL(userInput) TO numericInput
      *                Input must be a single positive integer only
                       IF (numericInput >= 1 AND numericInput <= 9)
                           MOVE numericInput TO validInput
                           MOVE validInput TO numArr(iterator)
                           MOVE 1 TO isValid
                       ELSE
                           DISPLAY " "
                           DISPLAY "INVALID INPUT"
                           DISPLAY "Must be within the given range."
                           DISPLAY " "
                       END-IF
                   ELSE
                       DISPLAY " "
                       DISPLAY "INVALID INPUT!"
                       DISPLAY "Must be a positive integer input only."
                       DISPLAY " "
                   END-IF
               END-PERFORM

               COMPUTE iterator = iterator + 1
           END-PERFORM.

      *A function to print the numbers in the array
       printNumArray.
           DISPLAY " ".
      *    Check first if the array is still empty
           IF numArr(1) = 0
               DISPLAY "ARRAY IS STILL EMPTY!"
               DISPLAY "Fill the array with positive integers first!"
           ELSE
               DISPLAY "PRINT ARRAY"
      
               MOVE 1 TO iterator
               PERFORM UNTIL iterator > 5
                   DISPLAY numArr(iterator) " " WITH NO ADVANCING
                   COMPUTE iterator = iterator + 1
               END-PERFORM
      
               DISPLAY " "
           END-IF.

      *A function to get the value of the largest number in the
      *initialized num array
       getLargestNum.
           MOVE 1 TO iterator.
           PERFORM UNTIL iterator > 5
               IF numArr(iterator) > largestNum
                   MOVE numArr(iterator) TO largestNum
               END-IF
      
               COMPUTE iterator = iterator + 1
           END-PERFORM.

      *A function to solve for the factorial of the largest number in the array
       getFactorialOfLargestNum.
           DISPLAY " ".

           IF numArr(1) = 0
               DISPLAY "ARRAY IS STILL EMPTY!"
               DISPLAY "Fill the array with positive integers first!"
           ELSE
      *        Update the value of the largest number variable
               PERFORM getLargestNum
               
               DISPLAY "FACTORIAL OF LARGEST NUMBER"
               DISPLAY largestNum "! = " WITH NO ADVANCING

      *        Compute for the factorial of the largest number
               MOVE 1 TO nFactorial
               PERFORM UNTIL largestNum = 0
                   COMPUTE nFactorial = nFactorial * largestNum
                   COMPUTE largestNum = largestNum - 1
               END-PERFORM
           
               MOVE nFactorial TO nFactorialFormatted
               DISPLAY FUNCTION TRIM(nFactorialFormatted LEADING)
           END-IF.

