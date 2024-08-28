      *DESCRIPTION: This program computes for the factorial of a number n.
      *AUTHOR: Keith Ginoel S. Gabinete
      *DATE: August 28, 2024 
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
           
       77 choice PIC 9(1) VALUE 1.
       77 exited PIC 9(1) VALUE 0.

       77 userInput PIC X(2).
       77 isValid PIC 9(1) VALUE 0.
       77 validInput PIC 9(1).

       77 largestNum PIC 9(1).
       77 iterator PIC 9(1) VALUE 1.

      *The largest possible factorial of a 1-digit number is 362880 (9!)
      *which is a 6-digit number
       77 nFactorial PIC 9(6).
       
      *Start of the code
      *NOTE: COBOL indexing starts with 1
       PROCEDURE DIVISION.
           PERFORM displayMenu UNTIL exited = 1.
      *Terminate the program
           STOP RUN.
      
      *A function to display the main menu
       displayMenu.
           DISPLAY "     MENU     ".
           DISPLAY "[1] Fill Array".
           DISPLAY "[2] Print Array".
           DISPLAY "[3] Factorial of Largest Number".
           DISPLAY "[4] Exit".

           DISPLAY "Choice: " WITH NO ADVANCING.
           ACCEPT choice.

           IF choice = 1
               DISPLAY "OKAY"
           ELSE
               IF choice = 2
                   PERFORM printNumArray
               ELSE
                   IF choice = 3
                       DISPLAY "ALRIGHT"
                   ELSE
                       DISPLAY "EXIT"
                       MOVE 1 TO exited
                   END-IF
               END-IF
           END-IF.
      
      *New line
           DISPLAY " ".


      *A function to fill the num array with positive integer inputs
      *ref: https://stackoverflow.com/questions/28167441/how-to-check-
      *valid-numeric-in-number-for-a-given-length
       fillNumArray.
           DISPLAY " ".
           DISPLAY "FILL ARRAY".

           MOVE 1 TO iterator.
           PERFORM UNTIL iterator > 5
               DISPLAY "Enter a positive integer (1-9): " 
               WITH NO ADVANCING
               

               COMPUTE iterator = iterator + 1
           END-PERFORM.

      *A function to print the numbers in the array
       printNumArray.
           DISPLAY " ".
      *Check first if the array is still empty
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

      *A function to update the value of the largest number
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
           PERFORM getLargestNum.

           MOVE 1 TO nFactorial.
           PERFORM UNTIL largestNum > 0
               COMPUTE nFactorial = nFactorial * largestNum
               COMPUTE largestNum = largestNum - 1
           END-PERFORM.
           
           DISPLAY nFactorial.
