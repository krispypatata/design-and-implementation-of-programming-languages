      *DESCRITPION: A program that computes the sum of two numbers.
      *AUTHOR: Keith Ginoel Gabinete
      *DATE: August 28, 2024
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLE.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           77 EXITED PIC 9 VALUE 0.
           77 CHOICE PIC 9.
           77 X PIC 9(5).
           77 Y PIC 9(5).
           77 S PIC 9(5).

       PROCEDURE DIVISION.
           PERFORM PMENU UNTIL EXITED = 1.
           STOP RUN.

      * function definition
           PMENU.
           DISPLAY "MENU ".
           DISPLAY "[1] Add two numbers ".
           DISPLAY "[2] Exit ".
           DISPLAY "Choice : " WITH NO ADVANCING.
           ACCEPT CHOICE.
           
           IF CHOICE = 1
               DISPLAY "Enter X: " WITH NO ADVANCING
               ACCEPT X
               DISPLAY "Enter Y: " WITH NO ADVANCING
               ACCEPT Y
               COMPUTE S = X + Y
               DISPLAY "Sum is " S
           ELSE
               MOVE 1 TO EXITED
           END-IF.
