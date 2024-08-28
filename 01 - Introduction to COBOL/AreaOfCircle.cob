      *Description: This program computes the area of a circle given its radius.
      *Author: Keith Ginoel S. Gabinete
      *Date: August 28, 2024

       IDENTIFICATION DIVISION.
       PROGRAM-ID. area.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           77 MY-PI PIC 9V9(2) VALUE 3.14.
           77 RAD PIC 9(2).
           77 C-AREA PIC 9(5).9(2).
           77 S-AREA PIC Z(4)9.9(2).
           77 EXITED PIC 9 VALUE 0.

       PROCEDURE DIVISION.
      *While loop
           PERFORM UNTIL EXITED = 1
      *Ask for radius
               DISPLAY "Enter radius (0 to exit): " WITH NO ADVANCING
               ACCEPT RAD

               IF RAD = 0
                   MOVE 1 TO EXITED
               ELSE
                   PERFORM COMPUTE-AREA
               END-IF
      
      *Blank line after each iteration
               DISPLAY " "

           END-PERFORM.
           STOP RUN.

      *Compute for the area of the Circle
       COMPUTE-AREA.
           COMPUTE C-AREA = MY-PI * (RAD ** 2).
           MOVE C-AREA TO S-AREA.
           DISPLAY "The area of the "
      *hyphen (-) is required
      -    "circle is " 
           FUNCTION TRIM(S-AREA LEADING) ".".
           
